## Energy Evaluation & Force Fields
**Contributor: Oliwier Misztal (omisztal)**

My primary responsibility within the project was the design and implementation of the energy evaluation modules, encompassing both `energy.f90` and `energy_all_atoms.f90`. These modules compute the system's total energy and handle the $\Delta E$ calculations required for the Metropolis acceptance/rejection criteria during Monte Carlo sampling. 

### United-Atom (UA) Model & TraPPE-UA Optimization
For the United-Atom implementation, the focus was on computational throughput. The energy routines are called continuously during the MC loop, making performance highly dependent on the efficiency of these calculations.

**Trigonometric Bypasses in Dihedral Calculations**
Calculating torsional energy typically requires evaluating the dihedral angle via expensive `acos` and `atan2` functions. To avoid this overhead, the `compute_cos_dihedral` function was written to compute the cosine of the angle directly using vector cross products and dot products of the normal vectors defined by the carbon backbone. 

```fortran
! Snippet from energy.f90: Direct cosine calculation
pure function compute_cos_dihedral(r1, r2, r3, r4) result(cos_phi)
  ! ... variable declarations ...
  b1 = r2 - r1
  b2 = r3 - r2
  b3 = r4 - r3

  n1 = cross_product(b1, b2)
  n2 = cross_product(b2, b3)

  nn1 = sum(n1**2)
  nn2 = sum(n2**2)

  ! Guard against collinear atoms (zero-length normals)
  if (nn1 < 1.0d-28 .or. nn2 < 1.0d-28) then
    cos_phi = 1.0d0   ! default to trans
    return
  end if

  ! cos(phi) = (n1 . n2) / (|n1| * |n2|)
  cos_phi = dot_product(n1, n2) / (sqrt(nn1) * sqrt(nn2))
end function compute_cos_dihedral
```

**Polynomial Expansion for TraPPE-UA**
The TraPPE-UA potential was integrated using the parameters defined by Martin & Siepmann (*J. Phys. Chem. B 102, 2569, 1998*). The original potential is defined using trigonometric functions: $$U(\phi) = c_1(1 + \cos\phi) + c_2(1 - \cos(2\phi)) + c_3(1 + \cos(3\phi))$$ By substituting $y = \cos\phi$ and applying trigonometric identities, the expression was reduced to a polynomial. This allows the torsion energy to be evaluated using only basic multiplication and addition, entirely eliminating transcendental function calls in the MC loop.

### Explicit Hydrogens and the OPLS-AA Force Field
The transition to the explicit hydrogen model required handling a significantly more complex topology. To accurately capture the steric hindrance and phase behavior of all-atom polyethylene, the **OPLS-AA force field** was implemented. This represents a significant increase in computational complexity over the UA model, requiring specialized algorithmic workarounds.

**The OPLS-AA Parameterization & Effective Backbone Potential**
In a standard OPLS-AA implementation, rotating a single C-C bond in a polymer chain requires calculating the torsional energy of **9 distinct dihedrals** crossing that bond: one C-C-C-C, four C-C-C-H, and four H-C-C-H interactions. Evaluating all 9 angles at every Monte Carlo step would be computationally prohibitive and bottleneck the simulation.

To solve this, I implemented an **"Effective Backbone Potential"** based on the optimized OPLS-AA framework for polyethylene developed by Sæther et al. (*Macromolecules 2021*). This mathematical formulation collapses the energetic contributions of all 9 dihedrals into a single effective torsional equation that only requires the coordinates of the carbon backbone (C-C-C-C). 

Using the pre-computed `opls_c1`, `opls_c2`, and `opls_c3` effective parameters, the code retains the rigorous physical accuracy of the all-atom force field while operating at the speed of a united-atom calculation. The polynomial expansion used in the UA model was adapted for these new OPLS-AA coefficients:

```fortran
! Snippet from energy_all_atoms.f90: OPLS-AA Effective Potential evaluation
pure function torsion_single(cos_phi) result(e)
  double precision, intent(in) :: cos_phi
  double precision :: e, y, y2, y3

  y = cos_phi   ! corresponds to trans = -1, cis = 1
  y2 = y * y
  y3 = y2 * y

  ! OPLS-AA evaluation using trigonometric identities
  ! cos(2x) = 2*cos^2(x) - 1 => 1 - cos(2x) = 2 - 2y^2
  ! cos(3x) = 4*cos^3(x) - 3*cos(x) => 1 + cos(3x) = 1 - 3y + 4y^3
  e = opls_c1 * (1.0d0 + y) &
    + opls_c2 * (2.0d0 - 2.0d0 * y2) &
    + opls_c3 * (1.0d0 - 3.0d0 * y + 4.0d0 * y3)
end function torsion_single
```

**Topology Mapping and Exclusion Matrix**
In the AA model, non-bonded Lennard-Jones interactions must be excluded for atoms separated by fewer than four bonds (1-2, 1-3, and 1-4 interactions) to prevent double-counting energies already captured by bond/angle/torsion parameters. An initialization routine, `init_energy_topology`, was built to dynamically map hydrogen atoms to their parent carbon atoms based on geometric distance. This mapping is then used to construct a global boolean matrix (`is_excluded`) that instantly determines if a given C-C, C-H, or H-H pair should bypass LJ evaluation.

**Dynamic Atom Lists for Rigid-Body Moves**
For the `delta_energy` subroutine in the AA model, recalculating the entire energy of the chain or blindly checking every atom pair is highly inefficient. Instead, the algorithm dynamically identifies which atoms moved during a pivot step and sorts their indices into stack-allocated arrays (`fixed_list` and `moved_list`). 

```fortran
! Snippet from energy_all_atoms.f90: Dynamic lists for delta E
n_fixed = 0
n_moved = 0
do i = 1, n_atoms
   if (sum((coords_new(i,:) - coords_old(i,:))**2) > 1.0d-12) then
     n_moved = n_moved + 1
     moved_list(n_moved) = i
   else
     n_fixed = n_fixed + 1
     fixed_list(n_fixed) = i
   end if
end do
```
By isolating these lists, the $\Delta E$ loop exclusively processes LJ interactions between atoms that have actually changed their relative distance. Using a secondary `pair_type_matrix`, the code instantly routes valid interactions to the appropriate C-C, C-H, or H-H pre-computed Lennard-Jones parameters, ensuring high throughput during dense all-atom calculations.

### Parallelization

#### `compute_total_energy`

The dominant cost in `compute_total_energy` is the $\mathcal{O}(N^2)$ double loop over non-bonded LJ pairs. Each iteration is independent — it reads a pair of coordinates, evaluates `lj_pair_energy`, and adds to a running sum — which makes it a natural fit for OpenMP worksharing. The outer loop over `i` is distributed across threads with `!$omp parallel do`, with the inner index `j`, the squared distance `r2`, and the dihedral cosine `cos_phi` declared private to prevent cross-thread interference. The global accumulators `e_lj` and `e_tors` are aggregated safely with a `reduction(+:...)` clause.

The two loops inside the subroutine have different iteration structures and were scheduled accordingly. The LJ pair loop has a triangular iteration space (the inner bound depends on `i`), so `schedule(dynamic, 10)` is used to prevent faster threads from sitting idle while slower ones finish large inner loops. The torsional loop over the carbon backbone is uniform in length and uses the default static scheduling, which has lower overhead when the workload is balanced.

Both parallel regions are wrapped in an `if(omp_total_energy)` runtime guard. In the current implementation, `omp_total_energy` is not read from the input file; it is enabled or disabled by the build configuration via the OpenMP-related compile-time settings in `src/lib/parameters.f90`. This means serial and OpenMP-enabled behavior are selected at compile time rather than toggled from `confs/input.dat`.

#### `delta_energy`

The `delta_energy` subroutine is the true bottleneck of the MC loop — it is called once per accepted or rejected move, millions of times per simulation. The key algorithmic feature here is the dynamic partitioning of atoms into `fixed_list` and `moved_list` at the start of each call. Only cross-interactions between fixed and moved atoms can have changed during a pivot, so the LJ loop processes exclusively those pairs rather than the full $\mathcal{O}(N^2)$ set.

The two nested loops over `f` (fixed) and `m` (moved) form a perfectly rectangular iteration space, which makes `collapse(2)` effective: it merges both loops into a single flat range of `n_fixed × n_moved` iterations and distributes them evenly across threads. Since pivot moves near chain ends move very few atoms, spawning threads for a handful of pairs would cost more than it saves. The parallel block is therefore gated by `if(omp_delta_energy .and. (n_fixed * n_moved > 1000))`, ensuring parallelization is only activated when the workload genuinely justifies it.

#### Benchmarking Code

To quantify the effect of thread count on both energy routines, a dedicated benchmark program `main_bench_total.f90` was written. It performs 10,000 back-to-back calls to `compute_total_energy` for a given chain size and reports the wall time via MPI_Wtime. Two shell scripts automate the full benchmark sweeps: `4.run_omp_total_test.sh` compiles and runs the benchmark binary across thread counts of 1, 2, 3, and 4 for chain sizes of 20, 50, 100, and 500 carbons, writing results to a CSV in `results/bench_omp_total/`. 

`5.run_omp_delta_test.sh` follows the same structure but targets `delta_energy` by running full 10M-step MC simulations via `main_parallel_replicas.x` and extracting wall time from the CPU output files. Both scripts are written for the `cerqt01.q` cluster queue and load the appropriate MPI module. The results are plotted by `plot_benchmarks.py`, which reads both CSVs and produces per-chain-size scaling plots as PDFs.

SIMD Exploration

An attempt was made to additionally exploit single-core SIMD vectorization on the `lj_pair_energy` function using the `!$omp declare simd` directive. The idea was to generate a vectorized variant of the function callable from an `!$omp simd`-annotated inner loop, allowing each thread to process multiple atom pairs simultaneously using AVX registers. In practice, activating this required structural changes to `delta_energy` that conflicted with the existing threading design: the `collapse(2)` clause and the inner-loop cycle on `is_excluded` are incompatible with SIMD regions, and removing them to support vectorization degraded the thread load balancing that `collapse(2)` provides. Given that the OpenMP threading was the primary parallelization goal, the SIMD experiment was abandoned and the directive was removed from the code. It remains a direction worth revisiting, particularly if the pair loops were refactored to use pre-filtered pair lists that eliminate the exclusion branch entirely.