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
To be implemented...