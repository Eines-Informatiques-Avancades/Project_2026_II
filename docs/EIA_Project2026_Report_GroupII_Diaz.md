## Monte Carlo Engine & Geometry
**Contributor: ManelDC55**

The primary contribution to the project consisted of the development of the core Monte Carlo engine, specifically focusing on the implementation of proposal moves and the geometric transformations required to explore the polymer's phase space efficiently.

### Implementation of the Pivot Move
To allow the polyethylene chain to change its conformation, the **Pivot Move** algorithm was implemented within the `rotate_dihedral` subroutine. Unlike local moves, the pivot move selects a random bond along the chain and rotates the entire subsequent segment (the "tail") as a rigid body.

To perform this 3D rotation without distorting bond lengths or bond angles, **Rodrigues' Rotation Formula** was utilized. This equation allows the rotation of any vector $\vec{v}$ (representing an atom's position relative to the pivot) around an arbitrary unit axis $\vec{k}$ (the chosen C-C bond) by an angle $\phi$:

$$\vec{v}_{rot} = \vec{v} \cos\phi + (\vec{k} \times \vec{v}) \sin\phi + \vec{k} (\vec{k} \cdot \vec{v}) (1 - \cos\phi)$$

### Handling $sp^3$ Hybridization in All-Atom (AA) Mode
A significant technical requirement was the transition from the United-Atom (UA) model to the **All-Atom (AA)** model. When explicit hydrogens are included, the geometry becomes highly restrictive due to the $sp^3$ hybridization of the carbon atoms.

The rotation logic was designed to ensure that when a carbon atom rotates, its two attached hydrogens rotate with the exact same angle and axis. This synchronization is essential to maintain constant C-H bond lengths and C-C-H angles, preventing non-physical atomic overlaps that would otherwise lead to the immediate rejection of the move by the Metropolis criterion.

```fortran
! Snippet from rotate_dihedral handling explicit hydrogens
if (explicit_h) then
  ! Hydrogens are stored after the carbon backbone in the coords array
  do i = n_carbons + 1, n_atoms
    do j = k + 1, n_carbons
      ! Identify if hydrogen i is bonded to the moving carbon j
      v = coords(i, :) - coords(j, :)
      if (vnorm(v) < 1.2d0) then 
        ! Apply Rodrigues' rotation relative to the pivot point
        v = coords(i, :) - pivot
        dot_uv = sum(axis * v)
        v_rot = v * cos_p + cross(axis, v) * sin_p + axis * dot_uv * (1.0d0 - cos_p)
        coords_new(i, :) = pivot + v_rot
        exit ! Hydrogen found, move to the next atom
      end if
    end do
  end do
end if


## Parallel Equilibration via Collaborative Population MPI

The primary contribution to this component of the project was the design and implementation of a **collaborative parallel equilibration** strategy, integrated into the master/worker framework of `main_parallel_star_equil_collab.f90`. The goal was to accelerate the equilibration phase — which can require millions of MC steps — by distributing the conformational search across multiple MPI workers that cooperate at regular synchronization points.

### Strategy:

Rather than assigning a single worker to equilibrate each configuration type independently, a group of workers (controlled by `workers_per_equil`) is assigned to each configuration type simultaneously. Each worker starts from the same initial geometry but with a different random number seed (`rng_seed + rank * 104729`), ensuring that trajectories immediately diverge and explore different regions of conformational space.

Every `sync_interval` MC steps, a synchronization protocol is executed. Each worker in the group sends its current total energy $E$ and radius of gyration $R_g$ to the master, which then selects the most representative configuration using a Boltzmann-weighted roulette wheel and broadcasts it back to all workers in the group. This allows the population to collectively converge toward the equilibrium region faster than any single trajectory would alone but allowing also to explore possible higher energies.

### Replica Selection with Boltzmann Roulette Wheel

At each synchronization point, the master computes a statistical weight for each replica $i$ based on its energy $E_i$:

$$w_i = \exp\!\left(-\frac{E_i - E_{\min}}{k_B \, T_{\text{virt}}}\right)$$

where $E_{\min}$ is the lowest energy observed in the current group and $T_{\text{virt}}$ is a **virtual temperature** (set to 3000 K), which is a purely algorithmic parameter with no physical meaning — it controls how aggressively low-energy replicas are favoured. A low $T_{\text{virt}}$ makes the selection nearly equivalent to always choosing the minimum-energy replica, while a high value gives all replicas nearly equal probability. The value 3000 K was chosen to maintain meaningful diversity across replicas while still biasing towards lower-energy configurations.

A uniformly distributed random number is then used to sample from this distribution (roulette wheel sampling), and the selected replica's coordinates are broadcast to all workers via point-to-point MPI calls.

```fortran
! Boltzmann roulette wheel selection
E_min_grp = minval(grp_energies)
do w = 1, workers_per_equil
  grp_boltz(w) = exp(-(grp_energies(w) - E_min_grp) / (kb * T_virt))
end do
tw = sum(grp_boltz)
call random_number(r_pick)
accum = 0.0d0
best_local = workers_per_equil
do w = 1, workers_per_equil
  accum = accum + grp_boltz(w) / tw
  if (r_pick <= accum) then
    best_local = w
    exit
  end if
end do
```

### Convergence Detection with Geweke Z-Test

The original implementation used the Welch t-test to detect equilibrium by comparing the means of two consecutive blocks of energy samples. While effective in some cases, the Welch test has a fundamental limitation for Markov chain data: it assumes that samples within each block are statistically independent, which is not true in correlated MC trajectories.

The Geweke diagnostic was adopted as a more rigorous alternative. It is specifically designed for convergence testing of Markov chains and accounts for sample autocorrelation through batch means variance estimation. The test compares the mean of the first $f_A = 10\%$ of an accumulated energy buffer against the mean of the last $f_B = 50\%$:

$$z = \frac{\bar{x}_A - \bar{x}_B}{\sqrt{SE_A^2 + SE_B^2}}$$

The standard errors $SE_A$ and $SE_B$ are not computed from raw sample variance but from **batch means**: each window is divided into $\lfloor\sqrt{n}\rfloor$ batches, and the variance of the batch means is used as the variance estimator. This correctly accounts for the autocorrelation structure of the MC chain.

A buffer of $n_{\text{Geweke}} = 300$ energy samples is maintained as a sliding window. The Geweke test is evaluated every `eval_freq` synchronization points once the buffer is full. Equilibrium is declared only when the test passes $n_{\text{consec}} = 3$ consecutive times, avoiding false positives caused by temporary plateaus.

```fortran
! Batch means SE for window A (first fA% of buffer)
nA     = max(2, n_geweke * fA_pct / 100)   ! = 30 samples
bA     = max(2, int(sqrt(dble(nA))))        ! number of batches
bsA    = nA / bA                            ! batch size
seA_E  = 0.0d0
do ib = 1, bA
  bm    = sum(tmp_E((ib-1)*bsA+1 : ib*bsA)) / dble(bsA)
  seA_E = seA_E + (bm - meanA_E)**2
end do
seA_E = seA_E / dble(bA * (bA - 1))        ! SE² of window A mean

! Geweke z-statistic
z_E = abs(meanA_E - meanB_E) / sqrt(seA_E + seB_E)

if (z_E < z_crit) then   ! z_crit = 1.96 (95% confidence)
  consec_passes(task) = consec_passes(task) + 1
  if (consec_passes(task) >= n_consec) equil_done(task) = .true.
else
  consec_passes(task) = 0  ! reset counter on failure
end if
```

The radius of gyration $R_g$ is also monitored with its own Geweke z-statistic and reported in the output, but it is deliberately excluded from the stopping criterion. In practice, $R_g$ was found to equilibrate at a different rate than the energy and would prevent convergence from being declared even when the energy had clearly stabilized.

### MPI Communication Protocol

The synchronization uses only point-to-point MPI operations (`MPI_Send` / `MPI_Recv`) to avoid the need for temporary communicators or collective operations across worker subgroups. At each sync point, the master loops over all $K$ workers in the active equilibration group:

| Step | Direction | Tag | Content |
|---|---|---|---|
| 1 | worker → master | `TAG_SYNC_OBS` | `[E_total, Rg2]` |
| 2 | master → worker | `TAG_SYNC_CTRL` | `[ctrl_signal, best_local_idx]` |
| 3 | worker → master | `TAG_EQUIL_COORDS` | full coordinate array |
| 4 | master → worker | `TAG_SYNC_COORDS` | winning coordinates |

Once the Geweke test declares equilibrium, the master saves the equilibrated geometry to `../results/equilibrated_cX.xyz` and unlocks the 10 production jobs for that configuration type in the task queue.