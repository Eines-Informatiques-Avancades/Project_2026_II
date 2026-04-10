## Initialization, I/O & Analysis Workflow
**Contributor: Itxaso Muñoz-Aldalur (itxasoma)**
My contribution started with the initialization side of the project. I developed the `initial_conf` module to generate the starting conformation of the polyethylene chain, first at the only-carbon model level and later, when requested, with explicit hydrogens added for visualization and all-atom runs.

I then implemented the `io` module to keep the workflow simple and reproducible. This module reads the simulation parameters from a modifiable file named `input.dat`, applies default values, strips comments, analyzes user options such as `n_carbons`, `n_steps`, `explicit_h`, `conf_type`, and `rng_seed`, and writes the generated structures and trajectories in XYZ format.

## Initial Configuration Design

A central part of my work consisted of constructing different initial geometries for the polymer chain. The builder is based on internal coordinates, with fixed bond length, fixed bond angle, a selectable dihedral angle, and an overlap check to avoid unphysical self-intersections during chain creation.

I implemented several configuration modes with different physical purposes: fully planar all-trans (`conf_type = 1`), fully random dihedrals (`conf_type = 2`), a simple RIS-like discrete model (`conf_type = 3`), a spring/helix-like case proposed by ai-murphy (`conf_type = 4`), and a perturbed trans case intended as a slight out of plane variation of the planar geometry (`conf_type = 5`, added as a small variation of `conf_type = 1`). I also refined the hydrogen model so that internal CH\(_2\) hydrogens are no longer coplanar with the C-C-C backbone, but instead are placed in a more chemically consistent out of plane arrangement, compatible with a local tetrahedral geometry.

This variety of initial configurations provides a broad set of tools to explore the conformational space of the polymer. This is especially relevant when the simulation is tested at different temperatures, since some initial states may lie closer to equilibrium than others.

## Driver Improvements & Scientific Output

I also edited the serial driver program to make the production runs easier to analyze. In particular, I implemented an annealing schedule in the main Monte Carlo loop to study the approach to low-temperature equilibrium (for example, from 500 K down to 300 K), and I added systematic string trimming in the output naming so that result files, trajectories, and plots carry a clean identifier of the simulation conditions.

Although annealing was not part of the requested production setup, which was based on constant-temperature simulations, it helped us verify that the rest of the code was behaving consistently. In particular, we observed that, as the temperature approaches zero, the configurations tend toward `conf_type = 1`, provided that they do not become trapped in twisted or tangled states during the evolution, as expected from the energetic model used.

In the same driver, I introduced `cpu_time` calls to monitor the execution time of the serial code. This provides a basic performance reference for later comparison with the MPI-parallel version, which is an important step in evaluating the actual benefit of parallelization. To make timing comparisons fair and consistent, the serial replica driver (`main_serial_replicas.f90`) was further updated to use wall-clock timing equivalent to `MPI_Wtime`, so that serial and parallel runtimes are measured in the same way.

On the analysis side, I improved the Python plotting script with a more scientific visual style. Besides energy, structural observables, and torsion distributions, I added an integrated autocorrelation-time estimate and an equilibration detector so that the torsion histograms are built only from the equilibrated production region. This follows standard practice based on autocorrelation analysis and automated equilibration detection aimed at maximizing the effectively uncorrelated sample count, based on [Chodera Lab (2015)](https://www.choderalab.org/publications/2015/6/30/a-simple-method-for-automated-equilibration-detection-in-molecular-simulations).

Finally, I also created a local Python environment and added the requirements to the `README.md`, so that all users can run the Python analysis scripts and produce the same plots. This included updating the `README.md` with a LaTeX requirement and adding `.venv` and `../bin/` to `.gitignore` to keep the repository clean.

## Cluster Compatibility

To ensure that the code could run on shared HPC infrastructure, I adapted the build system and submission scripts for cluster environments. This involved creating a Fortran 90-compatible `Makefile` by replacing `open(newunit=...)` with explicit `open(unit=...)` statements for compatibility with compilers that do not support the Fortran 2008 syntax. I also prepared a `1.run.sh` script that loads the appropriate GCC module on the CERQT2 cluster.

In addition, the `parameters.f90` file was adapted to work correctly on the cluster, including restoring the `#ifdef` preprocessor block and adding a comment in the `Makefile` explaining why `-I$(MPI_INC_DIR)` is commented out in certain build configurations.

## Parallelization

The MPI parallelization strategy I implemented is intentionally focused on independent replicas rather than on parallelizing the energy evaluation or the Monte Carlo step itself, as those directions were explored separately by omisztal and ManelDC55, respectively. Starting from `main_serial.f90`, I produced `main_parallel_replicas.f90`, in which three MPI ranks are launched simultaneously, each running the same Monte Carlo protocol with the same thermodynamic parameters but with a different initial configuration: `conf_type = 1`, `4`, and `5`. Each rank writes rank-labeled output files so that the three replicas can be compared directly without overwriting each other.

This implementation preserves the structure of the serial code as much as possible. The only essential modifications are the MPI initialization/finalization, the rank-dependent assignment of the initial configuration and random seed, and the labeled I/O. A complementary `main_serial_replicas.f90` was also written as a direct serial counterpart, running the same three configurations sequentially. Both use wall-clock timing (`MPI_Wtime` in the parallel version, and an equivalent timing strategy in the serial one) to ensure a fair performance comparison. The motivation for this simple parallelization was to provide a basis for later work on parallel Monte Carlo and energy evaluation within each process.

Additionally, I explored a second parallelization idea: adding MPI parallelism to the XYZ trajectory writing step. This was tested in a separate branch and showed only a marginal improvement, confirming that the real computational bottleneck lies in the energy evaluation and Monte Carlo acceptance-rejection logic rather than in the I/O. Therefore, this parallelization path was discarded.

Finally, the parallelization was extended further to post-processing and observable-statistics calculations in `main_parallel_observables.f90`, which benchmarks the code as a function of the number of MPI processes `np`. This main program is driven by the cluster submission script `3.run_parallel_observables_np.sh`, and the results form the basis of the parallel scaling analysis included in the project report.
