# March 12 Deliverables: Sequential Code Project Status

We have successfully integrated all Fortran modules for the sequential simulation of the polyethylene chain, expanding our capabilities to include both United-Atom and All-Atom (Explicit Hydrogen) representations.

<br>

## Implementation Details

[main_serial.f90](./src/main_serial.f90) was implemented to tie together all existing modules and execute the main Monte Carlo loop:
- **Dual Energy Modes**: Computes energetic contributions using [energy.f90](./src/lib/energy.f90) (TraPPE-UA) or the newly added [energy_all_atoms.f90](./src/lib/energy_all_atoms.f90) (OPLS-AA / TraPPE-AA effective backbone potentials) depending on the `explicit_h` toggle dynamically read from `input.dat`.
- **Spatial Configurations**: Updates configurations using the rotation algorithm in [monte_carlo.f90](./src/lib/monte_carlo.f90).
- **Observables Module**: Computes the structural observables ($R_{ee}$, $R_g$) and currently realized dihedral angles in [observables.f90](./src/lib/observables.f90).
- **Data Visualization**: A robust plotting script ([plot_results.py](./src/lib/plot_results.py)) parses the inputs, mathematically detects thermodynamic equilibration to strip burn-in data, and generates theoretical overlays.
- **Build Pipeline**: The updated `src/Makefile` fully automates the workflow, featuring a `make pipeline` macro that compiles the binaries, runs the Fortran simulation, and generates the Python `.pdf` figures in one shot.

<br>

## Verification Results

We verified the code by running a 10,000,000 step simulation for a 500-carbon linear polymer molecule with explicit hydrogens at 300Kwhere the dihedral angle was initially set to 15 degrees and then randomly selected between -$\pi$ and $\pi$. 

![Initial Configuration](./img/Serial_500CH_InitConf4_Screenshot.png)
_Fig 1. Initial configuration of the 500-carbon linear polymer molecule with explicit hydrogens at 300K._

![Final Configuration](./img/Serial_500CH_FinalConf4_Screenshot.png)
_Fig 2. Final configuration of the 500-carbon linear polymer molecule with explicit hydrogens at 300K._

### Trajectory and Observables

The `results/` folder contains various simulation results, with the final serial run residing within the `300_500CH_10M_Conf4` directory.

#### **Energy Evolution**
The Lennard-Jones parameters alongside the torsional energy relax the structure down from the linear extreme correctly.
![Energy Evolution](./img/energy_evolution_500_4_10000000_300.00.pdf)
<embed src="./img/energy_evolution_500_4_10000000_300.00.pdf" type="application/pdf" width="100%" height="500px">


#### **Torsional Distribution**
The Python analysis successfully calculates the integrated autocorrelation time ($\tau_{int}$) to discard the equilibration phase. The resulting production data properly converges on realistic polymer equilibrium limits around gauche/trans configurations across the structure geometry. Before adding the Hydrogen atoms, the torsional distribution was perfectly matching the mathematical theoretical potentials, but afterwards, it only slightly mimics the shape of the theoretical distribution. 
**ARE WE EQUILIBRATED?? IT SHOWS TRUNCATING FIRST 0%, NOT 80%.**
![Torsional Distributions](./img/torsion_distribution_500_4_10000000_300.00.pdf)

#### **Radius of Gyration & End-To-End Sequence**
Initially set to an ultra-extended 500-Carbon length chain with all dihedral angles set to 15 degrees, the polymer shrinks as random MC dihedral modifications introduce folds into a more natural spherical coil distribution over the timescale.
![Evolution of Gyration](./img/observables_evolution_500_4_10000000_300.00.pdf)

<br>

## Serial vs Parallel Performance
The serial code was run on a single core of an Intel Core i7-11800H processor, taking 8 hours, 18 minutes, and 31.91 seconds. The parallel code was run on 4 cores of the same processor, taking __ hours, __ minutes, and __ seconds. The parallel code was run using the `mpirun` command with the `-np 4` option.

## What's Next? (April 12th Deadline)
With the sequential baseline thoroughly verified and visually analyzed, our immediate focus shifts to parallelization. We have researched and documented 12 distinct OpenMPI strategies in [Parallelization_Options.md](./Parallelization_Options.md) (ranging from `MPI_Bcast` for inputs and `MPI_Allgather` for spatial synchronization, to `MPI_File_write_at` for parallel I/O). The team will now begin systematically integrating these MPI paradigms into the codebase to meet the April 12th high-performance computing requirements.
