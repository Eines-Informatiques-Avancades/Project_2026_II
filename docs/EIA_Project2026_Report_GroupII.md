# **Monte Carlo Simulation of a Simple Polymer Chain**

## Team **Project_2026_II**

| Name | Github | Files (Principal Author) | Parallelization Contributions |
| ---- | ------ | ----------------------- | ----------------------------- |
| MANEL DÍAZ CALVO | ManelDC55 | Monte Carlo, qsub script, Plotting* | Equilibration Optimization |
| OLIWIER MISZTAL | omisztal | Energy, Plotting* | Energy Calculations |
| ITXASO MUÑOZ ALDALUR | itxasoma | Initial Configurations, Shell Scripts, File I/O, Plotting* | Independent Replicas, Orchestrator Read/Broadcasting, Trajectory Parsing |
| ARTHUR IAN MURPHY (_Project Leader_) | ai-murphy | Makefile, Main, Observables, Plotting* | Ensemble Averaging |

<br>

## Project Overview 

In this assignment, our goal was to develop a Monte Carlo simulation program to explore the conformational landscape of a 500-Carbon linear polymer chain (polyethylene) with varying torsional angles. The simulation generates the initial conformation (including Hydrogens), measures torsional energy rotations, end-to-end distance, and intramolecular Lennard-Jones interactions, and finally analyzes & visualizes the results. 

After creating a serial processing version of the program, parallelization techniques involving both MPI and OpenMP were incorporated. These were compared to the serial version to demonstrate both their individual and combined High Performance Computing capabilities & benefits. 

Many techniques taught in class were crucial in the development of this program, including:
- **Git/Github** as a code respository & teamwork coordination platform
- **Makefile** to automate the compilation and execution of the code
- **Shell scripting** for facilitating server deployment
- **MPI & openMP** to parallelize the code
- **Python** for post-processing and plotting

<br>

## Repository Structure

Github was used as our team's code repository management system, and from the root of [the project](https://github.com/Eines-Informatiques-Avancades/Project_2026_II), the code is divided among 3 main folders: `src`, `results`, and `docs`. 

- `src` contains:
  - Makefile & shell scripts
  - Various versions of the main program (serial & parallel)
  - A `confs` folder where initial configurations are stored and compile-time options can be controlled via a file called `input.dat` 
  - A `lib` folder where module source files, python analysis files, and plotting style dependencies reside (_further detail below_)
- `results` contains the output data from the simulations
- `docs` contains documentation and reports, including `img` folder for embedded images

<br> 

In addition to these folders, a customary `README` file is included in the root directory detailing run instructions and code dependencies/requirements. The bulk of the code is written in Fortran, while the post-processing is done in Python.


  ### Module Library Introduction
  The following modules support the _`main...f90`_ files:
  - `energy.f90` & `energy_all_atoms.f90` - Permit **dual energy modes**, computing energetic contributions using either TraPPE-UA or OPLS-AA / TraPPE-AA (effective backbone potentials) depending on the `explicit_h` toggle dynamically read from `input.dat`.
  - `initial-conf.f90` - Generates initial configuration of the polymer chain.
  - `io.f90` - File input/ouput module.
  - `main-ini.f90` - 
  - `monte_carlo.f90` - Provides the rotation algorithm used to update spatial configurations.
  - `observables.f90` - Computes the structural observables: End-to-end distance ($R_{ee}$), Radius of Gyration ($R_g$), and torsion angles.
  - `parameters.f90` - Stores global parameters abstracted from _`main...f90`_ code. Works in tandem with `input.dat` settings.
  - _`plot_...py`_ files - Various scripts used for plotting observables and serial vs parallel code comparisons. 
  - `requirements.txt` - List of python module dependencies.
  - `science.mplstyle` - stylized document used in plotting to control visualization theme.
  - **Data Visualization**: A robust plotting script ([plot_results.py](./src/lib/plot_results.py))   parses the inputs, mathematically detects thermodynamic equilibration to strip burn-in data, and   generates theoretical overlays.
  - **Build Pipeline**: The updated `src/Makefile` fully automates the workflow, featuring a `make pipeline` macro that compiles the binaries, runs the Fortran simulation, and generates the Python `.pdf` figures in one shot.

<br>

## Code Management via Github

The project leader was responsible for reviewing all pull requests, ensuring that the code followed the project guidelines, and merging the code into the main branch. The team coordinated through a WhatsApp group chat and would regularly check in on each other's progress. Everyone was informed when code was merged so we could all pull the latest changes.

Most times, approving and merging was straightforward with no issues, but there was an interesting challenge when multiple uploads from different team members overlapped, causing conflicts. This forced us to learn about stashing and rebasing to the newly updated master branch before recommitting. The following process was ironed out to ensure a smooth integration:

1. Stash local (uncommitted) changes
   ```bash
      git stash
   ```
2. Pull latest changes from master branch
   ```bash
      git checkout master
      git pull main master
   ```
3. Rebase local changes onto master branch so it starts after the new merged code
   ```bash
      git checkout <fork-feature>/<fork-branch>
      git rebase master
   ```
4. Resolve any conflicts & re-apply stashed changes
   ```bash
      git stash pop
   ```
5. Commit and push (using ```--force``` parameter due to rebase)
   ```bash
      git push <fork-remote> <fork-branch> --force
   ```

<br>


## Makefile

As the project evolved, so did the Makefile. Here are some of the features we implemented:

<br>

Starting off, there are few main variables that control the compilation process. 

```makefile
FC       = gfortran
FFLAGS   = -O2 -Wall -Wextra
OBJ_DIR  = ../bin
EXE      = $(OBJ_DIR)/main_serial.x
LIB_OBJ  = $(OBJ_DIR)/parameters.o \
           $(OBJ_DIR)/io.o \
           ...
MAIN_OBJ = $(OBJ_DIR)/main_serial.o

NP          ?= 4
OMP_THREADS ?= 2
```
-  ```FC```: Stands for "Fortran Compiler". We tell it to use gfortran.
-  ```FFLAGS```: These are the compiler flags. ```-O2``` is the optimization schema, and ```-Wall -Wextra``` enables warnings.
-  ```OBJ_DIR``` & ```EXE```: Defines the compiled binaries directory and the name of the final executable.
-  ```LIB_OBJ``` & ```MAIN_OBJ```: Defines the object files for the library and the main program.
-  ```NP``` & ```OMP_THREADS```: Defines the number of processes (MPI) and threads (openMP) to use for the parallel version of the program. The default values are set to 4 and 2, respectively, but they can be overridden by setting them when compiling in the command line (e.g., ```make run_parallel NP=7 OMP_THREADS=3```).

<br>

In order to encompass both the serial and parallel versions of the program, a check is performed on the `make` command entered on the command-line for the word `parallel`. If found, the user's system is searched to ensure the environment has the necessary MPI and OpenMP libraries installed. If not, an error message is printed and the program exits. If they are installed, the compiler variable `FC` is overwritten and the MPI and OpenMP flags are added to the `FFLAGS` variable.

```makefile
ifneq (,$(findstring parallel,$(MAKECMDGOALS)))
  # MPI Compiler Wrapper Verification
  MPI_BIN := $(shell command -v mpif90 2>/dev/null)
  ifeq ($(strip $(MPI_BIN)),)
    $(error "MPI Compiler is required but 'mpif90' was not found! Please install it (e.g. 'sudo apt install openmpi-bin')")
  endif
  # Overwrite Compiler for MPI parallelization flag linking
  FC = mpif90
  ...
```

<br>

Instead of writing a rule for every single ```.f90``` file, a pattern rule (with `%`) is used.

```makefile
$(OBJ_DIR)/%.o: lib/%.f90
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -J$(OBJ_DIR) -I$(OBJ_DIR) -c $< -o $@
```
-  It says: "To make any ```.o``` file in the object directory (```bin/```) from a matching ```.f90``` file in ```lib/```, run this command."
-  ```-c $<```: Compiles the "first dependency" (```$<```, which is the ```.f90``` file) without linking (because that's done in the ```$(EXE)``` target).
-  ```-J$(OBJ_DIR) -I$(OBJ_DIR)```: Tells Fortran to put module ```.mod``` files in the ```bin/``` folder.

<br>

The following is an example of the `all` target (serial version of the program) with its executable structure, which we then follow up with the explicit dependencies to create the ```.o``` files for the library modules and the main program

```makefile
all: $(EXE)

$(EXE): $(LIB_OBJ) $(MAIN_OBJ)
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -o $@ $(LIB_OBJ) $(MAIN_OBJ)

$(OBJ_DIR)/energy.o: lib/energy.f90 \
                     $(OBJ_DIR)/parameters.o
...

$(OBJ_DIR)/main_serial.o: main_serial.f90 $(LIB_OBJ)
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -J$(OBJ_DIR) -I$(OBJ_DIR) -c $< -o $@

```
-  ```all```: This is the default target that needs ```$(EXE)``` to exist.
-  ```$(EXE)```: To build the final executable, it needs all the .o files from the ```../bin``` folder, which it makes sure exists (```mkdir -p```), and then links them all together using ```gfortran``` or ```mpif90``` to output (```-o $@```) the final program.
   - **Note**: The ```@``` symbol is used 2 different ways here: 
      1. Before the ```mkdir``` command to suppress being printed to the terminal.
      2. As an automatic variable representing the target name (```$(EXE)``` in this case).


<br>

Last, we declare the shortcut commands as ```.PHONY``` targets to prevent conflicts with files that may have the same name as a target.

```makefile
.PHONY: all clean run figures pipeline

run: all
	@mkdir -p ../results
	$(EXE)

figures:
	python lib/plot_results.py

clean: 
	rm -rf $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod $(EXE)

pipeline:
	$(MAKE) run
	$(MAKE) figures
```
-  This enables the following compile-time shortcuts:
   - ```make run```: Automatically builds the code (```all```) and then executes it.
   - ```make figures```: Envokes python to generate the plots from the ```../results``` folder.
   - ```make clean```: Acts like a reset button, deleting all compiled files to start fresh.
   - ```make pipeline```: A neat wrapper we built to run the code and generate the python plots back-to-back.

<br>

## Shell Scripting

<br>

## Parallelization with MPI & OpenMP

To maximize the performance of the Monte Carlo simulation, we employed a hybrid parallelization strategy combining **MPI** (Message Passing Interface) and **OpenMP**.

While both serve to speed up code, they operate on fundamentally different paradigms:

- **MPI (Distributed Memory)**: Operates on an Orchestrator/Worker topology. It treats each core as a completely isolated computer with its own dedicated memory. In our code, MPI is responsible for the macro-level distribution of work—managing the dynamic job queue and sending independent 1-million-step production replicas to free worker cores.
- **OpenMP (Shared Memory)**: Operates within a single process. It allows multiple lightweight threads to temporarily share the same pool of memory. In our code, OpenMP is responsible for the micro-level heavy lifting—specifically parallelizing the nested $\mathcal{O}(N^2)$ Lennard-Jones interaction loops inside a single Monte Carlo step.

Combining these two frameworks is tricky. If left unchecked, $N$ MPI processes might each try to spawn $M$ OpenMP threads simultaneously. If $N \times M$ exceeds the physical number of CPU cores available, it leads to "oversubscription"—where performance gains are lost while the operating system wastes all its time context-switching between threads.

We mitigated this risk by enforcing strict administrative control through our Makefile compilation arguments (NP and OMP_THREADS). This gives the user explicit, compile-time authority to cleanly balance the execution matrix (e.g., forcing OMP_THREADS=1 during heavily populated MPI ensemble sampling) to prevent thread-collisions and ensure hardware resources are optimized efficiently.

Further detail of how the parallelization was implemented within the code itself is included in the [Parallelization Section](./EIA_Project2026_Report_GroupII.md#parallelization-with-mpi--openmp).

<br>

## Python Post-Processing

<br>

## Serial Version Simulation Results

The serial version of our simulation starts with an equilibration run followed by a 10,000,000 MCS production run for a 500-carbon linear polymer molecule with explicit hydrogens at 300K. The dihedral angle was initially set to 15 degrees and then randomly selected between -$\pi$ and $\pi$. 

![Initial Configuration](./img/serial_geweke/VMD_serial_geweke_initial_geometry.png)
_Fig 1. Initial configuration of the 500-carbon linear polymer molecule with explicit hydrogens at 300K._

![Final Configuration](./img/serial_geweke/VMD_serial_geweke_final_geometry.png)
_Fig 2. Final configuration of the 500-carbon linear polymer molecule with explicit hydrogens at 300K._

### Geweke Results (Dynamic Runtime Equilibration)

Previously, the simulation relied on an arbitrary 10-million step burn-in process and a post-processing python Fast-Fourier Transform (FFT) script to determine the integrated autocorrelation time ($\tau_{int}$) and "guess" when the simulation had equilibrated over time. However, as noted in previous reviews, this often failed to properly truncate the data arrays (e.g., `TRUNCATING FIRST 0%, NOT 80%`) if consecutive correlations tricked the variance boundaries.

By rewriting the Fortran baseline to include the mathematical **Geweke Convergence Diagnostic** at runtime, the simulation now dynamically assesses the standard error between early and late moving-average windows and physically halts the "Equilibration Phase" the exact moment $Z < 1.96$ is satisfied across 3 consecutive checks. The production phase then natively restarts step counting from 1. 

The python plotting scripts were rewritten to seamlessly stitch these separate outputs together across a time-demarcation boundary:

#### **Radius of Gyration & End-to-End Sequence (Equilibration & Production)**
The polymer begins in an ultra-extended 500-Carbon length chain with fixed 15-degree dihedrals ($R_g \approx 200$ Å, $R_{ee} \approx 600$ Å), but the random MC modifications rapidly introduce folds. The Geweke algorithm successfully caught the structural collapse around 3.9 million steps (denoted by the vertical dashed demarcation line). Everything to the right of the dashed line behaves exactly as a natural random coil distribution with stable radial bounds.

![Evolution of Gyration](./img/serial_geweke/observables_evolution_500_4_10000000_300.00.png)

#### **Energy Evolution (Equilibration & Production)**
The initial massive spike down for the Lennard-Jones (LJ) energy clearly captures the steric clash resolving as the linear atoms drift off the fixed 15-degree axis. Following the Geweke demarcation line at ~3.9M steps, the Total Energy securely flatlines near $-150$ kcal/mol, mathematically verifying the conformation search has truly relaxed.

![Energy Evolution](./img/serial_geweke/energy_evolution_500_4_10000000_300.00.png)

#### **Torsional Distribution (Pure Production)**
Because the output arrays are now safely separated on disk, we can plot the torsional geometry using *only* the data mapped explicitly after the transition to Phase 2. The distribution favors values near $0$ rad (trans) and $\pm 3\pi/4$ (gauche). While the distribution closely mimics the TraPPE-AA theoretical polynomial, the steric resistance (LJ interactions) caused by adding explicit Hydrogen atoms natively shifts the physical curve away from perfect mathematical symmetry—exactly as theoretically expected!

![Torsional Distributions](./img/serial_geweke/torsion_distribution_500_4_10000000_300.00.png)

_**Why doesn't the torsional distribution follow the potential?**_

Here is why your simulation heavily favors $0$ radians (a cis-like geometry) despite the standard theoretical TraPPE/OPLS polynomial assigning it the highest energy ($\sim +4.755 \text{ kcal/mol}$):

1. **Exclusion of the 1-4 Steric Repulsion**

   The primary physical effect that prevents polymer chains from curling back onto themselves to form an exact $0$ torsion angle is the extreme steric clash between atoms located across the exact bond, which are a distance of 1-4 bonds apart. If we look at how topology is mapped in energy_all_atoms.f90, we see this logic:

   ```fortran
   ! If atoms are attached to carbons separated by fewer than 4 bonds, exclude LJ
   if (abs(backbone_pos(i) - backbone_pos(j)) < 4) then
     is_excluded(i, j) = .true.
   end if
   ```

   This is a standard molecular mechanics trick since the effective torsional potentials (opls_c1, opls_c2, etc.) supposedly absorb the 1-4 interactions. However, by blindly turning is_excluded = .true. for all Hydrogen-based interactions separated by fewer than 4 bonds, the massive van-der-Waals repulsive core of the explicit Hydrogen atoms across the bond is mathematically completely ignored by the simulation.

2. **Massive 1-5+ Lennard-Jones Attraction**

   Because the repulsive boundary is artificially turned off, the simulation's MC engine evaluates the energy of the geometry entirely based on how well it can compact the rest of the chain.

   When explicit hydrogens are included, a highly coiled and overlapped structure (which equates to consecutive $\sim 0$ torsion angles) brings the 1-5, 1-6, and 1-n explicit hydrogens into perfectly contiguous contact. This dense globule packing results in massive Lennard-Jones attractive energy (from slipping perfectly into the $2^{1/6}\sigma$ energy well across hundreds of interatomic pairs).

   Summary of the Physics: The simulation discovers that the dense stacking "reward" of collapsing dozens of explicit Hydrogens far exceeds the purely mathematical $\sim +4.7$ kcal/mol torsional "penalty" of having $\phi$ near $0$. Since the algorithm utilizes the Metropolis criterion purely on the Total Energy (E_tot = E_lj + E_tors), it continuously aggregates toward the $0$ radian well.

   If you eventually decide to expand the precision of this simulation, introducing a standard scaling rule mechanism like a scale_14 = 0.5 modifier for standard 1-4 Lennard-Jones explicit interactions inside the delta_energy function, instead of absolute exclusion, would rapidly restore the local minimum clusters expected naturally back to your diagram!

<br>

## Parallelization Techniques

### Independent Replicas

### Orchestrator Initial Config Read & Broadcast

### Equilibration Optimization

### Energy Calculations

### Ensemble Averaging

Moving to an Orchestrator-Worker (Star) topology helped with the transition of the serial environment to a dynamically balanced parallel framework with OpenMPI. It achieves maximum CPU utilization by ensuring free workers are constantly assigned tasks until the simulation is complete.

<br>

Although this architechure adds overhead by requiring a single core to abstain from participating in the simulation's calculations, the orchestrator node performs the administrative tasks such as the initial file reading as well as organizing and distributing the workload. This relies upon tags being sent between the nodes to coordinate the simulation.

```fortran
  ! MPI Tags
  integer, parameter :: TAG_REQUEST_WORK = 1
  integer, parameter :: TAG_DO_EQUIL     = 2
  integer, parameter :: TAG_DO_PROD      = 3
  integer, parameter :: TAG_WAIT         = 4
  integer, parameter :: TAG_DIE          = 5
  integer, parameter :: TAG_EQUIL_DONE   = 6
  integer, parameter :: TAG_PROD_DONE    = 7
```



First, the simulation is cleanly segregated into phases where the **Orchestrator Node** acts as a central dispatcher:

```fortran
      do while (completed_prods < size(prod_queue))
         ! Wait for any worker to finish its task and ask for more
         call MPI_Recv(msg, 1, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
         sender = status(MPI_SOURCE)
         tag_recvd = status(MPI_TAG)
```
- `MPI_Recv`: The orchestrator lies in wait for any incoming communication from the worker nodes. 
- `MPI_ANY_SOURCE`: Instead of statically assigning jobs to specific processors up-front, the orchestrator listens to the *entire pool* of available cores. As soon as a worker node finishes its current 1-million-step job, it pings the orchestrator, which instantly deploys the next job from the queue directly to that specific idle worker, ensuring optimal resource utilization. 
- **Staged Execution**: By cleanly separating the logic phases, the orchestrator refuses to load the 10 production jobs into the queue until the prerequisite equilibration run officially finishes. The moment equilibration is met, jobs pop into the queue and cores are reused instantly, guaranteeing no resources are ever permanently "blocked" waiting in idle lines.

<br>

Once a worker has sent an incoming communication signal back to the orchestrator, the orchestrator checks the tag and executes the appropriate logic:

```fortran
         if (tag_recvd == TAG_EQUIL_DONE) then
           ! ... (Orchestrator caches the equilibrated .xyz coordinates and unlocks the production queue)
         else if (tag_recvd == TAG_PROD_DONE) then 
           ! ... (Orchestrator tallies the completed production run)
         end if

         ! If jobs remain in the queue, immediately deploy the next one to the 'sender'
         if (jobs_assigned < size(prod_queue)) then
            call MPI_Send(TAG_DO_PROD, 1, MPI_INTEGER, sender, TAG_DO_PROD, MPI_COMM_WORLD, ierr)
```
- `MPI_Send`: Deploys the next instruction array sequentially to whichever Worker ID is populated in the `sender` variable.
- `TAG_DO_PROD`: An arbitrarily assigned integer tag that tells the receiving Worker process exactly which computational sub-routine to pivot into. 

<br>

The benefits of this star topology combined with phased execution of equilibration runs followed by production runs is compared below to the serial version of the program. As the number of dynamic active nodes increases, the time to complete 10-million MC Steps decreases, albeit non-ideally:


![MPI Parallel Speedup Analysis](img/parallel_star_speedup_plot.png)
_Fig. x: Comparing timing & efficiency of Ensemble Averaging with various core count to Serial production run_

<br>

Simply adding more cores does not result in a perfectly linear speedup. This can be attributed to parallelization overhead plus device hardware bottlenecks. The most efficient use of cores is found in factors of 10 (2 & 5 cores), in which cases all worker nodes are participating in the final group of remaining 1M MCS steps. The non-factors of 10 could see performance improvements with the addition of a dynamic algorithm that determines how many steps are left once there are fewer 1M MCS jobs remaining than cores participating. This would eliminates worker "dead-time" towards the end of the production run.

<br>

### Trajectory Analysis


## Serial vs Parallel Performance
The serial code was run on a single core of an Intel Core i7-11800H processor, taking 8 hours, 18 minutes, and 31.91 seconds. The parallel code was run on 4 cores of the same processor, taking __ hours, __ minutes, and __ seconds. The parallel code was run using the `mpirun` command with the `-np 4` option.

## Conclusion

