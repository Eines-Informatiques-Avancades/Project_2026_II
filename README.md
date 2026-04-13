# Project_2026_II

| Name | Github | Files (Principal Author) | Parallelization Contributions |
| ---- | ------ | ----------------------- | ----------------------------- |
| MANEL DÍAZ CALVO | ManelDC55 | Monte Carlo, qsub script, Plotting* | Equilibration Optimization |
| OLIWIER MISZTAL | omisztal | Energy, Shell Scripts, Plotting* | Energy Calculations |
| ITXASO MUÑOZ ALDALUR | itxasoma | Initial Configurations, Shell Scripts, File I/O, Plotting* | Independent Replicas, Orchestrator Read/Broadcasting |
| ARTHUR IAN MURPHY (_Project Leader_) | ai-murphy | Makefile, Main, Observables, Plotting* | Ensemble Averaging |

<p align='right' style='italics'>
<em>*Everyone participated in Plotting</em>
</p>

<br>

## File Structure:

```Project_2026_II\```
   * ```src\```: Source code
      * ```lib\```: Libraries/Modules (both Fortran and Python)
   * ```bin\```: Compiled binaries
   * ```docs\```: Documentation & reports
      * ```img\```: Final images used in reports
   * ```results\```: Data output files & plots

<br>

## Run Instructions from cerqt2.q:

The following steps will run a parallelized version of the code, utilizing 13 processing cores, with results of the simulation being placed in the `/results` directory.

1. **Navigate to the** `src` **directory:**
   ```bash
   cd src
   ```
   
2. **Run the following command:**
   ```bash
   qsub run_parallel.sh
   ```

3. **Generate plots:**
   
   ```bash
   python plot_results_parallel.py
   ```

<br>

See below for advanced options, including pipelines, individual parallelization technique implementations, and more.

<br>

## Run Instructions (Advanced):

Many additional features are avalable depending on the type of simulation desired (serial, parallel, parallel with only certain features, etc.). The following options make use of the robust Makefile from this project and perform different simulations:

-  `make clean && make run_serial_equil`
    - This will run a serialized version of the program on a single processor, where the initial configuration type is determined by the setting on line 4 (conf_type) of `input.dat` from the `confs` directoy. See report for an explanation of configuration types.
-  `make clean && make run_parallel_replicas`
    - This will run a parallelized version of the program, where configuration types 1, 4, & 5 will be simulated simultaneously. Equilibration is determined through an earlier iteration of an auto-correlation mechanism (not Geweke check). _Minimum 3 CPU cores required_
-  `make clean && make run_parallel_observables`
    - This will run post-processing to obtain observables
-  `make clean && make run_parallel_star`
    - This will run a parallelized version of the program, where configuration types 1, 4, & 5 will be simulated simultaneously. Equilibration is determined by a Geweke check. _Minimum 4 cores required if energy parallelization is off. Default is on, with 7 cores utilized_
-  `make clean && make run_parallel_combined`
    - This will run a parallelized version of the program, where configuration types 1, 4, & 5 will be simulated simultaneously. Equilibration is determined by a Geweke check, Orchestrator/Worker logic is implemented, Energy caldulations are threaded, and Ensemble Averaging is implemented. 
-  `make clean && make pipeline_parallel_combined`
    - This will do the same as above (`make clean && make run_parallel_combined`) while also running plotting on the output data from the parallelized simulation. _Statically assigns 13 CPU cores_
-  `make figures_parallel`
    - This will make plots on the pre-existing output data from the parallelized simulation. _Non-parallelized_




**Notes:**
- All simulations and plotting create and look for data within the results directory. It it possible for these files to be overwritten between simulations. Best practice is to move any plots or output data files to a save location (such as a new folder within the `results` directory) before running a new simulation
- `input.dat` also has variables that control whether an equilibration step is used or not within the simulation. An example equilibrated geometry is already saved in the `confs` directory under the name `equil_conf4_initial.xyz`.

## Resource Allocation
There are 2 compile-time arguments for controlling the number of CPUs allotted within parallel versions of the program:
- `NP` (Number of Processors) - controls the number of Orchestrators and workers via MPI. 
    - Minimum: 1 Orchestrator + 1 Worker 
    - Default: 4
- `OMP_THREADS` - controls the number of threads for energy calculations via openMP. 
    - If set to 1, energy parallelization is turned off.
    - Default: 2
- _Caution: Because there are 2 separate parallelization frameworks combined, these numbers compound as $$(\text{NP}-1) * \text{OMP\_THREADS} + 1 = \text{Number of cores used}$$ Defaults use 7 cores._




## Dependencies / Requirements

### Build & run (Fortran)
- A Fortran compiler compatible with Fortran90 (tested with `gfortran`)
- `make`

#### Additional requirements for parallel version
- MPI (Message Passing Interface) (tested with `mpif90`)
- OpenMP
- C-Preprocessor

The C-Preprocessor `cpp` should be included with `gfortran` but here are some instructions on how to install it, MPI, and OpenMP if needed:

```bash
sudo apt install cpp openmpi-bin libopenmpi-dev libgomp1 libomp-dev
```

### Post-processing (Python)
- Python 3
- Python packages:
  - `numpy`
  - `matplotlib`

Install Python dependencies in a virtual environment (recommended):

```bash
python3 -m venv .venv
source .venv/bin/activate
python3 -m pip install --upgrade pip
python3 -m pip install -r src/lib/requirements.txt
```

### Plotting requirements

The plotting script uses Matplotlib with LaTeX text rendering, so a working LaTeX installation is required in addition to the Python packages.

#### Ubuntu/Debian
```bash
sudo apt update
sudo apt install texlive texlive-latex-extra texlive-fonts-recommended dvipng cm-super
```