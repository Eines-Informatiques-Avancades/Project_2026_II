# Project_2026_II

List of participants:

- MANEL DÍAZ CALVO (github: ManelDC55)
- Itxaso MUÑOZ ALDALUR (github: itxasoma)
- ARTHUR IAN MURPHY  (github: ai-murphy, Project Leader)
- OLIWIER MISZTAL (github: omisztal)

---

### List of tasks:

- MonteCarlo: Manel Díaz Calvo
- Initial Conditions: Itxaso Muñoz Aldalur
- Post-Processing: Arthur Ian Murphy
- Energy: Oliwier Misztal

---

### Parallelization Efforts:

- Multiple configurations simulataneously: Itxaso Muñoz Aldalur
- Broadcasting input parameters to all workers: Itxaso Muñoz Aldalur
- Equilibrium optimization: Manel Díaz Calvo
- Energy calculations: Oliwier Misztal
- Ensemble averaging after equilibrium: Arthur Ian Murphy

---

### File Structure:

```Project_2026_II\```
   * ```src\```: Source code
      * ```lib\```: Libraries/Modules (both Fortran and Python)
   * ```bin\```: Compiled binaries
   * ```docs\```: Documentation & reports
      * ```img\```: Final images used in reports
   * ```results\```: Data output files & plots

---

### Run Instructions from cerqt2.q:

1. **Navigate to the** `src` **directory:**
   ```bash
   cd src
   ```
   
2. **Run the following command:**
   ```bash
   qsub run_parallel.sh
   ```

### Run Instructions (Advanced):

1. **Navigate to the** ```src``` **directory:**
   ```bash
   cd src
   ```


2. **Compile & run the simulation with plots (serial):**
   ```bash
   make pipeline
   ```
   - This will create the executable file ```main_serial.x``` (as well as all of the ```.o``` and ```.mod``` files) in the ```bin``` directory. 
   - It will also run the simulation using the parameters defined in the ```input.dat``` file & save the results in the ```results``` directory.
   - Lastly, it will generate plots of the results and save them in the ```results``` directory.


3. **Compile & run the simulation with plots (parallel):**
   ```bash
   make pipeline_parallel NP=4 OMP_THREADS=2
   ```
   - This will perform the same actions as `make pipeline` but will run the simulation in parallel using MPI and OpenMP. The executable created will be `main_parallel.x`.
   - Optional arguments:
     - `NP`: Number of MPI processes (default: 4)
     - `OMP_THREADS`: Number of OpenMP threads (default: 2)
     - _Caution: These numbers compound as $$(\text{NP}-1) * \text{OMP\_THREADS} = \text{Number of cores used}$$ due to multiple types of parallelism. Defaults use 7 cores._

4. **Generate plots (serial):**
   ```bash
   make figures
   ```
   This will generate plots of the serial results and save them in the ```results``` directory.


5. **Clean the code:**
   ```bash
   make clean
   ```
   This will remove all of the compiled files and the executable(s).


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