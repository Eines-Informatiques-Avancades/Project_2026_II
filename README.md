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

### File Structure:

```Project_2026_II\```
   * ```src\```: Source code
      * ```lib\```: Libraries/Modules (both Fortran and Python)
   * ```bin\```: Compiled binaries
   * ```docs\```: Documentation & reports
      * ```img\```: Final images used in reports
   * ```results\```: Data output files & plots

---

### Run Instructions:

1. **Navigate to the** ```src``` **directory:**
   ```bash
   cd src
   ```


2. **Compile the code:**
   ```bash
   make
   ```
   This will create the executable file ```main_serial.x``` (as well as all of the ```.o``` and ```.mod``` files) in the ```bin``` directory.


3. **Run the simulation:**
   ```bash
   make run
   ```
   This will run the simulation using the parameters defined in the ```input.dat``` file & save the results in the ```results``` directory.


4. **Generate plots:**
   ```bash
   make figures
   ```
   This will generate plots of the results and save them in the ```results``` directory.


5. **Clean the code:**
   ```bash
   make clean
   ```
   This will remove all of the compiled files and the executable.
