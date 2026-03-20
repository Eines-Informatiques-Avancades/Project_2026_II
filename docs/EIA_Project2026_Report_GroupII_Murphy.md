# Contributions

## Project Leader (Github reviews & merges)

As the project leader, I was responsible for reviewing all pull requests, ensuring that the code followed the project guidelines, and merging the code into the main branch. We coordinated through a WhatsApp group chat and would regularly check in on each other's progress. I would also signal to the group when code was merged so we could all pull the latest changes.

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



Starting off with the Makefile, there are few main variables that control the compilation process. 

```makefile
FC       = gfortran
FFLAGS   = -O2 -Wall -Wextra
OBJ_DIR  = ../bin
EXE      = $(OBJ_DIR)/main_serial.x
LIB_OBJ  = $(OBJ_DIR)/parameters.o \
           $(OBJ_DIR)/io.o \
           ...
MAIN_OBJ = $(OBJ_DIR)/main_serial.o
```
-  ```FC```: Stands for "Fortran Compiler". We tell it to use gfortran.
-  ```FFLAGS```: These are the compiler flags. ```-O2``` is the optimization schema, and ```-Wall -Wextra``` enables warnings.
-  ```OBJ_DIR``` & ```EXE```: Defines the compiled binaries directory and the name of the final executable.
-  ```LIB_OBJ``` & ```MAIN_OBJ```: Defines the object files for the library and the main program.

<br>

Then we have the targets and dependencies.

```makefile
all: $(EXE)

$(EXE): $(LIB_OBJ) $(MAIN_OBJ)
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -o $@ $(LIB_OBJ) $(MAIN_OBJ)
```
-  ```all```: This is the default target that needs ```$(EXE)``` to exist.
-  ```$(EXE)```: To build the final executable, it needs all the .o files from the ```../bin``` folder, which it makes sure exists (```mkdir -p```), and then links them all together using ```gfortran``` to output (```-o $@```) the final program.
   - Note: The ```@``` symbol is used 2 different ways here: 
      1. Before the ```mkdir``` command to suppress being printed to the terminal.
      2. As an automatic variable representing the target name (```$(EXE)``` in this case).

<br>

Next, instead of writing a rule for every single ```.f90``` file, we use a pattern rule (using ```%```).

```makefile
$(OBJ_DIR)/%.o: lib/%.f90
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -J$(OBJ_DIR) -I$(OBJ_DIR) -c $< -o $@
```
-  It says: "To make any ```.o``` file in the object directory (```bin/```) from a matching ```.f90``` file in ```lib/```, run this command."
-  ```-c $<```: Compiles the "first dependency" (```$<```, which is the ```.f90``` file) without linking (because that's done in the ```$(EXE)``` target).
-  ```-J$(OBJ_DIR) -I$(OBJ_DIR)```: Tells Fortran to put module ```.mod``` files in the ```bin/``` folder.

<br>

We follow that up with the explicit dependencies to create the ```.o``` files for the library modules and the main program, the latter of which requires all the library object files.

```makefile
$(OBJ_DIR)/energy.o: lib/energy.f90 \
                     $(OBJ_DIR)/parameters.o
...

$(OBJ_DIR)/main_serial.o: main_serial.f90 $(LIB_OBJ)
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -J$(OBJ_DIR) -I$(OBJ_DIR) -c $< -o $@
```

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

## Main

## Visualization

The visualization of our results is handled by ```plot_results.py```. It starts by reading the ```input.dat``` file to determine whether the simulation was run using explicit hydrogens so it knows which theoretical potential to compare against.

<br>

# --------- Itxaso Review Needed -------------

Before plotting the torsion angles, the script conducts a statistical analysis to detect when the Monte Carlo simulation reaches thermodynamic equilibrium, so it can discard the burn-in period.

```python
def detect_equilibration(x):
    # Chodera (2016) method to find optimal equilibration index (t0)
    n = len(x)
    best_neff = 0.0
    best_t0 = 0
    
    candidates = np.unique(np.linspace(0, int(n * 0.9), min(200, n)).astype(int))
    for t0 in candidates:
        sub = x[t0:]
        _, g = integrated_autocorr_time(sub)
        neff = (n - t0) / g
        if neff > best_neff:
            best_neff, best_t0 = neff, t0
            
    return best_t0
```

- ```integrated_autocorr_time(sub)```: Calculates the statistical inefficiency ($g$) and the integrated autocorrelation time ($\tau_{int}$) of the dataset using Fast Fourier Transforms (FFT).
- ```neff = (n - t0) / g```: Estimates the number of effectively uncorrelated samples.
- ```best_t0```: The algorithm sweeps through candidate discard points and selects the index t0 that maximizes the effective sample size, marking the end of the equilibration phase.

# ------------------------------------------------

Then, there are 3 defined functions for each of the plots using matplotlib:
* ```plot_energies```: Plots the total energy, LJ energy, and torsion energy against the MC steps.
* ```plot_observables```: Plots the radius of gyration and end-to-end distance against the MC steps.
* ```plot_torsions```: Plots the torsion angles against the MC steps and compares them to the theoretical potential.

Finally, the script moves on to generating the actual .pdf plots using matplotlib. The most complex of these is the torsion distribution plot, which overlays our histogram data against the theoretical TraPPE potentials.

```python
def plot_torsions(tors_file, energy_file, explicit_h=True):
    if not explicit_h:
        c1, c2, c3 = 0.705, -0.135, 1.572
    else:
        c1, c2, c3 = 0.8700, -0.0785, 1.5075
    def torsion_potential(phi):
        return (c1 * (1.0 + np.cos(phi))
              + c2 * (1.0 - np.cos(2.0 * phi))
              + c3 * (1.0 + np.cos(3.0 * phi)))
```

- ```explicit_h```: The boolean passed from our parser function.
- ```c1, c2, c3```: The TraPPE-UA United Atom coefficients, or the OPLS-AA Effective Backbone coefficients, loaded depending on the simulation type.
- ```torsion_potential(phi)```: Returns the theoretical energy (in kcal/mol) for any given dihedral angle ($\phi$) using trigonometric identities, which is then overlaid on the secondary Y-axis spanning across the histograms.

## Parallelization