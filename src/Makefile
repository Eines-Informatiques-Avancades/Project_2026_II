FC     = gfortran
FFLAGS = -O2 -Wall -Wextra

OBJ_DIR = ../bin
EXE     = $(OBJ_DIR)/main_serial.x

# List object files taking into account dependencies
LIB_OBJ = $(OBJ_DIR)/parameters.o \
          $(OBJ_DIR)/io.o \
          $(OBJ_DIR)/initial-conf.o \
          $(OBJ_DIR)/energy.o \
          $(OBJ_DIR)/energy_all_atoms.o \
          $(OBJ_DIR)/monte_carlo.o \
          $(OBJ_DIR)/observables.o

MAIN_OBJ = $(OBJ_DIR)/main_serial.o

all: $(EXE)

$(EXE): $(LIB_OBJ) $(MAIN_OBJ)
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -o $@ $(LIB_OBJ) $(MAIN_OBJ)

# Pattern rule for modules inside lib/
$(OBJ_DIR)/%.o: lib/%.f90
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -J$(OBJ_DIR) -I$(OBJ_DIR) -c $< -o $@

# Explicit dependencies
$(OBJ_DIR)/energy.o: lib/energy.f90 \
                     $(OBJ_DIR)/parameters.o

$(OBJ_DIR)/energy_all_atoms.o: lib/energy_all_atoms.f90 \
                               $(OBJ_DIR)/parameters.o

$(OBJ_DIR)/io.o: lib/io.f90 \
                 $(OBJ_DIR)/parameters.o

$(OBJ_DIR)/initial-conf.o: lib/initial-conf.f90 \
                           $(OBJ_DIR)/parameters.o

$(OBJ_DIR)/monte_carlo.o: lib/monte_carlo.f90 \
                          $(OBJ_DIR)/parameters.o \
                          $(OBJ_DIR)/initial-conf.o \
                          $(OBJ_DIR)/energy.o \
                          $(OBJ_DIR)/energy_all_atoms.o

$(OBJ_DIR)/observables.o: lib/observables.f90 \
                          $(OBJ_DIR)/energy.o \
                          $(OBJ_DIR)/energy_all_atoms.o

# Main program depends on everything
$(OBJ_DIR)/main_serial.o: main_serial.f90 $(LIB_OBJ)
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -J$(OBJ_DIR) -I$(OBJ_DIR) -c $< -o $@

.PHONY: all clean run figures pipeline

run: all
	@mkdir -p ../results
	$(EXE)

figures:
	python lib/plot_results.py

clean:
	rm -f $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod $(EXE)

pipeline:
	$(MAKE) run
	$(MAKE) figures

