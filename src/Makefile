# Author: Arthur Murphy
# Contributors: Itxaso Muñoz-Aldalur, Oliwier Misztal

FC     = gfortran
# *** CHANGED: added -cpp so #ifdef in parameters.f90 always works ***
FFLAGS = -O2 -Wall -Wextra -cpp

NP ?= 4
OMP_THREADS ?= 2

ifneq (,$(findstring parallel,$(MAKECMDGOALS)))
  MPI_BIN := $(shell command -v mpif90 2>/dev/null)
  ifeq ($(strip $(MPI_BIN)),)
    $(error "MPI Compiler is required but 'mpif90' was not found!")
  endif
  FC = mpif90

  MPI_MOD_FILE := $(shell find /usr /opt -name mpi.mod -type f 2>/dev/null | head -n 1)
  ifeq ($(strip $(MPI_MOD_FILE)),)
    $(error "MPI is required but mpi.mod was not found!")
  endif

  OMP_MOD_FILE := $(shell find /usr /opt -name omp_lib.mod -type f 2>/dev/null | head -n 1)
  ifeq ($(strip $(OMP_MOD_FILE)),)
    $(error "OpenMP is required but omp_lib.mod was not found!")
  endif
  FFLAGS += -fopenmp

  CPP_BIN := $(shell command -v cpp 2>/dev/null)
  ifeq ($(strip $(CPP_BIN)),)
    $(error "C-Preprocessor is required but 'cpp' was not found!")
  endif
  # *** NOTE: -cpp already in base FFLAGS, no need to add again ***
  ifneq ($(OMP_THREADS),1)
    FFLAGS += -D_USE_OPENMP
  endif
endif

OBJ_DIR = ../bin
EXE          = $(OBJ_DIR)/main_serial.x
EXE_PARALLEL = $(OBJ_DIR)/main_parallel.x

LIB_OBJ = $(OBJ_DIR)/parameters.o \
           $(OBJ_DIR)/io.o \
           $(OBJ_DIR)/initial-conf.o \
           $(OBJ_DIR)/energy.o \
           $(OBJ_DIR)/energy_all_atoms.o \
           $(OBJ_DIR)/monte_carlo.o \
           $(OBJ_DIR)/observables.o

MAIN_OBJ          = $(OBJ_DIR)/main_serial.o
MAIN_PARALLEL_OBJ = $(OBJ_DIR)/main_parallel.o

$(EXE): $(LIB_OBJ) $(MAIN_OBJ)
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -o $@ $(LIB_OBJ) $(MAIN_OBJ)

$(EXE_PARALLEL): $(LIB_OBJ) $(MAIN_PARALLEL_OBJ)
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -o $@ $(LIB_OBJ) $(MAIN_PARALLEL_OBJ)

$(OBJ_DIR)/%.o: lib/%.f90
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -J$(OBJ_DIR) -I$(OBJ_DIR) -c $< -o $@

$(OBJ_DIR)/energy.o:          lib/energy.f90          $(OBJ_DIR)/parameters.o
$(OBJ_DIR)/energy_all_atoms.o: lib/energy_all_atoms.f90 $(OBJ_DIR)/parameters.o
$(OBJ_DIR)/io.o:               lib/io.f90              $(OBJ_DIR)/parameters.o
$(OBJ_DIR)/initial-conf.o:     lib/initial-conf.f90    $(OBJ_DIR)/parameters.o
$(OBJ_DIR)/monte_carlo.o:      lib/monte_carlo.f90     $(OBJ_DIR)/parameters.o \
                                                        $(OBJ_DIR)/initial-conf.o \
                                                        $(OBJ_DIR)/energy.o \
                                                        $(OBJ_DIR)/energy_all_atoms.o
$(OBJ_DIR)/observables.o:      lib/observables.f90     $(OBJ_DIR)/energy.o \
                                                        $(OBJ_DIR)/energy_all_atoms.o

$(OBJ_DIR)/main_serial.o: main_serial.f90 $(LIB_OBJ)
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -J$(OBJ_DIR) -I$(OBJ_DIR) -c $< -o $@

$(OBJ_DIR)/main_parallel.o: main_parallel.f90 $(LIB_OBJ)
	@mkdir -p $(OBJ_DIR)
	$(FC) $(FFLAGS) -J$(OBJ_DIR) -I$(OBJ_DIR) -c $< -o $@

.PHONY: all parallel clean run run_parallel figures pipeline pipeline_parallel

all:      $(EXE)
parallel: $(EXE_PARALLEL)

run: all
	@mkdir -p ../results
	$(EXE)

run_parallel: parallel
	@mkdir -p ../results
	@echo "Starting MPI simulation with $(NP) processes and $(OMP_THREADS) OpenMP threads each..."
	OMP_NUM_THREADS=$(OMP_THREADS) mpirun -np $(NP) $(EXE_PARALLEL)

figures:
	python lib/plot_results.py

clean:
	rm -f $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod $(EXE) $(EXE_PARALLEL)
	rm -f *.mod *.o

pipeline:
	$(MAKE) run
	$(MAKE) figures

pipeline_parallel:
	$(MAKE) run_parallel
	$(MAKE) figures
