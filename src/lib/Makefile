FC     = gfortran
FFLAGS = -O2 -Wall -Wextra -std=f2008
DBG    = -O0 -g -fcheck=all -fbacktrace

EXE = test_init.x
OBJ = parameters.o io.o initial-conf.o main-ini.o

all: $(EXE)

$(EXE): $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

debug: FFLAGS=$(DBG)
debug: clean all

parameters.o: parameters.f90
	$(FC) $(FFLAGS) -c $<

io.o: io.f90 parameters.o
	$(FC) $(FFLAGS) -c $<

initial-conf.o: initial-conf.f90 parameters.o
	$(FC) $(FFLAGS) -c $<

main-ini.o: main-ini.f90 parameters.o io.o initial-conf.o
	$(FC) $(FFLAGS) -c $<

run: all
	./$(EXE)

clean:
	rm -f *.o *.mod $(EXE) 


