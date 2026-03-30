#!/bin/bash
#$ -N polyMC_diag
#$ -pe smp 3
#$ -q cerqt01.q
#$ -S /bin/bash
#$ -cwd
#$ -o polyMC_diag_$JOB_ID.out
#$ -e polyMC_diag_$JOB_ID.err

echo "========== JOB INFO =========="
echo "HOSTNAME: $(hostname)"
echo "PWD: $(pwd)"
echo "NSLOTS: $NSLOTS"
echo "SHELL: $SHELL"
echo "PATH before module init:"
echo "$PATH"
echo

echo "========== MODULE INIT =========="
if [ -f /etc/profile.d/modules.sh ]; then
    source /etc/profile.d/modules.sh
    echo "Loaded /etc/profile.d/modules.sh"
elif [ -f /usr/share/modules/init/bash ]; then
    source /usr/share/modules/init/bash
    echo "Loaded /usr/share/modules/init/bash"
else
    echo "No standard modules.sh found"
fi
echo

echo "========== MODULE COMMAND =========="
type module || echo "module command not found"
echo

echo "========== AVAILABLE MODULES =========="
module avail 2>&1
echo

echo "========== TRY COMMON LOADS =========="
module load gcc 2>&1
module load openmpi 2>&1
echo

echo "========== LOADED MODULES =========="
module list 2>&1
echo

echo "========== COMPILERS =========="
which gfortran 2>&1
which mpif90 2>&1
which mpirun 2>&1
echo

echo "========== VERSION INFO =========="
mpif90 --version 2>&1 || true
mpirun --version 2>&1 || true
echo

echo "========== MAKE TEST =========="
make clean
make parallel OMP_THREADS=1
echo "make parallel exit code: $?"
echo

echo "========== RUN TEST =========="
make run_parallel NP=$NSLOTS OMP_THREADS=1
echo "make run_parallel exit code: $?"
echo

echo "========== END =========="
