#!/bin/bash
#$ -N polyMC_serial_bench
#$ -pe smp 1
#$ -q cerqt01.q
#$ -S /bin/bash
#$ -cwd
#$ -o polyMC_serial_bench_20_5_106.out
#$ -e polyMC_serial_bench_20_5_106.err

. /etc/profile
module load gcc
export OMP_NUM_THREADS=1
export MPLBACKEND=Agg

# *** Clean BOTH src/ and bin/ — stale .mod in src/ was the bug ***
rm -f *.mod *.o *.x
rm -f ../bin/*.o ../bin/*.mod ../bin/*.x

make -j1
make -j1 run
