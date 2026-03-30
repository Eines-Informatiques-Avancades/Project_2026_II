#!/bin/bash
#$ -N polyMC_serial_bench
#$ -pe smp 1
#$ -q cerqt01.q
#$ -S /bin/bash
#$ -cwd
#$ -o polyMC_serial_bench_20_1_106.out
#$ -e polyMC_serial_bench_20_1_106.err

. /etc/profile
module load gcc

export OMP_NUM_THREADS=$NSLOTS
export MPLBACKEND=Agg

make clean
make
make run
make clean
