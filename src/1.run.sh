#!/bin/bash
#$ -N polyMC
#$ -pe smp 4
#$ -q cerqt01.q
#$ -S /bin/bash
#$ -cwd
#$ -o polyMC_$JOB_ID.out
#$ -e polyMC_$JOB_ID.err

. /etc/profile
module load gcc
export OMP_NUM_THREADS=$NSLOTS
export MPLBACKEND=Agg

make clean
make
make run
make clean