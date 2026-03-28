#!/bin/bash
#$ -N polyMC_parallel
#$ -pe smp 3
#$ -q cerqt01.q
#$ -S /bin/bash
#$ -cwd
#$ -o polyMC_parallel_$JOB_ID.out
#$ -e polyMC_parallel_$JOB_ID.err

source /etc/profile.d/modules.sh

module load gcc
module load openmpi

export MPLBACKEND=Agg
export OMP_NUM_THREADS=1

make clean
make parallel OMP_THREADS=1
make run_parallel NP=$NSLOTS OMP_THREADS=1
make clean
