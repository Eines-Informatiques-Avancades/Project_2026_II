#!/bin/bash
#$ -N polyMC_parallel
#$ -pe smp 3
#$ -q cerqt01.q
#$ -S /bin/bash
#$ -cwd
#$ -o polyMC_parallel_100_1_107.out
#$ -e polyMC_parallel_100_1_107.err
#$ -l h_rt=4:00:00

source /etc/profile.d/modules.sh

module load gcc
module load openmpi

export MPLBACKEND=Agg
export OMP_NUM_THREADS=1

make clean
make parallel OMP_THREADS=1
make run_parallel NP=$NSLOTS OMP_THREADS=1
make clean
