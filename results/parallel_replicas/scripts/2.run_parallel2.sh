#!/bin/bash
#$ -N polyMC_parallel_20
#$ -pe smp 4
#$ -q cerqt01.q
#$ -S /bin/bash
#$ -cwd
#$ -o polyMC_parallel_20_$JOB_ID.out
#$ -e polyMC_parallel_20_$JOB_ID.err

. /etc/profile
module load gcc
module load openmpi
export OMP_NUM_THREADS=1
export MPLBACKEND=Agg

# *** Clean BOTH src/ and bin/ ***
rm -f *.mod *.o *.x
rm -f ../bin/*.o ../bin/*.mod ../bin/*.x

make -j1 parallel
mkdir -p ../results

cat > confs/input.dat <<EOF
n_carbons  = 20
explicit_h = .true.
conf_type  = 1
rng_seed   = 1234
n_steps    = 1000000
EOF

echo "Running parallel: nc=20, steps=1000000"
mpirun -np 3 ../bin/main_parallel.x
