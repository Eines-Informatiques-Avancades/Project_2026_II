#!/bin/bash
#$ -N polyMC_parallel_bench
#$ -pe smp 4
#$ -q cerqt01.q
#$ -S /bin/bash
#$ -cwd
#$ -o polyMC_parallel_bench_$JOB_ID.out
#$ -e polyMC_parallel_bench_$JOB_ID.err

. /etc/profile
module load gcc
module load openmpi
export OMP_NUM_THREADS=1
export MPLBACKEND=Agg

# Clean
rm -f *.mod *.o *.x
rm -f ../bin/*.o ../bin/*.mod ../bin/*.x

make -j1 parallel
mkdir -p ../results

# N sweep: fixed n_steps = 1000000
for nc in 20 50 100 200; do
  cat > confs/input.dat <<EOF
n_carbons  = ${nc}
explicit_h = .true.
conf_type  = 1
rng_seed   = 1234
n_steps    = 1000000
EOF
  echo "Running parallel: nc=${nc}, steps=1000000"
  mpirun -np 3 ../bin/main_parallel_replicas.x
done

# Steps sweep: fixed n_carbons = 100
for ns in 100000 10000000; do
  cat > confs/input.dat <<EOF
n_carbons  = 100
explicit_h = .true.
conf_type  = 1
rng_seed   = 1234
n_steps    = ${ns}
EOF
  echo "Running parallel: nc=100, steps=${ns}"
  mpirun -np 3 ../bin/main_parallel_replicas.x
done

echo "All parallel benchmark runs complete."