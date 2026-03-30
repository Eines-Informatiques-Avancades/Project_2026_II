#!/bin/bash
#$ -N polyMC_serial_bench
#$ -pe smp 1
#$ -q cerqt01.q
#$ -S /bin/bash
#$ -cwd
#$ -o polyMC_serial_bench_$JOB_ID.out
#$ -e polyMC_serial_bench_$JOB_ID.err

. /etc/profile
module load gcc
export OMP_NUM_THREADS=1
export MPLBACKEND=Agg

make clean
make

mkdir -p ../results

# N sweep: fixed n_steps = 1000000, conf_types 1, 4, 5
for nc in 50 100 200; do
  for conf in 1 4 5; do
    cat > confs/input.dat <<EOF
n_carbons  = ${nc}
explicit_h = .true.
conf_type  = ${conf}
rng_seed   = 1234
n_steps    = 1000000
EOF
    echo "Running serial: nc=${nc}, conf=${conf}, steps=1000000"
    ../bin/main_serial.x
  done
done

# Steps sweep: fixed n_carbons = 100, conf_types 1, 4, 5
for ns in 100000 10000000; do
  for conf in 1 4 5; do
    cat > confs/input.dat <<EOF
n_carbons  = 100
explicit_h = .true.
conf_type  = ${conf}
rng_seed   = 1234
n_steps    = ${ns}
EOF
    echo "Running serial: nc=100, conf=${conf}, steps=${ns}"
    ../bin/main_serial.x
  done
done

make clean
echo "All serial benchmark runs complete."
