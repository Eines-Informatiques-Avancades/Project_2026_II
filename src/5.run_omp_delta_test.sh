#!/bin/bash
#$ -N polyMC_omp_delta_bench
#$ -pe smp 12
#$ -q cerqt01.q
#$ -S /bin/bash
#$ -cwd
#$ -o polyMC_omp_delta_bench_$JOB_ID.out
#$ -e polyMC_omp_delta_bench_$JOB_ID.err

. /etc/profile

# Module for OpenMP is suited for cerqt01.q. 
# Adjust as needed for other queues or environments.
module load openmpi/3.1.3_ics-2015.0

export MPLBACKEND=Agg

OUT_DIR="../results/bench_omp_delta"
mkdir -p $OUT_DIR
OUT_CSV="${OUT_DIR}/delta.csv"
echo "OMP_THREADS,N_CARBONS,N_STEPS,TIME_S" > $OUT_CSV

# Return the max final wall time across all rank cpu files for a given (nc, ns).
# Mirrors plot_parallel_replicas.py: final_time() takes the last column of the
# last data line; par_wall takes max() across ranks.
get_wall_time() {
  local nc=$1 ns=$2
  local max_t=0 t
  for f in ../results/cpu_${nc}_*_${ns}_300.00_rank*.dat; do
    [ -f "$f" ] || continue
    t=$(awk '!/^[[:space:]]*#/ && NF>=2 { last=$NF+0 } END { print last+0 }' "$f")
    max_t=$(awk -v a="$max_t" -v b="$t" 'BEGIN { print (a+0 > b+0 ? a+0 : b+0) }')
  done
  echo "$max_t"
}

run_delta_test() {
  local thr=$1
  make clean
  make parallel_replicas OMP_THREADS=$thr
  if [ ! -x ../bin/main_parallel_replicas.x ]; then
    echo "ERROR: build failed for OMP_THREADS=$thr — skipping" >&2
    return 1
  fi

  for nc in 50 100 500; do
    for ns in 10000000; do
      cat > confs/input.dat <<EOF
n_carbons  = ${nc}
explicit_h = .true.
conf_type  = 1
rng_seed   = 1234
n_steps    = ${ns}
EOF
      echo "Running (Delta OMP): Thr=${thr}, nc=${nc}, steps=${ns}"
      export OMP_NUM_THREADS=$thr
      rm -f ../results/cpu_${nc}_*_${ns}_300.00_rank*.dat
      mpirun -np 3 ../bin/main_parallel_replicas.x > /dev/null 2>&1
      time_s=$(get_wall_time "$nc" "$ns")
      echo "${thr},${nc},${ns},${time_s}" >> $OUT_CSV
    done
  done
}

for threads in 1 2 3 4; do
  run_delta_test $threads
done

make clean
echo "All omp_delta_energy benchmark runs complete."
