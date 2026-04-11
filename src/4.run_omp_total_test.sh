#!/bin/bash
#$ -N polyMC_omp_total_bench
#$ -pe smp 4
#$ -q cerqt01.q
#$ -S /bin/bash
#$ -cwd
#$ -o polyMC_omp_total_bench_$JOB_ID.out
#$ -e polyMC_omp_total_bench_$JOB_ID.err

. /etc/profile

module load gcc/8.3.0
module load openmpi/3.1.3_ics-2015.0

export MPLBACKEND=Agg

OUT_DIR="../results/bench_omp_total"
mkdir -p $OUT_DIR
OUT_CSV="${OUT_DIR}/total.csv"
echo "OMP_THREADS,N_CARBONS,TIME_S" > $OUT_CSV

run_total_test() {
  local thr=$1
  make -f Makefile_bench clean
  make -f Makefile_bench bench_total OMP_THREADS=$thr
  if [ ! -x ../bin/main_bench_total.x ]; then
    echo "ERROR: build failed for OMP_THREADS=$thr — skipping" >&2
    return 1
  fi

  for nc in 20 50 100 500; do
    cat > confs/input.dat <<EOF
n_carbons  = ${nc}
explicit_h = .true.
conf_type  = 1
rng_seed   = 1234
n_steps    = 1000
EOF
    echo "Running (Total OMP): Thr=${thr}, nc=${nc}"
    export OMP_NUM_THREADS=$thr
    TMPFILE="/tmp/bench_total_$$_${thr}_${nc}.txt"
    mpirun -np 1 ../bin/main_bench_total.x > "$TMPFILE" 2>&1
    out=$(grep BENCH_TOTAL_TIME "$TMPFILE")
    rm -f "$TMPFILE"

    # Parse output, e.g. "BENCH_TOTAL_TIME=       1.455000s"
    time_s=$(echo $out | sed 's/.*BENCH_TOTAL_TIME=\s*//' | sed 's/s//' | tr -d ' ')
    echo "${thr},${nc},${time_s}" >> $OUT_CSV
  done
}

for threads in 1 2 3 4; do
  run_total_test $threads
done

make -f Makefile_bench clean
echo "All omp_total_energy benchmark runs complete."