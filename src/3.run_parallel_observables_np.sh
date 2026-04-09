#!/bin/bash
#$ -N polyMC_parallel_observables_np
#$ -pe smp 8
#$ -q cerqt01.q
#$ -S /bin/bash
#$ -cwd
#$ -o polyMC_parallel_observables_np_$JOB_ID.out
#$ -e polyMC_parallel_observables_np_$JOB_ID.err

. /etc/profile
module load openmpi
export OMP_NUM_THREADS=1
export MPLBACKEND=Agg

SCRIPT_DIR="${SGE_O_WORKDIR}"
# Base directory: Manel's results in results_main_star_equil_collab: conf1/, conf4/, conf5/.
INPUT_DIR=$(readlink -f "${SCRIPT_DIR}/../results/results_main_star_equil_collab")

# New output dir
OUT_DIR="${SCRIPT_DIR}/../results/parallel_observables"
mkdir -p "${OUT_DIR}"
OUT_DIR=$(readlink -f "${OUT_DIR}")

BENCH_FILE="${OUT_DIR}/benchmark_np.dat"
MANIFEST_FILE="${OUT_DIR}/filelist_explicit.txt"

make clean
make parallel_observables || exit 1

mkdir -p "${OUT_DIR}"

echo "------------------------------------------------------------"
echo "Input dir    : ${INPUT_DIR}"
echo "Output dir   : ${OUT_DIR}"
echo "Manifest file: ${MANIFEST_FILE}"


# Find the recursive directories conf1/conf4/conf5 × equil/prod
DIRS=(
  "${INPUT_DIR}/conf1/equil"
  "${INPUT_DIR}/conf1/prod"
  "${INPUT_DIR}/conf4/equil"
  "${INPUT_DIR}/conf4/prod"
  "${INPUT_DIR}/conf5/equil"
  "${INPUT_DIR}/conf5/prod"
)

rm -f "${MANIFEST_FILE}"
touch "${MANIFEST_FILE}"
echo "Building explicit manifest..."

for D in "${DIRS[@]}"; do
  echo "  checking: ${D}"
  if [ -d "${D}" ]; then
    for F in "${D}"/trajectory_*.xyz; do
      if [ -f "${F}" ]; then
        printf '%s\n' "${F}" >> "${MANIFEST_FILE}"
      fi
    done
  else
    echo "WARNING: directory not found: ${D}" >&2
  fi
done

sort -o "${MANIFEST_FILE}" "${MANIFEST_FILE}"

N_TRAJ=$(wc -l < "${MANIFEST_FILE}")
echo "Trajectory files found in explicit manifest: ${N_TRAJ}"

if [ "${N_TRAJ}" -eq 0 ]; then
  echo "ERROR: No trajectory_*.xyz files found in the explicit directories." >&2
  echo "Manifest: ${MANIFEST_FILE}" >&2
  exit 1
fi

echo "First lines of manifest:"
head -10 "${MANIFEST_FILE}"
echo "------------------------------------------------------------"

cat > "${BENCH_FILE}" <<EOF
# np mpi_wall_time_s shell_wall_time_s exit_code
EOF

echo "Running MPI observables benchmark..."
echo "Bench file: ${BENCH_FILE}"

for NP in 1 2 4 8; do
  LOG_FILE="${OUT_DIR}/run_np${NP}.log"
  METRIC_FILE="${OUT_DIR}/metrics_np${NP}.dat"

  rm -f "${LOG_FILE}" "${METRIC_FILE}"

  echo "------------------------------------------------------------"
  echo "Running observables: np=${NP}"
  echo "Log file    : ${LOG_FILE}"
  echo "Metrics file: ${METRIC_FILE}"

  START_TS=$(date +%s.%N)

  mpirun -np ${NP} --wdir "${SCRIPT_DIR}" \
    "${SCRIPT_DIR}/../bin/main_parallel_observables.x" \
    "${INPUT_DIR}" "${METRIC_FILE}" "${OUT_DIR}" "${MANIFEST_FILE}" \
    > "${LOG_FILE}" 2>&1
  STATUS=$?

  END_TS=$(date +%s.%N)
  SHELL_WALL=$(awk -v s="${START_TS}" -v e="${END_TS}" 'BEGIN {printf "%.6f", (e-s)}')

  if [ -s "${METRIC_FILE}" ]; then
    MPI_WALL=$(awk 'NR==2 {print $2}' "${METRIC_FILE}")
  else
    MPI_WALL="NaN"
    echo "WARNING: metrics file missing or empty for np=${NP}" >&2
    echo "Last lines of ${LOG_FILE}:"
    tail -20 "${LOG_FILE}" 2>/dev/null
  fi

  echo "${NP} ${MPI_WALL} ${SHELL_WALL} ${STATUS}" >> "${BENCH_FILE}"
  echo "np=${NP}  mpi_wall=${MPI_WALL}  shell_wall=${SHELL_WALL}  exit_code=${STATUS}"

  if [ ${STATUS} -ne 0 ]; then
    echo "ERROR: mpirun failed for np=${NP} (exit ${STATUS})" >&2
    echo "Full log (${LOG_FILE}):"
    cat "${LOG_FILE}" 2>/dev/null
    break
  fi
done

make clean
echo "------------------------------------------------------------"
echo "Benchmark complete."
echo "Results stored in: ${BENCH_FILE}"
echo "Manifest used     : ${MANIFEST_FILE}"
