# plots_parallelTIME.py
# Speedup analysis: serial (3 sequential runs) vs parallel (3 MPI ranks)
# Author: Itxaso Muñoz-Aldalur

import numpy as np
import matplotlib.pyplot as plt
import os
import re
import glob

plt.style.use(os.path.join(os.path.dirname(__file__), 'science.mplstyle'))

RESULTS_DIR = '../../results/parallel_replicas/'
if not os.path.exists(RESULTS_DIR):
    print("Results directory not found.")
    exit(1)


# ── Helpers ───────────────────────────────────────────────────────────────────

def final_time(filepath):
    """
    Return the last wall-time value from a cpu .dat file.
    These files are mixed-column (energy, obs, torsion, cpu lines interleaved).
    CPU time lines are exactly 2 columns: step  wall_time_s
    We pick only those lines and return the time from the last one.
    """
    last_t = None
    try:
        with open(filepath, 'r') as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#'):
                    continue
                parts = line.split()
                if len(parts) == 2:
                    try:
                        last_t = float(parts[1])
                    except ValueError:
                        continue
    except Exception as e:
        print(f"  Warning: could not read {filepath}: {e}")
        return None

    if last_t is None:
        print(f"  Warning: no 2-column cpu line found in {filepath}")
    return last_t


def parse_cpu_files(results_dir):
    """
    Parse all cpu_*.dat files.
    parallel[(nc, ns)] = max wall time across the 3 ranks
    serial[(nc, ns)]   = sum of CPU times for conf 1, 4, 5
    """
    parallel = {}
    serial   = {}

    for fpath in sorted(glob.glob(os.path.join(results_dir, 'cpu_*.dat'))):
        fname = os.path.basename(fpath)

        core = re.match(
            r'cpu_(\d+)_(\d+)_(\d+)_([\d.]+)(?:_([\d.]+))?(_rank\d+)?\.dat$',
            fname
        )
        if core is None:
            continue

        nc  = int(core.group(1))
        ns  = int(core.group(3))
        key = (nc, ns)
        t   = final_time(fpath)
        if t is None:
            continue

        if core.group(6) is not None:   # has _rankX suffix → parallel
            if key not in parallel:
                parallel[key] = []
            parallel[key].append(t)
        else:                           # no rank suffix → serial
            if key not in serial:
                serial[key] = []
            serial[key].append(t)

    par_wall  = {k: max(v) for k, v in parallel.items() if len(v) == 3}
    ser_total = {k: sum(v) for k, v in serial.items()   if len(v) == 3}
    return par_wall, ser_total


# ── Data ──────────────────────────────────────────────────────────────────────

par_wall, ser_total = parse_cpu_files(RESULTS_DIR)
common_keys = sorted(set(par_wall) & set(ser_total))

if not common_keys:
    print("No matching (n_carbons, n_steps) pairs found for both serial and parallel.")
    print("Check that ../results/ contains cpu files for both runs.")
    exit(1)

n_fixed_steps = 1_000_000
n_fixed_carb  = 100

n_sweep = sorted([(nc, ns) for nc, ns in common_keys if ns == n_fixed_steps],
                 key=lambda x: x[0])
s_sweep = sorted([(nc, ns) for nc, ns in common_keys if nc == n_fixed_carb],
                 key=lambda x: x[1])


# ── Plot 1: wall time and speedup vs N_atoms ──────────────────────────────────

if n_sweep:
    nc_vals   = [nc for nc, ns in n_sweep]
    natoms    = [2*nc + 2 for nc in nc_vals]
    par_times = [par_wall[(nc, ns)]  for nc, ns in n_sweep]
    ser_times = [ser_total[(nc, ns)] for nc, ns in n_sweep]
    speedups  = [s/p for s, p in zip(ser_times, par_times)]

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(7, 3))

    ax1.plot(natoms, ser_times, 'o-', label='Serial (3 runs summed)')
    ax1.plot(natoms, par_times, 's--', label='Parallel (wall clock)')
    ax1.set_xlabel('Number of atoms')
    ax1.set_ylabel('Time (s)')
    ax1.set_title(f'Wall time vs $N_{{atoms}}$  ($n_{{steps}}=10^6$)')
    ax1.set_xlim(left=0)
    ax1.grid(True, alpha=0.3)
    ax1.legend()

    ax2.plot(natoms, speedups, 'D-', color='green')
    ax2.axhline(3.0, color='grey', linestyle=':', label='Ideal $S=3$')
    ax2.set_xlabel('Number of atoms')
    ax2.set_ylabel(r'Speedup $S = t_\mathrm{serial} / t_\mathrm{parallel}$')
    ax2.set_title(f'Speedup vs $N_{{atoms}}$  ($n_{{steps}}=10^6$)')
    ax2.set_xlim(left=0)
    ax2.set_ylim(bottom=0)
    ax2.grid(True, alpha=0.3)
    ax2.legend()

    plt.tight_layout()
    out = os.path.join(RESULTS_DIR, 'speedup_vs_natoms.pdf')
    plt.savefig(out)
    plt.close()
    print(f"Generated {os.path.basename(out)}")

    print(f"\n{'N_C':>6} {'N_atoms':>8} {'t_serial(s)':>13} {'t_parallel(s)':>15} {'Speedup':>9}")
    for (nc, ns), p, s, sp in zip(n_sweep, par_times, ser_times, speedups):
        print(f"{nc:>6} {2*nc+2:>8} {s:>13.2f} {p:>15.2f} {sp:>9.2f}")


# ── Plot 2: wall time and speedup vs n_steps ──────────────────────────────────

if s_sweep:
    ns_vals   = [ns for nc, ns in s_sweep]
    par_times = [par_wall[(nc, ns)]  for nc, ns in s_sweep]
    ser_times = [ser_total[(nc, ns)] for nc, ns in s_sweep]
    speedups  = [s/p for s, p in zip(ser_times, par_times)]

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(7, 3))

    ax1.loglog(ns_vals, ser_times, 'o-', label='Serial (3 runs summed)')
    ax1.loglog(ns_vals, par_times, 's--', label='Parallel (wall clock)')
    ax1.set_xlabel('MC steps')
    ax1.set_ylabel('Time (s)')
    ax1.set_title(f'Wall time vs $n_{{steps}}$  ($N_C={n_fixed_carb}$)')
    ax1.grid(True, alpha=0.3, which='both')
    ax1.legend()

    ax2.semilogx(ns_vals, speedups, 'D-', color='green')
    ax2.axhline(3.0, color='grey', linestyle=':', label='Ideal $S=3$')
    ax2.set_xlabel('MC steps')
    ax2.set_ylabel(r'Speedup $S = t_\mathrm{serial} / t_\mathrm{parallel}$')
    ax2.set_title(f'Speedup vs $n_{{steps}}$  ($N_C={n_fixed_carb}$)')
    ax2.set_ylim(bottom=0)
    ax2.grid(True, alpha=0.3, which='both')
    ax2.legend()

    plt.tight_layout()
    out = os.path.join(RESULTS_DIR, 'speedup_vs_nsteps.pdf')
    plt.savefig(out)
    plt.close()
    print(f"Generated {os.path.basename(out)}")

    print(f"\n{'n_steps':>12} {'t_serial(s)':>13} {'t_parallel(s)':>15} {'Speedup':>9}")
    for (nc, ns), p, s, sp in zip(s_sweep, par_times, ser_times, speedups):
        print(f"{ns:>12,} {s:>13.2f} {p:>15.2f} {sp:>9.2f}")

print("\nDone!")