# plot_parallel_observables.py
# Strong-scaling + observables comparison for main_parallel_observables.x
# Author: Itxaso Muñoz-Aldalur
#
# What it does:
#   1) Reads benchmark_np.dat with columns: np mpi_wall_time_s shell_wall_time_s exit_code
#   2) Computes speedup and efficiency from the np=1 MPI time.
#   3) Plots wall times, speedup, and efficiency vs np.
#   4) For summary_global_np*.dat, compares observables (Rg, Ree) across np for every (conf, phase).
#   5) Writes CSV tables for both benchmark and observables.

import os
import re
import csv
import glob
import math
import numpy as np
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
RESULTS_DIR = os.path.abspath(os.path.join(HERE, '../../results/parallel_observables'))
BENCH_FILE = os.path.join(RESULTS_DIR, 'benchmark_np.dat')
STYLE_FILE = os.path.join(HERE, 'science.mplstyle')

if os.path.exists(STYLE_FILE):
    plt.style.use(STYLE_FILE)
else:
    plt.style.use('default')


def is_finite_number(x):
    try:
        return math.isfinite(float(x))
    except Exception:
        return False


# ── Benchmark helpers ────────────────────────────────────────────────────────

def parse_benchmark(filepath):
    """
    Expected format:
      # np mpi_wall_time_s shell_wall_time_s exit_code
      1 12.34 12.56 0
      2  6.21  6.40 0
    """
    rows = []

    with open(filepath, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue

            parts = line.split()
            if len(parts) < 4:
                continue

            try:
                p = int(parts[0])
                mpi = float(parts[1]) if parts[1].lower() != 'nan' else np.nan
                shell = float(parts[2]) if parts[2].lower() != 'nan' else np.nan
                exit_code = int(parts[3])
                rows.append((p, mpi, shell, exit_code))
            except ValueError:
                continue

    if not rows:
        raise RuntimeError('No valid rows found in benchmark_np.dat')

    rows.sort(key=lambda x: x[0])
    np_vals = np.array([r[0] for r in rows], dtype=int)
    mpi_times = np.array([r[1] for r in rows], dtype=float)
    shell_times = np.array([r[2] for r in rows], dtype=float)
    exit_codes = np.array([r[3] for r in rows], dtype=int)
    return np_vals, mpi_times, shell_times, exit_codes


def compute_metrics(np_vals, mpi_times, exit_codes):
    valid = (exit_codes == 0) & np.isfinite(mpi_times)
    idx_serial = np.where((np_vals == 1) & valid)[0]
    if len(idx_serial) == 0:
        raise RuntimeError('benchmark_np.dat must contain a valid np=1 row with finite MPI time')

    t1 = mpi_times[idx_serial[0]]
    speedup = np.full_like(mpi_times, np.nan, dtype=float)
    efficiency = np.full_like(mpi_times, np.nan, dtype=float)

    speedup[valid] = t1 / mpi_times[valid]
    efficiency[valid] = speedup[valid] / np_vals[valid]
    return t1, speedup, efficiency, valid


# ── Observables helpers ──────────────────────────────────────────────────────

def discover_summary_files(results_dir):
    patterns = [
        os.path.join(results_dir, 'summary_global_np*.dat'),
        os.path.join(results_dir, 'summary_np*.dat'),
    ]
    files = []
    for pattern in patterns:
        files.extend(glob.glob(pattern))

    def extract_np(path):
        m = re.search(r'_np(\d+)\.dat$', os.path.basename(path))
        return int(m.group(1)) if m else 10**9

    files = sorted(set(files), key=extract_np)
    return files


def parse_summary_file(filepath):
    """
    Expected data lines in summary_global.dat:
      conf phase n_traj n_frames mean_Rg_A std_Rg_A mean_Ree_A std_Ree_A
    """
    data = []
    with open(filepath, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            parts = line.split()
            if len(parts) < 8:
                continue
            try:
                conf = int(parts[0])
                phase = parts[1]
                n_traj = int(parts[2])
                n_frames = int(parts[3])
                mean_rg = float(parts[4])
                std_rg = float(parts[5])
                mean_ree = float(parts[6])
                std_ree = float(parts[7])
            except ValueError:
                continue
            data.append({
                'conf': conf,
                'phase': phase,
                'n_traj': n_traj,
                'n_frames': n_frames,
                'mean_rg_A': mean_rg,
                'std_rg_A': std_rg,
                'mean_ree_A': mean_ree,
                'std_ree_A': std_ree,
            })
    return data


def load_observables_across_np(results_dir):
    files = discover_summary_files(results_dir)
    rows = []

    for filepath in files:
        m = re.search(r'_np(\d+)\.dat$', os.path.basename(filepath))
        if not m:
            continue
        np_val = int(m.group(1))
        for row in parse_summary_file(filepath):
            row2 = dict(row)
            row2['np'] = np_val
            rows.append(row2)

    rows.sort(key=lambda r: (r['conf'], r['phase'], r['np']))
    return rows


def add_np1_deltas(rows):
    baseline = {}
    for r in rows:
        key = (r['conf'], r['phase'])
        if r['np'] == 1:
            baseline[key] = r

    for r in rows:
        key = (r['conf'], r['phase'])
        if key in baseline:
            b = baseline[key]
            r['dRg_vs_np1_A'] = r['mean_rg_A'] - b['mean_rg_A']
            r['dRee_vs_np1_A'] = r['mean_ree_A'] - b['mean_ree_A']
        else:
            r['dRg_vs_np1_A'] = np.nan
            r['dRee_vs_np1_A'] = np.nan
    return rows


# ── Plotting ─────────────────────────────────────────────────────────────────

def plot_benchmark(np_vals, mpi_times, shell_times, speedup, efficiency, valid, outdir):
    fig, axes = plt.subplots(1, 3, figsize=(9, 3))
    ax0, ax1, ax2 = axes

    ax0.plot(np_vals, mpi_times, 'o-', label='MPI wall time')
    ax0.plot(np_vals, shell_times, 's--', label='Shell wall time')
    ax0.set_xlabel('MPI processes')
    ax0.set_ylabel('Time (s)')
    ax0.set_title('Wall time vs np')
    ax0.grid(True, alpha=0.3)
    ax0.legend(fontsize=8)

    ax1.plot(np_vals[valid], speedup[valid], 'o-', label='Measured speedup')
    ax1.plot(np_vals, np_vals, '--', color='grey', label='Ideal')
    ax1.set_xlabel('MPI processes')
    ax1.set_ylabel('Speedup')
    ax1.set_title('Speedup vs np')
    ax1.grid(True, alpha=0.3)
    ax1.legend(fontsize=8)

    ax2.plot(np_vals[valid], efficiency[valid], 'D-', color='green', label='Measured efficiency')
    ax2.axhline(1.0, color='grey', linestyle=':')
    ax2.set_xlabel('MPI processes')
    ax2.set_ylabel('Efficiency')
    ax2.set_title('Efficiency vs np')
    ax2.grid(True, alpha=0.3)
    ax2.legend(fontsize=8)

    plt.tight_layout()
    png = os.path.join(outdir, 'benchmark_vs_np.png')
    pdf = os.path.join(outdir, 'benchmark_vs_np.pdf')
    plt.savefig(png, dpi=200, bbox_inches='tight')
    plt.savefig(pdf, bbox_inches='tight')
    plt.close()
    return png, pdf


def plot_observables(rows, outdir):
    if not rows:
        return None, None

    groups = {}
    for r in rows:
        key = (r['conf'], r['phase'])
        groups.setdefault(key, []).append(r)

    fig, axes = plt.subplots(1, 2, figsize=(7, 3))
    ax_rg, ax_ree = axes

    for (conf, phase), vals in sorted(groups.items()):
        vals = sorted(vals, key=lambda x: x['np'])
        np_arr = np.array([v['np'] for v in vals], dtype=int)
        rg_arr = np.array([v['mean_rg_A'] for v in vals], dtype=float)
        ree_arr = np.array([v['mean_ree_A'] for v in vals], dtype=float)
        label = f'conf{conf}-{phase}'
        ax_rg.plot(np_arr, rg_arr, 'o-', label=label)
        ax_ree.plot(np_arr, ree_arr, 'o-', label=label)

    ax_rg.set_xlabel('MPI processes')
    ax_rg.set_ylabel('Mean Rg (A)')
    ax_rg.set_title('Rg vs np')
    ax_rg.grid(True, alpha=0.3)
    ax_rg.legend(fontsize=7, ncol=2)

    ax_ree.set_xlabel('MPI processes')
    ax_ree.set_ylabel('Mean Ree (A)')
    ax_ree.set_title('Ree vs np')
    ax_ree.grid(True, alpha=0.3)
    ax_ree.legend(fontsize=7, ncol=2)

    plt.tight_layout()
    png = os.path.join(outdir, 'observables_vs_np.png')
    pdf = os.path.join(outdir, 'observables_vs_np.pdf')
    plt.savefig(png, dpi=200, bbox_inches='tight')
    plt.savefig(pdf, bbox_inches='tight')
    plt.close()
    return png, pdf


# ── CSV writers ──────────────────────────────────────────────────────────────

def write_benchmark_csv(csv_out, np_vals, mpi_times, shell_times, exit_codes, speedup, efficiency):
    with open(csv_out, 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow(['np', 'mpi_wall_time_s', 'shell_wall_time_s', 'exit_code', 'speedup', 'efficiency'])
        for row in zip(np_vals, mpi_times, shell_times, exit_codes, speedup, efficiency):
            w.writerow(row)


def write_observables_csv(csv_out, rows):
    if not rows:
        return
    with open(csv_out, 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow([
            'np', 'conf', 'phase', 'n_traj', 'n_frames',
            'mean_rg_A', 'std_rg_A', 'mean_ree_A', 'std_ree_A',
            'dRg_vs_np1_A', 'dRee_vs_np1_A'
        ])
        for r in rows:
            w.writerow([
                r['np'], r['conf'], r['phase'], r['n_traj'], r['n_frames'],
                r['mean_rg_A'], r['std_rg_A'], r['mean_ree_A'], r['std_ree_A'],
                r['dRg_vs_np1_A'], r['dRee_vs_np1_A']
            ])


# ── Main ─────────────────────────────────────────────────────────────────────

def main():
    if not os.path.exists(RESULTS_DIR):
        raise SystemExit('Results directory not found: ' + RESULTS_DIR)
    if not os.path.exists(BENCH_FILE):
        raise SystemExit('benchmark_np.dat not found: ' + BENCH_FILE)

    np_vals, mpi_times, shell_times, exit_codes = parse_benchmark(BENCH_FILE)
    t1, speedup, efficiency, valid = compute_metrics(np_vals, mpi_times, exit_codes)

    bench_csv = os.path.join(RESULTS_DIR, 'benchmark_np_metrics.csv')
    write_benchmark_csv(bench_csv, np_vals, mpi_times, shell_times, exit_codes, speedup, efficiency)
    bench_png, bench_pdf = plot_benchmark(np_vals, mpi_times, shell_times, speedup, efficiency, valid, RESULTS_DIR)

    obs_rows = load_observables_across_np(RESULTS_DIR)
    obs_rows = add_np1_deltas(obs_rows)
    obs_csv = os.path.join(RESULTS_DIR, 'observables_vs_np.csv')
    write_observables_csv(obs_csv, obs_rows)
    obs_png, obs_pdf = plot_observables(obs_rows, RESULTS_DIR)

    print('\nBenchmark summary')
    print(f"{'np':>6} {'t_MPI(s)':>12} {'t_shell(s)':>12} {'exit':>8} {'Speedup':>10} {'Efficiency':>12}")
    for p, tm, ts, ex, sp, ef in zip(np_vals, mpi_times, shell_times, exit_codes, speedup, efficiency):
        sp_s = 'nan' if not is_finite_number(sp) else f'{sp:.3f}'
        ef_s = 'nan' if not is_finite_number(ef) else f'{ef:.3f}'
        tm_s = 'nan' if not is_finite_number(tm) else f'{tm:.4f}'
        ts_s = 'nan' if not is_finite_number(ts) else f'{ts:.4f}'
        print(f"{p:>6} {tm_s:>12} {ts_s:>12} {ex:>8d} {sp_s:>10} {ef_s:>12}")

    print('\nGenerated files:')
    print(' - ' + os.path.basename(bench_csv))
    print(' - ' + os.path.basename(bench_png))
    print(' - ' + os.path.basename(bench_pdf))

    if obs_rows:
        print(' - ' + os.path.basename(obs_csv))
        if obs_png is not None:
            print(' - ' + os.path.basename(obs_png))
        if obs_pdf is not None:
            print(' - ' + os.path.basename(obs_pdf))
    else:
        print('\nNo per-np summary files found for observables comparison.')
        print('To compare observables across np, save a copy after each run, e.g.:')
        print('  cp summary_global.dat summary_global_np${NP}.dat')

    print('\nDone!')


if __name__ == '__main__':
    main()