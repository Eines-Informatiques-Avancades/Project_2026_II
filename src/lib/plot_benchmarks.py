import pandas as pd
import matplotlib.pyplot as plt
import os

plt.style.use(os.path.join(os.path.dirname(__file__), 'science.mplstyle'))

OUT_DIR = os.path.join(os.path.dirname(__file__), '..', '..', 'results')

def plot_total_omp():
    csv_path = os.path.join(OUT_DIR, 'bench_omp_total', 'total.csv')
    if not os.path.exists(csv_path):
        print(f"Skipping Total Energy Plots. File not found: {csv_path}")
        return

    df = pd.read_csv(csv_path)
    colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
    threads = sorted(df['OMP_THREADS'].unique())
    color_map = {thr: colors[i % len(colors)] for i, thr in enumerate(threads)}

    serial = df[df['OMP_THREADS'] == 1].set_index('N_CARBONS')['TIME_S']

    fig, axes = plt.subplots(1, 2, figsize=(12, 5))

    # --- Panel 1: Wall time ---
    ax1 = axes[0]
    for thr in threads:
        sub = df[df['OMP_THREADS'] == thr].sort_values('N_CARBONS')
        ax1.plot(sub['N_CARBONS'], sub['TIME_S'], marker='o',
                 color=color_map[thr], label=f"{thr} thread{'s' if thr > 1 else ''}")
    ax1.set_xlabel("Number of Carbons")
    ax1.set_ylabel("Wall Time (s)  [10,000 evaluations]")
    ax1.set_title("Wall Time")

    # --- Panel 2: Parallel efficiency ---
    ax2 = axes[1]
    ax2.axhline(1.0, color='black', linestyle='--', linewidth=0.8, label='Ideal (E = 1)')
    for thr in threads:
        if thr == 1:
            continue
        d = df[df['OMP_THREADS'] == thr].sort_values('N_CARBONS').copy()
        d = d[d['N_CARBONS'].isin(serial.index)]
        d['efficiency'] = d.apply(
            lambda r: serial.loc[r['N_CARBONS']] / (thr * r['TIME_S']), axis=1)
        ax2.plot(d['N_CARBONS'], d['efficiency'], marker='o',
                 color=color_map[thr], label=f"{thr} threads")
    ax2.set_xlabel("Number of Carbons")
    ax2.set_ylabel("Parallel Efficiency  $E = T_1 / (p \cdot T_p)$")
    ax2.set_title("Parallel Efficiency")
    ax2.set_ylim(bottom=0)

    # Shared legend below both panels
    handles, labels = axes[0].get_legend_handles_labels()
    ideal_pairs = [(h, l) for h, l in zip(*axes[1].get_legend_handles_labels()) if l.startswith('Ideal')]
    all_handles = handles + [h for h, _ in ideal_pairs]
    all_labels  = labels  + [l for _, l in ideal_pairs]
    fig.legend(all_handles, all_labels, loc='lower center',
               ncol=len(all_handles), frameon=True,
               bbox_to_anchor=(0.5, -0.08))

    fig.suptitle("Total Energy Initialization Performance (OpenMP)", y=1.02)
    plt.tight_layout()
    plt.savefig(os.path.join(OUT_DIR, 'bench_omp_total', 'total_omp_performance.pdf'),
                bbox_inches='tight')
    plt.close()
    print("Generated Total Energy OMP plot.")

def plot_delta_omp():
    csv_path = os.path.join(OUT_DIR, 'bench_omp_delta', 'delta.csv')
    if not os.path.exists(csv_path):
        print(f"Skipping Delta Energy Plots. File not found: {csv_path}")
        return

    df = pd.read_csv(csv_path)
    colors = plt.rcParams['axes.prop_cycle'].by_key()['color']

    for ns in sorted(df['N_STEPS'].unique()):
        sub_ns = df[df['N_STEPS'] == ns]
        threads = sorted(sub_ns['OMP_THREADS'].unique())
        color_map = {thr: colors[i % len(colors)] for i, thr in enumerate(threads)}

        # Build serial baseline (1 thread) per N_CARBONS for efficiency calculation
        serial = sub_ns[sub_ns['OMP_THREADS'] == 1].set_index('N_CARBONS')['TIME_S']

        fig, axes = plt.subplots(1, 2, figsize=(12, 5))

        # --- Panel 1: Wall time ---
        ax1 = axes[0]
        for thr in threads:
            d = sub_ns[sub_ns['OMP_THREADS'] == thr].sort_values('N_CARBONS')
            ax1.plot(d['N_CARBONS'], d['TIME_S'], marker='o',
                     color=color_map[thr], label=f"{thr} thread{'s' if thr > 1 else ''}")
        ax1.set_xlabel("Number of Carbons")
        ax1.set_ylabel("Wall Time (s)")
        ax1.set_title("Wall Time")

        # --- Panel 2: Parallel efficiency ---
        ax2 = axes[1]
        ax2.axhline(1.0, color='black', linestyle='--', linewidth=0.8, label='Ideal (E = 1)')
        for thr in threads:
            if thr == 1:
                continue
            d = sub_ns[sub_ns['OMP_THREADS'] == thr].sort_values('N_CARBONS').copy()
            # Only include rows where serial baseline exists for that system size
            d = d[d['N_CARBONS'].isin(serial.index)]
            d['efficiency'] = d.apply(
                lambda r: serial.loc[r['N_CARBONS']] / (thr * r['TIME_S']), axis=1)
            ax2.plot(d['N_CARBONS'], d['efficiency'], marker='o',
                     color=color_map[thr], label=f"{thr} threads")
        ax2.set_xlabel("Number of Carbons")
        ax2.set_ylabel("Parallel Efficiency  $E = T_1 / (p \cdot T_p)$")
        ax2.set_title("Parallel Efficiency")
        ax2.set_ylim(bottom=0)

        # Shared legend below both panels
        handles, labels = axes[0].get_legend_handles_labels()
        eff_handles, eff_labels = axes[1].get_legend_handles_labels()
        # Merge: efficiency panel has the ideal line; use it as last entry
        ideal_pairs = [(h, l) for h, l in zip(eff_handles, eff_labels) if l.startswith('Ideal')]
        all_handles = handles + [h for h, _ in ideal_pairs]
        all_labels  = labels  + [l for _, l in ideal_pairs]
        fig.legend(all_handles, all_labels, loc='lower center',
                   ncol=len(all_labels), frameon=True,
                   bbox_to_anchor=(0.5, -0.08))

        fig.suptitle(f"Delta Energy OMP Benchmarks  ({ns:,} MC Steps)", y=1.02)
        plt.tight_layout()
        out_path = os.path.join(OUT_DIR, 'bench_omp_delta', f'delta_omp_{ns}_steps.pdf')
        plt.savefig(out_path, bbox_inches='tight')
        plt.close()

    print("Generated Delta Energy OMP plots.")

if __name__ == "__main__":
    print("Assuming execution from Project_2026_II/src/")
    plot_total_omp()
    plot_delta_omp()
    print("Finished Plotting Protocol.")
