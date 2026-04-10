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
    plt.figure(figsize=(9,6))
    for thr in sorted(df['OMP_THREADS'].unique()):
        sub = df[df['OMP_THREADS'] == thr].sort_values(by='N_CARBONS')
        plt.plot(sub['N_CARBONS'], sub['TIME_S'], marker='o', label=f"{thr} Threads")
    
    plt.xlabel("Number of Carbons (System Size)")
    plt.ylabel("Wall Time (s) [for 10,000 evaluations]")
    plt.title("Total Energy Initialization Performance (OpenMP)")
    plt.legend()
    plt.grid(True)
    plt.tight_layout()
    plt.savefig(os.path.join(OUT_DIR, 'bench_omp_total', 'total_omp_performance.pdf'))
    plt.close()
    print("Generated Total Energy OMP plot.")

def plot_delta_omp():
    csv_path = os.path.join(OUT_DIR, 'bench_omp_delta', 'delta.csv')
    if not os.path.exists(csv_path):
        print(f"Skipping Delta Energy Plots. File not found: {csv_path}")
        return

    df = pd.read_csv(csv_path)
    for ns in df['N_STEPS'].unique():
        plt.figure(figsize=(9,6))
        for thr in sorted(df['OMP_THREADS'].unique()):
            sub = df[(df['OMP_THREADS'] == thr) & (df['N_STEPS'] == ns)].sort_values(by='N_CARBONS')
            plt.plot(sub['N_CARBONS'], sub['TIME_S'], marker='o', label=f"{thr} Threads")
        
        plt.xlabel("Number of Carbons (System Size)")
        plt.ylabel("Wall Time (s)")
        plt.title(f"Delta Energy Profiling (MC Loop, {ns} Steps)")
        plt.legend()
        plt.grid(True)
        plt.tight_layout()
        plt.savefig(os.path.join(OUT_DIR, 'bench_omp_delta', f'delta_omp_{ns}_steps.pdf'))
        plt.close()
    print("Generated Delta Energy OMP plots.")

if __name__ == "__main__":
    print("Assuming execution from Project_2026_II/src/")
    plot_total_omp()
    plot_delta_omp()
    print("Finished Plotting Protocol.")
