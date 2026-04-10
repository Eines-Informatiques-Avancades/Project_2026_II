import numpy as np
import matplotlib.pyplot as plt
import os
import glob
import re

# Dynamically resolve the absolute path to Project_2026_II/results/
OUTPUT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '../../results/'))

def get_last_time(filepath):
    """Read the last line of a CPU dat file and extract the elapsed time."""
    try:
        with open(filepath, 'r') as f:
            lines = f.readlines()
            # Iterate backwards to find the last valid data line
            for line in reversed(lines):
                if line.strip() and not line.startswith('#'):
                    # Output format is assumed: <step> <cpu_elapsed>
                    return float(line.split()[1])
    except Exception as e:
        print(f"Error reading {filepath}: {e}")
    return 0.0

def get_mpi_wall_time(folder_path, num_workers):
    """
    Sum the CPU times for each worker across all its tasks,
    then return the maximum sum among all workers (the true wall time).
    """
    worker_times = {f"w{i}": 0.0 for i in range(1, num_workers + 1)}
    
    cpu_files = glob.glob(os.path.join(folder_path, "cpu_prod_*.dat"))
    if not cpu_files:
        print(f"Warning: No CPU files found in {folder_path}")
        return 0.0, {}

    for filepath in cpu_files:
        filename = os.path.basename(filepath)
        # Extract worker id using regex, e.g., cpu_prod_..._w1.dat -> 1
        m = re.search(r'_w(\d+)\.dat', filename)
        if m:
            worker_id = f"w{m.group(1)}"
            last_time = get_last_time(filepath)
            
            if worker_id in worker_times:
                worker_times[worker_id] += last_time

    # The slowest worker dictates the total wall clock time for the batch
    max_time = max(worker_times.values()) if worker_times.values() else 0.0
    return max_time, worker_times

def main():
    # Attempt to load the science style 
    style_path = os.path.join(os.path.dirname(__file__), 'science.mplstyle')
    if os.path.exists(style_path):
        plt.style.use(style_path)

    # === Data Extraction ===
    # 1. Serial Wall Time
    serial_file = os.path.join(OUTPUT_DIR, "serial_geweke_CH_conf4_10Mil_300K/prod_cpu_500_4_10000000_300.00.dat")
    serial_time = get_last_time(serial_file)
    print(f"1-Worker (Serial) Wall Time: {serial_time:.2f} s")

    # 2. Parallel 2-Workers
    folder_2w = os.path.join(OUTPUT_DIR, "parallel_star/geweke_2workers")
    time_2w, wt_2 = get_mpi_wall_time(folder_2w, num_workers=2)
    print(f"2-Worker Wall Time (Max):    {time_2w:.2f} s")
    for w, t in wt_2.items(): print(f"  -> {w}: {t:.2f} s")

    # 3. Parallel 3-Workers
    folder_3w = os.path.join(OUTPUT_DIR, "parallel_star/geweke_3workers")
    time_3w, wt_3 = get_mpi_wall_time(folder_3w, num_workers=3)
    print(f"3-Worker Wall Time (Max):    {time_3w:.2f} s")
    for w, t in wt_3.items(): print(f"  -> {w}: {t:.2f} s")

    # 4. Parallel 4-Workers
    folder_4w = os.path.join(OUTPUT_DIR, "parallel_star/geweke_4workers")
    time_4w, wt_4 = get_mpi_wall_time(folder_4w, num_workers=4)
    print(f"4-Worker Wall Time (Max):    {time_4w:.2f} s")
    for w, t in wt_4.items(): print(f"  -> {w}: {t:.2f} s")

    # 5. Parallel 5-Workers
    folder_5w = os.path.join(OUTPUT_DIR, "parallel_star/geweke_5workers")
    time_5w, wt_5 = get_mpi_wall_time(folder_5w, num_workers=5)
    print(f"5-Worker Wall Time (Max):    {time_5w:.2f} s")
    for w, t in wt_5.items(): print(f"  -> {w}: {t:.2f} s")

    if serial_time == 0.0 or time_2w == 0.0 or time_3w == 0.0 or time_4w == 0.0 or time_5w == 0.0:
        print("\n[!] Warning: Missing or empty data files. The plot will have zeros.")

    # === Plotting Arrays ===
    # Using 'Active Worker Nodes' for scale (Serial equivalent = 1 active calculation)
    workers = [1, 2, 3, 4, 5]
    times = [serial_time, time_2w, time_3w, time_4w, time_5w]
    
    # Calculate relative speedup (Ts / Tp)
    speedups = [serial_time / t if t > 0 else 0.0 for t in times]
    ideal_speedups = workers  # Ideal speedup equals number of working nodes

    # === Build the Figure ===
    fig, ax1 = plt.subplots(figsize=(9, 6))

    # Time Axis (Left)
    color1 = '#000080' # Navy
    ax1.set_xlabel('Number of Active Worker Nodes', fontweight='bold')
    ax1.set_ylabel('Overall Wall Clock Time (s)', color=color1, fontweight='bold')
    ax1.plot(workers, times, marker='o', markersize=8, color=color1, linewidth=2.5, label='Measured Time')
    ax1.tick_params(axis='y', labelcolor=color1)
    ax1.set_xticks(workers)
    
    # Speedup Axis (Right)
    ax2 = ax1.twinx()  
    color2 = '#8B0000' # DarkRed
    color_ideal = '#87CEFA' # LightSkyBlue (for the ideal line)
    
    ax2.set_ylabel('Speedup Factor', color=color2, fontweight='bold')  
    ax2.plot(workers, speedups, marker='s', markersize=8, linestyle='-', color=color2, linewidth=2.5, label='Measured Speedup')
    
    # Start the ideal curve exactly at speedups[0] to root it elegantly if we aren't exactly 1.0
    ideal_curve = [speedups[0] * w for w in workers]
    ax2.plot(workers, ideal_curve, marker='^', linestyle='--', color=color_ideal, linewidth=2, label='Ideal Linear Speedup')
    ax2.tick_params(axis='y', labelcolor=color2)

    # Plot Polish
    plt.title('MPI Ensemble Averaging Speedup (10 Million MC Steps)', fontsize=14, fontweight='bold')
    fig.tight_layout()
    ax1.grid(True, linestyle=':', alpha=0.6)
    
    # Combined Legend
    lines_1, labels_1 = ax1.get_legend_handles_labels()
    lines_2, labels_2 = ax2.get_legend_handles_labels()
    ax1.legend(lines_1 + lines_2, labels_1 + labels_2, loc='center left')
    
    # === Save Outputs ===
    output_png = os.path.join(OUTPUT_DIR, 'parallel_star_speedup_plot.png')
    output_pdf = os.path.join(OUTPUT_DIR, 'parallel_star_speedup_plot.pdf')
    
    plt.savefig(output_png, dpi=300)
    plt.savefig(output_pdf)
    print(f"\nGenerated {os.path.basename(output_png)} and {os.path.basename(output_pdf)}")

if __name__ == '__main__':
    main()
