import matplotlib.pyplot as plt
import numpy as np
import os

def main():
    # Attempt to load the science plotting style if available
    style_path = os.path.join(os.path.dirname(__file__), 'science.mplstyle')
    if os.path.exists(style_path):
        plt.style.use(style_path)
    else:
        # Fallback to a clean style
        plt.style.use('seaborn-v0_8-whitegrid')

    # Data Configuration
    sec_to_hr = 3600.0

    # Serial runs breakdown (seconds)
    serial_data = {
        'Conf 5 (Prod)': 45260,
        'Conf 5 (Equil)': 14474,
        'Conf 4 (Prod)': 45411,
        'Conf 4 (Equil)': 24991,
        'Conf 1 (Prod)': 45902,
        'Conf 1 (Equil)': 24075,
    }

    # Parallel tracking
    parallel_wall = 52004 / sec_to_hr
    parallel_cpu = 320599 / sec_to_hr

    fig, ax = plt.subplots(figsize=(8, 6))

    # --- Plot 1: Serial Execution Stack ---
    bottom = 0
    # Custom color palette pairing equilibrations with their productions
    colors = ['#2ca02c', '#98df8a', '#ff7f0e', '#ffbb78', '#1f77b4', '#aec7e8']
    labels = list(serial_data.keys())
    values_hr = [v / sec_to_hr for v in serial_data.values()]

    for i in range(len(values_hr)):
        ax.bar('Serial Execution\n(Single Core)', values_hr[i], bottom=bottom, color=colors[i], 
               edgecolor='black', zorder=3, alpha=0.9, width=0.6, label=labels[i])
        
        # Add labels directly into the larger blocks
        if values_hr[i] > 3:
            ax.text(0, bottom + values_hr[i]/2, f"{values_hr[i]:.1f}h", 
                    ha='center', va='center', color='black', fontsize=10, fontweight='bold')
        bottom += values_hr[i]

    # Total floating label for Serial
    ax.text(0, bottom + 1, f"Total: {bottom:.1f}h", ha='center', va='bottom', fontsize=12, fontweight='bold')

    # --- Plot 2: Parallel Master-Worker Execution ---
    ax.bar('Parallel Execution\n(Master + 6 Workers)', parallel_wall, 
           color='#9467bd', edgecolor='black', zorder=3, alpha=0.9, width=0.6, 
           label='All Confs - Equil \& Prod \n(89.05h CPU time)')
    
    ax.text(1, parallel_wall/2, f"{parallel_wall:.1f}h", 
            ha='center', va='center', color='white', fontsize=11, fontweight='bold')
    ax.text(1, parallel_wall + 1, f"Total: {parallel_wall:.1f}h", 
            ha='center', va='bottom', fontsize=12, fontweight='bold')

    # Plot invisible proxy artist for CPU time context in legend
    #ax.bar('Parallel Execution\n(Master + 6 Workers)', 0, label=f'Total CPU Work: {parallel_cpu:.1f}h', color='none', edgecolor='none')

    # Formatting
    ax.set_ylabel('Wall-Clock Time (Hours)', fontsize=14, fontweight='bold')
    ax.set_title('Real-World Speedup: Serial vs Combined Parallelization', fontsize=16, fontweight='bold', pad=20, loc='center')
    ax.grid(axis='y', linestyle='--', alpha=0.7, zorder=0)
    
    # Customize the Y-axis constraints slightly higher to accommodate floating totals
    ax.set_ylim(0, bottom + 6)

    # Reorder legend to match visual stacking (top-to-bottom physically)
    handles, labels = ax.get_legend_handles_labels()
    # Pull out the CPU stat to bottom
    cpu_handle, cpu_label = handles.pop(-1), labels.pop(-1)
    
    # Reverse loop items
    handles.reverse()
    labels.reverse()
    
    # Add CPU stat to bottom of legend
    handles.append(cpu_handle)
    labels.append(cpu_label)

    lgd = ax.legend(handles, labels, title="Component Breakdown", 
                    loc='upper right', bbox_to_anchor=(0.96, 0.96), fontsize=10, 
                    frameon=True)
    lgd.get_title().set_fontweight('bold')

    plt.tight_layout()

    # Dynamic dynamic filepath save
    base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    target_dir = os.path.join(base_dir, '../docs/img')
    os.makedirs(target_dir, exist_ok=True)
    
    out_png = os.path.join(target_dir, 'parallel_combined_v_serial_plot.png')
    out_pdf = os.path.join(target_dir, 'parallel_combined_v_serial_plot.pdf')
    
    plt.savefig(out_png, dpi=300, bbox_extra_artists=(lgd,), bbox_inches='tight')
    plt.savefig(out_pdf, bbox_extra_artists=(lgd,), bbox_inches='tight')
    print(f"Generated Plot -> {out_png}")


if __name__ == '__main__':
    main()
