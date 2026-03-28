import numpy as np
import matplotlib.pyplot as plt
import os
import glob
import re

OUTPUT_DIR = '../results/'

def get_explicit_h_setting():
    input_path = os.path.join(os.path.dirname(__file__), '../confs/input.dat')
    try:
        with open(input_path, 'r') as f:
            for line in f:
                if line.strip().startswith('!'): continue
                if 'explicit_h' in line and '=' in line:
                    val_str = line.split('=')[1].strip().lower()
                    if val_str == '.true.': return True
                    elif val_str == '.false.': return False
    except FileNotFoundError:
        pass
    return True

# Base colors for the configurations (dark, mid, light)
BASE_COLORS = {
    0: ('#000080', '#1E90FF', '#87CEFA'), # Conf 1: Navy, DodgerBlue, LightSkyBlue
    1: ('#8B0000', '#FF0000', '#FA8072'), # Conf 4: DarkRed, Red, Salmon
    2: ('#006400', '#32CD32', '#98FB98'), # Conf 5: DarkGreen, LimeGreen, PaleGreen
    3: ('#4B0082', '#9932CC', '#E6E6FA'), # Indigo
}

def main():
    style_path = os.path.join(os.path.dirname(__file__), 'science.mplstyle')
    if os.path.exists(style_path):
        plt.style.use(style_path)

    # Detect all output files dynamically based on config IDs
    prod_energy_files = glob.glob(os.path.join(OUTPUT_DIR, 'energy_prod_c*_sd*.dat'))
    configs = set()
    for f in prod_energy_files:
        m = re.search(r'prod_c(\d+)_', os.path.basename(f))
        if m: configs.add(int(m.group(1)))
    configs = sorted(list(configs))

    if not configs:
        print("No production files found in results/ directory.")
        return

    explicit_h = get_explicit_h_setting()

    # Pre-setup the 3 desired figures
    fig_ener, ax_ener = plt.subplots(figsize=(10, 6))
    fig_obs, (ax_rg, ax_ree) = plt.subplots(2, 1, figsize=(10, 8), sharex=True)
    fig_tors, ax_tors = plt.subplots(figsize=(10, 6))

    for c_idx, c_val in enumerate(configs):
        c_str = f"c{c_val}"
        color_dark, color_mid, color_light = BASE_COLORS[c_idx % len(BASE_COLORS)]

        # Sorted files to logically 'collapse' them sequentially by seed string order
        e_files = sorted(glob.glob(os.path.join(OUTPUT_DIR, f'energy_prod_{c_str}_sd*.dat')))
        o_files = sorted(glob.glob(os.path.join(OUTPUT_DIR, f'observables_prod_{c_str}_sd*.dat')))
        t_files = sorted(glob.glob(os.path.join(OUTPUT_DIR, f'torsions_prod_{c_str}_sd*.dat')))
        tr_files = sorted(glob.glob(os.path.join(OUTPUT_DIR, f'trajectory_prod_{c_str}_sd*.xyz')))

        e_tot_list, e_lj_list, e_tors_list = [], [], []
        rg_list, ree_list = [], []
        tors_list = []

        # Read and parse all data sequentially
        for ef in e_files:
            try:
                data = np.loadtxt(ef)
                if data.ndim == 2:
                    e_tot_list.append(data[:, 1])
                    e_lj_list.append(data[:, 2])
                    e_tors_list.append(data[:, 3])
            except Exception: pass

        for of in o_files:
            try:
                data = np.loadtxt(of)
                if data.ndim == 2:
                    rg_list.append(data[:, 1])
                    ree_list.append(data[:, 2])
            except Exception: pass

        for tf in t_files:
            try:
                with open(tf, 'r') as f:
                    for line in f.readlines():
                        if line.startswith('#'): continue
                        tors_list.extend([float(x) for x in line.split()[1:]])
            except Exception: pass

        # 1. Plotting Energy (Intertwined collapse)
        if e_tot_list:
            min_len = min(len(arr) for arr in e_tot_list)
            e_tot_cat = np.column_stack([arr[:min_len] for arr in e_tot_list]).flatten()
            e_lj_cat  = np.column_stack([arr[:min_len] for arr in e_lj_list]).flatten()
            e_tors_cat= np.column_stack([arr[:min_len] for arr in e_tors_list]).flatten()
            
            # Synthesize an X-axis up to 10M Steps
            steps_cat = np.linspace(10000, len(e_tot_cat)*10000, len(e_tot_cat))

            ax_ener.plot(steps_cat, e_tot_cat, label=f'Conf {c_val} (Total)', color=color_dark, alpha=0.9, linewidth=1)
            ax_ener.plot(steps_cat, e_lj_cat, label=f'Conf {c_val} (LJ)', color=color_mid, alpha=0.9, linewidth=1)
            ax_ener.plot(steps_cat, e_tors_cat, label=f'Conf {c_val} (Torsional)', color=color_light, alpha=0.9, linewidth=1)

            print(f"Conf {c_val} Scalar Averages over 10M MCS:")
            print(f"  E_Total: {np.mean(e_tot_cat):.4f} | E_LJ: {np.mean(e_lj_cat):.4f} | E_Tors: {np.mean(e_tors_cat):.4f}")

        # 2. Plotting Observables (Intertwined collapse)
        if rg_list and ree_list:
            min_len = min(len(arr) for arr in rg_list)
            rg_cat = np.column_stack([arr[:min_len] for arr in rg_list]).flatten()
            ree_cat = np.column_stack([arr[:min_len] for arr in ree_list]).flatten()
            steps_cat_obs = np.linspace(10000, len(rg_cat)*10000, len(rg_cat))

            ax_rg.plot(steps_cat_obs, rg_cat, label=f'Conf {c_val} (Rg)', color=color_dark, alpha=0.9, linewidth=1)
            ax_ree.plot(steps_cat_obs, ree_cat, label=f'Conf {c_val} (End-to-End)', color=color_mid, alpha=0.9, linewidth=1)
            print(f"  Rg_avg:  {np.mean(rg_cat):.4f} | Ree_avg: {np.mean(ree_cat):.4f}\n")

        # 3. Plotting Torsion Distributions
        if tors_list:
            ax_tors.hist(tors_list, bins=60, density=True, alpha=0.4, color=color_dark, 
                         label=f'Conf {c_val} Dist')

        # 4. XYZ Trajectory specific processing
        # "New .xyz file with 1st frame as starting config, and 10 next frames as the final chains"
        if tr_files:
            xyz_out = os.path.join(OUTPUT_DIR, f'final_conformations_c{c_val}.xyz')
            with open(xyz_out, 'w') as out_f:
                
                # Retrieve the very first frame of the very first production file
                first_tr = tr_files[0]
                first_frame_lines = []
                try:
                    with open(first_tr, 'r') as f:
                        for line in f:
                            if not first_frame_lines and line.strip().isdigit():
                                num_atoms = int(line.strip())
                                first_frame_lines.append(line)
                            elif first_frame_lines:
                                first_frame_lines.append(line)
                                if len(first_frame_lines) == num_atoms + 2:
                                    first_frame_lines[1] = f"Starting Conformation (Frame 1)\n"
                                    break
                    out_f.writelines(first_frame_lines)
                except Exception as e:
                    print(f"XYZ Warning: {e}")

                # Retrieve the LAST frame of each of the 10 production files
                for idx, tr in enumerate(tr_files):
                    last_frame_lines = []
                    current_frame = []
                    try:
                        with open(tr, 'r') as f:
                            lines = f.readlines()
                            i = 0
                            while i < len(lines):
                                if lines[i].strip().isdigit():
                                    num_atoms = int(lines[i].strip())
                                    if i + num_atoms + 2 <= len(lines):
                                        current_frame = lines[i:i+num_atoms+2]
                                        i += num_atoms + 2
                                    else:
                                        break
                                else:
                                    i += 1
                            last_frame_lines = current_frame
                        if last_frame_lines:
                            last_frame_lines[1] = f"Final Conformation Chain {idx+1} (Simulation Frame {idx+2})\n"
                            out_f.writelines(last_frame_lines)
                    except Exception: pass
            print(f"Generated {os.path.basename(xyz_out)} (1 starting frame + 10 final frames)")

    # ── Finalize Energy Plot ── 
    ax_ener.set_xlabel('Cumulative MC Steps (10 Runs Collapsed)')
    ax_ener.set_ylabel('Energy (kcal/mol)')
    ax_ener.set_title('Energy Evolution')
    ax_ener.legend(bbox_to_anchor=(1.01, 1), loc='upper left', prop={'size': 9})
    fig_ener.tight_layout()
    fig_ener.savefig(os.path.join(OUTPUT_DIR, 'parallel_energy_evolution.png'), dpi=300)
    fig_ener.savefig(os.path.join(OUTPUT_DIR, 'parallel_energy_evolution.pdf'))
    print("Generated parallel_energy_evolution.pdf")

    # ── Finalize Observables Plot ── 
    ax_rg.set_ylabel('Radius of Gyration (Å)')
    ax_rg.set_title('Structural Observables Evolution')
    ax_rg.legend(bbox_to_anchor=(1.01, 1), loc='upper left', prop={'size': 9})
    
    ax_ree.set_xlabel('Cumulative MC Steps (10 Runs Collapsed)')
    ax_ree.set_ylabel('End-to-End Distance (Å)')
    ax_ree.legend(bbox_to_anchor=(1.01, 1), loc='upper left', prop={'size': 9})
    fig_obs.tight_layout()
    fig_obs.savefig(os.path.join(OUTPUT_DIR, 'parallel_observables_evolution.png'), dpi=300)
    fig_obs.savefig(os.path.join(OUTPUT_DIR, 'parallel_observables_evolution.pdf'))
    print("Generated parallel_observables_evolution.pdf")

    # ── Finalize Torsions Plot & Add TraPPE Potential ── 
    if not explicit_h:
        c1, c2, c3 = 0.705, -0.135, 1.572
    else:
        c1, c2, c3 = 0.8700, -0.0785, 1.5075
    
    phi_grid = np.linspace(0.0, np.pi, 500)
    Uphi = (c1 * (1.0 + np.cos(phi_grid))
          + c2 * (1.0 - np.cos(2.0 * phi_grid))
          + c3 * (1.0 + np.cos(3.0 * phi_grid)))

    ax_tors2 = ax_tors.twinx()
    ax_tors2.plot(phi_grid, Uphi, color='black', linestyle='--', linewidth=2, label='TraPPE Potential')
    ax_tors2.set_ylabel('Torsion Potential (kcal/mol)')

    ax_tors.set_xlabel('Torsion Angle (rad)')
    ax_tors.set_ylabel('Probability Density')
    ax_tors.set_xlim(0, np.pi)
    ax_tors.set_xticks([0, np.pi/4, np.pi/2, 3*np.pi/4, np.pi])
    ax_tors.set_xticklabels([r'$0$', r'$\pi/4$', r'$\pi/2$', r'$3\pi/4$', r'$\pi$'])
    ax_tors.set_title('Torsion Distribution (Averaged across 10-seed ensembles)')
    
    lines1, labels1 = ax_tors.get_legend_handles_labels()
    lines2, labels2 = ax_tors2.get_legend_handles_labels()
    ax_tors.legend(lines1 + lines2, labels1 + labels2, bbox_to_anchor=(1.01, 1), loc='upper left', prop={'size': 9})
    
    fig_tors.tight_layout()
    fig_tors.savefig(os.path.join(OUTPUT_DIR, 'parallel_torsion_distribution.png'), dpi=300)
    fig_tors.savefig(os.path.join(OUTPUT_DIR, 'parallel_torsion_distribution.pdf'))
    print("Generated parallel_torsion_distribution.pdf")

if __name__ == '__main__':
    main()
