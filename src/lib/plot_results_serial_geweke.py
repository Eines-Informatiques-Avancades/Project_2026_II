import numpy as np
import matplotlib.pyplot as plt
import os
import glob

# Style handling
style_path = os.path.join(os.path.dirname(__file__), 'science.mplstyle')
if os.path.exists(style_path):
    plt.style.use(style_path)

# Point to the specific isolated production directory
OUTPUT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '../../results'))
if not os.path.exists(OUTPUT_DIR):
    print(f"Results directory not found at {OUTPUT_DIR}")
    exit(1)

# Base our unique runs entirely off the discovered production files
PROD_ENERGY_FILES = sorted(glob.glob(os.path.join(OUTPUT_DIR, 'prod_energy_*.dat')))

# ── Data Parsing Functions ────────────────────────────────────────────────────
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
        print("Could not find input.dat, defaulting explicit_h=True")
        pass
    return True

# ── Plotting functions ─────────────────────────────────────────────────────────

def plot_energies(equil_file, prod_file, run_tag):
    try:
        e_data_prod = np.loadtxt(prod_file)
        
        plt.figure()
        
        if equil_file and os.path.exists(equil_file):
            e_data_equil = np.loadtxt(equil_file)
            
            # Offset the production steps to continue cleanly from the final equilibration step
            steps_e = e_data_equil[:, 0]
            max_equil_step = steps_e[-1]
            steps_p = e_data_prod[:, 0] + max_equil_step
            
            # Stitch arrays
            steps = np.concatenate((steps_e, steps_p))
            e_tot = np.concatenate((e_data_equil[:, 1], e_data_prod[:, 1]))
            e_lj = np.concatenate((e_data_equil[:, 2], e_data_prod[:, 2]))
            e_tors = np.concatenate((e_data_equil[:, 3], e_data_prod[:, 3]))
            
            plt.plot(steps, e_tot, label='Total Energy', alpha=0.8)
            plt.plot(steps, e_lj,  label='LJ Energy',    alpha=0.8)
            plt.plot(steps, e_tors,label='Torsion Energy',alpha=0.8)
            
            # Line of demarcation representing the transition from Equil -> Prod
            # (Vertical line across the time X-axis)
            plt.axvline(x=max_equil_step, color='k', linestyle='--', linewidth=1.5, label='Equil/Prod Demarcation')
            plt.title('Energy Evolution (Equilibration + Production)')
            plot_type = "stitched"
        else:
            steps = e_data_prod[:, 0]
            e_tot = e_data_prod[:, 1]
            e_lj  = e_data_prod[:, 2]
            e_tors = e_data_prod[:, 3]

            plt.plot(steps, e_tot, label='Total Energy', alpha=0.8)
            plt.plot(steps, e_lj,  label='LJ Energy',    alpha=0.8)
            plt.plot(steps, e_tors,label='Torsion Energy',alpha=0.8)
            plt.title('Energy Evolution (Production Only)')
            plot_type = "production-only"

        plt.xlabel('MC Steps')
        plt.ylabel('Energy (kcal/mol)')
        plt.xlim(left=-steps.max() * 0.01)
        plt.legend(loc='center', bbox_to_anchor=(0.6, 0.25))
        plt.grid(True, alpha=0.3)
        
        pdf_file = os.path.join(OUTPUT_DIR, f'energy_evolution_{run_tag}.pdf')
        png_file = os.path.join(OUTPUT_DIR, f'energy_evolution_{run_tag}.png')
        plt.savefig(pdf_file)
        plt.savefig(png_file, dpi=300)
        plt.close()
        print(f"Generated {plot_type} energy plots for {run_tag}")
    except Exception as e:
        print(f"Error plotting energies: {e}")


def plot_observables(equil_file, prod_file, run_tag):
    try:
        o_data_prod = np.loadtxt(prod_file)

        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(5.25, 3.9372), sharex=True)
        
        if equil_file and os.path.exists(equil_file):
            o_data_equil = np.loadtxt(equil_file)

            # Offset production steps
            steps_e = o_data_equil[:, 0]
            max_equil_step = steps_e[-1]
            steps_p = o_data_prod[:, 0] + max_equil_step
            
            # Stitch arrays
            steps = np.concatenate((steps_e, steps_p))
            rg = np.concatenate((o_data_equil[:, 1], o_data_prod[:, 1]))
            ree = np.concatenate((o_data_equil[:, 2], o_data_prod[:, 2]))

            ax1.axvline(x=max_equil_step, color='k', linestyle='--', linewidth=1)
            ax2.axvline(x=max_equil_step, color='k', linestyle='--', linewidth=1)
            ax1.set_title('Structural Observables Evolution (Equil + Prod)')
            plot_type = "stitched"
        else:
            steps = o_data_prod[:, 0]
            rg = o_data_prod[:, 1]
            ree = o_data_prod[:, 2]
            
            ax1.set_title('Structural Observables Evolution (Production Only)')
            plot_type = "production-only"

        ax1.plot(steps, rg, color='blue')
        ax1.set_ylabel('Radius of Gyration (Å)')
        ax1.set_xlim(left=-steps.max() * 0.01)
        ax1.grid(True, alpha=0.3)
        
        ax2.plot(steps, ree, color='red')
        ax2.set_xlabel('MC Steps')
        ax2.set_ylabel('End-to-End Distance (Å)')
        ax2.set_xlim(left=-steps.max() * 0.01)
        ax2.grid(True, alpha=0.3)
        
        plt.tight_layout()
        pdf_file = os.path.join(OUTPUT_DIR, f'observables_evolution_{run_tag}.pdf')
        png_file = os.path.join(OUTPUT_DIR, f'observables_evolution_{run_tag}.png')
        plt.savefig(pdf_file)
        plt.savefig(png_file, dpi=300)
        plt.close()
        print(f"Generated {plot_type} observables plots for {run_tag}")
    except Exception as e:
        print(f"Error plotting observables: {e}")


def plot_torsions(tors_file, explicit_h=True):
    try:
        if not explicit_h:
            c1, c2, c3 = 0.705, -0.135, 1.572
        else:
            c1, c2, c3 = 0.8700, -0.0785, 1.5075

        def torsion_potential(phi):
            return (c1 * (1.0 + np.cos(phi))
                  + c2 * (1.0 - np.cos(2.0 * phi))
                  + c3 * (1.0 + np.cos(3.0 * phi)))

        with open(tors_file, 'r') as f:
            lines = f.readlines()
            
        run_tag = os.path.splitext(os.path.basename(tors_file))[0].replace('prod_torsions_', '')
        all_angles = []
        for line in lines:
            if line.startswith('#'):
                continue
            parts = line.split()
            # Read all torsions natively since 100% of the dataset is purely Phase 2 production
            all_angles.extend(float(x) for x in parts[1:])

        all_angles = np.array(all_angles)
        phi_grid = np.linspace(0.0, np.pi, 500)
        Uphi = torsion_potential(phi_grid)

        fig, ax1 = plt.subplots()
        ax1.hist(all_angles, bins=60, density=True, alpha=0.7,
                 color='purple', edgecolor='black',
                 label='Production distribution')
        ax1.set_xlabel('Torsion Angle (rad)')
        ax1.set_ylabel('Probability Density')
        ax1.set_xlim(0, np.pi)
        ticks = [0, np.pi/4, np.pi/2, 3*np.pi/4, np.pi]
        ax1.set_xticks(ticks)
        ax1.set_xticklabels([r'$0$', r'$\pi/4$', r'$\pi/2$', r'$3\pi/4$', r'$\pi$'])
        ax1.grid(True, alpha=0.3)

        ax2 = ax1.twinx()
        if not explicit_h:
            ax2.plot(phi_grid, Uphi, label='TraPPE-UA potential')
        else:
            ax2.plot(phi_grid, Uphi, label='OPLS-AA potential')
        ax2.set_ylabel('Torsion Potential (kcal/mol)')

        lines1, labels1 = ax1.get_legend_handles_labels()
        lines2, labels2 = ax2.get_legend_handles_labels()
        ax1.legend(lines1 + lines2, labels1 + labels2, loc='upper right')

        plt.title('Production Torsion Distribution and Potential')
        plt.tight_layout()
        pdf_file = os.path.join(OUTPUT_DIR, f'torsion_distribution_{run_tag}.pdf')
        png_file = os.path.join(OUTPUT_DIR, f'torsion_distribution_{run_tag}.png')
        plt.savefig(pdf_file)
        plt.savefig(png_file, dpi=300)
        plt.close()
        print(f"Generated pure-production torsion plots for {run_tag}")

    except Exception as e:
        print(f"Error plotting torsions: {e}")


# ── Main ───────────────────────────────────────────────────────────────────────
if __name__ == '__main__':
    print(f"Generating plots from simulation results in {OUTPUT_DIR}...")
    if not PROD_ENERGY_FILES:
        print("No production 'prod_' energy files found! Verify directory path.")
        
    for prod_energy_file in PROD_ENERGY_FILES:
        run_tag = os.path.splitext(os.path.basename(prod_energy_file))[0].replace('prod_energy_', '')
        
        # Locate corresponding Phase 1 and Phase 2 files 
        equil_energy_file = os.path.join(OUTPUT_DIR, f'equil_energy_{run_tag}.dat')
        equil_obs_file    = os.path.join(OUTPUT_DIR, f'equil_observables_{run_tag}.dat')
        prod_obs_file     = os.path.join(OUTPUT_DIR, f'prod_observables_{run_tag}.dat')
        prod_tors_file    = os.path.join(OUTPUT_DIR, f'prod_torsions_{run_tag}.dat')
        
        print(f'\nProcessing run: {run_tag}')
        
        if os.path.exists(prod_energy_file):
            plot_energies(equil_energy_file if os.path.exists(equil_energy_file) else None, prod_energy_file, run_tag)
        else:
            print(f'Missing production energy file for {run_tag}')
            
        if os.path.exists(prod_obs_file):
            plot_observables(equil_obs_file if os.path.exists(equil_obs_file) else None, prod_obs_file, run_tag)
        else:
            print(f'Missing production observables file for {run_tag}')
            
        if os.path.exists(prod_tors_file):
            plot_torsions(prod_tors_file, explicit_h=get_explicit_h_setting())
        else:
            print(f'Missing {os.path.basename(prod_tors_file)}')
            
    print("\nDone!")
