# authors: ai-murphy, itxasoma
import numpy as np
import matplotlib.pyplot as plt
import os
import glob

# Import scientific style science.mplstyle from the same directory as this script
plt.style.use(os.path.join(os.path.dirname(__file__), 'science.mplstyle'))

# Create plots in the results directory
OUTPUT_DIR = '../results/'
if not os.path.exists(OUTPUT_DIR):
    print("Results directory not found. Please run the simulation first.")
    exit(1)

ENERGY_FILES = sorted(glob.glob(os.path.join(OUTPUT_DIR, 'energy_*.dat')))


def plot_energies(energy_file):
    try:
        data = np.loadtxt(energy_file)
        run_tag = os.path.splitext(os.path.basename(energy_file))[0].replace('energy_', '')
        steps = data[:, 0]
        e_tot = data[:, 1]
        e_lj = data[:, 2]
        e_tors = data[:, 3]

        plt.figure()
        plt.plot(steps, e_tot, label='Total Energy', alpha=0.8)
        plt.plot(steps, e_lj, label='LJ Energy', alpha=0.8)
        plt.plot(steps, e_tors, label='Torsion Energy', alpha=0.8)
        plt.xlabel('MC Steps')
        plt.ylabel('Energy (kcal/mol)')
        plt.title('Energy Evolution')
        plt.xlim(left=0)
        plt.legend()
        plt.grid(True, alpha=0.3)
        out_file = os.path.join(OUTPUT_DIR, f'energy_evolution_{run_tag}.pdf')
        plt.savefig(out_file)
        plt.close()
        print(f"Generated {os.path.basename(out_file)}.pdf")
    except Exception as e:
        print(f"Error plotting energies: {e}")

def plot_observables(obs_file):
    try:
        data = np.loadtxt(obs_file)
        run_tag = os.path.splitext(os.path.basename(obs_file))[0].replace('observables_', '')
        steps = data[:, 0]
        rg = data[:, 1]
        ree = data[:, 2]

        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(5.25, 3.9372), sharex=True)
        
        ax1.plot(steps, rg, color='blue')
        ax1.set_ylabel('Radius of Gyration (Å)')
        ax1.set_title('Structural Observables Evolution')
        ax1.set_xlim(left=0)
        ax1.grid(True, alpha=0.3)

        ax2.plot(steps, ree, color='red')
        ax2.set_xlabel('MC Steps')
        ax2.set_ylabel('End-to-End Distance (Å)')
        ax2.set_xlim(left=0)
        ax2.grid(True, alpha=0.3)

        plt.tight_layout()
        out_file = os.path.join(OUTPUT_DIR, f'observables_evolution_{run_tag}.pdf')
        plt.savefig(out_file)
        plt.close()
        print(f"Generated {os.path.basename(out_file)}")
    except Exception as e:
        print(f"Error plotting observables: {e}")

def plot_torsions(tors_file):
    try:
        # TraPPE-UA torsional coefficients (same as in energy.f90)
        c1 = 0.705
        c2 = -0.135
        c3 = 1.572

        def torsion_potential(phi):
            return(
                c1 * (1.0 + np.cos(phi))
                + c2 * (1.0 - np.cos(2.0 * phi))
                + c3 * (1.0 + np.cos(3.0 * phi))
            )

        # Read the last few frames to get a distribution of equilibrated torsion angles
        with open(tors_file, 'r') as f:
            lines = f.readlines()

        run_tag = os.path.splitext(os.path.basename(tors_file))[0].replace('torsions_', '')

        # Take the last 20% of the frames for the histogram
        n_frames = len(lines) - 1  # excluding header
        start_idx = max(1, int(n_frames * 0.8))

        all_angles = []
        for line in lines[start_idx:]:
            if line.startswith('#'):
                continue
            parts = line.split()
            angles = [float(x) for x in parts[1:]]
            all_angles.extend(angles)

        all_angles = np.array(all_angles)

        # Analytic torsion potential on the same angular domain as the stored data
        phi_grid = np.linspace(0.0, np.pi, 500)
        Uphi = torsion_potential(phi_grid)

        fig, ax1 = plt.subplots()

        ax1.hist(
            all_angles,
            bins=60,
            density=True,
            alpha=0.7,
            color='purple',
            edgecolor='black',
            label='Histogram'
        )
        ax1.set_xlabel('Torsion Angle (rad)')
        ax1.set_ylabel('Probability Density')
        ax1.set_xlim(0, np.pi)

        ticks = [0, np.pi/4, np.pi/2, 3*np.pi/4, np.pi]
        tick_labels = [r'$0$', r'$\pi/4$', r'$\pi/2$', r'$3\pi/4$', r'$\pi$']
        ax1.set_xticks(ticks)
        ax1.set_xticklabels(tick_labels)
        ax1.grid(True, alpha=0.3)

        ax2 = ax1.twinx()
        ax2.plot(phi_grid, Uphi, label='TraPPE-UA potential')
        ax2.set_ylabel('Torsion Potential (kcal/mol)')

        lines1, labels1 = ax1.get_legend_handles_labels()
        lines2, labels2 = ax2.get_legend_handles_labels()
        ax1.legend(lines1 + lines2, labels1 + labels2, loc='upper right')

        plt.title('Equilibrium Torsion Distribution and Potential')
        plt.tight_layout()
        out_file = os.path.join(OUTPUT_DIR, f'torsion_distribution_{run_tag}.pdf')
        plt.savefig(out_file)
        plt.close()
        print(f"Generated {os.path.basename(out_file)}")

    except Exception as e:
        print(f"Error plotting torsions: {e}")


if __name__ == '__main__':
    print("Generating plots from simulation results...")

    for energy_file in ENERGY_FILES:
        run_tag = os.path.splitext(os.path.basename(energy_file))[0].replace('energy_', '')

        obs_file = os.path.join(OUTPUT_DIR, f'observables_{run_tag}.dat')
        tors_file = os.path.join(OUTPUT_DIR, f'torsions_{run_tag}.dat')

        print(f'Processing run: {run_tag}')

        plot_energies(energy_file)

        if os.path.exists(obs_file):
            plot_observables(obs_file)
        else:
            print(f'Missing {os.path.basename(obs_file)}')

        if os.path.exists(tors_file):
            plot_torsions(tors_file)
        else:
            print(f'Missing {os.path.basename(tors_file)}')

    print("Done!")
