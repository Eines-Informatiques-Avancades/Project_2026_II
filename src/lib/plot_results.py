import numpy as np
import matplotlib.pyplot as plt
import os

# Create plots in the results directory
OUTPUT_DIR = '../results/'
if not os.path.exists(OUTPUT_DIR):
    print("Results directory not found. Please run the simulation first.")
    exit(1)

def plot_energies():
    try:
        data = np.loadtxt(os.path.join(OUTPUT_DIR, 'energy.dat'))
        steps = data[:, 0]
        e_tot = data[:, 1]
        e_lj = data[:, 2]
        e_tors = data[:, 3]

        plt.figure(figsize=(10, 6))
        plt.plot(steps, e_tot, label='Total Energy', alpha=0.8)
        plt.plot(steps, e_lj, label='LJ Energy', alpha=0.8)
        plt.plot(steps, e_tors, label='Torsion Energy', alpha=0.8)
        plt.xlabel('MC Steps')
        plt.ylabel('Energy (kcal/mol)')
        plt.title('Energy Evolution')
        plt.legend()
        plt.grid(True)
        plt.savefig(os.path.join(OUTPUT_DIR, 'energy_evolution.png'), dpi=300)
        plt.close()
        print("Generated energy_evolution.png")
    except Exception as e:
        print(f"Error plotting energies: {e}")

def plot_observables():
    try:
        data = np.loadtxt(os.path.join(OUTPUT_DIR, 'observables.dat'))
        steps = data[:, 0]
        rg = data[:, 1]
        ree = data[:, 2]

        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8), sharex=True)
        
        ax1.plot(steps, rg, color='blue')
        ax1.set_ylabel('Radius of Gyration (Å)')
        ax1.set_title('Structural Observables Evolution')
        ax1.grid(True)

        ax2.plot(steps, ree, color='red')
        ax2.set_xlabel('MC Steps')
        ax2.set_ylabel('End-to-End Distance (Å)')
        ax2.grid(True)

        plt.tight_layout()
        plt.savefig(os.path.join(OUTPUT_DIR, 'observables_evolution.png'), dpi=300)
        plt.close()
        print("Generated observables_evolution.png")
    except Exception as e:
        print(f"Error plotting observables: {e}")

def plot_torsions():
    try:
        # Read the last few frames to get a distribution of equilibrated torsion angles
        with open(os.path.join(OUTPUT_DIR, 'torsions.dat'), 'r') as f:
            lines = f.readlines()
        
        # Take the last 20% of the frames for the histogram
        n_frames = len(lines) - 1 # excluding header
        start_idx = max(1, int(n_frames * 0.8))
        
        all_angles = []
        for line in lines[start_idx:]:
            if line.startswith('#'): continue
            parts = line.split()
            angles = [float(x) for x in parts[1:]]
            all_angles.extend(angles)
            
        all_angles = np.array(all_angles)
        # Convert 0..pi or -pi..pi to degrees for easier interpretation if needed
        # Or just plot in radians
        
        plt.figure(figsize=(10, 6))
        plt.hist(all_angles, bins=60, density=True, alpha=0.7, color='purple', edgecolor='black')
        plt.xlabel('Torsion Angle (rad)')
        plt.ylabel('Probability Density')
        plt.title('Equilibrium Torsion Angle Distribution')
        plt.grid(True)
        plt.savefig(os.path.join(OUTPUT_DIR, 'torsion_distribution.png'), dpi=300)
        plt.close()
        print("Generated torsion_distribution.png")
    except Exception as e:
        print(f"Error plotting torsions: {e}")

if __name__ == '__main__':
    print("Generating plots from simulation results...")
    plot_energies()
    plot_observables()
    plot_torsions()
    print("Done!")
