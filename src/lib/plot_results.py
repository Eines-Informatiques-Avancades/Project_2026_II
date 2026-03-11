# authors: ai-murphy, itxasoma
import numpy as np
import matplotlib.pyplot as plt
import os
import glob

plt.style.use(os.path.join(os.path.dirname(__file__), 'science.mplstyle'))

OUTPUT_DIR = '../results/'
if not os.path.exists(OUTPUT_DIR):
    print("Results directory not found.")
    exit(1)

ENERGY_FILES = sorted(glob.glob(os.path.join(OUTPUT_DIR, 'energy_*.dat')))


# ── Data Parsing Functions ────────────────────────────────────────────────────
def get_explicit_h_setting():
    # The plot_results.py sits in src/lib, so input.dat is in src/confs/
    input_path = os.path.join(os.path.dirname(__file__), '../confs/input.dat')
    try:
        with open(input_path, 'r') as f:
            for line in f:
                # ignore comments
                if line.strip().startswith('!'):
                    continue
                
                # Look for explicit_h assignment
                if 'explicit_h' in line and '=' in line:
                    val_str = line.split('=')[1].strip().lower()
                    if val_str == '.true.':
                        return True
                    elif val_str == '.false.':
                        return False
    except FileNotFoundError:
        print("Could not find input.dat, defaulting explicit_h=True")
        pass
        
    return True  # Default fallback if not found


# ── Autocorrelation functions ──────────────────────────────────────────────────

def integrated_autocorr_time(x):
    """
    Estimate integrated autocorrelation time tau_int of a 1D timeseries x
    using the automated windowing procedure (Madras & Sokal 1988).
    Returns tau_int and statistical inefficiency g = 1 + 2*tau_int.
    """
    n = len(x)
    mu = np.mean(x)
    dx = x - mu
    # Full ACF via FFT
    f = np.fft.fft(dx, n=2*n)
    acf = np.fft.ifft(f * np.conj(f)).real[:n]
    acf /= acf[0]  # normalize

    # Truncate at first negative value (Chodera's simple truncation)
    cutoff = np.argmax(acf < 0)
    if cutoff == 0:
        cutoff = n
    tau_int = 0.5 + np.sum(acf[1:cutoff])
    g = max(1.0, 1.0 + 2.0 * tau_int)
    return tau_int, g


def detect_equilibration(x):
    """
    Chodera (2016) method: find t0 that maximises N_eff = (N - t0) / g(t0).
    Returns t0 (index), tau_int, g, N_eff at the optimal point.
    """
    n = len(x)
    best_neff = 0.0
    best_t0 = 0
    best_tau = 0.0
    best_g = 1.0

    # Subsample candidates to keep cost manageable
    candidates = np.unique(np.linspace(0, int(n * 0.9), min(200, n)).astype(int))
    for t0 in candidates:
        sub = x[t0:]
        if len(sub) < 10:
            break
        _, g = integrated_autocorr_time(sub)
        neff = (n - t0) / g
        if neff > best_neff:
            best_neff = neff
            best_t0 = t0
            best_tau, best_g = integrated_autocorr_time(sub)

    return best_t0, best_tau, best_g, best_neff


# ── Plotting functions ─────────────────────────────────────────────────────────

def plot_energies(energy_file):
    try:
        data = np.loadtxt(energy_file)
        run_tag = os.path.splitext(os.path.basename(energy_file))[0].replace('energy_', '')
        steps = data[:, 0]
        e_tot, e_lj, e_tors = data[:, 1], data[:, 2], data[:, 3]

        plt.figure()
        plt.plot(steps, e_tot, label='Total Energy', alpha=0.8)
        plt.plot(steps, e_lj,  label='LJ Energy',    alpha=0.8)
        plt.plot(steps, e_tors,label='Torsion Energy',alpha=0.8)
        plt.xlabel('MC Steps')
        plt.ylabel('Energy (kcal/mol)')
        plt.title('Energy Evolution')
        plt.xlim(left=-steps.max() * 0.01)  # small negative margin
        plt.legend(loc='center', bbox_to_anchor=(0.5, 0.4))
        plt.grid(True, alpha=0.3)
        out_file = os.path.join(OUTPUT_DIR, f'energy_evolution_{run_tag}.pdf')
        plt.savefig(out_file)
        plt.close()
        print(f"Generated {os.path.basename(out_file)}")
    except Exception as e:
        print(f"Error plotting energies: {e}")


def plot_observables(obs_file):
    try:
        data = np.loadtxt(obs_file)
        run_tag = os.path.splitext(os.path.basename(obs_file))[0].replace('observables_', '')
        steps, rg, ree = data[:, 0], data[:, 1], data[:, 2]

        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(5.25, 3.9372), sharex=True)
        ax1.plot(steps, rg, color='blue')
        ax1.set_ylabel('Radius of Gyration (Å)')
        ax1.set_title('Structural Observables Evolution')
        ax1.set_xlim(left=-steps.max() * 0.01)  # small negative margin
        ax1.grid(True, alpha=0.3)
        ax2.plot(steps, ree, color='red')
        ax2.set_xlabel('MC Steps')
        ax2.set_ylabel('End-to-End Distance (Å)')
        ax2.set_xlim(left=-steps.max() * 0.01)  # small negative margin
        ax2.grid(True, alpha=0.3)
        plt.tight_layout()
        out_file = os.path.join(OUTPUT_DIR, f'observables_evolution_{run_tag}.pdf')
        plt.savefig(out_file)
        plt.close()
        print(f"Generated {os.path.basename(out_file)}")
    except Exception as e:
        print(f"Error plotting observables: {e}")


def plot_torsions(tors_file, energy_file, explicit_h=True):
    try:
        if not explicit_h:
            c1, c2, c3 = 0.705, -0.135, 1.572
        else:
            c1, c2, c3 = 0.8700, -0.0785, 1.5075

        def torsion_potential(phi):
            return (c1 * (1.0 + np.cos(phi))
                  + c2 * (1.0 - np.cos(2.0 * phi))
                  + c3 * (1.0 + np.cos(3.0 * phi)))

        # ── Detect equilibration from total energy ─────────────────────────
        edata  = np.loadtxt(energy_file)
        e_tot  = edata[:, 1]
        n_energy = len(e_tot)

        t0_idx, tau_int, g, n_eff = detect_equilibration(e_tot)

        # Map energy-frame index back to torsion-frame index
        # (assumes both files have the same number of rows)
        with open(tors_file, 'r') as f:
            lines = f.readlines()
        n_tors = sum(1 for l in lines if not l.startswith('#'))
        scale  = n_tors / n_energy            # in case output frequencies differ
        start_tors = max(1, int(t0_idx * scale))

        equil_pct = 100.0 * start_tors / n_tors
        print(f"  Equilibration detected at frame {start_tors}/{n_tors} "
              f"({equil_pct:.1f}% discarded)")
        print(f"  tau_int = {tau_int:.1f} frames, g = {g:.2f}, N_eff ≈ {n_eff:.0f}")

        # ── Collect production torsion angles ──────────────────────────────
        run_tag = os.path.splitext(os.path.basename(tors_file))[0].replace('torsions_', '')
        all_angles = []
        frame_idx = 0
        for line in lines:
            if line.startswith('#'):
                continue
            frame_idx += 1
            if frame_idx <= start_tors:
                continue
            parts = line.split()
            all_angles.extend(float(x) for x in parts[1:])

        all_angles = np.array(all_angles)

        phi_grid = np.linspace(0.0, np.pi, 500)
        Uphi = torsion_potential(phi_grid)

        fig, ax1 = plt.subplots()
        ax1.hist(all_angles, bins=60, density=True, alpha=0.7,
                 color='purple', edgecolor='black',
                 label=f'Equilibrated distribution\n(discarded first {equil_pct:.0f}\%)')
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
            ax2.plot(phi_grid, Uphi, label='TraPPE-AA potential')
        ax2.set_ylabel('Torsion Potential (kcal/mol)')

        lines1, labels1 = ax1.get_legend_handles_labels()
        lines2, labels2 = ax2.get_legend_handles_labels()
        ax1.legend(lines1 + lines2, labels1 + labels2, loc='upper right')

        # Annotate with tau_int on the plot
        ax1.text(0.02, 0.97,
                 rf'$\tau_{{int}}={tau_int:.0f}$ frames, $N_{{eff}}\approx{n_eff:.0f}$',
                 transform=ax1.transAxes, va='top', fontsize=7)

        plt.title('Equilibrium Torsion Distribution and Potential')
        plt.tight_layout()
        out_file = os.path.join(OUTPUT_DIR, f'torsion_distribution_{run_tag}.pdf')
        plt.savefig(out_file)
        plt.close()
        print(f"Generated {os.path.basename(out_file)}")

    except Exception as e:
        print(f"Error plotting torsions: {e}")


# ── Main ───────────────────────────────────────────────────────────────────────

if __name__ == '__main__':
    print("Generating plots from simulation results...")
    for energy_file in ENERGY_FILES:
        run_tag = os.path.splitext(os.path.basename(energy_file))[0].replace('energy_', '')
        obs_file  = os.path.join(OUTPUT_DIR, f'observables_{run_tag}.dat')
        tors_file = os.path.join(OUTPUT_DIR, f'torsions_{run_tag}.dat')
        print(f'Processing run: {run_tag}')
        plot_energies(energy_file)
        if os.path.exists(obs_file):
            plot_observables(obs_file)
        else:
            print(f'Missing {os.path.basename(obs_file)}')
        if os.path.exists(tors_file):
            plot_torsions(tors_file, energy_file, explicit_h=get_explicit_h_setting())
        else:
            print(f'Missing {os.path.basename(tors_file)}')
    print("Done!")
