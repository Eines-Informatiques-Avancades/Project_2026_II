"""
xyz_to_observables.py
---------------------
Recomputes the radius of gyration (Rg) and end-to-end distance (Ree)
from an XYZ trajectory file using only the carbon atom coordinates,
bypassing the n_carbons/n_atoms dimension mismatch in observables.f90
that produced garbled values in the original .dat output.

The formulas are exact replicas of observables.f90:

  compute_rg:
    rCOM = (1/nc) * sum_i(ri)          (unweighted, equal mass carbons)
    Rg2  = (1/nc) * sum_i |ri - rCOM|^2
    Rg   = sqrt(Rg2)                   [Angstroms]

  compute_end_to_end:
    Ree2 = |r(nc) - r(1)|^2           (first to last carbon)
    Ree  = sqrt(Ree2)                  [Angstroms]

Output format matches the Fortran output exactly:
    # Step Rg End_to_End
    <step:I10>  <Rg:F15.4>  <Ree:F15.4>

so the existing plot_results_serial_geweke.py works without modification.

Usage
-----
  python xyz_to_observables.py <trajectory.xyz> [--n_carbons N] [--out output.dat]

If --n_carbons is omitted the script counts 'C' atoms in the first frame.

Examples
--------
  # Auto-detect n_carbons, write alongside the XYZ file:
  python xyz_to_observables.py prod_trajectory_500_4_10000000_300_00.xyz

  # Explicit carbon count and output path:
  python xyz_to_observables.py traj.xyz --n_carbons 500 --out prod_observables_fixed.dat
"""

import argparse
import re
import sys
import numpy as np
from pathlib import Path


# ── Geometry — exact replicas of observables.f90 ─────────────────────────────

def compute_rg(carbon_coords):
    """
    Squared radius of gyration of the carbon backbone, then sqrt.
    Matches compute_rg in observables.f90 (unweighted COM).

    Parameters
    ----------
    carbon_coords : ndarray, shape (nc, 3)

    Returns
    -------
    rg : float   Radius of gyration in Å
    rg2 : float  Squared radius of gyration in Å²
    """
    com = carbon_coords.mean(axis=0)               # shape (3,)
    diff = carbon_coords - com                      # shape (nc, 3)
    rg2 = float((diff * diff).sum() / len(carbon_coords))
    return np.sqrt(rg2), rg2


def compute_end_to_end(carbon_coords):
    """
    Squared end-to-end distance between first and last carbon, then sqrt.
    Matches compute_end_to_end in observables.f90.

    Parameters
    ----------
    carbon_coords : ndarray, shape (nc, 3)

    Returns
    -------
    ree : float   End-to-end distance in Å
    ree2 : float  Squared end-to-end distance in Å²
    """
    diff = carbon_coords[-1] - carbon_coords[0]
    ree2 = float(np.dot(diff, diff))
    return np.sqrt(ree2), ree2


# ── XYZ parser ────────────────────────────────────────────────────────────────

def parse_xyz(path, n_carbons=None):
    """
    Generator that yields (step, carbon_coords) for every frame.

    step           : integer MC step parsed from the comment line
                     ('Step <N>' format written by main_serial_equil.f90)
    carbon_coords  : ndarray of shape (n_carbons, 3), dtype float64
    """
    with open(path, 'r') as fh:
        frame_idx = 0
        while True:
            header = fh.readline()
            if not header:
                break
            try:
                n_atoms = int(header.strip())
            except ValueError:
                break

            comment = fh.readline()

            # Parse step number from comment line ("Step <N> E=...")
            step = frame_idx
            m = re.search(r'Step\s+(\d+)', comment)
            if m:
                step = int(m.group(1))
            else:
                # Fallback: take the first integer token
                for tok in comment.split():
                    if tok.lstrip('-').isdigit():
                        try:
                            step = int(tok)
                            break
                        except ValueError:
                            pass

            symbols = []
            coords  = []
            for _ in range(n_atoms):
                parts = fh.readline().split()
                symbols.append(parts[0])
                coords.append((float(parts[1]), float(parts[2]), float(parts[3])))

            # Select carbon atoms (symbol == 'C')
            c_idx = [i for i, s in enumerate(symbols) if s == 'C']
            if n_carbons is not None:
                c_idx = c_idx[:n_carbons]

            carbon_coords = np.array([coords[i] for i in c_idx], dtype=np.float64)

            yield step, carbon_coords
            frame_idx += 1


# ── Writer ────────────────────────────────────────────────────────────────────

def write_observables(out_path, records):
    """
    Write observables in the format produced by the Fortran code:

        # Step Rg End_to_End
        <I10>  <F15.4>  <F15.4>

    Parameters
    ----------
    out_path : str or Path
    records  : iterable of (step, rg, ree)
    """
    with open(out_path, 'w') as fh:
        fh.write('# Step Rg End_to_End\n')
        for step, rg, ree in records:
            fh.write(f'{step:10d}{rg:15.4f}{ree:15.4f}\n')


# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument('xyz', help='Input XYZ trajectory file')
    parser.add_argument('--n_carbons', type=int, default=None,
                        help='Number of carbon atoms (auto-detected if omitted)')
    parser.add_argument('--out', default=None,
                        help='Output .dat file path (default: <xyz_stem>_observables_recomputed.dat)')
    args = parser.parse_args()

    xyz_path = Path(args.xyz)
    if not xyz_path.exists():
        print(f"ERROR: file not found: {xyz_path}", file=sys.stderr)
        sys.exit(1)

    out_path = Path(args.out) if args.out else \
        xyz_path.with_name(xyz_path.stem + '_observables_recomputed.dat')

    print(f"Input  : {xyz_path}")
    print(f"Output : {out_path}")

    # Auto-detect n_carbons from first frame if not provided
    if args.n_carbons is None:
        for _, carbon_coords in parse_xyz(str(xyz_path)):
            args.n_carbons = len(carbon_coords)
            break
    print(f"n_carbons = {args.n_carbons}")

    records = []
    for frame_num, (step, carbon_coords) in enumerate(
            parse_xyz(str(xyz_path), n_carbons=args.n_carbons)):

        rg,  _  = compute_rg(carbon_coords)
        ree, _  = compute_end_to_end(carbon_coords)
        records.append((step, rg, ree))

        if (frame_num + 1) % 100 == 0:
            print(f"  Processed {frame_num + 1} frames (step {step})...")

    print(f"Total frames processed: {len(records)}")
    write_observables(out_path, records)
    print(f"Done. Written to {out_path}")

    # Quick summary statistics
    rg_arr  = np.array([r[1] for r in records])
    ree_arr = np.array([r[2] for r in records])
    print()
    print("=== Summary ===")
    print(f"  Rg  — mean: {rg_arr.mean():.2f} Å,  std: {rg_arr.std():.2f} Å,  "
          f"min: {rg_arr.min():.2f},  max: {rg_arr.max():.2f}")
    print(f"  Ree — mean: {ree_arr.mean():.2f} Å,  std: {ree_arr.std():.2f} Å,  "
          f"min: {ree_arr.min():.2f},  max: {ree_arr.max():.2f}")


if __name__ == '__main__':
    main()
