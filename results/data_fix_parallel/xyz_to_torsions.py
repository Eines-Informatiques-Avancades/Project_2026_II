"""
xyz_to_torsions.py
------------------
Recomputes backbone torsion angles from an XYZ trajectory file using
only the carbon atom coordinates, bypassing the n_carbons/n_atoms
dimension mismatch that garbled the original .dat output.

The dihedral convention is identical to energy_all_atoms.f90:
  - cos(phi) = (n1 . n2) / (|n1| * |n2|)
  - n1 = (r2-r1) x (r3-r2),  n2 = (r3-r2) x (r4-r3)
  - trans  -> cos_phi = -1  -> phi = pi   (IUPAC convention)
  - cis    -> cos_phi = +1  -> phi = 0

Usage
-----
  python xyz_to_torsions.py <trajectory.xyz> [--n_carbons N] [--out output.dat]

If --n_carbons is omitted the script counts 'C' atoms in the first frame.
"""

import argparse
import sys
import numpy as np


# ── Geometry ──────────────────────────────────────────────────────────────────

def cross(a, b):
    return np.array([
        a[1]*b[2] - a[2]*b[1],
        a[2]*b[0] - a[0]*b[2],
        a[0]*b[1] - a[1]*b[0],
    ])


def compute_cos_dihedral(r1, r2, r3, r4):
    """Exact replica of energy_all_atoms.f90 compute_cos_dihedral."""
    b1 = r2 - r1
    b2 = r3 - r2
    b3 = r4 - r3

    n1 = cross(b1, b2)
    n2 = cross(b2, b3)

    nn1 = np.dot(n1, n1)
    nn2 = np.dot(n2, n2)

    # Guard against collinear atoms (mirrors the Fortran: default to trans)
    if nn1 < 1.0e-28 or nn2 < 1.0e-28:
        return -1.0   # trans (cos_phi = -1, phi = pi)

    cos_phi = np.dot(n1, n2) / (np.sqrt(nn1) * np.sqrt(nn2))

    # Numerical safety clamp before acos
    return float(np.clip(cos_phi, -1.0, 1.0))


def torsion_angles(carbon_coords):
    """
    Compute all n_carbons-3 backbone dihedral angles (in radians)
    for a single frame.  Returns a 1-D numpy array of length n_carbons-3.
    """
    nc = len(carbon_coords)
    phis = np.empty(nc - 3)
    for i in range(nc - 3):
        cos_phi = compute_cos_dihedral(
            carbon_coords[i],
            carbon_coords[i + 1],
            carbon_coords[i + 2],
            carbon_coords[i + 3],
        )
        phis[i] = np.arccos(cos_phi)
    return phis


# ── XYZ parser ────────────────────────────────────────────────────────────────

def parse_xyz(path, n_carbons=None):
    """
    Generator that yields (step, carbon_coords) for every frame.

    step           : integer MC step parsed from the comment line
    carbon_coords  : numpy array of shape (n_carbons, 3)

    The comment line is expected to contain 'Step <N>' anywhere in it
    (as written by main_serial_equil.f90).  If not found, a sequential
    frame index is used instead.
    """
    with open(path, 'r') as fh:
        frame_idx = 0
        while True:
            # --- header line ---
            header = fh.readline()
            if not header:
                break                       # end of file
            try:
                n_atoms = int(header.strip())
            except ValueError:
                break

            # --- comment / step line ---
            comment = fh.readline()
            step = frame_idx               # fallback
            for tok in comment.split():
                if tok.lstrip('-').isdigit():
                    try:
                        step = int(tok)
                        break
                    except ValueError:
                        pass
            # more robust: look for "Step <N>"
            import re
            m = re.search(r'Step\s+(\d+)', comment)
            if m:
                step = int(m.group(1))

            # --- atom lines ---
            symbols = []
            coords  = []
            for _ in range(n_atoms):
                line = fh.readline().split()
                symbols.append(line[0])
                coords.append([float(line[1]), float(line[2]), float(line[3])])

            # Identify carbon indices (atoms labelled 'C')
            c_indices = [i for i, s in enumerate(symbols) if s == 'C']

            # If n_carbons was specified, keep only the first n_carbons carbons
            if n_carbons is not None:
                c_indices = c_indices[:n_carbons]

            carbon_coords = np.array([coords[i] for i in c_indices])

            yield step, carbon_coords
            frame_idx += 1


# ── Writer ────────────────────────────────────────────────────────────────────

def write_torsions(out_path, frames):
    """
    Write torsion data in the same format as the Fortran output:
      # Step Torsion_Angles(rad)...
      <step>  <phi1>  <phi2>  ...
    """
    with open(out_path, 'w') as fh:
        fh.write('# Step Torsion_Angles(rad)...\n')
        for step, phis in frames:
            # Match Fortran format: I10 for step, F10.4 for each angle
            angle_str = ''.join(f'{phi:10.4f}' for phi in phis)
            fh.write(f'{step:10d}{angle_str}\n')


# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('xyz', help='Input XYZ trajectory file')
    parser.add_argument('--n_carbons', type=int, default=None,
                        help='Number of carbon atoms per frame (auto-detected if omitted)')
    parser.add_argument('--out', default=None,
                        help='Output .dat file (default: <xyz_stem>_recomputed.dat)')
    args = parser.parse_args()

    from pathlib import Path
    xyz_path = Path(args.xyz)
    if not xyz_path.exists():
        print(f"ERROR: file not found: {xyz_path}", file=sys.stderr)
        sys.exit(1)

    out_path = args.out or str(xyz_path.with_name(xyz_path.stem + '_torsions_recomputed.dat'))

    print(f"Input  : {xyz_path}")
    print(f"Output : {out_path}")

    # Two-pass approach: first pass just to count carbons if not given
    if args.n_carbons is None:
        print("Auto-detecting n_carbons from first frame...", end=' ')
        for step, carbon_coords in parse_xyz(str(xyz_path)):
            args.n_carbons = len(carbon_coords)
            print(args.n_carbons)
            break

    print(f"n_carbons = {args.n_carbons}  ->  {args.n_carbons - 3} dihedrals per frame")

    # Process all frames
    processed_frames = []
    for frame_num, (step, carbon_coords) in enumerate(
            parse_xyz(str(xyz_path), n_carbons=args.n_carbons)):
        phis = torsion_angles(carbon_coords)
        processed_frames.append((step, phis))
        if (frame_num + 1) % 100 == 0:
            print(f"  Processed {frame_num + 1} frames (step {step})...")

    print(f"Total frames processed: {len(processed_frames)}")
    write_torsions(out_path, processed_frames)
    print(f"Done. Written to {out_path}")

    return out_path, processed_frames


if __name__ == '__main__':
    main()
