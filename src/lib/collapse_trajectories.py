#!/usr/bin/env python3
import sys
import glob
import os
import re

def main():
    # Allow directory as an argument, default to the local parallel results
    if len(sys.argv) > 1:
        output_dir = sys.argv[1]
    else:
        output_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '../../results/main_parallel_local'))

    if not os.path.exists(output_dir):
        print(f"Directory not found: {output_dir}")
        sys.exit(1)

    tr_files = glob.glob(os.path.join(output_dir, 'prod_trajectory_c*_sd*.xyz'))
    
    configs = set()
    for f in tr_files:
        m = re.search(r'prod_trajectory_c(\d+)_', os.path.basename(f))
        if m: configs.add(int(m.group(1)))
        
    configs = sorted(list(configs))
    
    if not configs:
        print(f"No trajectory files found in {output_dir}")
        return
        
    for c_val in configs:
        c_str = f"c{c_val}"
        # Logically collapse them sequentially by seed string order, matching plot array loading algorithm
        t_files = sorted(glob.glob(os.path.join(output_dir, f'prod_trajectory_{c_str}_sd*.xyz')))
        if not t_files: continue
        
        all_file_frames = []
        
        for tr in t_files:
            frames = []
            with open(tr, 'r') as f:
                lines = f.readlines()
                i = 0
                while i < len(lines):
                    if lines[i].strip().isdigit():  # Frame header check (num atoms)
                        num_atoms = int(lines[i].strip())
                        if i + num_atoms + 2 <= len(lines):
                            frames.append(lines[i:i+num_atoms+2])
                            i += num_atoms + 2
                        else:
                            break
                    else:
                        i += 1
            all_file_frames.append(frames)
            
        # Determine min length across datasets to act exactly like np.column_stack padding
        min_len = min((len(f) for f in all_file_frames)) if all_file_frames else 0
        if min_len == 0:
            print(f"Warning: No complete frames parsed for Conf {c_val}")
            continue
            
        xyz_out = os.path.join(output_dir, f'collapsed_trajectory_{c_str}.xyz')
        
        total_frames = min_len * len(t_files)
        print(f"Stitching {len(t_files)} trajectories for Conf {c_val} into --> {os.path.basename(xyz_out)}")
        print(f"  (Interlacing {min_len} frames from each file = {total_frames} total frames)")
        
        with open(xyz_out, 'w') as out_f:
            # The column-stack mimicking loop: sequentially grab frame `i` from dataset `j`
            for frame_idx in range(min_len):
                for file_idx in range(len(t_files)):
                    frame_lines = all_file_frames[file_idx][frame_idx]
                    out_f.writelines(frame_lines)
                    
    print("\nTrajectory collapsing complete!")

if __name__ == '__main__':
    main()
