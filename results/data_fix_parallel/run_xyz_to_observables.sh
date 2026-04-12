#!/usr/bin/env bash

# Loop through all matching .xyz files in the current directory
for file in equil*.xyz; do
    # Safety check in case no .xyz files are found
    [ -f "$file" ] || continue
    
    echo "----------------------------------------"
    echo "Processing: $file"
    python3 xyz_to_observables.py "$file"
done

echo "----------------------------------------"
echo "All trajectory files processed!"

#for xyz_file in *.xyz; do
#    [ -f "$xyz_file" ] || continue
#    
#    echo "Processing $xyz_file..."
#    
#    # Generate the proper target filenames by cleanly substituting strings
#    obs_file="${xyz_file/prod_trajectory/prod_observables}"
#    obs_file="${obs_file/.xyz/.dat}"
#    
#    tors_file="${xyz_file/prod_trajectory/prod_torsions}"
#    tors_file="${tors_file/.xyz/.dat}"
#    
#    # Run algorithms and explicitly pass the target filenames using --out
#    python3 xyz_to_observables.py "$xyz_file" --out "$obs_file"
#    python3 xyz_to_torsions.py "$xyz_file" --out "$tors_file"
#done
#echo "Regeneration directly to final target filenames completed cleanly!"