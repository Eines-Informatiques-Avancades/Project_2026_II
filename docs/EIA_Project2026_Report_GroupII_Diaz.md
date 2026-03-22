## Monte Carlo Engine & Geometry
**Contributor: ManelDC55**

The primary contribution to the project consisted of the development of the core Monte Carlo engine, specifically focusing on the implementation of proposal moves and the geometric transformations required to explore the polymer's phase space efficiently.

### Implementation of the Pivot Move
To allow the polyethylene chain to change its conformation, the **Pivot Move** algorithm was implemented within the `rotate_dihedral` subroutine. Unlike local moves, the pivot move selects a random bond along the chain and rotates the entire subsequent segment (the "tail") as a rigid body.

To perform this 3D rotation without distorting bond lengths or bond angles, **Rodrigues' Rotation Formula** was utilized. This equation allows the rotation of any vector $\vec{v}$ (representing an atom's position relative to the pivot) around an arbitrary unit axis $\vec{k}$ (the chosen C-C bond) by an angle $\phi$:

$$\vec{v}_{rot} = \vec{v} \cos\phi + (\vec{k} \times \vec{v}) \sin\phi + \vec{k} (\vec{k} \cdot \vec{v}) (1 - \cos\phi)$$

### Handling $sp^3$ Hybridization in All-Atom (AA) Mode
A significant technical requirement was the transition from the United-Atom (UA) model to the **All-Atom (AA)** model. When explicit hydrogens are included, the geometry becomes highly restrictive due to the $sp^3$ hybridization of the carbon atoms.

The rotation logic was designed to ensure that when a carbon atom rotates, its two attached hydrogens rotate with the exact same angle and axis. This synchronization is essential to maintain constant C-H bond lengths and C-C-H angles, preventing non-physical atomic overlaps that would otherwise lead to the immediate rejection of the move by the Metropolis criterion.

```fortran
! Snippet from rotate_dihedral handling explicit hydrogens
if (explicit_h) then
  ! Hydrogens are stored after the carbon backbone in the coords array
  do i = n_carbons + 1, n_atoms
    do j = k + 1, n_carbons
      ! Identify if hydrogen i is bonded to the moving carbon j
      v = coords(i, :) - coords(j, :)
      if (vnorm(v) < 1.2d0) then 
        ! Apply Rodrigues' rotation relative to the pivot point
        v = coords(i, :) - pivot
        dot_uv = sum(axis * v)
        v_rot = v * cos_p + cross(axis, v) * sin_p + axis * dot_uv * (1.0d0 - cos_p)
        coords_new(i, :) = pivot + v_rot
        exit ! Hydrogen found, move to the next atom
      end if
    end do
  end do
end if