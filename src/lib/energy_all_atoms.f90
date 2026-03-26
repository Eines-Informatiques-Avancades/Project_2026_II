! Module: energy_all_atoms
! Author: Oliwier Misztal (omisztal)
! 
! Handles energy evaluations for the Monte Carlo simulation of an EXPLICIT HYDROGEN
! (All-Atom) polyethylene chain. 
! 
! Features:
! - Fully utilizes the OPLS-AA force field optimized for Polyethylene 
!   (Reference: Saether et al., Macromolecules 2021).
! - Topology mapping for H-atoms.
! - Optimized Metropolis delta-energy routine for rigid-body pivot moves.
! - Uses an "Effective Backbone Potential" to compute all 9 dihedrals around 
!   a C-C bond using a single C-C-C-C calculation.

module energy_all_atoms
  use parameters
  implicit none
  private

  public :: init_energy_topology
  public :: compute_total_energy
  public :: delta_energy

  ! Exclusion matrix (prevents LJ calculations for atoms separated by < 4 bonds)
  logical, allocatable, public :: is_excluded(:,:)

  ! Fast atom type mapping (1 = Carbon, 2 = Hydrogen)
  integer, allocatable :: atom_itype(:)
  integer :: pair_type_matrix(2, 2)

  ! Pre-computed LJ parameters: Index 1 = C-C, 2 = C-H, 3 = H-H
  double precision :: sig2(3), four_eps(3), rc2(3), e_shift(3)

contains

  ! Initializes the topology, exclusion matrix, and interaction parameters.
  ! MUST be called exactly once before the Monte Carlo loop begins.
  subroutine init_energy_topology(n_atoms, n_carbons, coords, symbols)
    integer, intent(in) :: n_atoms, n_carbons
    double precision, intent(in) :: coords(n_atoms, 3)
    character(len=2), intent(in) :: symbols(n_atoms)
    
    integer, allocatable :: backbone_pos(:)
    integer :: i, j

    ! 1. Pre-compute Lennard-Jones parameters (CC=1, CH=2, HH=3)
    sig2(1) = sigma_cc**2; four_eps(1) = 4.0d0 * eps_cc; rc2(1) = (2.5d0 * sigma_cc)**2
    sig2(2) = sigma_ch**2; four_eps(2) = 4.0d0 * eps_ch; rc2(2) = (2.5d0 * sigma_ch)**2
    sig2(3) = sigma_hh**2; four_eps(3) = 4.0d0 * eps_hh; rc2(3) = (2.5d0 * sigma_hh)**2
    
    do i = 1, 3
       e_shift(i) = four_eps(i) * ((sig2(i)/rc2(i))**6 - (sig2(i)/rc2(i))**3)
    end do

    ! 2. Initialize the fast pair-type lookup matrix
    ! Maps (Carbon=1, Hydrogen=2) combinations to parameter indices
    pair_type_matrix(1, 1) = 1 ! C-C
    pair_type_matrix(1, 2) = 2 ! C-H
    pair_type_matrix(2, 1) = 2 ! H-C
    pair_type_matrix(2, 2) = 3 ! H-H

    allocate(atom_itype(n_atoms))
    do i = 1, n_atoms
      if (symbols(i)(1:1) == 'C') then
        atom_itype(i) = 1
      else
        atom_itype(i) = 2
      end if
    end do

    ! 3. Map Hydrogen topology to the Carbon backbone
    allocate(backbone_pos(n_atoms))
    do i = 1, n_carbons
      backbone_pos(i) = i
    end do
    
    do i = n_carbons + 1, n_atoms
      ! Find which carbon this hydrogen is bonded to by checking distance
      ! C-H bond is ~1.09 A. Squared distance threshold 1.44 A^2 is safe.
      do j = 1, n_carbons
        if (sum((coords(i,:) - coords(j,:))**2) < 1.44d0) then
          backbone_pos(i) = j
          exit
        end if
      end do
    end do

    ! 4. Build the 1-2, 1-3, 1-4 exclusion matrix for non-bonded interactions
    allocate(is_excluded(n_atoms, n_atoms))
    is_excluded = .false.
    do i = 1, n_atoms
      do j = 1, n_atoms
        ! If atoms are attached to carbons separated by fewer than 4 bonds, exclude LJ
        if (abs(backbone_pos(i) - backbone_pos(j)) < 4) then
          is_excluded(i, j) = .true.
        end if
      end do
    end do

    deallocate(backbone_pos)
  end subroutine init_energy_topology

  ! Calculates the Truncated and Shifted LJ energy for a single pair
  pure function lj_pair_energy(r2, itype1, itype2) result(e)
    double precision, intent(in) :: r2
    integer, intent(in) :: itype1, itype2
    double precision :: e, sr2, sr6
    integer :: pt

    pt = pair_type_matrix(itype1, itype2)

    if (r2 > rc2(pt)) then
      e = 0.0d0
    elseif (r2 < 1.0d-12) then
      e = 1.0d10  ! Penalty to ensure MC rejection for overlapping atoms
    else
      sr2  = sig2(pt) / r2
      sr6  = sr2 * sr2 * sr2
      e    = four_eps(pt) * (sr6 * sr6 - sr6) - e_shift(pt)
    end if
  end function lj_pair_energy

  ! Calculates total system energy from scratch
  subroutine compute_total_energy(coords, n_atoms, n_carbons, e_total, e_lj, e_tors)
    integer, intent(in)           :: n_atoms, n_carbons
    double precision, intent(in)  :: coords(n_atoms, 3)
    double precision, intent(out) :: e_total, e_lj, e_tors
    
    double precision :: r2, cos_phi
    integer :: i, j

    ! Total Lennard-Jones Energy
    e_lj = 0.0d0
    
    !$omp parallel do if(omp_total_energy) private(j, r2) reduction(+:e_lj) schedule(dynamic, 10)
    do i = 1, n_atoms - 1
      do j = i + 1, n_atoms
        if (is_excluded(i,j)) cycle
        r2 = sum((coords(j,:) - coords(i,:))**2)
        e_lj = e_lj + lj_pair_energy(r2, atom_itype(i), atom_itype(j))
      end do
    end do
    !$omp end parallel do

    ! Total Torsional Energy (Calculated only for the Carbon Backbone using the Effective Potential)
    e_tors = 0.0d0
    
    !$omp parallel do if(omp_total_energy) private(cos_phi) reduction(+:e_tors)
    do i = 1, n_carbons - 3
      cos_phi = compute_cos_dihedral(coords(i,:), coords(i+1,:), coords(i+2,:), coords(i+3,:))
      e_tors = e_tors + torsion_single(cos_phi)
    end do
    !$omp end parallel do
    
    e_total = e_lj + e_tors
  end subroutine compute_total_energy

  ! Delta Energy calculation for Monte Carlo pivot moves
  subroutine delta_energy(coords_old, coords_new, n_atoms, n_carbons, k, de_total, de_lj, de_tors)
    integer, intent(in)           :: n_atoms, n_carbons, k
    double precision, intent(in)  :: coords_old(n_atoms, 3), coords_new(n_atoms, 3)
    double precision, intent(out) :: de_total, de_lj, de_tors
    
    double precision :: cos_phi_old, cos_phi_new, r2_old, r2_new
    integer :: i, j, f, m
    integer :: n_fixed, n_moved
    
    ! Fast-evaluation arrays allocated on the stack
    integer :: fixed_list(n_atoms), moved_list(n_atoms)  

    ! 1. Detect which atoms actually moved during the pivot step, and isolate the lists.
    n_fixed = 0
    n_moved = 0
    do i = 1, n_atoms
       if (sum((coords_new(i,:) - coords_old(i,:))**2) > 1.0d-12) then
         n_moved = n_moved + 1
         moved_list(n_moved) = i
       else
         n_fixed = n_fixed + 1
         fixed_list(n_fixed) = i
       end if
    end do

    ! --- 2. Delta Torsional Energy ---
    de_tors = 0.0d0
    if (k >= 2) then
      ! Only the backbone torsion crossing the pivot point changes
      cos_phi_old = compute_cos_dihedral(coords_old(k-1,:), coords_old(k,:), coords_old(k+1,:), coords_old(k+2,:))
      cos_phi_new = compute_cos_dihedral(coords_new(k-1,:), coords_new(k,:), coords_new(k+1,:), coords_new(k+2,:))
      de_tors = torsion_single(cos_phi_new) - torsion_single(cos_phi_old)
    end if

    ! --- 3. Delta Lennard-Jones Energy ---
    de_lj = 0.0d0
    
    !$omp parallel do if(omp_delta_energy .and. (n_fixed * n_moved > 1000)) &
    !$omp private(f, i, m, j, r2_old, r2_new) reduction(+:de_lj) collapse(2)
    do f = 1, n_fixed
      do m = 1, n_moved
        i = fixed_list(f)
        j = moved_list(m)
        
        ! Excluded pairs check (1-2, 1-3, 1-4)
        if (is_excluded(i, j)) cycle

        r2_old = sum((coords_old(j,:) - coords_old(i,:))**2)
        r2_new = sum((coords_new(j,:) - coords_new(i,:))**2)

        de_lj = de_lj + lj_pair_energy(r2_new, atom_itype(i), atom_itype(j)) &
                      - lj_pair_energy(r2_old, atom_itype(i), atom_itype(j))
      end do
    end do
    !$omp end parallel do

    de_total = de_lj + de_tors
  end subroutine delta_energy

  ! Calculates the cosine of the dihedral angle directly using cross products
  pure function compute_cos_dihedral(r1, r2, r3, r4) result(cos_phi)
    double precision, intent(in) :: r1(3), r2(3), r3(3), r4(3)
    double precision :: cos_phi
    double precision :: b1(3), b2(3), b3(3), n1(3), n2(3), nn1, nn2
    
    b1 = r2 - r1
    b2 = r3 - r2
    b3 = r4 - r3
    
    n1 = cross_product(b1, b2)
    n2 = cross_product(b2, b3)
    
    nn1 = sum(n1**2)
    nn2 = sum(n2**2)
    
    ! Guard against collinear atoms (zero-length normals)
    if (nn1 < 1.0d-28 .or. nn2 < 1.0d-28) then
      cos_phi = -1.0d0  ! Default to trans (180 degrees)
      return
    end if
    
    cos_phi = dot_product(n1, n2) / (sqrt(nn1) * sqrt(nn2))
  end function compute_cos_dihedral

  ! Calculates the torsional energy for a single dihedral angle using OPLS-AA
  ! The parameters opls_c1, opls_c2, opls_c3 from Sæther et al. (2021)
  ! are already defined as effective potentials in parameters.f90
  pure function torsion_single(cos_phi) result(e)
    double precision, intent(in) :: cos_phi
    double precision :: e, y, y2, y3

    y = cos_phi   ! corresponds to trans = -1, cis = 1
    
    ! Pre-compute powers for efficiency
    y2 = y * y
    y3 = y2 * y

    ! OPLS-AA evaluation using trigonometric identities
    ! cos(2x) = 2*cos^2(x) - 1 => 1 - cos(2x) = 2 - 2y^2
    ! cos(3x) = 4*cos^3(x) - 3*cos(x) => 1 + cos(3x) = 1 - 3y + 4y^3
    e = opls_c1 * (1.0d0 + y) &
      + opls_c2 * (2.0d0 - 2.0d0 * y2) &
      + opls_c3 * (1.0d0 - 3.0d0 * y + 4.0d0 * y3)
  end function torsion_single

  ! Cross product utility
  pure function cross_product(a, b) result(c)
    double precision, intent(in) :: a(3), b(3)
    double precision :: c(3)
    c(1) = a(2)*b(3) - a(3)*b(2)
    c(2) = a(3)*b(1) - a(1)*b(3)
    c(3) = a(1)*b(2) - a(2)*b(1)
  end function cross_product

end module energy_all_atoms