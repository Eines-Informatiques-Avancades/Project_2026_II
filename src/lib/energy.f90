! Module: energy
! Author: Oliwier Misztal (omisztal)
! 
! Handles energy evaluations for the Monte Carlo simulation of a united-atom 
! polyethylene chain. Includes full energy calculations and an optimized 
! delta-energy routine for fast Metropolis acceptance/rejection steps.
!
! Using the TraPPE-UA torsional potential for improved physical 
! accuracy in long polymer melts.

module energy
  use parameters
  implicit none
  private

  public :: compute_total_energy, compute_lj_energy
  public :: compute_torsion_energy, delta_energy
  public :: compute_cos_dihedral, torsion_single, lj_pair_energy

  ! --- Lennard-Jones Constants ---
  ! Pre-computing squared values and shifts to save CPU cycles in the MC loop
  double precision, parameter :: sig2      = sigma_cc * sigma_cc
  double precision, parameter :: four_eps  = 4.0d0 * eps_cc
  double precision, parameter :: rc        = 2.5d0 * sigma_cc
  double precision, parameter :: rc2       = rc * rc
  
  double precision, parameter :: rc_sr2    = sig2 / rc2
  double precision, parameter :: rc_sr6    = rc_sr2 * rc_sr2 * rc_sr2
  double precision, parameter :: rc_sr12   = rc_sr6 * rc_sr6
  double precision, parameter :: e_shift   = four_eps * (rc_sr12 - rc_sr6)

  ! --- TraPPE-UA Torsional Coefficients (kcal/mol) ---
  ! Reference: Martin & Siepmann, J. Phys. Chem. B 102, 2569 (1998)
  ! Formula: U(phi) = c1*(1 + cos(phi)) + c2*(1 - cos(2*phi)) + c3*(1 + cos(3*phi))
  double precision, parameter :: trappe_c1 =  0.705d0
  double precision, parameter :: trappe_c2 = -0.135d0
  double precision, parameter :: trappe_c3 =  1.572d0

  ! Minimum index separation for non-bonded interactions (i.e., 1-5 interactions and beyond)
  integer, parameter :: min_sep = 4

contains

  ! Calculates the LJ energy for a single pair given their squared distance (r2).
  ! Returns 0 if beyond the cutoff distance.
  pure function lj_pair_energy(r2) result(e)
    double precision, intent(in) :: r2
    double precision :: e, sr2, sr6, sr12

    if (r2 > rc2 .or. r2 < 1.0d-12) then
      e = 0.0d0
    else
      sr2  = sig2 / r2
      sr6  = sr2 * sr2 * sr2
      sr12 = sr6 * sr6
      e    = four_eps * (sr12 - sr6) - e_shift
    end if
  end function lj_pair_energy

  ! Calculates the cosine of the dihedral angle directly using the dot product 
  ! of the normal vectors of the two planes. This avoids expensive atan2 and cos calls.
  pure function compute_cos_dihedral(r1, r2, r3, r4) result(cos_phi)
    double precision, intent(in) :: r1(3), r2(3), r3(3), r4(3)
    double precision :: cos_phi
    double precision :: b1(3), b2(3), b3(3)
    double precision :: n1(3), n2(3)
    double precision :: nn1, nn2

    b1 = r2 - r1
    b2 = r3 - r2
    b3 = r4 - r3

    n1 = cross_product(b1, b2)
    n2 = cross_product(b2, b3)

    nn1 = sum(n1**2)
    nn2 = sum(n2**2)

    ! Guard against collinear atoms (zero-length normals)
    if (nn1 < 1.0d-28 .or. nn2 < 1.0d-28) then
      cos_phi = 1.0d0   ! default to trans
      return
    end if

    ! cos(phi) = (n1 . n2) / (|n1| * |n2|)
    cos_phi = dot_product(n1, n2) / (sqrt(nn1) * sqrt(nn2))
  end function compute_cos_dihedral

  ! Calculates the torsional energy for a single dihedral angle
  pure function torsion_single(cos_phi) result(e)
    double precision, intent(in) :: cos_phi
    double precision :: e, y, y2, y3

    y = -cos_phi
    
    ! Pre-compute powers for efficiency
    y2 = y * y
    y3 = y2 * y

    ! TraPPE-UA evaluation using trigonometric identities:
    ! cos(2x) = 2*cos^2(x) - 1
    ! cos(3x) = 4*cos^3(x) - 3*cos(x)
    ! U(y) = c1*(1 + y) + c2*(1 - (2*y^2 - 1)) + c3*(1 + (4*y^3 - 3*y))
    ! Simplifying the c2 term: 1 - (2y^2 - 1) = 2 - 2y^2
    e = trappe_c1 * (1.0d0 + y) &
      + trappe_c2 * (2.0d0 - 2.0d0 * y2) &
      + trappe_c3 * (1.0d0 - 3.0d0 * y + 4.0d0 * y3)
  end function torsion_single

  ! Calculates the total Lennard-Jones energy for the entire polymer chain
  function compute_lj_energy(coords, nc) result(e_lj)
    integer, intent(in) :: nc
    double precision, intent(in) :: coords(nc, 3)
    double precision :: e_lj, dx, dy, dz, r2
    integer :: i, j

    e_lj = 0.0d0
    do i = 1, nc - min_sep
      do j = i + min_sep, nc
        dx = coords(j,1) - coords(i,1)
        dy = coords(j,2) - coords(i,2)
        dz = coords(j,3) - coords(i,3)
        r2 = dx*dx + dy*dy + dz*dz
        e_lj = e_lj + lj_pair_energy(r2)
      end do
    end do
  end function compute_lj_energy

  ! Calculates the total torsional energy for the entire polymer chain
  function compute_torsion_energy(coords, nc) result(e_tors)
    integer, intent(in) :: nc
    double precision, intent(in) :: coords(nc, 3)
    double precision :: e_tors, cos_phi
    integer :: i

    e_tors = 0.0d0
    do i = 1, nc - 3
      cos_phi = compute_cos_dihedral(coords(i,:), coords(i+1,:), coords(i+2,:), coords(i+3,:))
      e_tors = e_tors + torsion_single(cos_phi)
    end do
  end function compute_torsion_energy

  ! Convenience routine to get total energy and its components
  subroutine compute_total_energy(coords, nc, e_total, e_lj, e_tors)
    integer, intent(in)           :: nc
    double precision, intent(in)  :: coords(nc, 3)
    double precision, intent(out) :: e_total, e_lj, e_tors

    e_lj   = compute_lj_energy(coords, nc)
    e_tors = compute_torsion_energy(coords, nc)
    e_total = e_lj + e_tors
  end subroutine compute_total_energy

  ! Calculates the change in energy (delta E) for a Monte Carlo pivot move.
  subroutine delta_energy(coords_old, coords_new, nc, k, de_total, de_lj, de_tors)
    integer, intent(in)           :: nc, k
    double precision, intent(in)  :: coords_old(nc, 3), coords_new(nc, 3)
    double precision, intent(out) :: de_total, de_lj, de_tors
    
    double precision :: cos_phi_old, cos_phi_new
    double precision :: dx_o, dy_o, dz_o, r2_old, dx_n, dy_n, dz_n, r2_new
    integer :: i, j, moved_start

    de_tors = 0.0d0
    de_lj   = 0.0d0
    moved_start = k + 2

    ! --- 1. Delta Torsional Energy ---
    
    ! Torsion 1: Atoms (k-1, k, k+1, k+2). 
    ! Atom k+2 moves relative to the fixed k-1, k, k+1 plane.
    if (k >= 2) then
      cos_phi_old = compute_cos_dihedral(coords_old(k-1,:), coords_old(k,:), coords_old(k+1,:), coords_old(k+2,:))
      cos_phi_new = compute_cos_dihedral(coords_new(k-1,:), coords_new(k,:), coords_new(k+1,:), coords_new(k+2,:))
      de_tors = torsion_single(cos_phi_new) - torsion_single(cos_phi_old)
    end if

    ! Torsion 2: Atoms (k, k+1, k+2, k+3). 
    ! The plane defined by k, k+1, k+2 changes relative to k+1, k+2, k+3.
    if (k + 3 <= nc) then
      cos_phi_old = compute_cos_dihedral(coords_old(k,:), coords_old(k+1,:), coords_old(k+2,:), coords_old(k+3,:))
      cos_phi_new = compute_cos_dihedral(coords_new(k,:), coords_new(k+1,:), coords_new(k+2,:), coords_new(k+3,:))
      de_tors = de_tors + torsion_single(cos_phi_new) - torsion_single(cos_phi_old)
    end if

    ! --- 2. Delta Lennard-Jones Energy ---
    ! Only pairs that cross the boundary between the fixed segment (1 to k+1) 
    ! and the moved segment (k+2 to nc) change their distance.
    do i = 1, moved_start - 1
      do j = max(moved_start, i + min_sep), nc
        ! Old distance
        dx_o = coords_old(j,1) - coords_old(i,1)
        dy_o = coords_old(j,2) - coords_old(i,2)
        dz_o = coords_old(j,3) - coords_old(i,3)
        r2_old = dx_o*dx_o + dy_o*dy_o + dz_o*dz_o

        ! New distance
        dx_n = coords_new(j,1) - coords_new(i,1)
        dy_n = coords_new(j,2) - coords_new(i,2)
        dz_n = coords_new(j,3) - coords_new(i,3)
        r2_new = dx_n*dx_n + dy_n*dy_n + dz_n*dz_n

        de_lj = de_lj + lj_pair_energy(r2_new) - lj_pair_energy(r2_old)
      end do
    end do

    de_total = de_lj + de_tors
  end subroutine delta_energy

  ! Cross product utility
  pure function cross_product(a, b) result(c)
    double precision, intent(in) :: a(3), b(3)
    double precision :: c(3)
    c(1) = a(2)*b(3) - a(3)*b(2)
    c(2) = a(3)*b(1) - a(1)*b(3)
    c(3) = a(1)*b(2) - a(2)*b(1)
  end function cross_product

end module energy