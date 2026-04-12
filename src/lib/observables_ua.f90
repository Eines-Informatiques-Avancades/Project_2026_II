! Module observables
! Computes radius of gyration, end-to-end distance, and extracts torsion angles.
! Author: Arthur Murphy

module observables
  use energy, only: compute_cos_dihedral
  implicit none
  private

  public :: compute_rg
  public :: compute_end_to_end
  public :: compute_torsion_angles

contains

  ! Function: compute_rg
  ! Author: ai-murphy
  ! ---------------------------------------------------------------------------
  ! Calculates the squared radius of gyration
  ! ---------------------------------------------------------------------------
  function compute_rg(nc, coords) result(rg2)
    integer, intent(in) :: nc                     ! Number of atoms
    double precision, intent(in) :: coords(nc, 3) ! Coordinates of atoms
    double precision :: rg2, com(3), dx, dy, dz   ! Radius of gyration, center of mass, differences in coordinates
    integer :: i                                  ! Loop counter

    com = 0.0d0
    do i = 1, nc
      com(:) = com(:) + coords(i, :)
    end do
    com = com / dble(nc)

    rg2 = 0.0d0
    do i = 1, nc
      dx = coords(i, 1) - com(1)
      dy = coords(i, 2) - com(2)
      dz = coords(i, 3) - com(3)
      rg2 = rg2 + dx*dx + dy*dy + dz*dz
    end do
    rg2 = rg2 / dble(nc)

  end function compute_rg

  ! Function: compute_end_to_end
  ! Author: ai-murphy
  ! ---------------------------------------------------------------------------
  ! Calculates the squared end-to-end distance
  ! ---------------------------------------------------------------------------
  function compute_end_to_end(nc, coords) result(ree2)
    integer, intent(in) :: nc                     ! Number of atoms
    double precision, intent(in) :: coords(nc, 3) ! Coordinates of atoms
    double precision :: ree2, dx, dy, dz          ! End-to-end distance, differences in coordinates

    dx = coords(nc, 1) - coords(1, 1)
    dy = coords(nc, 2) - coords(1, 2)
    dz = coords(nc, 3) - coords(1, 3)
    ree2 = dx*dx + dy*dy + dz*dz

  end function compute_end_to_end

  ! Subroutine: compute_torsion_angles
  ! Author: ai-murphy
  ! ---------------------------------------------------------------------------
  ! Extracts all torsion angles (in radians) into array phis
  ! ---------------------------------------------------------------------------
  subroutine compute_torsion_angles(nc, coords, phis)
    integer, intent(in) :: nc                     ! Number of atoms
    double precision, intent(in) :: coords(nc, 3) ! Coordinates of atoms
    double precision, intent(out) :: phis(nc - 3) ! Torsion angles
    double precision :: cos_phi                   ! Cosine of torsion angles
    integer :: i                                  ! Loop counter

    do i = 1, nc - 3
      cos_phi = compute_cos_dihedral(coords(i,:), coords(i+1,:), coords(i+2,:), coords(i+3,:))
      ! Prevent acos domain errors from numerical issues
      if (cos_phi > 1.0d0) cos_phi = 1.0d0
      if (cos_phi < -1.0d0) cos_phi = -1.0d0
      phis(i) = acos(cos_phi)
    end do
  end subroutine compute_torsion_angles

end module observables
