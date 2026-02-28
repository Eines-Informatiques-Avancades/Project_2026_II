! Module for the initial conformation of the N-carbon chain

! initial-conf.f90
!
! Initial configuration generator for polyethylene:
! - Carbon-only chain (united atom backbone)
! - Optional explicit hydrogens for visualization
!
! Provides:
!   generate_initial_configuration(n_carbons, explicit_h, conf_type, rng_seed, symbols, coords_all)
!
module initial_conf
  use parameters
  implicit none

  double precision, parameter :: deg2rad = pi / 180.0d0
  double precision, parameter :: angle_tetra_deg = 109.4712206d0
  double precision, parameter :: bond_ch = 1.09d0                 ! Angstrom
  double precision, parameter :: overlap_cut = 0.85d0 * sigma_cc   ! Angstrom

contains

! Propose a dihedral angle phi based on conf_type
  double precision function propose_phi(conf_type) result(phi)
    ! Author: itxasoma
    integer, intent(in) :: conf_type
    double precision :: r
    integer :: pick

    call random_number(r)

    select case (conf_type)
    case(1)  ! all-trans
      phi = pi
    case(2)  ! random uniform in [-pi, pi]
      phi = (2.0d0*r - 1.0d0) * pi
    case(3)  ! simple RIS-like: {-60, 60, 180} equally likely
      pick = int(3.0d0*r) + 1
      if (pick == 1) then
        phi = -60.0d0 * deg2rad
      else if (pick == 2) then
        phi =  60.0d0 * deg2rad
      else
        phi = 180.0d0 * deg2rad
      endif
    case default
      phi = (2.0d0*r - 1.0d0) * pi
    end select
  end function propose_phi

! Implements the internal-coordinate rule, as we do when building a Zmatrix from scratch.
  function place_next_atom(r_im3, r_im2, r_im1, phi) result(r_i)
    ! Author: itxasoma
    ! Place atom i given previous three atoms (i-3,i-2,i-1),
    ! using fixed C-C bond length and fixed C-C-C bond angle,
    ! and dihedral angle phi.
    double precision, intent(in):: r_im3(3), r_im2(3), r_im1(3), phi
    double precision:: r_i(3)
    double precision:: theta, u(3), w(3), t(3)
    double precision:: b1(3), b2(3), dir(3)

    theta = bond_ang * deg2rad

    ! u points from current atom (i-1) toward previous atom (i-2)
    u  = unit_vec(r_im2 - r_im1)

    b1 = r_im2 - r_im3
    b2 = r_im1 - r_im2

    ! w is normal to plane (i-3,i-2,i-1)
    w = cross(b1, b2)
    if (vnorm(w) < 1.0d-12) then
      ! collinear fallback: choose any perpendicular
      if (abs(u(1)) < 0.9d0) then
        w = cross(u, (/1.0d0, 0.0d0, 0.0d0/))
      else
        w = cross(u, (/0.0d0, 1.0d0, 0.0d0/))
      endif
    endif
    w = unit_vec(w)

    ! t completes right-handed basis
    t = unit_vec(cross(w, u))

    ! New bond direction (from r_im1 to r_i)
    dir = u*cos(theta) + t*sin(theta)*cos(phi) + w*sin(theta)*sin(phi)

    r_i = r_im1 + bond_len * dir
  end function place_next_atom

  logical function ok_no_overlap(coords_c, i_new) result(ok)
    ! Author: YOUR_NAME
    double precision, intent(in) :: coords_c(:, :)
    integer, intent(in) :: i_new
    integer :: j
    double precision :: rij(3)

    ok = .true.

    ! Only check against atoms 1..i_new-3 (skip near neighbors)
    do j = 1, i_new-3
      rij = coords_c(i_new,:) - coords_c(j,:)
      if (vnorm(rij) < overlap_cut) then
        ok = .false.
        return
      endif
    enddo
  end function ok_no_overlap

  subroutine seed_rng(rng_seed)
    ! Author: YOUR_NAME
    integer, intent(in) :: rng_seed
    integer :: n, k
    integer, allocatable :: seed(:)

    call random_seed(size=n)
    allocate(seed(n))
    do k = 1, n
      seed(k) = rng_seed + 37*(k-1)
    enddo
    call random_seed(put=seed)
    deallocate(seed)
  end subroutine seed_rng

  subroutine build_chain_c_only(n_carbons, conf_type, rng_seed, coords_c)
    ! Author: YOUR_NAME
    integer, intent(in) :: n_carbons, conf_type, rng_seed
    double precision, intent(out) :: coords_c(n_carbons, 3)

    integer :: i, tries
    double precision :: theta, phi
    double precision :: v23(3)

    call seed_rng(rng_seed)

    theta = bond_ang * deg2rad

    ! Seed first 3 atoms in the xy-plane
    coords_c(1,:) = (/0.0d0, 0.0d0, 0.0d0/)
    coords_c(2,:) = (/bond_len, 0.0d0, 0.0d0/)
    v23 = (/ -bond_len*cos(theta), bond_len*sin(theta), 0.0d0 /)
    coords_c(3,:) = coords_c(2,:) + v23

    do i = 4, n_carbons
      tries = 0
      do
        tries = tries + 1
        if (tries > 5000) then
          write(*,*) "ERROR: failed to place atom ", i, " without overlap."
          stop 1
        endif

        phi = propose_phi(conf_type)
        coords_c(i,:) = place_next_atom(coords_c(i-3,:), coords_c(i-2,:), coords_c(i-1,:), phi)

        if (ok_no_overlap(coords_c, i)) exit
      enddo
    enddo
  end subroutine build_chain_c_only

  subroutine place_ch3_terminal(r_c, r_neighbor, h_xyz)
    ! Author: YOUR_NAME
    !
    ! Place 3 hydrogens around a terminal carbon (CH3), roughly tetrahedral.
    double precision, intent(in) :: r_c(3), r_neighbor(3)
    double precision, intent(out) :: h_xyz(3,3)

    double precision :: u(3), e2(3), e3(3)
    double precision :: a, b, phi(3), dir(3)
    integer :: k

    u = unit_vec(r_neighbor - r_c)   ! direction to neighbor carbon

    if (abs(u(1)) < 0.9d0) then
      e2 = unit_vec(cross(u, (/1.0d0, 0.0d0, 0.0d0/)))
    else
      e2 = unit_vec(cross(u, (/0.0d0, 1.0d0, 0.0d0/)))
    endif
    e3 = unit_vec(cross(u, e2))

    ! For a perfect tetrahedron: cos(109.47°) = -1/3
    a = -1.0d0/3.0d0
    b = sqrt(1.0d0 - a*a)

    phi(1) = 0.0d0
    phi(2) = 2.0d0*pi/3.0d0
    phi(3) = 4.0d0*pi/3.0d0

    do k = 1, 3
      dir = a*u + b*(cos(phi(k))*e2 + sin(phi(k))*e3)
      h_xyz(k,:) = r_c + bond_ch * unit_vec(dir)
    enddo
  end subroutine place_ch3_terminal

  subroutine place_ch2_internal(r_prev, r_c, r_next, h_xyz2)
    ! Author: YOUR_NAME
    !
    ! Place 2 hydrogens for an internal carbon (CH2), approximate tetrahedral.
    double precision, intent(in) :: r_prev(3), r_c(3), r_next(3)
    double precision, intent(out) :: h_xyz2(2,3)

    double precision :: u_prev(3), u_next(3), bis(3)
    double precision :: e1(3), n(3), e2(3), e3(3)
    double precision :: theta, dir1(3), dir2(3)

    u_prev = unit_vec(r_prev - r_c)
    u_next = unit_vec(r_next - r_c)

    bis = u_prev + u_next
    if (vnorm(bis) < 1.0d-12) bis = u_prev
    e1 = unit_vec(-bis)   ! points roughly away from the two C-C bonds

    n = cross(u_prev, u_next)
    if (vnorm(n) < 1.0d-12) then
      if (abs(e1(1)) < 0.9d0) then
        n = cross(e1, (/1.0d0, 0.0d0, 0.0d0/))
      else
        n = cross(e1, (/0.0d0, 1.0d0, 0.0d0/))
      endif
    endif
    e2 = unit_vec(n)
    e3 = unit_vec(cross(e1, e2))

    theta = 0.5d0 * angle_tetra_deg * deg2rad

    dir1 = cos(theta)*e1 + sin(theta)*e3
    dir2 = cos(theta)*e1 - sin(theta)*e3

    h_xyz2(1,:) = r_c + bond_ch * unit_vec(dir1)
    h_xyz2(2,:) = r_c + bond_ch * unit_vec(dir2)
  end subroutine place_ch2_internal

  subroutine add_explicit_h(n_carbons, coords_c, symbols, coords_all)
    ! Author: YOUR_NAME
    integer, intent(in) :: n_carbons
    double precision, intent(in) :: coords_c(n_carbons, 3)
    character(len=2), allocatable, intent(out) :: symbols(:)
    double precision, allocatable, intent(out) :: coords_all(:, :)

    integer :: n_h, n_atoms, i, idx
    double precision :: h3(3,3), h2(2,3)

    n_h = 2*n_carbons + 2
    n_atoms = n_carbons + n_h

    allocate(symbols(n_atoms))
    allocate(coords_all(n_atoms, 3))

    ! Carbons first
    do i = 1, n_carbons
      symbols(i) = "C"
      coords_all(i,:) = coords_c(i,:)
    enddo

    idx = n_carbons

    ! Terminal 1 (CH3)
    call place_ch3_terminal(coords_c(1,:), coords_c(2,:), h3)
    coords_all(idx+1,:) = h3(1,:); symbols(idx+1) = "H"
    coords_all(idx+2,:) = h3(2,:); symbols(idx+2) = "H"
    coords_all(idx+3,:) = h3(3,:); symbols(idx+3) = "H"
    idx = idx + 3

    ! Internal CH2
    do i = 2, n_carbons-1
      call place_ch2_internal(coords_c(i-1,:), coords_c(i,:), coords_c(i+1,:), h2)
      coords_all(idx+1,:) = h2(1,:); symbols(idx+1) = "H"
      coords_all(idx+2,:) = h2(2,:); symbols(idx+2) = "H"
      idx = idx + 2
    enddo

    ! Terminal N (CH3)
    call place_ch3_terminal(coords_c(n_carbons,:), coords_c(n_carbons-1,:), h3)
    coords_all(idx+1,:) = h3(1,:); symbols(idx+1) = "H"
    coords_all(idx+2,:) = h3(2,:); symbols(idx+2) = "H"
    coords_all(idx+3,:) = h3(3,:); symbols(idx+3) = "H"
    idx = idx + 3

    if (idx /= n_atoms) then
      write(*,*) "ERROR: H counting mismatch. idx=", idx, " expected=", n_atoms
      stop 1
    endif
  end subroutine add_explicit_h

  subroutine generate_initial_configuration(n_carbons, explicit_h, conf_type, rng_seed, symbols, coords_all)
    ! Author: YOUR_NAME
    integer, intent(in) :: n_carbons, conf_type, rng_seed
    logical, intent(in) :: explicit_h
    character(len=2), allocatable, intent(out) :: symbols(:)
    double precision, allocatable, intent(out) :: coords_all(:, :)

    double precision, allocatable :: coords_c(:, :)

    allocate(coords_c(n_carbons, 3))
    call build_chain_c_only(n_carbons, conf_type, rng_seed, coords_c)

    if (.not. explicit_h) then
      allocate(symbols(n_carbons))
      allocate(coords_all(n_carbons, 3))
      symbols = "C"
      coords_all = coords_c
    else
      call add_explicit_h(n_carbons, coords_c, symbols, coords_all)
    endif

    deallocate(coords_c)
  end subroutine generate_initial_configuration


! FOR EASIER CALCULATIONS:
! Functions for mathematical operations on vectors.
  pure double precision function vnorm2(v)
    double precision, intent(in) :: v(3)
    vnorm2 = v(1)*v(1) + v(2)*v(2) + v(3)*v(3)
  end function vnorm2

  pure double precision function vnorm(v)
    double precision, intent(in) :: v(3)
    vnorm = sqrt(vnorm2(v))
  end function vnorm

  pure function unit_vec(v) result(u)
    double precision, intent(in) :: v(3)
    double precision :: u(3)
    double precision :: r
    r = vnorm(v)
    if (r < 1.0d-14) then
      u = (/0.0d0, 0.0d0, 0.0d0/)
    else
      u = v / r
    endif
  end function unit_vec

  pure function cross(a, b) result(c)
    double precision, intent(in) :: a(3), b(3)
    double precision :: c(3)
    c(1) = a(2)*b(3) - a(3)*b(2)
    c(2) = a(3)*b(1) - a(1)*b(3)
    c(3) = a(1)*b(2) - a(2)*b(1)
  end function cross




end module initial_conf

