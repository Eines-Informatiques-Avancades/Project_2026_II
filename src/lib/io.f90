! Module input/output for reading/writing
module io
  use parameters
  implicit none

contains

  subroutine read_input_nml(filename, n_carbons, explicit_h, conf_type, rng_seed, xyz_file)
    character(len=*), intent(in)  :: filename
    integer,          intent(out) :: n_carbons
    logical,          intent(out) :: explicit_h
    integer,          intent(out) :: conf_type
    integer,          intent(out) :: rng_seed
    character(len=256), intent(out) :: xyz_file

    integer :: u, ios

    ! NAMELIST must be before any executable statements.
    namelist /sim/ n_carbons, explicit_h, conf_type, rng_seed, xyz_file

    ! Defaults
    n_carbons  = 20
    explicit_h = .false.
    conf_type  = 1
    rng_seed   = 12345
    xyz_file   = "../confs/init.xyz"

    open(newunit=u, file=filename, status="old", action="read", iostat=ios)
    if (ios /= 0) then
      write(*,*) "ERROR: cannot open namelist file: ", trim(filename)
      stop 1
    end if

    read(u, nml=sim, iostat=ios)
    if (ios /= 0) then
      write(*,*) "ERROR: failed reading namelist group &sim from: ", trim(filename)
      stop 1
    end if
    close(u)
  end subroutine read_input_nml


  subroutine write_xyz(filename, comment, symbols, coords)
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: comment
    character(len=*), intent(in) :: symbols(:)
    double precision, intent(in) :: coords(:, :)   ! (n_atoms, 3)

    integer :: u, i, n

    n = size(symbols)
    if (size(coords,1) /= n .or. size(coords,2) /= 3) then
      write(*,*) "ERROR: write_xyz got inconsistent shapes"
      stop 1
    end if

    open(newunit=u, file=filename, status="replace", action="write")
    write(u,'(I0)') n
    write(u,'(A)') trim(comment)
    do i = 1, n
      write(u,'(A2,1X,F15.8,1X,F15.8,1X,F15.8)') trim(symbols(i)), coords(i,1), coords(i,2), coords(i,3)
    end do
    close(u)
  end subroutine write_xyz

end module io


