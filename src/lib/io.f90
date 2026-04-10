! Module input/output for reading/writing files
! Author: Itxaso Muñoz-Aldalur
! Contributors: Arthur Murphy

module io
  use parameters
  implicit none

  character(len=*), parameter:: input_path  = "confs/input.dat"
  character(len=*), parameter:: output_path = "confs/initial.xyz"

contains

  subroutine read_input_dat(n_carbons, n_steps, explicit_h, conf_type, rng_seed, xyz_file)
    character(len=256), intent(out):: xyz_file
    integer, intent(out):: n_carbons
    integer, intent(out):: n_steps
    logical, intent(out):: explicit_h
    integer, intent(out):: conf_type
    integer, intent(out):: rng_seed
    integer:: u, ios, p, c
    character(len=512):: line
    character(len=128):: key
    character(len=384):: val
    character(len=256):: tmp

    ! Defaults
    n_carbons  = 20
    n_steps    = 1000000
    explicit_h = .false.
    conf_type  = 1
    rng_seed   = 12345
    xyz_file   = output_path   

    u=10
    open(unit=u, file=input_path, status="old", action="read", iostat=ios)
    if (ios /= 0) then
      write(*,*) "ERROR: cannot open input file: ", trim(input_path)
      stop 1
    endif

    do
      read(u,'(A)', iostat=ios) line
      if (ios /= 0) exit

      ! Remove comments starting with !
      c = index(line, "!")
      if (c > 0) line = line(:c-1)

      if (len_trim(line) == 0) cycle

      ! Split "key = value"
      p = index(line, "=")
      if (p <= 0) cycle   ! ignore malformed lines silently 

      key = adjustl(trim(line(:p-1)))
      val = adjustl(trim(line(p+1:)))

      call to_lower_inplace(key)

      select case (trim(key))
      case("n_carbons")
        read(val, *, iostat=ios) n_carbons
        if (ios /= 0) call bad_value("n_carbons", val)

      case("n_steps")
        read(val, *, iostat=ios) n_steps
        if (ios /= 0) call bad_value("n_steps", val)

      case("explicit_h")
        read(val, *, iostat=ios) explicit_h
        if (ios /= 0) call bad_value("explicit_h", val)

      case("conf_type")
        read(val, *, iostat=ios) conf_type
        if (ios /= 0) call bad_value("conf_type", val)

      case("rng_seed")
        read(val, *, iostat=ios) rng_seed
        if (ios /= 0) call bad_value("rng_seed", val)

      case("xyz_file")
        call parse_string(val, tmp)
        xyz_file = output_path
      
      ! Equilibrated configurations
      case("equil_conf1_xyz")
        call parse_string(val, tmp)
        if (trim(tmp) /= ".false." .and. trim(tmp) /= "false" .and. trim(tmp)/="") equil_conf1_xyz_file = tmp
      case("equil_conf2_xyz")
        call parse_string(val, tmp)
        if (trim(tmp) /= ".false." .and. trim(tmp) /= "false" .and. trim(tmp)/="") equil_conf2_xyz_file = tmp
      case("equil_conf3_xyz")
        call parse_string(val, tmp)
        if (trim(tmp) /= ".false." .and. trim(tmp) /= "false" .and. trim(tmp)/="") equil_conf3_xyz_file = tmp
      case("equil_conf4_xyz")
        call parse_string(val, tmp)
        if (trim(tmp) /= ".false." .and. trim(tmp) /= "false" .and. trim(tmp)/="") equil_conf4_xyz_file = tmp
      case("equil_conf5_xyz")
        call parse_string(val, tmp)
        if (trim(tmp) /= ".false." .and. trim(tmp) /= "false" .and. trim(tmp)/="") equil_conf5_xyz_file = tmp

      case default
        ! write(*,*) "Warning: unknown key: ", trim(key)
      end select
    enddo

    close(u)

  contains

    subroutine to_lower_inplace(s)
      character(len=*), intent(inout):: s
      integer :: i, ia
      do i = 1, len(s)
        ia = iachar(s(i:i))
        if (ia >= iachar('A') .and. ia <= iachar('Z')) then
          s(i:i) = achar(ia + 32)
        endif
      enddo
    end subroutine to_lower_inplace

    subroutine parse_string(s, out)
      character(len=*), intent(in):: s
      character(len=*), intent(out):: out
      integer :: n

      out = adjustl(trim(s))
      n = len_trim(out)
      if (n >= 2) then
        if ( (out(1:1) == '"' .and. out(n:n) == '"') .or. &
             (out(1:1) == "'" .and. out(n:n) == "'") ) then
          out = out(2:n-1)
        endif
      endif
    end subroutine parse_string

    subroutine bad_value(k, v)
      character(len=*), intent(in):: k, v
      write(*,*) "ERROR: bad value for ", trim(k), " = ", trim(v)
      stop 1
    end subroutine bad_value

  end subroutine read_input_dat


  subroutine write_xyz(filename, comment, symbols, coords)
    character(len=*), intent(in) :: filename
    character(len=*), intent(in):: comment
    character(len=*), intent(in):: symbols(:)
    double precision, intent(in):: coords(:, :)   ! (n_atoms, 3)
    integer:: u, i, n, ios

    n = size(symbols)
    if (size(coords,1) /= n .or. size(coords,2) /= 3) then
      write(*,*) "ERROR: write_xyz got inconsistent shapes"
      stop 1
    endif

    u=10
    open(unit=u, file=filename, status="replace", action="write", iostat=ios)
    if (ios /= 0) then
      write(*,*) "ERROR: cannot create xyz file: ", trim(filename)
      stop 1
    endif

    write(u,'(I0)') n
    write(u,'(A)') trim(comment)
    do i = 1, n
      write(u,'(A2,1X,F15.8,1X,F15.8,1X,F15.8)') trim(symbols(i)), coords(i,1), coords(i,2), coords(i,3)
    enddo
    close(u)
  end subroutine write_xyz


  subroutine append_xyz(u, comment, symbols, coords)
    integer, intent(in) :: u
    character(len=*), intent(in):: comment
    character(len=*), intent(in):: symbols(:)
    double precision, intent(in):: coords(:, :)

    ! Just write one more XYZ frame to an already-open unit.
    call write_xyz_unit(u, comment, symbols, coords)
  end subroutine append_xyz

  subroutine write_xyz_unit(u, comment, symbols, coords)
    integer, intent(in) :: u
    character(len=*), intent(in):: comment
    character(len=*), intent(in):: symbols(:)
    double precision, intent(in):: coords(:, :)   ! (n_atoms, 3)
    integer:: i, n

    n = size(symbols)
    if (size(coords,1) /= n .or. size(coords,2) /= 3) then
      write(*,*) "ERROR: write_xyz_unit got inconsistent shapes"
      stop 1
    endif

    write(u,'(I0)') n
    write(u,'(A)') trim(comment)
    do i = 1, n
      write(u,'(A2,1X,F15.8,1X,F15.8,1X,F15.8)') trim(symbols(i)), &
           coords(i,1), coords(i,2), coords(i,3)
    enddo
  end subroutine write_xyz_unit

  subroutine read_xyz(filename, coords, symbols)
    character(len=*), intent(in) :: filename
    double precision, allocatable, intent(inout) :: coords(:, :)
    character(len=2), allocatable, intent(inout) :: symbols(:)
    integer :: u, ios, n_atoms, i
    character(len=512) :: comment
    
    u = 88
    open(unit=u, file=trim(filename), status="old", action="read", iostat=ios)
    if (ios /= 0) then
      write(*,*) "ERROR: cannot open XYZ file: ", trim(filename)
      stop 1
    endif
    
    ! Read number of atoms
    read(u, *, iostat=ios) n_atoms
    if (ios /= 0) then
      write(*,*) "ERROR: failed reading atom count from ", trim(filename)
      stop 1
    endif
    
    ! Read the comment line
    read(u, '(A)') comment
    
    ! Reallocate memory precisely to the file's atom count
    if (allocated(coords)) deallocate(coords)
    if (allocated(symbols)) deallocate(symbols)
    allocate(coords(n_atoms, 3))
    allocate(symbols(n_atoms))
    
    ! Parse coordinates
    do i = 1, n_atoms
      read(u, *, iostat=ios) symbols(i), coords(i, 1), coords(i, 2), coords(i, 3)
      if (ios /= 0) then
         write(*,*) "ERROR: failed reading atom ", i, " from ", trim(filename)
         stop 1
      endif
    enddo
    
    close(u)
    write(*,*) "Successfully loaded geometry from: ", trim(filename)
  end subroutine read_xyz


end module io
