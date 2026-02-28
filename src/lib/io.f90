! Module input/output for reading/writing files
module io
  use parameters
  implicit none

  character(len=*), parameter:: input_path  = "../confs/input.dat"
  character(len=*), parameter:: output_path = "../confs/initial.xyz"

contains

  subroutine read_input_dat(n_carbons, explicit_h, conf_type, rng_seed, xyz_file)
    character(len=256), intent(out):: xyz_file
    integer, intent(out):: n_carbons
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
    explicit_h = .false.
    conf_type  = 1
    rng_seed   = 12345
    xyz_file   = output_path   

    open(newunit=u, file=input_path, status="old", action="read", iostat=ios)
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
    character(len=*), intent(in) :: comment
    character(len=*), intent(in) :: symbols(:)
    double precision, intent(in) :: coords(:, :)   ! (n_atoms, 3)

    integer :: u, i, n, ios

    n = size(symbols)
    if (size(coords,1) /= n .or. size(coords,2) /= 3) then
      write(*,*) "ERROR: write_xyz got inconsistent shapes"
      stop 1
    endif

    open(newunit=u, file=filename, status="replace", action="write", iostat=ios)
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

end module io
