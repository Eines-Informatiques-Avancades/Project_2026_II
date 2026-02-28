program main_ini
  use parameters
  use io
  use initial_conf
  implicit none

  integer :: n_carbons, conf_type, rng_seed
  logical :: explicit_h
  character(len=256) :: xyz_file
  character(len=2), allocatable :: symbols(:)
  double precision, allocatable :: coords(:, :)
  character(len=256) :: comment

  call read_input_dat(n_carbons, explicit_h, conf_type, rng_seed, xyz_file)

  call generate_initial_configuration(n_carbons, explicit_h, conf_type, rng_seed, symbols, coords)

  write(comment,'(A,I0,A,L1,A,I0)') "init_conf n_carbons=", n_carbons, &
                                   " explicit_h=", explicit_h, " conf_type=", conf_type

  call write_xyz(trim(xyz_file), trim(comment), symbols, coords)

  write(*,'(A)') "OK: wrote " // trim(xyz_file)
  write(*,'(A,I0)') "Atoms: ", size(symbols)

end program main_ini

