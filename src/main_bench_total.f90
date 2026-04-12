! Benchmark program for total energy computation using OpenMP parallelism.
! Measures CPU time across varying system sizes and configurations.
!
! Author: Oliwier Misztal

program main_bench_total
  use mpi
  use parameters
  use io
  use initial_conf
  use energy, only: compute_total_energy_ua => compute_total_energy
  use energy_all_atoms, only: init_energy_topology, &
                              compute_total_energy_aa => compute_total_energy
  implicit none

  integer :: n_carbons, n_steps, n_atoms, conf_type, rng_seed
  logical :: explicit_h
  character(len=256) :: xyz_file

  character(len=2), allocatable :: symbols(:)
  double precision, allocatable :: coords(:, :)

  double precision :: E_total, E_lj, E_tors
  double precision :: cpu_start, cpu_elapsed

  integer :: ierr, rank, num_procs
  integer :: i, benchmark_steps

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  if (rank == 0) then
    call read_input_dat(n_carbons, n_steps, explicit_h, conf_type, rng_seed, xyz_file)
  end if

  call MPI_Bcast(n_carbons, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(n_steps,   1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(explicit_h,1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(conf_type, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(rng_seed,  1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(xyz_file, 256, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)

  call generate_initial_configuration(n_carbons, explicit_h, conf_type, rng_seed, symbols, coords)

  n_atoms = size(symbols)

  if (explicit_h) then
    call init_energy_topology(n_atoms, n_carbons, coords, symbols)
    call compute_total_energy_aa(coords, n_atoms, n_carbons, E_total, E_lj, E_tors)
  else
    call compute_total_energy_ua(coords, n_carbons, E_total, E_lj, E_tors)
  end if

  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  ! For benchmarking, we will perform a fixed number of total energy evaluations.
  benchmark_steps = 10000

  cpu_start = MPI_Wtime()

  do i = 1, benchmark_steps
    if (explicit_h) then
      call compute_total_energy_aa(coords, n_atoms, n_carbons, E_total, E_lj, E_tors)
    else
      call compute_total_energy_ua(coords, n_carbons, E_total, E_lj, E_tors)
    end if
  end do

  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  cpu_elapsed = MPI_Wtime() - cpu_start

  if (rank == 0) then
    write(*,'(A,F15.6,A)') "BENCH_TOTAL_TIME=", cpu_elapsed, "s"
  end if

  deallocate(symbols, coords)
  call MPI_Finalize(ierr)

end program main_bench_total
