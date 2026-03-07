! main_serial.f90
! Main (driver) program to run the Monte Carlo simulation

program main_serial
  use parameters
  use io
  use initial_conf
  use energy
  use monte_carlo
  use observables
  implicit none

  integer :: n_carbons, conf_type, rng_seed
  logical :: explicit_h
  character(len=256) :: xyz_file

  character(len=2), allocatable :: symbols(:)
  double precision, allocatable :: coords(:, :)

  ! MC parameters
  integer, parameter :: n_steps = 1000000
  integer, parameter :: print_interval = 10000
  !double precision, parameter :: T = 300.0d0
  double precision :: beta
  double precision :: max_delta
  ! Annealing:
  double precision, parameter :: T_ini = 500.0d0   ! starting temperature (K)
  double precision, parameter :: T_fin = 300.0d0      ! final temperature (K)
  double precision:: T, dT ! instantaneous temperature, temperature decrement per step (K)

  ! System state
  double precision :: E_total, E_lj, E_tors
  double precision :: rg2, ree2
  double precision, allocatable :: phis(:)
  logical :: accepted_step
  integer :: total_accepted, istep
  integer :: u_ener, u_obs, u_traj, u_tors
  character(len=256) :: comment

  ! 1. Initialize
  call read_input_dat(n_carbons, explicit_h, conf_type, rng_seed, xyz_file)
  call generate_initial_configuration(n_carbons, explicit_h, conf_type, rng_seed, symbols, coords)

  T = T_ini 
  dT = (T_ini - T_fin) / dble(n_steps)
  beta = 1.0d0 / (kb * T)
  max_delta = 0.2d0 ! radians (approx 11 degrees)
  total_accepted = 0

  allocate(phis(size(symbols) - 3))

  ! Calculate initial energy
  call compute_total_energy(coords, n_carbons, E_total, E_lj, E_tors)

  ! Save the pure initial structure to confs/initial.xyz
  write(comment, '(A,F15.4)') "Step 0 (Initial) E=", E_total
  call write_xyz('../src/confs/initial.xyz', trim(comment), symbols, coords)

  ! Open output files in ../results/
  open(newunit=u_ener, file='../results/energy.dat', status='replace')
  write(u_ener, '(A)') '# Step E_total E_lj E_tors'

  open(newunit=u_obs, file='../results/observables.dat', status='replace')
  write(u_obs, '(A)') '# Step Rg End_to_End'

  open(newunit=u_tors, file='../results/torsions.dat', status='replace')
  write(u_tors, '(A)') '# Step Torsion_Angles(rad)...'

  open(newunit=u_traj, file='../results/trajectory.xyz', status='replace')

  write(*,'(A)') " [MC Simulation] Initialization Complete"
  write(*,'(A,I0)') " [MC Simulation] Carbons: ", n_carbons
  write(*,'(A,I0)') " [MC Simulation] Total Steps: ", n_steps
  !write(*,'(A,F10.4)') " [MC Simulation] Temp (K): ", T
  write(*,'(A,F10.4,A,F10.4)') " [MC Simulation] T_ini (K): ", T_ini, "  T_fin (K): ", T_fin
  write(*,'(A,ES12.4)') " [MC Simulation] Cooling factor per step: ", dT
  write(*,'(A)') " ------------------------------------------------------------"

  ! 2. Main Monte Carlo Loop
  do istep = 1, n_steps

    ! Annealing:
    T = T_ini - dT * dble(istep - 1)
    if (T < T_fin) T = T_fin
    beta = 1.0d0 / (kb * T)

    call mc_step(n_carbons, size(symbols), coords, symbols, explicit_h, &
                 beta, max_delta, E_total, E_lj, E_tors, accepted_step)
    
    if (accepted_step) total_accepted = total_accepted + 1

    ! Output periodically
    if (mod(istep, print_interval) == 0 .or. istep == 1) then
      ! Energies
      write(u_ener, '(I10, 3F15.4)') istep, E_total, E_lj, E_tors

      ! Observables
      rg2 = compute_rg(size(symbols), coords)
      ree2 = compute_end_to_end(size(symbols), coords)
      write(u_obs, '(I10, 2F15.4)') istep, sqrt(rg2), sqrt(ree2)

      ! Torsions
      call compute_torsion_angles(size(symbols), coords, phis)
      write(u_tors, '(I10)', advance='no') istep
      write(u_tors, '(*(F10.4))') phis
      
      ! Trajectory
      write(comment, '(A,I0,A,F15.4)') "Step ", istep, " E=", E_total
      call append_xyz(u_traj, comment, symbols, coords)

      ! Terminal progress
      write(*,'(A,I10,A,F12.4,A,F6.2,A)') &
          " Step:", istep, " | Energy:", E_total, &
          " | Acc:", (dble(total_accepted)/dble(istep))*100.0d0, "%"
    end if
  end do

  write(*,'(A)') " ------------------------------------------------------------"
  write(*,'(A)') " [MC Simulation] Finished Successfully"

  close(u_ener)
  close(u_obs)
  close(u_tors)
  close(u_traj)
  deallocate(symbols, coords, phis)


end program main_serial
