! main_serial.f90
! Main (driver) program to run the Monte Carlo simulation
! authors: ai-murphy, itxasoma

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
  double precision, parameter :: T_ini = 300.0d0   ! starting temperature (K)
  double precision, parameter :: T_fin = 300.0d0      ! final temperature (K)
  double precision :: T, dT ! instantaneous temperature, temperature decrement per step (K)

  ! System state
  double precision :: E_total, E_lj, E_tors
  double precision :: rg2, ree2
  double precision, allocatable :: phis(:)
  logical :: accepted_step
  integer :: total_accepted, istep
  integer :: u_ener, u_obs, u_traj, u_tors, u_cpu
  character(len=256) :: comment
  character(len=256) :: run_tag, temp_tag
  character(len=256) :: energy_file, obs_file, tors_file, cpu_file
  character(len=32)  :: s_ncarb, s_conf, s_nsteps, s_tini, s_tfin
  double precision :: cpu_start, cpu_now, cpu_elapsed


  ! 1. Initialize
  call read_input_dat(n_carbons, explicit_h, conf_type, rng_seed, xyz_file)
  call generate_initial_configuration(n_carbons, explicit_h, conf_type, rng_seed, symbols, coords)

  T = T_ini 
  dT = (T_ini - T_fin) / dble(n_steps)
  beta = 1.0d0 / (kb * T)
  max_delta = 0.2d0 ! radians (approx 11 degrees)
  total_accepted = 0

  ! Make the outputs have the name of the parameters used in the simulation:
  write(s_ncarb,  '(I0)') n_carbons
  write(s_conf,   '(I0)') conf_type
  write(s_nsteps, '(I0)') n_steps
  write(s_tini,   '(F8.2)') T_ini
  write(s_tfin,   '(F8.2)') T_fin

  if (abs(T_ini - T_fin) < 1.0d-12) then
    temp_tag = trim(adjustl(s_tini))    ! If constant T, just use T_ini
  else
    temp_tag = trim(adjustl(s_tini)) // '_' // trim(adjustl(s_tfin))
  end if

  run_tag = trim(s_ncarb) // '_' // trim(s_conf) // '_' // trim(s_nsteps) // '_' // trim(temp_tag)
  energy_file = '../results/energy_'      // trim(run_tag) // '.dat'
  obs_file    = '../results/observables_' // trim(run_tag) // '.dat'
  tors_file   = '../results/torsions_'    // trim(run_tag) // '.dat'
  cpu_file    = '../results/cpu_'         // trim(run_tag) // '.dat'


  !allocate(phis(size(symbols) - 3))
  allocate(phis(n_carbons - 3))


  ! Calculate initial energy
  call compute_total_energy(coords, n_carbons, E_total, E_lj, E_tors)

  ! Save the pure initial structure to confs/initial.xyz
  write(comment, '(A,F15.4)') "Step 0 (Initial) E=", E_total
  call write_xyz('../src/confs/initial.xyz', trim(comment), symbols, coords)

  ! Open output files in ../results/
  open(newunit=u_ener, file=trim(energy_file), status='replace')
  write(u_ener, '(A)') '# Step E_total E_lj E_tors'

  open(newunit=u_obs, file=trim(obs_file), status='replace')
  write(u_obs, '(A)') '# Step Rg End_to_End'

  open(newunit=u_tors, file=trim(tors_file), status='replace')
  write(u_tors, '(A)') '# Step Torsion_Angles(rad)...'

  open(newunit=u_cpu, file=trim(cpu_file), status='replace')
  write(u_cpu, '(A)') '# Step CPU_Time_s'


  write(*,'(A)') " [MC Simulation] Initialization Complete"
  write(*,'(A,I0)') " [MC Simulation] Carbons: ", n_carbons
  write(*,'(A,I0)') " [MC Simulation] Total Steps: ", n_steps
  !write(*,'(A,F10.4)') " [MC Simulation] Temp (K): ", T
  write(*,'(A,F10.4,A,F10.4)') " [MC Simulation] T_ini (K): ", T_ini, "  T_fin (K): ", T_fin
  write(*,'(A,ES12.4)') " [MC Simulation] Cooling factor per step: ", dT
  write(*,'(A)') " ------------------------------------------------------------"

  ! 2. Main Monte Carlo Loop
  call cpu_time(cpu_start)
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

      ! CPU time
      call cpu_time(cpu_now)
      cpu_elapsed = cpu_now - cpu_start
      write(u_cpu, '(I10, F15.6)') istep, cpu_elapsed

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
  close(u_cpu)

  deallocate(symbols, coords, phis)


end program main_serial
