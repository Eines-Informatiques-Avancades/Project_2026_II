! main_serial_full.f90
! Main (driver) program to run the Monte Carlo simulation
! Author: Arthur Murphy
! Contributors: Itxaso Muñoz-Aldalur, Manel Diaz
!
! Combined Version: 
! 1. Equilibration Phase (runs until Geweke convergence).
! 2. Production Phase (runs for n_steps after equilibration).

program main_serial
  use parameters
  use io
  use initial_conf
  use energy_all_atoms, only: init_energy_topology, &
                              compute_total_energy_aa => compute_total_energy
  use energy, only: compute_total_energy_ua => compute_total_energy
  use monte_carlo
  use observables
  implicit none

  integer :: n_carbons, n_steps, n_atoms, conf_type, rng_seed
  logical :: explicit_h
  character(len=256) :: xyz_file
  character(len=2), allocatable :: symbols(:)
  double precision, allocatable :: coords(:, :)

  ! MC parameters
  integer, parameter :: print_interval = 10000
  double precision :: beta
  double precision :: max_delta

  ! Annealing:
  double precision, parameter :: T_ini = 300.0d0   ! starting temperature (K)
  double precision, parameter :: T_fin = 300.0d0   ! final temperature (K)
  double precision :: T, dT

  ! System state
  double precision :: E_total, E_lj, E_tors
  double precision :: rg2, ree2
  double precision, allocatable :: phis(:)
  logical :: accepted_step
  integer :: total_accepted, istep
  integer :: u_ener, u_obs, u_traj, u_tors, u_cpu
  character(len=256) :: comment
  character(len=256) :: run_tag, temp_tag
  
  ! File names for both phases
  character(len=256) :: energy_file_eq, obs_file_eq, tors_file_eq, cpu_file_eq, traj_file_eq
  character(len=256) :: energy_file_pr, obs_file_pr, tors_file_pr, cpu_file_pr, traj_file_pr
  
  character(len=32)  :: s_ncarb, s_conf, s_nsteps, s_tini, s_tfin
  double precision :: cpu_start, cpu_now, cpu_elapsed

  ! ── Geweke convergence test variables ────────────────────────────────────────
  integer, parameter :: geweke_sample_interval = 10000
  integer, parameter :: n_geweke  = 300
  integer, parameter :: fA_pct    = 10
  integer, parameter :: fB_pct    = 50
  integer, parameter :: n_consec  = 3
  integer, parameter :: eval_freq = 10
  double precision, parameter :: z_crit = 1.96d0

  double precision :: gbuf_E(n_geweke)
  double precision :: gbuf_Rg(n_geweke)

  integer :: gbuf_count
  integer :: consec_passes
  logical :: equilibrated

  integer :: nA, nB, nBstart, bA, bB, bsA, bsB, ib
  double precision :: meanA_E, meanB_E, seA_E, seB_E, bm, z_E
  double precision :: tmp_E(n_geweke)
  double precision :: meanA_Rg, meanB_Rg, seA_Rg, seB_Rg, z_Rg
  double precision :: tmp_Rg(n_geweke)

  ! ─────────────────────────────────────────────────────────────────────────────

  ! 1. Initialize
  call read_input_dat(n_carbons, n_steps, explicit_h, conf_type, rng_seed, xyz_file)
  call generate_initial_configuration(n_carbons, explicit_h, conf_type, rng_seed, symbols, coords)

  T = T_ini
  dT = (T_ini - T_fin) / dble(n_steps)
  beta = 1.0d0 / (kb * T)
  max_delta = 0.35d0 ! radians (approx 20 degrees)
  n_atoms = size(symbols)

  ! Initialise Geweke state
  gbuf_E  = 0.0d0
  gbuf_Rg = 0.0d0
  gbuf_count   = 0
  consec_passes = 0
  equilibrated = .false.

  ! Naming setup
  write(s_ncarb,  '(I0)') n_carbons
  write(s_conf,   '(I0)') conf_type
  write(s_nsteps, '(I0)') n_steps
  write(s_tini,   '(F8.2)') T_ini
  write(s_tfin,   '(F8.2)') T_fin

  if (abs(T_ini - T_fin) < 1.0d-12) then
    temp_tag = trim(adjustl(s_tini))
  else
    temp_tag = trim(adjustl(s_tini)) // '_' // trim(adjustl(s_tfin))
  end if

  run_tag = trim(s_ncarb) // '_' // trim(s_conf) // '_' // trim(s_nsteps) // '_' // trim(temp_tag)
  
  ! File paths for Equilibration
  energy_file_eq = '../results/energy_equil_'      // trim(run_tag) // '.dat'
  obs_file_eq    = '../results/observables_equil_' // trim(run_tag) // '.dat'
  tors_file_eq   = '../results/torsions_equil_'    // trim(run_tag) // '.dat'
  cpu_file_eq    = '../results/cpu_equil_'         // trim(run_tag) // '.dat'
  traj_file_eq   = '../results/trajectory_equil_'  // trim(run_tag) // '.xyz'

  ! File paths for Production
  energy_file_pr = '../results/energy_prod_'      // trim(run_tag) // '.dat'
  obs_file_pr    = '../results/observables_prod_' // trim(run_tag) // '.dat'
  tors_file_pr   = '../results/torsions_prod_'    // trim(run_tag) // '.dat'
  cpu_file_pr    = '../results/cpu_prod_'         // trim(run_tag) // '.dat'
  traj_file_pr   = '../results/trajectory_prod_'  // trim(run_tag) // '.xyz'

  allocate(phis(n_carbons - 3))

  ! Calculate initial energy
  if (explicit_h) then
    call init_energy_topology(n_atoms, n_carbons, coords, symbols)
    call compute_total_energy_aa(coords, n_atoms, n_carbons, E_total, E_lj, E_tors)
  else
    call compute_total_energy_ua(coords, n_carbons, E_total, E_lj, E_tors)
  end if

  ! Save the pure initial structure
  write(comment, '(A,F15.4)') "Step 0 (Initial) E=", E_total
  call write_xyz('../src/confs/initial.xyz', trim(comment), symbols, coords)

  ! =========================================================================
  ! PHASE 1: EQUILIBRATION
  ! =========================================================================
  u_ener = 10
  u_obs  = 11
  u_tors = 12
  u_cpu  = 13
  u_traj = 14

  open(unit=u_tors, file=trim(tors_file_eq), status='replace')
  write(u_tors, '(A)') '# Step Torsion_Angles(rad)...'

  open(unit=u_cpu, file=trim(cpu_file_eq), status='replace')
  write(u_cpu, '(A)') '# Step CPU_Time_s'

  open(unit=u_traj, file=trim(traj_file_eq), status='replace')

  open(unit=u_ener, file=trim(energy_file_eq), status='replace')
  write(u_ener, '(A)') '# Step E_total E_lj E_tors'
  open(unit=u_obs,  file=trim(obs_file_eq),    status='replace')
  write(u_obs,  '(A)') '# Step Rg End_to_End'

  if (explicit_h) then
     call init_energy_topology(n_atoms, n_carbons, coords, symbols)
  end if

  write(*,'(A)') " [MC Simulation] Initialization Complete"
  write(*,'(A)') " [MC Simulation] --- PHASE 1: EQUILIBRATION ---"
  
  call cpu_time(cpu_start)
  istep = 0
  total_accepted = 0

  do while (.not. equilibrated)
    istep = istep + 1

    ! Annealing schedule
    if (istep <= n_steps) then
      T = T_ini - dT * dble(istep - 1)
    else
      T = T_fin
    end if
    if (T < T_fin) T = T_fin
    beta = 1.0d0 / (kb * T)

    call mc_step(n_carbons, n_atoms, coords, symbols, explicit_h, &
                 beta, max_delta, E_total, E_lj, E_tors, accepted_step)

    if (accepted_step) total_accepted = total_accepted + 1

    ! Output periodically
    if (mod(istep, print_interval) == 0 .or. istep == 1) then
      write(u_ener, *) istep, E_total, E_lj, E_tors
      rg2  = compute_rg(n_carbons, coords)
      ree2 = compute_end_to_end(n_carbons, coords)
      write(u_obs, '(I10,2F15.4)') istep, sqrt(rg2), sqrt(ree2)
      call compute_torsion_angles(n_carbons, coords, phis)
      write(u_tors, '(I10)', advance='no') istep
      write(u_tors, '(*(F10.4))') phis
      write(comment,'(A,I0,A,F15.4)') "Step ",istep," E=",E_total
      call append_xyz(u_traj, comment, symbols, coords)
      call cpu_time(cpu_now)
      cpu_elapsed = cpu_now - cpu_start
      write(u_cpu, '(I10,F15.6)') istep, cpu_elapsed

      write(*,'(A,I10,A,F12.4,A,F5.1,A)') &
          " [EQUIL] Step:", istep, " | Energy:", E_total, &
          " | Acc:", (dble(total_accepted)/dble(istep))*100.0d0, "%"
    end if

    ! Geweke Check
    if (mod(istep, geweke_sample_interval) == 0) then
      gbuf_count = gbuf_count + 1
      if (gbuf_count <= n_geweke) then
        gbuf_E(gbuf_count) = E_total
      else
        gbuf_E(1:n_geweke-1) = gbuf_E(2:n_geweke)
        gbuf_E(n_geweke)     = E_total
      end if

      if (gbuf_count <= n_geweke) then
        gbuf_Rg(gbuf_count) = rg2
      else
        gbuf_Rg(1:n_geweke-1) = gbuf_Rg(2:n_geweke)
        gbuf_Rg(n_geweke)     = rg2
      end if

      if (gbuf_count >= n_geweke .and. mod(gbuf_count, eval_freq) == 0) then
        nA      = max(2, n_geweke * fA_pct / 100)
        nB      = max(2, n_geweke * fB_pct / 100)
        nBstart = n_geweke - nB + 1
        tmp_E   = gbuf_E

        meanA_E = sum(tmp_E(1:nA)) / dble(nA)
        bA  = max(2, int(sqrt(dble(nA))))
        bsA = nA / bA
        seA_E = 0.0d0
        do ib = 1, bA
          bm    = sum(tmp_E((ib-1)*bsA+1 : ib*bsA)) / dble(bsA)
          seA_E = seA_E + (bm - meanA_E)**2
        end do
        seA_E = seA_E / dble(bA * (bA - 1))

        meanB_E = sum(tmp_E(nBstart:n_geweke)) / dble(nB)
        bB  = max(2, int(sqrt(dble(nB))))
        bsB = nB / bB
        seB_E = 0.0d0
        do ib = 1, bB
          bm    = sum(tmp_E(nBstart+(ib-1)*bsB : nBstart+ib*bsB-1)) / dble(bsB)
          seB_E = seB_E + (bm - meanB_E)**2
        end do
        seB_E = seB_E / dble(bB * (bB - 1))

        z_E = 0.0d0
        if (seA_E + seB_E > 1.0d-12) &
          z_E = abs(meanA_E - meanB_E) / sqrt(seA_E + seB_E)

        if (z_E < z_crit) then
          consec_passes = consec_passes + 1
          if (consec_passes >= n_consec) then
            equilibrated = .true.
            write(*,'(/,A,I10,A)') &
              " [Geweke] *** EQUILIBRIUM REACHED at step ", istep, " ***"
          end if
        else
          consec_passes = 0
        end if
        tmp_Rg   = gbuf_Rg
        meanA_Rg = sum(tmp_Rg(1:nA)) / dble(nA)
        seA_Rg   = 0.0d0
        do ib = 1, bA
          bm     = sum(tmp_Rg((ib-1)*bsA+1 : ib*bsA)) / dble(bsA)
          seA_Rg = seA_Rg + (bm - meanA_Rg)**2
        end do
        seA_Rg   = seA_Rg / dble(bA * (bA - 1))
        meanB_Rg = sum(tmp_Rg(nBstart:n_geweke)) / dble(nB)
        seB_Rg   = 0.0d0
        do ib = 1, bB
          bm     = sum(tmp_Rg(nBstart+(ib-1)*bsB : nBstart+ib*bsB-1)) / dble(bsB)
          seB_Rg = seB_Rg + (bm - meanB_Rg)**2
        end do
        seB_Rg = seB_Rg / dble(bB * (bB - 1))
        z_Rg   = 0.0d0
        if (seA_Rg + seB_Rg > 1.0d-12) &
          z_Rg = abs(meanA_Rg - meanB_Rg) / sqrt(seA_Rg + seB_Rg)

        write(*,'(A,F7.4,A,F10.4,A,F10.4,A,F7.4)') &
          " [Geweke] z_E=", z_E, &
          " muA_E=", meanA_E, " muB_E=", meanB_E, " z_Rg=", z_Rg
      end if
    end if
  end do

  ! Close equilibration files
  close(u_ener)
  close(u_obs)
  close(u_tors)
  close(u_traj)
  close(u_cpu)

  ! =========================================================================
  ! PHASE 2: PRODUCTION
  ! =========================================================================
  write(*,'(A)') " ------------------------------------------------------------"
  write(*,'(A)') " [MC Simulation] --- PHASE 2: PRODUCTION ---"
  write(*,'(A,I0,A)') " [MC Simulation] Running for ", n_steps, " steps..."

  u_ener = 20
  u_obs  = 21
  u_tors = 22
  u_cpu  = 23
  u_traj = 24

  open(unit=u_ener, file=trim(energy_file_pr), status='replace')
  write(u_ener, '(A)') '# Step E_total E_lj E_tors'
  open(unit=u_obs, file=trim(obs_file_pr), status='replace')
  write(u_obs, '(A)') '# Step Rg End_to_End'
  open(unit=u_tors, file=trim(tors_file_pr), status='replace')
  write(u_tors, '(A)') '# Step Torsion_Angles(rad)...'
  open(unit=u_cpu, file=trim(cpu_file_pr), status='replace')
  write(u_cpu, '(A)') '# Step CPU_Time_s'
  open(unit=u_traj, file=trim(traj_file_pr), status='replace')

  total_accepted = 0
  call cpu_time(cpu_start) ! Reset CPU timer for production phase

  ! Force temperature to be strictly T_fin for the whole production run
  T = T_fin
  beta = 1.0d0 / (kb * T)

  do istep = 1, n_steps
    call mc_step(n_carbons, n_atoms, coords, symbols, explicit_h, &
                 beta, max_delta, E_total, E_lj, E_tors, accepted_step)
    
    if (accepted_step) total_accepted = total_accepted + 1

    ! Output periodically
    if (mod(istep, print_interval) == 0 .or. istep == 1) then
      write(u_ener, '(I10, 3F15.4)') istep, E_total, E_lj, E_tors
      rg2  = compute_rg(n_carbons, coords)
      ree2 = compute_end_to_end(n_carbons, coords)
      write(u_obs, '(I10, 2F15.4)') istep, sqrt(rg2), sqrt(ree2)
      call compute_torsion_angles(n_carbons, coords, phis)
      write(u_tors, '(I10)', advance='no') istep
      write(u_tors, '(*(F10.4))') phis
      write(comment, '(A,I0,A,F15.4)') "Step ", istep, " E=", E_total
      call append_xyz(u_traj, comment, symbols, coords)
      call cpu_time(cpu_now)
      cpu_elapsed = cpu_now - cpu_start
      write(u_cpu, '(I10, F15.6)') istep, cpu_elapsed

      write(*,'(A,I10,A,F12.4,A,F6.2,A)') &
          " [PROD]  Step:", istep, " | Energy:", E_total, &
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
