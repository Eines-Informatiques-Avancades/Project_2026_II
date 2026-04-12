! main_parallel_star.f90
! Dynamic Master/Worker MPI pattern for Parallel Sampling
! Author: Arthur Murphy
! Contributors: Itxaso Muñoz-Aldalur
! 

program main_parallel_star
  use mpi
  use parameters
  use io
  use initial_conf
  use energy, only: compute_total_energy_ua => compute_total_energy
  use energy_all_atoms, only: init_energy_topology, &
                               compute_total_energy_aa => compute_total_energy
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
  double precision, parameter :: T_ini = 300.0d0
  double precision, parameter :: T_fin = 300.0d0
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
  character(len=256) :: energy_file, obs_file, tors_file, cpu_file, traj_file
  character(len=32)  :: s_ncarb, s_conf, s_nsteps, s_tini, s_tfin, s_seed
  double precision :: cpu_start, cpu_now, cpu_elapsed
  !double precision :: omp_get_wtime

  ! Geweke convergence variables
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
  integer :: nA, nB, nBstart, bA, bB, bsA, bsB, ib
  double precision :: meanA_E, meanB_E, seA_E, seB_E, bm, z_E
  double precision :: meanA_Rg, meanB_Rg, seA_Rg, seB_Rg, z_Rg
  double precision :: tmp_E(n_geweke), tmp_Rg(n_geweke)

  ! MPI Tags
  integer, parameter :: TAG_REQUEST_WORK = 1
  integer, parameter :: TAG_DO_EQUIL     = 2
  integer, parameter :: TAG_DO_PROD      = 3
  integer, parameter :: TAG_WAIT         = 4
  integer, parameter :: TAG_DIE          = 5
  integer, parameter :: TAG_EQUIL_DONE   = 6
  integer, parameter :: TAG_PROD_DONE    = 7

  ! MPI variables
  integer :: ierr, rank, num_procs
  integer :: status(MPI_STATUS_SIZE)
  integer :: msg, worker, tag, c_type, seed_to_use, idx, p
  !integer :: equil_confs(3)
  
  ! Master specific variables
  !integer :: prod_queue(30)
  integer :: next_equil, next_prod, completed_prods, total_available_prods
  integer :: waiting_workers(1000)
  integer :: num_waiting
  double precision, allocatable :: master_coords(:,:,:)
  !logical :: needs_equil(3)
  character(len=256) :: check_file

  ! Setup configs
  !equil_confs = (/ 1, 4, 5 /)

  ! TEMPORARY OVERWRITING TO GET JUST 1 CONF TYPE
  integer :: equil_confs(1) = (/ 4 /)
  integer :: prod_queue(10)
  logical :: needs_equil(1)


  ! 1. MPI Initialize
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierr)

  if (num_procs < 2) then
     if (rank == 0) write(*,*) "ERROR: This code requires at least 2 cores (1 Master + 1 Worker)"
     call MPI_Finalize(ierr)
     stop
  end if

  ! 2. Initialize
  ! Read input only on rank 0
  if (rank == 0) then
    call read_input_dat(n_carbons, n_steps, explicit_h, conf_type, rng_seed, xyz_file)
  end if
  ! Broadcast input parameters to all ranks
  call MPI_Bcast(n_carbons, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(n_steps,   1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(explicit_h,1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(rng_seed,  1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(xyz_file, 256, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)

  ! Have every rank secretly generate a dummy initial state simply to get n_atoms and correctly allocate arrays
  call generate_initial_configuration(n_carbons, explicit_h, equil_confs(1), rng_seed, symbols, coords)
  n_atoms = size(symbols)
  ! Torsion angles array
  allocate(phis(n_carbons - 3))

  ! Now split logic based on Rank
  
  if (rank == 0) then
     ! ---------------------------------------------------------
     ! MASTER NODE LOGIC
     ! ---------------------------------------------------------
     write(*,*) "[Rank 0] Master initialized. Managing Task Queue for ", num_procs - 1, " workers."
     allocate(master_coords(size(equil_confs), size(coords, 1), size(coords, 2)))
     
     next_equil = 1
     next_prod = 1
     completed_prods = 0
     total_available_prods = 0
     num_waiting = 0
     needs_equil = .true. ! default, but dynamically overwritten based on confs/input.dat

     ! ==== Equilibration Bypass Check ====
     do idx = 1, size(equil_confs)
        c_type = equil_confs(idx)
        check_file = ""
        if (c_type == 1) check_file = equil_conf1_xyz_file
        if (c_type == 2) check_file = equil_conf2_xyz_file
        if (c_type == 3) check_file = equil_conf3_xyz_file
        if (c_type == 4) check_file = equil_conf4_xyz_file
        if (c_type == 5) check_file = equil_conf5_xyz_file
        
        ! If a file is provided instead of .false.
        if (len_trim(check_file) > 0) then
           write(*,*) "[Master] Bypassing Equil for Conf ", c_type, " using file!"
           call read_xyz(check_file, coords, symbols)
           master_coords(idx,:,:) = coords(:,:)
           needs_equil(idx) = .false.
           
           ! Instantly unlock 10 production jobs for this configuration
           do p = 1, 10
              total_available_prods = total_available_prods + 1
              prod_queue(total_available_prods) = idx
           end do
        end if
     end do
     ! ================================

     do while (completed_prods < size(prod_queue))
        ! Wait for message
        call MPI_Recv(msg, 1, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
        worker = status(MPI_SOURCE)
        tag = status(MPI_TAG)
        
        if (tag == TAG_REQUEST_WORK) then
           num_waiting = num_waiting + 1
           waiting_workers(num_waiting) = worker

        else if (tag == TAG_EQUIL_DONE) then
           idx = msg
           call MPI_Recv(master_coords(idx,:,:), size(coords), MPI_DOUBLE_PRECISION, worker, &
                         TAG_EQUIL_DONE, MPI_COMM_WORLD, status, ierr)
           write(*,*) "[Master] Worker ", worker, " FINISHED EQUILIBRATION for conf_type ", &
                         equil_confs(idx), "! Unlocking productions."
           ! Unlock 10 production jobs
           do p = 1, 10
              total_available_prods = total_available_prods + 1
              prod_queue(total_available_prods) = idx
           end do

        else if (tag == TAG_PROD_DONE) then
           completed_prods = completed_prods + 1
           write(*,*) "[Master] Worker ", worker, " FINISHED PRODUCTION. Total finished: ", & 
                      completed_prods, "/", size(prod_queue)
        end if
        
        ! Try pushing work to currently waiting workers
        do while (num_waiting > 0)
           worker = waiting_workers(num_waiting)

           ! Skip over pre-loaded equilibration configs
           do while (next_equil <= size(equil_confs) .and. .not. needs_equil(next_equil))
              next_equil = next_equil + 1
           end do

           if (next_equil <= size(equil_confs)) then
              ! Assign Equilibration
              c_type = equil_confs(next_equil)
              call MPI_Send(c_type, 1, MPI_INTEGER, worker, TAG_DO_EQUIL, MPI_COMM_WORLD, ierr)
              seed_to_use = rng_seed + next_equil
              call MPI_Send(seed_to_use, 1, MPI_INTEGER, worker, TAG_DO_EQUIL, MPI_COMM_WORLD, ierr)
              
              write(*,*) "[Master] Dispatching EQUILIBRATION (conf ", c_type, ") to Worker ", worker
              next_equil = next_equil + 1
              num_waiting = num_waiting - 1

           else if (next_prod <= total_available_prods) then
              ! Assign Production
              idx = prod_queue(next_prod)
              c_type = equil_confs(idx)
              call MPI_Send(c_type, 1, MPI_INTEGER, worker, TAG_DO_PROD, MPI_COMM_WORLD, ierr)
              ! Give it its replica ID (1 to 10) as part of the seed parameter
              seed_to_use = rng_seed + 100 * next_prod
              call MPI_Send(seed_to_use, 1, MPI_INTEGER, worker, TAG_DO_PROD, MPI_COMM_WORLD, ierr)
              ! Send the specific equilibrated coordinates!
              call MPI_Send(master_coords(idx,:,:), size(coords), MPI_DOUBLE_PRECISION, &
                            worker, TAG_DO_PROD, MPI_COMM_WORLD, ierr)
              
              write(*,*) "[Master] Dispatching PRODUCTION (conf ", c_type, " seed ", &
                         seed_to_use, ") to Worker ", worker
              next_prod = next_prod + 1
              num_waiting = num_waiting - 1
              
           else
              ! No equilibrations left to start, but remaining productions are still locked!
              ! Stop assigning for now (leave worker in queue until an EQUIL_DONE unblocks more jobs)
              exit 
           end if
        end do
     end do
     
     ! Everything is done, send kill signals to all workers
     write(*,*) "[Master] Simulation Suite Completed! Terminating workers."
     do worker = 1, num_procs - 1
        call MPI_Send(0, 1, MPI_INTEGER, worker, TAG_DIE, MPI_COMM_WORLD, ierr)
     end do
     
     deallocate(master_coords)

  else
     ! ---------------------------------------------------------
     ! WORKER NODE LOGIC
     ! ---------------------------------------------------------
     do while (.true.)
        ! Request work
        call MPI_Send(0, 1, MPI_INTEGER, 0, TAG_REQUEST_WORK, MPI_COMM_WORLD, ierr)
        ! Block until Master specifies what to do
        call MPI_Recv(c_type, 1, MPI_INTEGER, 0, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
        tag = status(MPI_TAG)
        
        if (tag == TAG_DIE) then
           write(*,*) "[Worker ", rank, "] Shutting down safely."
           exit
           
        else if (tag == TAG_DO_EQUIL) then
           call MPI_Recv(seed_to_use, 1, MPI_INTEGER, 0, TAG_DO_EQUIL, MPI_COMM_WORLD, status, ierr)
           
           ! Determine internal array index
           if (c_type == 1) idx = 1
           if (c_type == 4) idx = 2
           if (c_type == 5) idx = 3
           
           call generate_initial_configuration(n_carbons, explicit_h, c_type, seed_to_use, symbols, coords)
           
           ! Make the outputs have the name of the parameters used in the simulation:
           write(s_ncarb,  '(I0)') n_carbons
           write(s_conf,   '(I0)') c_type
           write(s_seed,   '(I0)') seed_to_use
           run_tag = trim(s_ncarb)//'_'//trim(s_conf)//'_equil_sd'//trim(s_seed)//'_rk'//trim(s_ncarb) ! Reuse vars to make string
           ! Fix the string cleanly:
           write(s_ncarb, '(I0)') rank
           run_tag = 'c'//trim(s_conf)//'_sd'//trim(s_seed)//'_w'//trim(s_ncarb)
           
           energy_file = '../results/equil_energy_'      // trim(run_tag) // '.dat'
           obs_file    = '../results/equil_observables_' // trim(run_tag) // '.dat'
           tors_file   = '../results/equil_torsions_'    // trim(run_tag) // '.dat'
           cpu_file    = '../results/equil_cpu_'         // trim(run_tag) // '.dat'
           traj_file   = '../results/equil_trajectory_'  // trim(run_tag) // '.xyz'

           ! Open output files in ../results/
           u_ener = 11; u_obs  = 12; u_tors = 13; u_cpu  = 14; u_traj = 15
           open(unit=u_ener, file=trim(energy_file), status='replace'); write(u_ener, '(A)') '# Step E_total E_lj E_tors'
           open(unit=u_obs, file=trim(obs_file), status='replace'); write(u_obs, '(A)') '# Step Rg End_to_End'
           open(unit=u_tors, file=trim(tors_file), status='replace'); write(u_tors, '(A)') '# Step Torsion_Angles(rad)...'
           open(unit=u_cpu, file=trim(cpu_file), status='replace'); write(u_cpu, '(A)') '# Step CPU_Time_s'
           open(unit=u_traj, file=trim(traj_file), status='replace')

           ! Calculate initial energy (depending on explicit_h setting in input.dat)
           if (explicit_h) then
             call init_energy_topology(n_atoms, n_carbons, coords, symbols)
             call compute_total_energy_aa(coords, n_atoms, n_carbons, E_total, E_lj, E_tors)
           else
             call compute_total_energy_ua(coords, n_carbons, E_total, E_lj, E_tors)
           end if

           dT = (T_ini - T_fin) / dble(n_steps)
           max_delta = 1.1d0  ! radians (approx 60 degrees)
           total_accepted = 0
           gbuf_count = 0
           consec_passes = 0
           gbuf_E = 0.0d0
           gbuf_Rg = 0.0d0

           cpu_start = MPI_Wtime()
           ! 3. Main Monte Carlo Loop
           ! Unlimited loop until equilibrium is achieved
           do istep = 1, 99999999
              if (istep <= n_steps) then
                 T = T_ini - dT * dble(istep - 1)
              else
                 T = T_fin
              end if
              beta = 1.0d0 / (kb * T)
              
              call mc_step(n_carbons, n_atoms, coords, symbols, explicit_h, &
                           beta, max_delta, E_total, E_lj, E_tors, accepted_step)
              if (accepted_step) total_accepted = total_accepted + 1
              
              if (mod(istep, print_interval) == 0 .or. istep == 1) then
                 ! Energies
                 write(u_ener, '(I10, 3F15.4)') istep, E_total, E_lj, E_tors
                 
                 ! Observables
                 rg2 = compute_rg(n_carbons, coords)
                 ree2 = compute_end_to_end(n_carbons, coords)
                 write(u_obs, '(I10, 2F15.4)') istep, sqrt(rg2), sqrt(ree2)
                 
                 ! Torsions
                 call compute_torsion_angles(n_carbons, coords, phis)
                 write(u_tors, '(I10)', advance='no') istep
                 write(u_tors, '(*(F10.4))') phis
                 
                 ! Trajectory
                 write(comment, '(A,I0,A,F15.4)') "Step ", istep, " E=", E_total
                 call append_xyz(u_traj, comment, symbols, coords)
                 
                 ! CPU time
                 cpu_now = MPI_Wtime()
                 cpu_elapsed = cpu_now - cpu_start
                 write(u_cpu, '(I10, F15.6)') istep, cpu_elapsed
              end if

              ! --- Geweke Check ---
              if (abs(T - T_fin) < 1.0d-8) then
                if (mod(istep, geweke_sample_interval) == 0) then
                  gbuf_count = gbuf_count + 1
                  if (gbuf_count <= n_geweke) then
                    gbuf_E(gbuf_count) = E_total
                    gbuf_Rg(gbuf_count) = rg2
                  else
                    gbuf_E(1:n_geweke-1) = gbuf_E(2:n_geweke)
                    gbuf_Rg(1:n_geweke-1) = gbuf_Rg(2:n_geweke)
                    gbuf_E(n_geweke) = E_total
                    gbuf_Rg(n_geweke) = rg2
                  end if
            
                  if (gbuf_count >= n_geweke .and. mod(gbuf_count, eval_freq) == 0) then
                    nA      = max(2, n_geweke * fA_pct / 100)
                    nB      = max(2, n_geweke * fB_pct / 100)
                    nBstart = n_geweke - nB + 1
                    tmp_E   = gbuf_E
                    tmp_Rg  = gbuf_Rg
            
                    meanA_E = sum(tmp_E(1:nA)) / dble(nA)
                    meanB_E = sum(tmp_E(nBstart:n_geweke)) / dble(nB)
                    bA = max(2, int(sqrt(dble(nA)))); bsA = nA / bA
                    seA_E = 0.0d0
                    do ib = 1, bA
                      bm = sum(tmp_E((ib-1)*bsA+1:ib*bsA)) / dble(bsA)
                      seA_E = seA_E + (bm - meanA_E)**2
                    end do
                    seA_E = seA_E / (dble(bA) * dble(bA - 1))
                    bB = max(2, int(sqrt(dble(nB)))); bsB = nB / bB
                    seB_E = 0.0d0
                    do ib = 1, bB
                      bm = sum(tmp_E(nBstart+(ib-1)*bsB:nBstart+ib*bsB-1)) / dble(bsB)
                      seB_E = seB_E + (bm - meanB_E)**2
                    end do
                    seB_E = seB_E / (dble(bB) * dble(bB - 1))
                    z_E = 0.0d0
                    if (seA_E + seB_E > 1.0d-12) &
                      z_E = abs(meanA_E - meanB_E) / sqrt(seA_E + seB_E)
            
                    meanA_Rg = sum(tmp_Rg(1:nA)) / dble(nA)
                    meanB_Rg = sum(tmp_Rg(nBstart:n_geweke)) / dble(nB)
                    seA_Rg = 0.0d0
                    do ib = 1, bA
                      bm = sum(tmp_Rg((ib-1)*bsA+1:ib*bsA)) / dble(bsA)
                      seA_Rg = seA_Rg + (bm - meanA_Rg)**2
                    end do
                    seA_Rg = seA_Rg / (dble(bA) * dble(bA - 1))
                    seB_Rg = 0.0d0
                    do ib = 1, bB
                      bm = sum(tmp_Rg(nBstart+(ib-1)*bsB:nBstart+ib*bsB-1)) / dble(bsB)
                      seB_Rg = seB_Rg + (bm - meanB_Rg)**2
                    end do
                    seB_Rg = seB_Rg / (dble(bB) * dble(bB - 1))
                    z_Rg = 0.0d0
                    if (seA_Rg + seB_Rg > 1.0d-12) &
                      z_Rg = abs(meanA_Rg - meanB_Rg) / sqrt(seA_Rg + seB_Rg)
            
                    write(*,'(A,I0,A,F7.4,A,F10.4,A,F10.4,A,F7.4)') &
                      " [Worker ", rank, "] z_E=", z_E, " muA=", meanA_E, " muB=", meanB_E, " z_Rg=", z_Rg
            
                    if (z_E < z_crit) then
                      consec_passes = consec_passes + 1
                      write(*,'(A,I0,A,I2,A,I2,A)') " [Worker ", rank, "] PASSED (", consec_passes, "/", n_consec, ")"
                      if (consec_passes >= n_consec) then
                        write(*,'(A,I0,A,I10,A)') " *** EQUILIBRIUM DETECTED (Worker ", rank, ") at step ", istep, " ***"
                        exit
                      end if
                    else
                      if (consec_passes > 0) write(*,'(A,I0,A)') " [Worker ", rank, "] failed - reset"
                      consec_passes = 0
                    end if
                  end if
                end if
              end if
              ! --- End Geweke Check ---
           end do
           close(u_ener); close(u_obs); close(u_tors); close(u_traj); close(u_cpu)

           cpu_now = MPI_Wtime()
           write(*,'(A,I0,A,I0,A,F10.2,A)') "[Worker ", rank, &
                "] Finished EQUILIBRATION for conf ", c_type, &
                " in ", (cpu_now - cpu_start), " seconds."

           ! Send back equilibrated coords!
           call MPI_Send(idx, 1, MPI_INTEGER, 0, TAG_EQUIL_DONE, MPI_COMM_WORLD, ierr)
           call MPI_Send(coords, size(coords), MPI_DOUBLE_PRECISION, 0, TAG_EQUIL_DONE, MPI_COMM_WORLD, ierr)

        else if (tag == TAG_DO_PROD) then
           call MPI_Recv(seed_to_use, 1, MPI_INTEGER, 0, TAG_DO_PROD, MPI_COMM_WORLD, status, ierr)
           ! Dummy generate to seed internal RNG correctly, then instantly overwrite coordinates
           call generate_initial_configuration(n_carbons, explicit_h, c_type, seed_to_use, symbols, coords)
           call MPI_Recv(coords, size(coords), MPI_DOUBLE_PRECISION, 0, TAG_DO_PROD, MPI_COMM_WORLD, status, ierr)
           
           ! Calculate initial energy (depending on explicit_h setting in input.dat)
           if (explicit_h) then
             call init_energy_topology(n_atoms, n_carbons, coords, symbols)
             call compute_total_energy_aa(coords, n_atoms, n_carbons, E_total, E_lj, E_tors)
           else
             call compute_total_energy_ua(coords, n_carbons, E_total, E_lj, E_tors)
           end if

           ! Output config
           write(s_conf, '(I0)') c_type
           write(s_seed, '(I0)') seed_to_use
           write(s_ncarb, '(I0)') rank
           run_tag = 'c'//trim(s_conf)//'_sd'//trim(s_seed)//'_w'//trim(s_ncarb)

           energy_file = '../results/prod_energy_'      // trim(run_tag) // '.dat'
           obs_file    = '../results/prod_observables_' // trim(run_tag) // '.dat'
           tors_file   = '../results/prod_torsions_'    // trim(run_tag) // '.dat'
           cpu_file    = '../results/prod_cpu_'         // trim(run_tag) // '.dat'
           traj_file   = '../results/prod_trajectory_'  // trim(run_tag) // '.xyz'

           u_ener = 11; u_obs  = 12; u_tors = 13; u_cpu  = 14; u_traj = 15
           open(unit=u_ener, file=trim(energy_file), status='replace'); write(u_ener, '(A)') '# Step E_total E_lj E_tors'
           open(unit=u_obs, file=trim(obs_file), status='replace'); write(u_obs, '(A)') '# Step Rg End_to_End'
           open(unit=u_tors, file=trim(tors_file), status='replace'); write(u_tors, '(A)') '# Step Torsion_Angles(rad)...'
           open(unit=u_cpu, file=trim(cpu_file), status='replace'); write(u_cpu, '(A)') '# Step CPU_Time_s'
           open(unit=u_traj, file=trim(traj_file), status='replace')

           max_delta = 0.2d0
           T = T_fin
           beta = 1.0d0 / (kb * T)
           ! reset acceptance rate
           total_accepted = 0

           cpu_start = MPI_Wtime()
           ! 3. Main Monte Carlo Loop
           ! Production loop: EXACTLY 1,000,000 steps without checking equilibrium
           do istep = 1, 1000000
              call mc_step(n_carbons, n_atoms, coords, symbols, explicit_h, &
                           beta, max_delta, E_total, E_lj, E_tors, accepted_step)
              if (accepted_step) total_accepted = total_accepted + 1

              if (mod(istep, print_interval) == 0 .or. istep == 1) then
                 write(u_ener, '(I10, 3F15.4)') istep, E_total, E_lj, E_tors
                 rg2 = compute_rg(n_carbons, coords)
                 ree2 = compute_end_to_end(n_carbons, coords)
                 write(u_obs, '(I10, 2F15.4)') istep, sqrt(rg2), sqrt(ree2)
                 call compute_torsion_angles(n_carbons, coords, phis)
                 write(u_tors, '(I10)', advance='no') istep
                 write(u_tors, '(*(F10.4))') phis
                 write(comment, '(A,I0,A,F15.4)') "Step ", istep, " E=", E_total
                 call append_xyz(u_traj, comment, symbols, coords)
                 cpu_now = MPI_Wtime()
                 cpu_elapsed = cpu_now - cpu_start
                 write(u_cpu, '(I10, F15.6)') istep, cpu_elapsed
              end if
           end do
           close(u_ener); close(u_obs); close(u_tors); close(u_traj); close(u_cpu)
           
           cpu_now = MPI_Wtime()
           write(*,'(A,I0,A,I0,A,F10.2,A)') "[Worker ", rank, &
                "] Finished PRODUCTION for conf ", c_type, &
                " in ", (cpu_now - cpu_start), " seconds."

           ! Tell Master we're done
           call MPI_Send(0, 1, MPI_INTEGER, 0, TAG_PROD_DONE, MPI_COMM_WORLD, ierr)
        end if
     end do
     
  end if

  deallocate(coords, symbols, phis)
  call MPI_Finalize(ierr)

end program main_parallel_star
