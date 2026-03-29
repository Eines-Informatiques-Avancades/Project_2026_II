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

  ! Equilibrium variables
  integer, parameter :: block_size = 100
  integer, parameter :: sample_interval = 1000
  integer :: e_count1 = 0, e_count2 = 0
  double precision :: sum_e1 = 0.0d0, sum_sq_e1 = 0.0d0, mu1, var1
  double precision :: sum_e2 = 0.0d0, sum_sq_e2 = 0.0d0, mu2, var2
  double precision :: t_stat
  double precision, parameter :: t_crit = 1.96d0
  logical :: record_block2 = .false.

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
  integer :: equil_confs(3)
  
  ! Master specific variables
  integer :: prod_queue(30)
  integer :: next_equil, next_prod, completed_prods, total_available_prods
  integer :: waiting_workers(1000)
  integer :: num_waiting
  double precision, allocatable :: master_coords(:,:,:)

  ! Setup configs
  equil_confs = (/ 1, 4, 5 /)

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
     allocate(master_coords(3, size(coords, 1), size(coords, 2)))
     
     next_equil = 1
     next_prod = 1
     completed_prods = 0
     total_available_prods = 0
     num_waiting = 0
     
     do while (completed_prods < 30)
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
                      completed_prods, "/30"
        end if
        
        ! Try pushing work to currently waiting workers
        do while (num_waiting > 0)
           worker = waiting_workers(num_waiting)
           if (next_equil <= 3) then
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
           run_tag = 'equil_c'//trim(s_conf)//'_sd'//trim(s_seed)//'_w'//trim(s_ncarb)
           
           energy_file = '../results/energy_'      // trim(run_tag) // '.dat'
           obs_file    = '../results/observables_' // trim(run_tag) // '.dat'
           tors_file   = '../results/torsions_'    // trim(run_tag) // '.dat'
           cpu_file    = '../results/cpu_'         // trim(run_tag) // '.dat'
           traj_file   = '../results/trajectory_'  // trim(run_tag) // '.xyz'

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
           sum_e1 = 0.0d0; sum_sq_e1 = 0.0d0; e_count1 = 0
           sum_e2 = 0.0d0; sum_sq_e2 = 0.0d0; e_count2 = 0
           record_block2 = .false.

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

              ! Check for equilibrium using Welch's t-test (Welch, B.L., Biometrika, 34(1/2), 1947)
              if (abs(T - T_fin) < 1.0d-8) then
                if (mod(istep, sample_interval) == 0) then
                  if (.not. record_block2) then
                    sum_e1    = sum_e1 + E_total
                    sum_sq_e1 = sum_sq_e1 + (E_total * E_total)
                    e_count1  = e_count1 + 1
                    if (e_count1 == block_size) record_block2 = .true.
                  else
                    sum_e2    = sum_e2 + E_total
                    sum_sq_e2 = sum_sq_e2 + (E_total * E_total)
                    e_count2  = e_count2 + 1
                    if (e_count2 == block_size) then
                       mu1  = sum_e1 / dble(block_size)
                       var1 = (sum_sq_e1 - dble(block_size)*mu1*mu1) / dble(block_size - 1)
                       mu2  = sum_e2 / dble(block_size)
                       var2 = (sum_sq_e2 - dble(block_size)*mu2*mu2) / dble(block_size - 1)
                       if (var1 + var2 > 1.0d-12) then
                          t_stat = abs(mu1 - mu2) / sqrt((var1 + var2) / dble(block_size))
                       else
                          t_stat = 0.0d0
                       end if

                       if (t_stat < t_crit) then
                          exit
                       end if

                       sum_e1 = sum_e2; sum_sq_e1 = sum_sq_e2; e_count1 = block_size
                       sum_e2 = 0.0d0; sum_sq_e2 = 0.0d0; e_count2 = 0
                    end if
                  end if
                end if 
              end if
           end do
           close(u_ener); close(u_obs); close(u_tors); close(u_traj); close(u_cpu)

           cpu_now = MPI_Wtime()
           write(*,'(A,I0,A,I0,A,F10.2,A)') "[Worker ", rank, "] Finished EQUILIBRATION for conf ", c_type, " in ", (cpu_now - cpu_start), " seconds."

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
           run_tag = 'prod_c'//trim(s_conf)//'_sd'//trim(s_seed)//'_w'//trim(s_ncarb)

           energy_file = '../results/energy_'      // trim(run_tag) // '.dat'
           obs_file    = '../results/observables_' // trim(run_tag) // '.dat'
           tors_file   = '../results/torsions_'    // trim(run_tag) // '.dat'
           cpu_file    = '../results/cpu_'         // trim(run_tag) // '.dat'
           traj_file   = '../results/trajectory_'  // trim(run_tag) // '.xyz'

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
           write(*,'(A,I0,A,I0,A,F10.2,A)') "[Worker ", rank, "] Finished PRODUCTION for conf ", c_type, " in ", (cpu_now - cpu_start), " seconds."

           ! Tell Master we're done
           call MPI_Send(0, 1, MPI_INTEGER, 0, TAG_PROD_DONE, MPI_COMM_WORLD, ierr)
        end if
     end do
     
  end if

  deallocate(coords, symbols, phis)
  call MPI_Finalize(ierr)

end program main_parallel_star
