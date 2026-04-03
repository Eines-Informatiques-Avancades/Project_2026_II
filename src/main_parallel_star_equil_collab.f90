! main_parallel_star_equil_collab.f90
!
! Fusion of two strategies:
!
!   1. Dynamic master/worker queue (from main_parallel_star_geweke):
!      The master manages a task queue. Workers request work, execute it,
!      and report back. Production runs are unlocked as equilibrations finish.
!
!   2. Collaborative multi-worker equilibration (from main_equil_parallel):
!      For each conf_type, a GROUP of workers equilibrates together.
!      Every sync_interval steps they all report E and Rg2 to the master.
!      The master runs a Boltzmann roulette, selects the best replica, and
!      broadcasts those coordinates back to all workers in the group.
!      Convergence is detected by the master via the Geweke z-test on the
!      energy stream of the winning replica (n_consec consecutive passes).
!
! Worker states (managed by master):
!   IDLE  → worker sent TAG_REQUEST_WORK and is waiting
!   EQUIL → worker is part of an active equilibration group
!   PROD  → worker is running a production simulation
!
! Equilibration group sync protocol (point-to-point, MPI_COMM_WORLD only):
!   Every sync_interval MC steps each equil worker:
!     1. MPI_Send [E, Rg2]        → master  (TAG_SYNC_OBS)
!     2. MPI_Recv [ctrl, best_w]  ← master  (TAG_SYNC_CTRL)
!     3. MPI_Send coords          → master  (TAG_EQUIL_COORDS)  [all workers]
!     4. if ctrl=continue: MPI_Recv coords ← master (TAG_SYNC_COORDS)
!
!   Master loops over all K workers in the group for each step.
!   No collective operations (Gather/Bcast) — no temporary communicators.
!
! Authors: Arthur Murphy, Itxaso Muñoz-Aldalur, Manel Diaz Calvo
!          (point-to-point sync version)

program main_parallel_star_equil_collab
  use mpi
  use parameters
  use io
  use initial_conf
  use energy,           only: compute_total_energy_ua => compute_total_energy
  use energy_all_atoms, only: init_energy_topology, &
                               compute_total_energy_aa => compute_total_energy
  use monte_carlo
  use observables
  implicit none

  ! ── Input & system ──────────────────────────────────────────────────────────
  integer            :: n_carbons, n_steps, n_atoms, conf_type, rng_seed
  logical            :: explicit_h
  character(len=256) :: xyz_file

  character(len=2),  allocatable :: symbols(:)
  double precision,  allocatable :: coords(:,:)
  double precision,  allocatable :: best_coords(:,:)

  ! ── MC parameters ───────────────────────────────────────────────────────────
  integer, parameter :: print_interval = 10000
  integer, parameter :: sync_interval  = 10000

  double precision, parameter :: T_ini = 300.0d0
  double precision, parameter :: T_fin = 300.0d0
  double precision :: T, dT, beta, max_delta

  ! ── System state ────────────────────────────────────────────────────────────
  double precision :: E_total, E_lj, E_tors, rg2, ree2
  double precision, allocatable :: phis(:)
  logical  :: accepted_step
  integer  :: total_accepted, istep, isync

  ! ── Output ──────────────────────────────────────────────────────────────────
  integer            :: u_ener, u_obs, u_traj, u_tors, u_cpu
  character(len=256) :: comment, run_tag
  character(len=256) :: energy_file, obs_file, tors_file, cpu_file, traj_file
  character(len=256) :: equil_xyz_file
  character(len=32)  :: s_rank, s_conf, s_seed
  double precision   :: cpu_start
  double precision   :: omp_get_wtime

  ! ── MPI ─────────────────────────────────────────────────────────────────────
  integer :: ierr, rank, num_procs
  integer :: status_arr(MPI_STATUS_SIZE)

  ! ── Tags ────────────────────────────────────────────────────────────────────
  integer, parameter :: TAG_REQUEST_WORK = 1
  integer, parameter :: TAG_DO_EQUIL     = 2
  integer, parameter :: TAG_DO_PROD      = 3
  integer, parameter :: TAG_DIE          = 5
  integer, parameter :: TAG_EQUIL_DONE   = 6
  integer, parameter :: TAG_PROD_DONE    = 7
  integer, parameter :: TAG_SYNC_OBS     = 20  ! worker → master: [E, Rg2]
  integer, parameter :: TAG_SYNC_CTRL    = 21  ! master → worker: [ctrl, best_local]
  integer, parameter :: TAG_EQUIL_COORDS = 22  ! worker → master: coords
  integer, parameter :: TAG_SYNC_COORDS  = 23  ! master → worker: best coords

  ! ── Boltzmann roulette ───────────────────────────────────────────────────────
  double precision, parameter :: T_virt = 10000.0d0

  ! ── Geweke convergence ───────────────────────────────────────────────────────
  integer, parameter :: n_geweke  = 300
  integer, parameter :: fA_pct    = 10
  integer, parameter :: fB_pct    = 50
  integer, parameter :: n_consec  = 3
  integer, parameter :: eval_freq = 10
  double precision, parameter :: z_crit = 1.96d0

  ! ── Equil group size ─────────────────────────────────────────────────────────
  ! TEST:  workers_per_equil=2, n_equil_confs=1, NP=3
  ! FULL:  workers_per_equil=4, n_equil_confs=3, NP=40
  integer, parameter :: workers_per_equil = 2
  integer, parameter :: n_equil_confs     = 1
  integer :: equil_confs(n_equil_confs)

  ! ── Master variables ─────────────────────────────────────────────────────────
  integer :: prod_queue(30)
  integer :: next_equil, next_prod, completed_prods, total_available_prods
  integer, allocatable :: worker_state(:)
  integer, allocatable :: worker_equil_task(:)
  integer :: equil_group_ranks(workers_per_equil, n_equil_confs)
  logical :: equil_active(n_equil_confs)
  logical :: equil_done(n_equil_confs)
  double precision :: grp_energies(workers_per_equil)
  double precision :: grp_rg2(workers_per_equil)
  double precision :: grp_boltz(workers_per_equil)
  double precision :: obs_buf(2)
  double precision :: gbuf_E(n_geweke,  n_equil_confs)
  double precision :: gbuf_Rg(n_geweke, n_equil_confs)
  integer :: gbuf_count(n_equil_confs)
  integer :: consec_passes(n_equil_confs)
  integer :: total_sync_steps(n_equil_confs)
  double precision, allocatable :: master_coords(:,:,:)
  double precision :: E_min_grp, E_best_grp, Rg2_best_grp
  double precision :: r_pick, accum, tw
  integer :: best_local, best_global_rank
  integer :: w, p, task, k
  integer :: ctrl_snd(2)
  integer :: nA, nB, nBstart, bA, bB, bsA, bsB, ib
  double precision :: meanA_E, meanB_E, seA_E, seB_E, bm, z_E
  double precision :: meanA_Rg, meanB_Rg, seA_Rg, seB_Rg, z_Rg
  double precision :: tmp_E(n_geweke), tmp_Rg(n_geweke)
  integer :: msg, worker, tag, idx, c_type, seed_to_use
  integer :: num_idle, idle_workers(1000)
  logical :: iprobe_flag

  ! ── Worker variables ─────────────────────────────────────────────────────────
  integer :: my_task_tag, my_c_type, my_seed, my_idx, my_local_idx
  integer :: ctrl_recv(2)
  logical :: i_am_winner, equil_running

  ! ────────────────────────────────────────────────────────────────────────────
  equil_confs(1) = 1
  if (n_equil_confs >= 2) equil_confs(2) = 4
  if (n_equil_confs >= 3) equil_confs(3) = 5

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank,      ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierr)

  if (num_procs < 2) then
    if (rank == 0) write(*,*) "ERROR: Need at least 2 MPI ranks."
    call MPI_Finalize(ierr); stop
  end if
  if (rank == 0 .and. num_procs - 1 < workers_per_equil) then
    write(*,'(A,I2,A,I2,A)') &
      "ERROR: Need at least ", workers_per_equil+1, &
      " MPI ranks (1 master + ", workers_per_equil, " equil workers)."
    call MPI_Finalize(ierr); stop
  end if

  if (rank == 0) then
    call read_input_dat(n_carbons, n_steps, explicit_h, conf_type, rng_seed, xyz_file)
  end if
  call MPI_Bcast(n_carbons,  1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(n_steps,    1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(explicit_h, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(rng_seed,   1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

  call generate_initial_configuration(n_carbons, explicit_h, equil_confs(1), &
                                      rng_seed, symbols, coords)
  n_atoms = size(symbols)
  allocate(phis(n_carbons - 3))
  allocate(best_coords(size(coords,1), size(coords,2)))

  ! ============================================================================
  ! MASTER BRANCH
  ! ============================================================================
  if (rank == 0) then

    allocate(worker_state(1:num_procs-1))
    allocate(worker_equil_task(1:num_procs-1))
    allocate(master_coords(n_equil_confs, size(coords,1), size(coords,2)))

    worker_state       = 0
    worker_equil_task  = 0
    equil_active       = .false.
    equil_done         = .false.
    gbuf_E             = 0.0d0
    gbuf_Rg            = 0.0d0
    gbuf_count         = 0
    consec_passes      = 0
    total_sync_steps   = 0
    next_equil             = 1
    next_prod              = 1
    completed_prods        = 0
    total_available_prods  = 0
    num_idle               = 0

    write(*,'(/,A)')      " +----------------------------------------------------+"
    write(*,'(A,I4,A)')   " |  Total workers        : ", num_procs-1, "                             |"
    write(*,'(A,I4,A)')   " |  Workers per equil    : ", workers_per_equil, "                             |"
    write(*,'(A,I4,A)')   " |  Equil conf_types     : ", n_equil_confs, "                             |"
    write(*,'(A,F9.1,A)') " |  T_virt               : ", T_virt, " K                      |"
    write(*,'(A,I4,A)')   " |  Geweke buffer        : ", n_geweke, " samples                    |"
    write(*,'(A,I4,A)')   " |  Consecutive passes   : ", n_consec, "                             |"
    write(*,'(A)')        " |  Sync                 : point-to-point only        |"
    write(*,'(A)')        " +----------------------------------------------------+"

    do while (completed_prods < 30 .or. any(equil_active))

      ! (a) Drain pending queue messages
      do
        call MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &
                        iprobe_flag, status_arr, ierr)
        if (.not. iprobe_flag) exit

        call MPI_Recv(msg, 1, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, &
                      MPI_COMM_WORLD, status_arr, ierr)
        worker = status_arr(MPI_SOURCE)
        tag    = status_arr(MPI_TAG)

        if (tag == TAG_REQUEST_WORK) then
          num_idle = num_idle + 1
          idle_workers(num_idle) = worker
          worker_state(worker) = 0

        else if (tag == TAG_EQUIL_DONE) then
          task = msg
          call MPI_Recv(master_coords(task,:,:), size(coords), &
                        MPI_DOUBLE_PRECISION, worker, TAG_EQUIL_DONE, &
                        MPI_COMM_WORLD, status_arr, ierr)
          equil_done(task)   = .true.
          equil_active(task) = .false.
          write(*,'(A,I3,A,I2,A)') &
            " [Master] Worker ", worker, &
            " finished equil conf=", equil_confs(task), &
            ". Unlocking 10 production jobs."
          do p = 1, 10
            total_available_prods = total_available_prods + 1
            prod_queue(total_available_prods) = task
          end do

        else if (tag == TAG_PROD_DONE) then
          completed_prods = completed_prods + 1
          worker_state(worker) = 0
          num_idle = num_idle + 1
          idle_workers(num_idle) = worker
          write(*,'(A,I3,A,I2,A)') &
            " [Master] Worker ", worker, &
            " finished production. Total: ", completed_prods, "/30"
        end if
      end do

      ! (b) Dispatch work to idle workers
      do while (num_idle > 0)
        if (next_equil <= n_equil_confs) then
          task = next_equil
          if (num_idle >= workers_per_equil .and. .not. equil_active(task)) then
            do k = 1, workers_per_equil
              worker = idle_workers(num_idle - k + 1)
              equil_group_ranks(k, task) = worker
              worker_state(worker)       = 1
              worker_equil_task(worker)  = task
              c_type      = equil_confs(task)
              seed_to_use = rng_seed + task * 100000 + k * 104729
              call MPI_Send(c_type,      1, MPI_INTEGER, worker, TAG_DO_EQUIL, &
                            MPI_COMM_WORLD, ierr)
              call MPI_Send(seed_to_use, 1, MPI_INTEGER, worker, TAG_DO_EQUIL, &
                            MPI_COMM_WORLD, ierr)
              call MPI_Send(task,        1, MPI_INTEGER, worker, TAG_DO_EQUIL, &
                            MPI_COMM_WORLD, ierr)
              call MPI_Send(k,           1, MPI_INTEGER, worker, TAG_DO_EQUIL, &
                            MPI_COMM_WORLD, ierr)
              write(*,'(A,I3,A,I2,A,I2,A,I2)') &
                " [Master] EQUIL worker ", worker, &
                " conf=", c_type, " task=", task, " local=", k
            end do
            num_idle           = num_idle - workers_per_equil
            equil_active(task) = .true.
            next_equil         = next_equil + 1
          else
            exit
          end if

        else if (next_prod <= total_available_prods .and. num_idle > 0) then
          task        = prod_queue(next_prod)
          c_type      = equil_confs(task)
          seed_to_use = rng_seed + 100 * next_prod
          worker      = idle_workers(num_idle)
          num_idle    = num_idle - 1
          call MPI_Send(c_type,      1, MPI_INTEGER, worker, TAG_DO_PROD, &
                        MPI_COMM_WORLD, ierr)
          call MPI_Send(seed_to_use, 1, MPI_INTEGER, worker, TAG_DO_PROD, &
                        MPI_COMM_WORLD, ierr)
          call MPI_Send(master_coords(task,:,:), size(coords), &
                        MPI_DOUBLE_PRECISION, worker, TAG_DO_PROD, &
                        MPI_COMM_WORLD, ierr)
          worker_state(worker) = 2
          write(*,'(A,I3,A,I2,A,I6)') &
            " [Master] PROD worker ", worker, &
            " conf=", c_type, " seed=", seed_to_use
          next_prod = next_prod + 1
        else
          exit
        end if
      end do

      ! (c) Sync round for each active equil group
      do task = 1, n_equil_confs
        if (.not. equil_active(task)) cycle

        ! Step 1: Receive [E, Rg2] from each worker
        do w = 1, workers_per_equil
          call MPI_Recv(obs_buf, 2, MPI_DOUBLE_PRECISION, &
                        equil_group_ranks(w, task), TAG_SYNC_OBS, &
                        MPI_COMM_WORLD, status_arr, ierr)
          grp_energies(w) = obs_buf(1)
          grp_rg2(w)      = obs_buf(2)
        end do

        total_sync_steps(task) = total_sync_steps(task) + sync_interval

        ! Boltzmann roulette
        E_min_grp = minval(grp_energies(1:workers_per_equil))
        do w = 1, workers_per_equil
          grp_boltz(w) = exp(-(grp_energies(w) - E_min_grp) / (kb * T_virt))
        end do
        tw = sum(grp_boltz)
        call random_number(r_pick)
        best_local = workers_per_equil
        accum = 0.0d0
        do w = 1, workers_per_equil
          accum = accum + grp_boltz(w) / tw
          if (r_pick <= accum) then
            best_local = w
            exit
          end if
        end do
        E_best_grp       = grp_energies(best_local)
        Rg2_best_grp     = grp_rg2(best_local)
        best_global_rank = equil_group_ranks(best_local, task)

        write(*,'(A,I2,A,I10,A,I3,A,F10.4,A,F10.4,A,F8.4)') &
          " [Master/task=", task, "] step=", total_sync_steps(task), &
          " sel=", best_local, &
          " E_sel=", E_best_grp, &
          " E_min=", E_min_grp, &
          " Rg=", sqrt(max(Rg2_best_grp, 0.0d0))

        ! Geweke buffer update
        gbuf_count(task) = gbuf_count(task) + 1
        if (gbuf_count(task) <= n_geweke) then
          gbuf_E(gbuf_count(task),  task) = E_best_grp
          gbuf_Rg(gbuf_count(task), task) = Rg2_best_grp
        else
          gbuf_E(1:n_geweke-1,  task) = gbuf_E(2:n_geweke,  task)
          gbuf_Rg(1:n_geweke-1, task) = gbuf_Rg(2:n_geweke, task)
          gbuf_E(n_geweke,  task)     = E_best_grp
          gbuf_Rg(n_geweke, task)     = Rg2_best_grp
        end if

        ! Geweke evaluation
        ctrl_snd(1) = 0
        if (gbuf_count(task) >= n_geweke .and. &
            mod(gbuf_count(task), eval_freq) == 0) then

          nA      = max(2, n_geweke * fA_pct / 100)
          nB      = max(2, n_geweke * fB_pct / 100)
          nBstart = n_geweke - nB + 1
          tmp_E   = gbuf_E(:, task)
          tmp_Rg  = gbuf_Rg(:, task)

          meanA_E = sum(tmp_E(1:nA)) / dble(nA)
          meanB_E = sum(tmp_E(nBstart:n_geweke)) / dble(nB)
          bA = max(2, int(sqrt(dble(nA)))); bsA = nA / bA
          seA_E = 0.0d0
          do ib = 1, bA
            bm = sum(tmp_E((ib-1)*bsA+1:ib*bsA)) / dble(bsA)
            seA_E = seA_E + (bm - meanA_E)**2
          end do
          seA_E = seA_E / dble(bA*(bA-1))
          bB = max(2, int(sqrt(dble(nB)))); bsB = nB / bB
          seB_E = 0.0d0
          do ib = 1, bB
            bm = sum(tmp_E(nBstart+(ib-1)*bsB:nBstart+ib*bsB-1)) / dble(bsB)
            seB_E = seB_E + (bm - meanB_E)**2
          end do
          seB_E = seB_E / dble(bB*(bB-1))
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
          seA_Rg = seA_Rg / dble(bA*(bA-1))
          seB_Rg = 0.0d0
          do ib = 1, bB
            bm = sum(tmp_Rg(nBstart+(ib-1)*bsB:nBstart+ib*bsB-1)) / dble(bsB)
            seB_Rg = seB_Rg + (bm - meanB_Rg)**2
          end do
          seB_Rg = seB_Rg / dble(bB*(bB-1))
          z_Rg = 0.0d0
          if (seA_Rg + seB_Rg > 1.0d-12) &
            z_Rg = abs(meanA_Rg - meanB_Rg) / sqrt(seA_Rg + seB_Rg)

          write(*,'(A,I2,A,F7.4,A,F10.4,A,F10.4,A,F7.4)') &
            " [Geweke/task=", task, "] z_E=", z_E, &
            " muA=", meanA_E, " muB=", meanB_E, " z_Rg=", z_Rg

          if (z_E < z_crit) then
            consec_passes(task) = consec_passes(task) + 1
            write(*,'(A,I2,A,I2,A,I2,A)') &
              " [Geweke/task=", task, "] PASSED (", &
              consec_passes(task), "/", n_consec, ")"
            if (consec_passes(task) >= n_consec) then
              ctrl_snd(1) = 1
              write(*,'(A,I2,A,I10,A)') &
                " [Master] *** EQUIL DONE task=", task, &
                " at step ", total_sync_steps(task), " ***"
            end if
          else
            if (consec_passes(task) > 0) &
              write(*,'(A,I2,A)') " [Geweke/task=", task, "] failed — reset"
            consec_passes(task) = 0
          end if
        end if

        ! Step 2: Send [ctrl, best_local] to all workers in group
        ctrl_snd(2) = best_local
        do w = 1, workers_per_equil
          call MPI_Send(ctrl_snd, 2, MPI_INTEGER, &
                        equil_group_ranks(w, task), TAG_SYNC_CTRL, &
                        MPI_COMM_WORLD, ierr)
        end do

        ! Step 3: Receive coords from every worker (need winner's coords)
        do w = 1, workers_per_equil
          if (equil_group_ranks(w, task) == best_global_rank) then
            call MPI_Recv(best_coords, size(best_coords), MPI_DOUBLE_PRECISION, &
                          equil_group_ranks(w, task), TAG_EQUIL_COORDS, &
                          MPI_COMM_WORLD, status_arr, ierr)
          else
            call MPI_Recv(coords, size(coords), MPI_DOUBLE_PRECISION, &
                          equil_group_ranks(w, task), TAG_EQUIL_COORDS, &
                          MPI_COMM_WORLD, status_arr, ierr)
            ! discard non-winner coords
          end if
        end do

        ! Step 4: If continuing, send best coords to all workers
        if (ctrl_snd(1) == 0) then
          do w = 1, workers_per_equil
            call MPI_Send(best_coords, size(best_coords), MPI_DOUBLE_PRECISION, &
                          equil_group_ranks(w, task), TAG_SYNC_COORDS, &
                          MPI_COMM_WORLD, ierr)
          end do
        else
          ! Convergence: save geometry, mark inactive
          equil_active(task) = .false.
          write(s_conf, '(I0)') equil_confs(task)
          equil_xyz_file = '../results/equilibrated_c' // trim(s_conf) // '.xyz'
          write(comment, '(A,I0,A,F15.4)') &
            "Collab-Geweke equil step ", total_sync_steps(task), &
            " E=", E_best_grp
          call write_xyz(trim(equil_xyz_file), trim(comment), symbols, best_coords)
          master_coords(task,:,:) = best_coords
          write(*,'(A,A)') " [Master] Saved: ", trim(equil_xyz_file)
        end if

      end do  ! task loop

    end do  ! master main loop

    write(*,*) "[Master] All done. Terminating workers."
    do worker = 1, num_procs - 1
      call MPI_Send(0, 1, MPI_INTEGER, worker, TAG_DIE, MPI_COMM_WORLD, ierr)
    end do
    deallocate(worker_state, worker_equil_task, master_coords)

  ! ============================================================================
  ! WORKER BRANCH
  ! ============================================================================
  else

    do while (.true.)

      call MPI_Send(0, 1, MPI_INTEGER, 0, TAG_REQUEST_WORK, MPI_COMM_WORLD, ierr)
      call MPI_Recv(my_c_type, 1, MPI_INTEGER, 0, MPI_ANY_TAG, &
                    MPI_COMM_WORLD, status_arr, ierr)
      my_task_tag = status_arr(MPI_TAG)

      if (my_task_tag == TAG_DIE) then
        write(*,'(A,I3,A)') " [Worker ", rank, "] Shutting down."
        exit

      else if (my_task_tag == TAG_DO_EQUIL) then

        call MPI_Recv(my_seed,      1, MPI_INTEGER, 0, TAG_DO_EQUIL, &
                      MPI_COMM_WORLD, status_arr, ierr)
        call MPI_Recv(my_idx,       1, MPI_INTEGER, 0, TAG_DO_EQUIL, &
                      MPI_COMM_WORLD, status_arr, ierr)
        call MPI_Recv(my_local_idx, 1, MPI_INTEGER, 0, TAG_DO_EQUIL, &
                      MPI_COMM_WORLD, status_arr, ierr)

        call generate_initial_configuration(n_carbons, explicit_h, my_c_type, &
                                            my_seed, symbols, coords)
        if (explicit_h) then
          call init_energy_topology(n_atoms, n_carbons, coords, symbols)
          call compute_total_energy_aa(coords, n_atoms, n_carbons, E_total, E_lj, E_tors)
        else
          call compute_total_energy_ua(coords, n_carbons, E_total, E_lj, E_tors)
        end if

        write(s_rank, '(I0)') rank
        write(s_conf, '(I0)') my_c_type
        run_tag     = 'equil_c' // trim(s_conf) // '_w' // trim(s_rank)
        energy_file = '../results/energy_'      // trim(run_tag) // '.dat'
        obs_file    = '../results/observables_' // trim(run_tag) // '.dat'
        tors_file   = '../results/torsions_'    // trim(run_tag) // '.dat'
        cpu_file    = '../results/cpu_'         // trim(run_tag) // '.dat'
        traj_file   = '../results/trajectory_'  // trim(run_tag) // '.xyz'

        u_ener = 10; u_obs = 11; u_tors = 12; u_cpu = 13; u_traj = 14
        open(unit=u_ener, file=trim(energy_file), status='replace')
        write(u_ener, '(A)') '# Step E_total E_lj E_tors'
        open(unit=u_obs,  file=trim(obs_file),    status='replace')
        write(u_obs,  '(A)') '# Step Rg End_to_End'
        open(unit=u_tors, file=trim(tors_file),   status='replace')
        write(u_tors, '(A)') '# Step Torsion_Angles(rad)...'
        open(unit=u_cpu,  file=trim(cpu_file),    status='replace')
        write(u_cpu,  '(A)') '# Step CPU_Time_s'
        open(unit=u_traj, file=trim(traj_file),   status='replace')

        dT             = (T_ini - T_fin) / dble(max(n_steps, 1))
        max_delta      = 0.35d0
        total_accepted = 0
        istep          = 0
        cpu_start      = omp_get_wtime()
        i_am_winner    = .false.
        equil_running  = .true.

        write(*,'(A,I3,A,I2,A,I2,A,I0)') &
          " [Worker ", rank, "] EQUIL conf=", my_c_type, &
          " task=", my_idx, " local=", my_local_idx, " seed=", my_seed

        do while (equil_running)

          do isync = 1, sync_interval
            istep = istep + 1
            T = merge(T_ini - dT*dble(istep-1), T_fin, istep <= n_steps)
            beta = 1.0d0 / (kb * T)
            call mc_step(n_carbons, n_atoms, coords, symbols, explicit_h, &
                         beta, max_delta, E_total, E_lj, E_tors, accepted_step)
            if (accepted_step) total_accepted = total_accepted + 1

            if (mod(istep, print_interval) == 0 .or. istep == 1) then
              write(u_ener, '(I10,3F15.4)') istep, E_total, E_lj, E_tors
              rg2  = compute_rg(n_carbons, coords)
              ree2 = compute_end_to_end(n_carbons, coords)
              write(u_obs, '(I10,2F15.4)') istep, sqrt(rg2), sqrt(ree2)
              call compute_torsion_angles(n_carbons, coords, phis)
              write(u_tors, '(I10)', advance='no') istep
              write(u_tors, '(*(F10.4))') phis
              write(comment,'(A,I0,A,F15.4)') "Step ",istep," E=",E_total
              call append_xyz(u_traj, comment, symbols, coords)
              write(u_cpu,'(I10,F15.6)') istep, omp_get_wtime()-cpu_start
              write(*,'(A,I3,A,I10,A,F12.4,A,F5.1,A)') &
                " [Worker ",rank,"] step=",istep, &
                "  E=",E_total, &
                "  acc=",(dble(total_accepted)/dble(istep))*100.0d0,"%"
            end if
          end do

          ! Sync point
          rg2        = compute_rg(n_carbons, coords)
          obs_buf(1) = E_total
          obs_buf(2) = rg2

          ! Step 1: send obs to master
          call MPI_Send(obs_buf, 2, MPI_DOUBLE_PRECISION, 0, TAG_SYNC_OBS, &
                        MPI_COMM_WORLD, ierr)

          ! Step 2: receive ctrl from master
          call MPI_Recv(ctrl_recv, 2, MPI_INTEGER, 0, TAG_SYNC_CTRL, &
                        MPI_COMM_WORLD, status_arr, ierr)
          i_am_winner = (my_local_idx == ctrl_recv(2))

          ! Step 3: send coords to master (all workers send)
          call MPI_Send(coords, size(coords), MPI_DOUBLE_PRECISION, 0, &
                        TAG_EQUIL_COORDS, MPI_COMM_WORLD, ierr)

          ! Step 4: receive best coords or exit
          if (ctrl_recv(1) == 0) then
            call MPI_Recv(best_coords, size(best_coords), MPI_DOUBLE_PRECISION, &
                          0, TAG_SYNC_COORDS, MPI_COMM_WORLD, status_arr, ierr)
            coords = best_coords
            if (explicit_h) then
              call compute_total_energy_aa(coords, n_atoms, n_carbons, &
                                           E_total, E_lj, E_tors)
            else
              call compute_total_energy_ua(coords, n_carbons, E_total, E_lj, E_tors)
            end if
          else
            equil_running = .false.
          end if

        end do  ! equil loop

        close(u_ener); close(u_obs); close(u_tors); close(u_traj); close(u_cpu)

        if (i_am_winner) then
          write(*,'(A,I3,A,I2,A,I10)') &
            " [Worker ", rank, "] EQUIL DONE task=", my_idx, " step=", istep
          call MPI_Send(my_idx, 1, MPI_INTEGER, 0, TAG_EQUIL_DONE, &
                        MPI_COMM_WORLD, ierr)
          call MPI_Send(coords, size(coords), MPI_DOUBLE_PRECISION, 0, &
                        TAG_EQUIL_DONE, MPI_COMM_WORLD, ierr)
        end if

      else if (my_task_tag == TAG_DO_PROD) then

        call MPI_Recv(my_seed, 1, MPI_INTEGER, 0, TAG_DO_PROD, &
                      MPI_COMM_WORLD, status_arr, ierr)
        call MPI_Recv(coords, size(coords), MPI_DOUBLE_PRECISION, 0, TAG_DO_PROD, &
                      MPI_COMM_WORLD, status_arr, ierr)

        if (explicit_h) then
          call init_energy_topology(n_atoms, n_carbons, coords, symbols)
          call compute_total_energy_aa(coords, n_atoms, n_carbons, E_total, E_lj, E_tors)
        else
          call compute_total_energy_ua(coords, n_carbons, E_total, E_lj, E_tors)
        end if

        write(s_rank, '(I0)') rank
        write(s_conf, '(I0)') my_c_type
        write(s_seed, '(I0)') my_seed
        run_tag     = 'prod_c'//trim(s_conf)//'_sd'//trim(s_seed)//'_w'//trim(s_rank)
        energy_file = '../results/energy_'      // trim(run_tag) // '.dat'
        obs_file    = '../results/observables_' // trim(run_tag) // '.dat'
        tors_file   = '../results/torsions_'    // trim(run_tag) // '.dat'
        cpu_file    = '../results/cpu_'         // trim(run_tag) // '.dat'
        traj_file   = '../results/trajectory_'  // trim(run_tag) // '.xyz'

        u_ener = 10; u_obs = 11; u_tors = 12; u_cpu = 13; u_traj = 14
        open(unit=u_ener, file=trim(energy_file), status='replace')
        write(u_ener, '(A)') '# Step E_total E_lj E_tors'
        open(unit=u_obs,  file=trim(obs_file),    status='replace')
        write(u_obs,  '(A)') '# Step Rg End_to_End'
        open(unit=u_tors, file=trim(tors_file),   status='replace')
        write(u_tors, '(A)') '# Step Torsion_Angles(rad)...'
        open(unit=u_cpu,  file=trim(cpu_file),    status='replace')
        write(u_cpu,  '(A)') '# Step CPU_Time_s'
        open(unit=u_traj, file=trim(traj_file),   status='replace')

        max_delta = 0.2d0
        T         = T_fin
        beta      = 1.0d0 / (kb * T)
        total_accepted = 0
        cpu_start = omp_get_wtime()

        do istep = 1, 1000000
          call mc_step(n_carbons, n_atoms, coords, symbols, explicit_h, &
                       beta, max_delta, E_total, E_lj, E_tors, accepted_step)
          if (accepted_step) total_accepted = total_accepted + 1
          if (mod(istep, print_interval) == 0 .or. istep == 1) then
            write(u_ener, '(I10,3F15.4)') istep, E_total, E_lj, E_tors
            rg2  = compute_rg(n_carbons, coords)
            ree2 = compute_end_to_end(n_carbons, coords)
            write(u_obs, '(I10,2F15.4)') istep, sqrt(rg2), sqrt(ree2)
            call compute_torsion_angles(n_carbons, coords, phis)
            write(u_tors, '(I10)', advance='no') istep
            write(u_tors, '(*(F10.4))') phis
            write(comment,'(A,I0,A,F15.4)') "Step ",istep," E=",E_total
            call append_xyz(u_traj, comment, symbols, coords)
            write(u_cpu,'(I10,F15.6)') istep, omp_get_wtime()-cpu_start
          end if
        end do
        close(u_ener); close(u_obs); close(u_tors); close(u_traj); close(u_cpu)
        call MPI_Send(0, 1, MPI_INTEGER, 0, TAG_PROD_DONE, MPI_COMM_WORLD, ierr)

      end if

    end do  ! worker main loop

  end if  ! rank == 0 / else

  deallocate(coords, symbols, phis, best_coords)
  call MPI_Finalize(ierr)

end program main_parallel_star_equil_collab