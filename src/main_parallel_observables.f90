! main_parallel_observables.f90
! Author: Itxaso Muñoz-Aldalur
!
! MPI post-processing of trajectory_*.xyz files, from main_parallel_star_equil_collab.f90.
! Motivation: the trajectories are already generated, so we can do a simple parallelization by 
! processing different files on different ranks. 
! - The bash creates a manifest with the files in:
!    conf1/equil, conf1/prod,
!    conf4/equil, conf4/prod,
!    conf5/equil, conf5/prod
!  - Rank 0 reads the manifest and broadcasts it to all ranks.
!  - Each rank processes a round-robin subset of the files, accumulating sums for observables.
!  - Mid-run checkpoint: MPI_Allreduce synchronises partial sums across all ranks so every
!    rank can compute and print the global partial mean as a convergence diagnostic.
!  - After processing, MPI_Reduce to get global sums and counts.

program main_parallel_observables
  use mpi
  use observables
  implicit none

  integer, parameter:: max_path = 512
  integer, parameter:: n_conf   = 3
  integer, parameter:: n_phase  = 2
  integer, parameter:: conf_values(n_conf) = (/1, 4, 5/)
  character(len=*), parameter:: phase_names(n_phase) = (/ 'equil', 'prod ' /)
  character(len=*), parameter:: default_base_dir = '../results/results_main_star_equil_collab'
  character(len=*), parameter:: default_out_dir  = '../results/parallel_observables'
  character(len=*), parameter:: default_manifest = '../results/parallel_observables/filelist_explicit.txt'

  integer:: ierr, rank, num_procs
  integer:: nfiles, ifile
  character(len=max_path), allocatable:: files(:)
  character(len=max_path):: base_dir
  character(len=max_path):: out_dir_rt
  character(len=max_path):: metrics_file
  character(len=max_path):: manifest_file
  logical:: have_metrics_file

  integer:: local_file_count(n_conf, n_phase),  global_file_count(n_conf, n_phase)
  integer:: local_frame_count(n_conf, n_phase), global_frame_count(n_conf, n_phase)
  integer:: local_nphi(n_conf, n_phase),        global_nphi(n_conf, n_phase)

  double precision:: local_sum_rg(n_conf, n_phase),   global_sum_rg(n_conf, n_phase)
  double precision:: local_sum_rg2(n_conf, n_phase),  global_sum_rg2(n_conf, n_phase)
  double precision:: local_sum_ree(n_conf, n_phase),  global_sum_ree(n_conf, n_phase)
  double precision:: local_sum_ree2(n_conf, n_phase), global_sum_ree2(n_conf, n_phase)

  double precision, allocatable:: local_sum_phi(:,:,:),  global_sum_phi(:,:,:)
  double precision, allocatable:: local_sum_phi2(:,:,:), global_sum_phi2(:,:,:)

  ! Checkpoint arrays (MPI_Allreduce mid-run) 
  integer:: ckpt_frame_count(n_conf, n_phase), ic_ckpt, ip_ckpt
  double precision:: ckpt_sum_rg(n_conf, n_phase)
  double precision:: partial_mean_rg

  integer:: max_phi
  double precision:: t0, t1, wall_time

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierr)

  call get_base_dir(base_dir)
  call get_metrics_file(metrics_file, have_metrics_file)
  call get_out_dir(out_dir_rt)
  call get_manifest_file(manifest_file)

  ! Rank 0 reads the manifest created by the bash script. Then it broadcasts to everyone.
  call read_file_list(trim(manifest_file), files, nfiles)

  if (nfiles <= 0) then
    if (rank == 0) then
      write(*,'(A)') 'No trajectory_*.xyz files found.'
      write(*,'(A)') 'Manifest file: ' // trim(manifest_file)
      write(*,'(A)') 'Expected under: ' // trim(base_dir)
      call flush(6)
    endif
    call MPI_Finalize(ierr)
    stop
  endif

  if (rank == 0) then
    call estimate_max_phi(files, nfiles, max_phi)
  endif
  call MPI_Bcast(max_phi, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  if (max_phi < 1) max_phi = 1

  allocate(local_sum_phi(max_phi,  n_conf, n_phase))
  allocate(global_sum_phi(max_phi, n_conf, n_phase))
  allocate(local_sum_phi2(max_phi,  n_conf, n_phase))
  allocate(global_sum_phi2(max_phi, n_conf, n_phase))

  local_file_count  = 0
  local_frame_count = 0
  local_nphi        = 0
  local_sum_rg      = 0.0d0
  local_sum_rg2     = 0.0d0
  local_sum_ree     = 0.0d0
  local_sum_ree2    = 0.0d0
  local_sum_phi     = 0.0d0
  local_sum_phi2    = 0.0d0

  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  t0 = MPI_Wtime()

  ! File distribution: each rank processes its subset
  do ifile = rank + 1, nfiles, num_procs
    call process_trajectory(trim(files(ifile)), max_phi, &
         local_file_count, local_frame_count, &
         local_sum_rg, local_sum_rg2, local_sum_ree, local_sum_ree2, &
         local_nphi, local_sum_phi, local_sum_phi2)
  enddo

  ! Mid-run checkpoint: MPI_Allreduce so every rank sees the global partial mean
  ! Each rank has finished its local file set. Before the final MPI_Reduce (which
  ! only rank 0 sees), we perform an MPI_Allreduce on frame counts and Rg sums so
  ! that ALL ranks can compute and report the global partial mean as a convergence
  ! diagnostic. 
  call MPI_Allreduce(local_frame_count, ckpt_frame_count, n_conf*n_phase, &
                     MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
  call MPI_Allreduce(local_sum_rg, ckpt_sum_rg, n_conf*n_phase, &
                     MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)

  ! Every rank prints its view of the global partial mean (convergence diagnostic)
  if (rank == 0) then
    write(*,'(A)') '------------------------------------------------------------'
    write(*,'(A,I0,A)') '[checkpoint] Global partial Rg means after all ', &
                         num_procs, ' ranks finished local processing:'
    do ic_ckpt = 1, n_conf
      do ip_ckpt = 1, n_phase
        if (ckpt_frame_count(ic_ckpt, ip_ckpt) > 0) then
          partial_mean_rg = ckpt_sum_rg(ic_ckpt, ip_ckpt) / &
                            dble(ckpt_frame_count(ic_ckpt, ip_ckpt))
          write(*,'(A,I0,A,A,A,F10.4,A,I0,A)') &
               '  conf', conf_values(ic_ckpt), ' / ', &
               trim(phase_names(ip_ckpt)), ': mean Rg = ', &
               partial_mean_rg, ' Ang  (', &
               ckpt_frame_count(ic_ckpt, ip_ckpt), ' frames)'
        end if
      end do
    end do
    write(*,'(A)') '[checkpoint] Proceeding to final MPI_Reduce and write step.'
    write(*,'(A)') '------------------------------------------------------------'
    call flush(6)
  end if

  ! Barrier: all ranks synchronise before the final Reduce + write
  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  ! Final MPI_Reduce: collect all partial sums on rank 0 
  call MPI_Reduce(local_file_count, global_file_count, n_conf*n_phase, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
  call MPI_Reduce(local_frame_count, global_frame_count, n_conf*n_phase, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
  call MPI_Reduce(local_nphi, global_nphi, n_conf*n_phase, MPI_INTEGER, MPI_MAX, 0, MPI_COMM_WORLD, ierr)

  call MPI_Reduce(local_sum_rg, global_sum_rg, n_conf*n_phase, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
  call MPI_Reduce(local_sum_rg2, global_sum_rg2, n_conf*n_phase, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
  call MPI_Reduce(local_sum_ree, global_sum_ree, n_conf*n_phase, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
  call MPI_Reduce(local_sum_ree2, global_sum_ree2, n_conf*n_phase, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  call MPI_Reduce(local_sum_phi, global_sum_phi, max_phi*n_conf*n_phase, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
  call MPI_Reduce(local_sum_phi2, global_sum_phi2, max_phi*n_conf*n_phase, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  t1 = MPI_Wtime()
  wall_time = t1 - t0

  if (rank == 0) then
    call write_summary(trim(out_dir_rt), trim(base_dir), nfiles, num_procs, wall_time, &
         global_file_count, global_frame_count, global_nphi, &
         global_sum_rg, global_sum_rg2, global_sum_ree, global_sum_ree2, &
         global_sum_phi, global_sum_phi2, max_phi)

    if (have_metrics_file) then
      call write_run_metrics(trim(metrics_file), num_procs, nfiles, wall_time)
    endif

    write(*,'(A)') '------------------------------------------------------------'
    write(*,'(A,I0)')      ' [parallel_observables] files processed: ', nfiles
    write(*,'(A,I0)')      ' [parallel_observables] MPI ranks:       ', num_procs
    write(*,'(A,F12.6,A)') ' [parallel_observables] wall time:      ', wall_time, ' s'
    write(*,'(A,1X,F12.6)') 'BENCHMARK_MPI_WALL', wall_time
    write(*,'(A)') ' [parallel_observables] manifest used: ' // trim(manifest_file)
    write(*,'(A)') ' [parallel_observables] results written to: ' // trim(out_dir_rt)
    call flush(6)
  endif

  deallocate(files, local_sum_phi, global_sum_phi, local_sum_phi2, global_sum_phi2)
  call MPI_Finalize(ierr)

contains

  ! Reads the base directory from the command line (argument 1).
  subroutine get_base_dir(dir)
    character(len=*), intent(out):: dir
    integer:: nargs
    nargs = command_argument_count()
    if (nargs >= 1) then
      call get_command_argument(1, dir)
      if (len_trim(dir) == 0) dir = default_base_dir
    else
      dir = default_base_dir
    endif
  end subroutine get_base_dir

  !Reads metrics file from command line (argument 2). If not present, no metrics will be written.
  subroutine get_metrics_file(fname, present_arg)
    character(len=*), intent(out):: fname
    logical, intent(out):: present_arg
    integer:: nargs
    fname = ''
    present_arg = .false.
    nargs = command_argument_count()
    if (nargs >= 2) then
      call get_command_argument(2, fname)
      if (len_trim(fname) > 0) present_arg = .true.
    endif
  end subroutine get_metrics_file

  ! Reads the output directory from the command line (argument 3). If not present, uses default.
  subroutine get_out_dir(dir)
    character(len=*), intent(out):: dir
    integer:: nargs
    nargs = command_argument_count()
    if (nargs >= 3) then
      call get_command_argument(3, dir)
      if (len_trim(dir) == 0) dir = default_out_dir
    else
      dir = default_out_dir
    endif
  end subroutine get_out_dir

  ! Reads the manifest file from the command line (argument 4). If not present, uses default.
  subroutine get_manifest_file(fname)
    character(len=*), intent(out):: fname
    integer:: nargs
    nargs = command_argument_count()
    if (nargs >= 4) then
      call get_command_argument(4, fname)
      if (len_trim(fname) == 0) fname = default_manifest
    else
      fname = default_manifest
    endif
  end subroutine get_manifest_file

  ! Reads list of files from manifest. Rank 0 reads and bcst to all ranks.
  subroutine read_file_list(manifest, list, n)
    character(len=*), intent(in):: manifest
    character(len=max_path), allocatable, intent(out):: list(:)
    integer, intent(out):: n
    integer:: ios, u_manifest, count
    character(len=max_path):: line
    character(len=max_path), allocatable:: tmp_list(:)

    u_manifest = 10
    n = 0

    if (rank == 0) then
      open(unit=u_manifest, file=trim(manifest), status='old', action='read', iostat=ios)
      if (ios /= 0) then
        write(*,'(A)') 'ERROR: could not open manifest file: ' // trim(manifest)
        call flush(6)
        n = 0
      else
        count = 0
        do
          read(u_manifest,'(A)',iostat=ios) line
          if (ios /= 0) exit
          if (len_trim(line) > 0) count = count + 1
        enddo
        close(u_manifest)

        n = count
        write(*,'(A)')   '[read_file_list] manifest = ' // trim(manifest)
        write(*,'(A,I0)') '[read_file_list] nfiles   = ', n
        call flush(6)

        if (n > 0) then
          allocate(tmp_list(n))
          open(unit=u_manifest, file=trim(manifest), status='old', action='read', iostat=ios)
          if (ios == 0) then
            count = 0
            do
              read(u_manifest,'(A)',iostat=ios) line
              if (ios /= 0) exit
              if (len_trim(line) > 0) then
                count = count + 1
                tmp_list(count) = adjustl(trim(line))
              endif
            enddo
            close(u_manifest)
          else
            n = 0
            deallocate(tmp_list)
          endif
        endif
      endif
    endif

    call MPI_Bcast(n, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

    if (n <= 0) then
      allocate(list(0))
      return
    endif

    allocate(list(n))
    if (rank == 0) list = tmp_list

    call MPI_Bcast(list, n*max_path, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)

    if (rank == 0) then
      if (allocated(tmp_list)) deallocate(tmp_list)
    endif
  end subroutine read_file_list

  ! Estimates the max num of torsions (max_phi) by reading the first valid trajectory file. 
  ! This is needed to allocate arrays for summing phi and phi^2.
  subroutine estimate_max_phi(list, n, max_phi_out)
    character(len=max_path), intent(in):: list(:)
    integer, intent(in):: n
    integer, intent(out):: max_phi_out
    integer:: u_first, ios, natoms, i, ncarb, ifile
    character(len=2), allocatable:: syms(:)
    double precision, allocatable:: xyz(:,:)
    character(len=512):: comment

    u_first = 20
    max_phi_out = 1

    do ifile = 1, n
      open(unit=u_first, file=trim(list(ifile)), status='old', action='read', iostat=ios)
      if (ios /= 0) cycle

      read(u_first,*,iostat=ios) natoms
      if (ios /= 0) then
        close(u_first)
        cycle
      endif

      read(u_first,'(A)',iostat=ios) comment
      if (ios /= 0) then
        close(u_first)
        cycle
      endif

      allocate(syms(natoms), xyz(natoms,3))
      do i = 1, natoms
        read(u_first,*,iostat=ios) syms(i), xyz(i,1), xyz(i,2), xyz(i,3)
        if (ios /= 0) exit
      enddo
      close(u_first)

      if (ios == 0) then
        ncarb = infer_n_carbons(syms, natoms)
        max_phi_out = max(1, ncarb - 3)
        deallocate(syms, xyz)
        return
      endif

      deallocate(syms, xyz)
    enddo
  end subroutine estimate_max_phi

  ! Counts num of C atoms to infer number of torsions (ncarb - 3).
  integer function infer_n_carbons(symbols, natoms) result(nc)
    character(len=2), intent(in):: symbols(natoms)
    integer, intent(in):: natoms
    integer:: i
    nc = 0
    do i = 1, natoms
      if (trim(adjustl(symbols(i))) == 'C') nc = nc + 1
    enddo
  end function infer_n_carbons

  ! Classify the path according to configuration (conf1/conf4/conf5) and phase (equil/prod)!!
  subroutine classify_path(path, ic, ip)
    character(len=*), intent(in):: path
    integer, intent(out):: ic, ip
    character(len=max_path):: p

    p = path
    call to_lower_inplace(p)

    ic = 0
    ip = 0

    if (index(p, '/equil/') > 0 .or. index(p, 'trajectory_equil') > 0 .or. &
        index(p, 'equil_c') > 0) ip = 1
    if (index(p, '/prod/') > 0 .or. index(p, 'trajectory_prod') > 0 .or. &
        index(p, 'prod_c') > 0) ip = 2

    if (index(p, '/conf1/') > 0 .or. index(p, 'conf1') > 0 .or. index(p, '_c1_') > 0 .or. &
        index(p, 'equil_c1') > 0 .or. index(p, 'prod_c1') > 0) ic = 1
    if (index(p, '/conf4/') > 0 .or. index(p, 'conf4') > 0 .or. index(p, '_c4_') > 0 .or. &
        index(p, 'equil_c4') > 0 .or. index(p, 'prod_c4') > 0) ic = 2
    if (index(p, '/conf5/') > 0 .or. index(p, 'conf5') > 0 .or. index(p, '_c5_') > 0 .or. &
        index(p, 'equil_c5') > 0 .or. index(p, 'prod_c5') > 0) ic = 3
  end subroutine classify_path

  ! String to lowercase in-place (co-pilot suggestion)
  subroutine to_lower_inplace(str)
    character(len=*), intent(inout):: str
    integer:: i, c
    do i = 1, len_trim(str)
      c = iachar(str(i:i))
      if (c >= iachar('A') .and. c <= iachar('Z')) str(i:i) = achar(c + 32)
    enddo
  end subroutine to_lower_inplace

  ! Extracts coordinates of the carbon atoms in the backbone. Used to compute observables.
  subroutine extract_carbon_coords(symbols, coords, natoms, ncarb, ccoords)
    character(len=2), intent(in):: symbols(natoms)
    double precision, intent(in):: coords(natoms, 3)
    integer, intent(in):: natoms, ncarb
    double precision, intent(out):: ccoords(ncarb, 3)
    integer:: i, j
    j = 0
    do i = 1, natoms
      if (trim(adjustl(symbols(i))) == 'C') then
        j = j + 1
        if (j <= ncarb) ccoords(j, :) = coords(i, :)
      endif
    enddo
  end subroutine extract_carbon_coords

  ! Process a full trajectory file.
  ! Updates the local sums and counts for the corresponding conf/phase. 
  ! The global sums/counts will be obtained by MPI_Reduce after all files are processed.
  subroutine process_trajectory(path, max_phi_in, file_count, frame_count, &
       sum_rg, sum_rg2, sum_ree, sum_ree2, nphi_map, sum_phi, sum_phi2)
    character(len=*), intent(in)   :: path
    integer, intent(in):: max_phi_in
    integer, intent(inout):: file_count(n_conf, n_phase)
    integer, intent(inout):: frame_count(n_conf, n_phase)
    integer, intent(inout):: nphi_map(n_conf, n_phase)
    double precision, intent(inout):: sum_rg(n_conf, n_phase), sum_rg2(n_conf, n_phase)
    double precision, intent(inout):: sum_ree(n_conf, n_phase), sum_ree2(n_conf, n_phase)
    double precision, intent(inout):: sum_phi(max_phi_in, n_conf, n_phase)
    double precision, intent(inout):: sum_phi2(max_phi_in, n_conf, n_phase)
    integer:: u_traj
    integer:: ic, ip, ios, natoms, i, ncarb, nphi
    character(len=512):: comment
    character(len=2), allocatable:: symbols(:)
    double precision, allocatable:: coords(:,:), backbone(:,:), phis(:)
    double precision:: rg2, ree2, rg, ree

    u_traj = 30

    call classify_path(path, ic, ip)
    if (ic == 0 .or. ip == 0) return

    open(unit=u_traj, file=trim(path), status='old', action='read', iostat=ios)
    if (ios /= 0) return

    file_count(ic, ip) = file_count(ic, ip) + 1

    do
      read(u_traj,*,iostat=ios) natoms
      if (ios /= 0) exit

      read(u_traj,'(A)',iostat=ios) comment
      if (ios /= 0) exit

      if (.not. allocated(symbols)) then
        allocate(symbols(natoms), coords(natoms,3))
      else if (size(symbols) /= natoms) then
        deallocate(symbols, coords)
        allocate(symbols(natoms), coords(natoms,3))
      endif

      do i = 1, natoms
        read(u_traj,*,iostat=ios) symbols(i), coords(i,1), coords(i,2), coords(i,3)
        if (ios /= 0) exit
      enddo
      if (ios /= 0) exit

      ncarb = infer_n_carbons(symbols, natoms)
      if (ncarb < 4) cycle

      if (.not. allocated(backbone)) then
        allocate(backbone(ncarb,3))
      else if (size(backbone,1) /= ncarb) then
        deallocate(backbone)
        allocate(backbone(ncarb,3))
      endif
      call extract_carbon_coords(symbols, coords, natoms, ncarb, backbone)

      nphi = ncarb - 3
      if (.not. allocated(phis)) then
        allocate(phis(nphi))
      else if (size(phis) /= nphi) then
        deallocate(phis)
        allocate(phis(nphi))
      endif

      rg2  = compute_rg(ncarb, backbone)
      ree2 = compute_end_to_end(ncarb, backbone)
      rg   = sqrt(max(rg2,  0.0d0))
      ree  = sqrt(max(ree2, 0.0d0))
      call compute_torsion_angles(ncarb, backbone, phis)

      frame_count(ic, ip)      = frame_count(ic, ip) + 1
      nphi_map(ic, ip)         = max(nphi_map(ic, ip), nphi)
      sum_rg(ic, ip)           = sum_rg(ic, ip)   + rg
      sum_rg2(ic, ip)          = sum_rg2(ic, ip)  + rg * rg
      sum_ree(ic, ip)          = sum_ree(ic, ip)  + ree
      sum_ree2(ic, ip)         = sum_ree2(ic, ip) + ree * ree
      sum_phi(1:nphi,  ic, ip) = sum_phi(1:nphi,  ic, ip) + phis(1:nphi)
      sum_phi2(1:nphi, ic, ip) = sum_phi2(1:nphi, ic, ip) + phis(1:nphi) * phis(1:nphi)
    enddo

    close(u_traj)
    if (allocated(symbols))   deallocate(symbols, coords)
    if (allocated(backbone))  deallocate(backbone)
    if (allocated(phis))      deallocate(phis)
  end subroutine process_trajectory


  ! Writes global summary file with mean and std of Rg and Ree for each conf/phase.
  ! Also write summary_global_np<nranks>.dat
  subroutine write_summary(outbase, inbase, total_files, nranks, wt, &
       file_count, frame_count, nphi_map, &
       sum_rg, sum_rg2, sum_ree, sum_ree2, sum_phi, sum_phi2, max_phi_in)
    character(len=*), intent(in):: outbase, inbase
    integer, intent(in):: total_files, nranks, max_phi_in
    integer, intent(in):: file_count(n_conf, n_phase)
    integer, intent(in):: frame_count(n_conf, n_phase)
    integer, intent(in):: nphi_map(n_conf, n_phase)
    double precision, intent(in):: wt
    double precision, intent(in):: sum_rg(n_conf, n_phase),  sum_rg2(n_conf, n_phase)
    double precision, intent(in):: sum_ree(n_conf, n_phase), sum_ree2(n_conf, n_phase)
    double precision, intent(in):: sum_phi(max_phi_in, n_conf, n_phase)
    double precision, intent(in):: sum_phi2(max_phi_in, n_conf, n_phase)
    integer:: u_summary, u_tors
    integer:: ic, ip, i, nframes, nphi, conf_id
    character(len=16) :: s_conf, s_np
    character(len=512):: summary_file, summary_file_np, tors_file
    double precision:: mean_rg, std_rg, mean_ree, std_ree, mean_phi, std_phi, varx

    u_summary = 40
    u_tors    = 50

    summary_file = trim(outbase) // '/summary_global.dat'
    write(s_np,'(I0)') nranks
    summary_file_np = trim(outbase) // '/summary_global_np' // trim(s_np) // '.dat'

    ! Standard summary file
    open(unit=u_summary, file=trim(summary_file), status='replace', action='write')
    write(u_summary,'(A)') '# MPI post-processing of trajectory_*.xyz files'
    write(u_summary,'(A)') '# input_base_dir = ' // trim(inbase)
    write(u_summary,'(A,I0)') '# total_trajectory_files = ', total_files
    write(u_summary,'(A,I0)') '# mpi_ranks = ', nranks
    write(u_summary,'(A,F15.6)') '# wall_time_s = ', wt
    write(u_summary,'(A)') '# conf phase n_traj n_frames mean_Rg_A std_Rg_A mean_Ree_A std_Ree_A'

    do ic = 1, n_conf
      conf_id = conf_values(ic)
      do ip = 1, n_phase
        nframes = frame_count(ic, ip)
        if (nframes > 0) then
          mean_rg  = sum_rg(ic, ip) / dble(nframes)
          varx     = max(sum_rg2(ic, ip) / dble(nframes) - mean_rg * mean_rg, 0.0d0)
          std_rg   = sqrt(varx)

          mean_ree = sum_ree(ic, ip) / dble(nframes)
          varx     = max(sum_ree2(ic, ip) / dble(nframes) - mean_ree * mean_ree, 0.0d0)
          std_ree  = sqrt(varx)
        else
          mean_rg  = 0.0d0
          std_rg   = 0.0d0
          mean_ree = 0.0d0
          std_ree  = 0.0d0
        endif

        write(u_summary,'(I4,1X,A5,1X,I8,1X,I10,1X,F14.6,1X,F14.6,1X,F14.6,1X,F14.6)') &
             conf_id, trim(phase_names(ip)), file_count(ic, ip), nframes, &
             mean_rg, std_rg, mean_ree, std_ree
      enddo
    enddo
    close(u_summary)

    ! Per-np summary file
    open(unit=u_summary, file=trim(summary_file_np), status='replace', action='write')
    write(u_summary,'(A)') '# MPI post-processing of trajectory_*.xyz files'
    write(u_summary,'(A)') '# input_base_dir = ' // trim(inbase)
    write(u_summary,'(A,I0)') '# total_trajectory_files = ', total_files
    write(u_summary,'(A,I0)') '# mpi_ranks = ', nranks
    write(u_summary,'(A,F15.6)') '# wall_time_s = ', wt
    write(u_summary,'(A)') '# conf phase n_traj n_frames mean_Rg_A std_Rg_A mean_Ree_A std_Ree_A'

    do ic = 1, n_conf
      conf_id = conf_values(ic)
      do ip = 1, n_phase
        nframes = frame_count(ic, ip)
        if (nframes > 0) then
          mean_rg  = sum_rg(ic, ip) / dble(nframes)
          varx     = max(sum_rg2(ic, ip) / dble(nframes) - mean_rg * mean_rg, 0.0d0)
          std_rg   = sqrt(varx)

          mean_ree = sum_ree(ic, ip) / dble(nframes)
          varx     = max(sum_ree2(ic, ip) / dble(nframes) - mean_ree * mean_ree, 0.0d0)
          std_ree  = sqrt(varx)
        else
          mean_rg  = 0.0d0
          std_rg   = 0.0d0
          mean_ree = 0.0d0
          std_ree  = 0.0d0
        endif

        write(u_summary,'(I4,1X,A5,1X,I8,1X,I10,1X,F14.6,1X,F14.6,1X,F14.6,1X,F14.6)') &
             conf_id, trim(phase_names(ip)), file_count(ic, ip), nframes, &
             mean_rg, std_rg, mean_ree, std_ree
      enddo
    enddo
    close(u_summary)

    do ic = 1, n_conf
      conf_id = conf_values(ic)
      write(s_conf,'(I0)') conf_id
      do ip = 1, n_phase
        tors_file = trim(outbase) // '/torsions_conf' // trim(s_conf) // '_' // &
                    trim(phase_names(ip)) // '.dat'

        open(unit=u_tors, file=trim(tors_file), status='replace', action='write')
        write(u_tors,'(A)') '# torsion_index mean_phi_rad std_phi_rad'
        nphi = nphi_map(ic, ip)
        if (frame_count(ic, ip) > 0 .and. nphi > 0) then
          do i = 1, nphi
            mean_phi = sum_phi(i, ic, ip) / dble(frame_count(ic, ip))
            varx     = max(sum_phi2(i, ic, ip) / dble(frame_count(ic, ip)) - mean_phi * mean_phi, 0.0d0)
            std_phi  = sqrt(varx)
            write(u_tors,'(I8,1X,F16.8,1X,F16.8)') i, mean_phi, std_phi
          enddo
        endif
        close(u_tors)
      enddo
    enddo
  end subroutine write_summary

  ! Write the benchmark metrics file with number of ranks, wall time, and num files processed, used for performance analysis.
  subroutine write_run_metrics(fname, nranks, ntraj, wt)
    character(len=*), intent(in):: fname
    integer, intent(in):: nranks, ntraj
    double precision, intent(in):: wt
    integer:: u_metrics, ios

    u_metrics = 60

    open(unit=u_metrics, file=trim(fname), status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') 'WARNING: could not write metrics file: ' // trim(fname)
      call flush(6)
      return
    endif

    write(u_metrics,'(A)') '# np mpi_wall_time_s nfiles'
    write(u_metrics,'(I0,1X,F12.6,1X,I0)') nranks, wt, ntraj
    close(u_metrics)
  end subroutine write_run_metrics

end program main_parallel_observables