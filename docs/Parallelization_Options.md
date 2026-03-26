## 1. Broadcasting Input Parameters (```MPI_Bcast```) -- DONE
**Concept**: File I/O operations are very slow if hundreds of processors try to read the same file at once. Instead, have only the "Master" processor (Rank 0) read ```input.dat```, and then broadcast the variables to all "Worker" processors.

```fortran
! Add this near the top of main_serial.f90
use mpi

integer :: ierr, rank, num_procs
call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)

! Only Rank 0 reads the file
if (rank == 0) then
  call read_input_dat(n_carbons, n_steps, explicit_h, conf_type, rng_seed, xyz_file)
end if

! Broadcast the basic parameters from Rank 0 to everyone else
call MPI_Bcast(n_carbons, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
call MPI_Bcast(n_steps, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
```


## 2. Time Synchronization (```MPI_Barrier```)  -- DONE (each rank has its own cpu timing file)
**Concept**: Before you start your CPU timer (```call cpu_time(cpu_start)```), you want to make sure every processor has finished setting up the initial topology and coordinates so the benchmark is accurate.

```fortran
! After init_energy_topology and generating coords...
! Make everyone wait here until all processors catch up
call MPI_Barrier(MPI_COMM_WORLD, ierr)

! Now start the clock!
if (rank == 0) call cpu_time(cpu_start)
```

## 3. Ensemble Averaging (```MPI_Reduce```)  -- In Process (Arthur - main_parallel_star.f90)
**Concept**: Instead of running 1 long simulation of 10,000,000 steps, have 10 processors run 1,000,000 steps each with different random seeds. At the end, you can sum up the total accepted moves or average the energies across all instances.

```fortran
! Each rank has its own local 'total_accepted' count from its MC loop.
integer :: global_accepted
! Rank 0 gets the SUM of 'total_accepted' from all ranks
call MPI_Reduce(total_accepted, global_accepted, 1, MPI_INTEGER, &
                MPI_SUM, 0, MPI_COMM_WORLD, ierr)
if (rank == 0) then
  write(*,*) "Total accepted moves across all replicas:", global_accepted
end if
```

## 4. Parallel Tempering / Replica Exchange (```MPI_Send``` & ```MPI_Recv```)
**Concept**: Run a different temperature (```T```) on each processor. Periodically, you pause the Monte Carlo loop and have adjacent temperature processors swap their entire coordinate arrays if they pass a Metropolis criterion, allowing the system to escape local energy minimums.

```fortran
! Inside the MC loop, every 100,000 steps...
integer :: status(MPI_STATUS_SIZE)

if (mod(istep, 100000) == 0) then
  if (rank == 0) then
    ! Rank 0 sends its energy to Rank 1, receives Rank 1's energy
    call MPI_Send(E_total, 1, MPI_DOUBLE_PRECISION, 1, 0, MPI_COMM_WORLD, ierr)
    call MPI_Recv(neighbor_E, 1, MPI_DOUBLE_PRECISION, 1, 0, MPI_COMM_WORLD, status, ierr)
  else if (rank == 1) then
    ! Rank 1 receives from 0, sends to 0
    call MPI_Recv(neighbor_E, 1, MPI_DOUBLE_PRECISION, 0, 0, MPI_COMM_WORLD, status, ierr)
    call MPI_Send(E_total, 1, MPI_DOUBLE_PRECISION, 0, 0, MPI_COMM_WORLD, ierr)
  end if
  ! ... evaluate Metropolis probability to swap coordinates ...
end if
```

## 5. Task Distribution for Energy Calculations (```MPI_Scatter```)
**Concept**: The Lennard-Jones calculation $O(N^2)$ inside ```compute_total_energy``` takes a long time for huge polymers. The Master processor can break the array of coordinates into chunks and deal them out like a deck of cards to Worker processors to calculate partial energies.

```fortran
! Assuming rank 0 holds the full 'coords' array and we want to send chunks
! 'chunk_coords' is a smaller array allocated on each worker
integer :: chunk_size
chunk_size = (n_atoms * 3) / num_procs

! Scatter the global coords to local chunks
call MPI_Scatter(coords, chunk_size, MPI_DOUBLE_PRECISION, &
                 chunk_coords, chunk_size, MPI_DOUBLE_PRECISION, &
                 0, MPI_COMM_WORLD, ierr)

! Now each rank runs its own mini energy loop on 'chunk_coords'
```

## 6. Collecting Final Snapshots (```MPI_Gather```)
**Concept**: Once the simulation is over, you might want to create a single giant ```.xyz``` trajectory file that contains the final frame of the polymer from every single replica.

```fortran
! Let n_atoms*3 be the total doubles per coordinate array.
! 'all_coords' is a massive array allocated ONLY on Rank 0 to hold everything.
integer :: num_doubles
num_doubles = n_atoms * 3

call MPI_Gather(coords, num_doubles, MPI_DOUBLE_PRECISION, &
                all_coords, num_doubles, MPI_DOUBLE_PRECISION, &
                0, MPI_COMM_WORLD, ierr)

if (rank == 0) then
  ! Rank 0 now loops through all_coords and writes them to a single file
end if

call MPI_FINALIZE(ierr)
```

## 7. Global Data Synchronization (```MPI_Allgather```)
**Concept**: Suppose you split the 500-carbon polymer into chunks across 5 CPUs, so each CPU only proposes MC moves for its 100 atoms. After a round of local moves, they all need the new, updated positions of the entire polymer before they can calculate the next iteration's energy. Allgather ensures every rank ends up with the full, reconstructed coords array.

```fortran
! rank 0 has atoms 1-100, rank 1 has 101-200, etc. stored in 'local_coords'
integer :: count
count = (n_atoms / num_procs) * 3  ! (X, Y, Z per atom)
! Every processor sends its local chunk and receives everyone else's chunks
! Reconstructing the global 'coords' array on EVERY processor.
call MPI_Allgather(local_coords, count, MPI_DOUBLE_PRECISION, &
                   coords, count, MPI_DOUBLE_PRECISION, &
                   MPI_COMM_WORLD, ierr)
```

## 8. Non-Blocking Ghost-Atom Exchange (```MPI_Isend``` & ```MPI_Irecv```)
**Concept**: In a spatial domain decomposition model, processors only manage a 3D "box" of the simulation. If a polymer crosses the box boundary, processors must exchange edge coordinates ("ghost atoms"). Non-blocking calls let the CPU continue doing local math while the network handles the data transfer in the background, preventing idle wait times.

```fortran
integer :: request_send, request_recv
integer :: status(MPI_STATUS_SIZE)
! Initiate receiving edge atoms from the left neighbor
call MPI_Irecv(left_ghost_atoms, num_ghosts*3, MPI_DOUBLE_PRECISION, &
               rank-1, 0, MPI_COMM_WORLD, request_recv, ierr)
! Initiate sending edge atoms to the right neighbor
call MPI_Isend(right_edge_atoms, num_ghosts*3, MPI_DOUBLE_PRECISION, &
               rank+1, 0, MPI_COMM_WORLD, request_send, ierr)
! ... Compute core LJ energies for the atoms safely in the middle of the box ...
! Now wait for the boundary transfer to finish before computing boundary energy
call MPI_Wait(request_recv, status, ierr)
```

## 9. Shared Decision Making (```MPI_Allreduce```)
**Concept**: If processors are cooperating to calculate the energy of a massive single polymer step (e.g. Rank 0 calculates $E_{lj}$, Rank 1 calculates $E_{tors}$), all of them need the final summed delta_E simultaneously to pass into the Metropolis ```exp(-beta * delta_E)``` function so they all accept or reject the same move collectively.

```fortran
double precision :: local_delta_E, global_delta_E

! Sum all the local energy chunks into a single global value 
! AND distribute that global value back to all processors instantly
call MPI_Allreduce(local_delta_E, global_delta_E, 1, MPI_DOUBLE_PRECISION, &
                   MPI_SUM, MPI_COMM_WORLD, ierr)

! Now every rank has the same global_delta_E to make the acceptance choice
if (exp(-beta * global_delta_E) > random_number) then
   accepted_step = .true.
end if
```

## 10. Splitting Processors into Sub-Groups (```MPI_Comm_split```)
**Concept**: Want to run 4 completely different types of polymers at the same time? You can split ```MPI_COMM_WORLD``` so that Ranks 0-7 simulate a 500-carbon chain, and Ranks 8-15 simulate a 1000-carbon chain. They effectively act as mini clusters that don't interfere with each other.

```fortran
integer :: color, new_comm, sub_rank

! Let's divide processors by whether they are Even or Odd
color = mod(rank, 2) 

! Split the communicator into two separate realms based on 'color'
call MPI_Comm_split(MPI_COMM_WORLD, color, rank, new_comm, ierr)

! Get your new rank ID inside your sub-group (0, 1, 2, 3...)
call MPI_COMM_RANK(new_comm, sub_rank, ierr)

if (color == 0) then
  ! Run United Atom simulation
else
  ! Run All Atom simulation
end if
```

## 11. High-Precision Parallel Timing (```MPI_Wtime```)
**Concept**: The standard Fortran cpu_time() can act unpredictably under MPI load (sometimes summing time across threads). For accurate, high-resolution wall-clock timing of your code's performance, MPI_Wtime() is the standard.

```fortran
double precision :: t_start, t_end, 

call MPI_Barrier(MPI_COMM_WORLD, ierr) ! Sync everyone up
t_start = MPI_Wtime()                  ! Start the clock

call mc_step(...)                      ! Run intensive MC move

t_end = MPI_Wtime()                    ! Stop the clock
t_elapsed = t_end - t_start

if (rank == 0) write(*,*) "MC Step took", t_elapsed, "seconds."
```

## 12. Parallel Trajectory Writing (```MPI_File_write_at```)
**Concept**: Instead of forcing Rank 0 to write the entire trajectory.xyz file (which bottlenecks if the polymer is huge), MPI-IO allows every processor to write to the exact same file on the hard drive simultaneously without corrupting it, by calculating byte offsets.

```fortran
integer :: fh, offset
integer (kind=MPI_OFFSET_KIND) :: disp
! Assume each processor simulates 50 atoms. 
! Rank 0 writes line 1-50, Rank 1 writes line 51-100...

! Open the shared XYZ file
call MPI_File_open(MPI_COMM_WORLD, 'trajectory_mpi.xyz', &
                   MPI_MODE_CREATE + MPI_MODE_WRONLY, MPI_INFO_NULL, fh, ierr)

! Calculate where in the file this specific rank should dump its text
disp = rank * bytes_per_chunk 

! Write directly to that spot in the shared file concurrently
call MPI_File_write_at(fh, disp, local_string_buffer, string_length, &
                       MPI_CHARACTER, MPI_STATUS_IGNORE, ierr)

call MPI_File_close(fh, ierr)
```

## 13. Synchronized Exploration -- Manel
**Concept**: To accelerate convergence towards thermodynamic equilibrium, multiple workers explore different regions of the phase space starting from the same initial configuration. Periodically, the "Master" evaluates all trajectories, selects the one with the lowest energy (the most stable state found so far), and redistributes it as the new universal starting point for all workers.


## 14. Parallel Initial Configuration Sampling (MPI_Comm_rank) -- Itxaso
**Concept**: To test whether the simulation results are independent of the starting geometry, each processor generates a different initial configuration (different conf_type or rng_seed) and runs the full MC loop (same T, same MCSTEPS, same everything else) completely in parallel. Zero communication is needed during the MC loop itself, since each rank is a fully self-contained replica. The only coordination needed is at startup (to assign identities) and at shutdown (to finalize MPI).


# Energy Calculations Parallelization
To enable control over parallelization the switches need to be implemented - e.g. in `parameters.f90`:

```fortran
! In parameters.f90
  ! OpenMP Parallelization Switches
  logical :: omp_total_energy = .true.
  logical :: omp_delta_energy = .true.
```

Here are 3 options for parallelizing the energy:

## 1. Total Energy (Reduction & Dynamic Scheduling) -- Oliwier, to be tested
**Concept**: We parallelize the outer loop of the total energy calculation. To prevent threads from overwriting each other when summing the energy (data races), we use the `reduction(+:e_lj)` clause. Because the inner loop shrinks as `i` grows, we use `schedule(dynamic)` so threads grab chunks of work as they become available, preventing load imbalance. The `if(omp_total_energy)` clause acts as the on/off switch.

```fortran
  ! Inside compute_total_energy subroutine
  e_lj = 0.0d0
  
  !$omp parallel do if(omp_total_energy) private(j, r2) reduction(+:e_lj) schedule(dynamic, 10)
  do i = 1, n_atoms - 1
    do j = i + 1, n_atoms
      if (is_excluded(i,j)) cycle
      r2 = sum((coords(j,:) - coords(i,:))**2)
      e_lj = e_lj + lj_pair_energy(r2, atom_itype(i), atom_itype(j))
    end do
  end do
  !$omp end parallel do
```
*(Note: We can apply this exact same structure to the `e_tors` loop directly below it).*

## 2. Delta Energy (Nested Loop Collapse & Overhead Mitigation) -- Oliwier, to be tested
**Concept**: In the Monte Carlo delta energy step, we have an outer loop over `n_fixed` and an inner loop over `n_moved`. We use `collapse(2)` to unroll these two perfectly nested loops into one large loop, distributing the maximum amount of work across the threads. 

Because waking up threads takes time, we combine the switch (`omp_delta_energy`) with a mathematical check (`n_fixed * n_moved > 1000`). If an MC move only displaces a few atoms, this ensures the program falls back to single-core execution to save time. This value should be tuned based on the system size and hardware - this needs to be tested to find the optimal value.

```fortran
  ! Inside delta_energy subroutine
  de_lj = 0.0d0
  
  !$omp parallel do if(omp_delta_energy .and. (n_fixed * n_moved > 1000)) &
  !$omp private(f, i, m, j, r2_old, r2_new) reduction(+:de_lj) collapse(2)
  do f = 1, n_fixed
    do m = 1, n_moved
      i = fixed_list(f)
      j = moved_list(m)
      
      if (is_excluded(i, j)) cycle

      r2_old = sum((coords_old(j,:) - coords_old(i,:))**2)
      r2_new = sum((coords_new(j,:) - coords_new(i,:))**2)

      de_lj = de_lj + lj_pair_energy(r2_new, atom_itype(i), atom_itype(j)) &
                    - lj_pair_energy(r2_old, atom_itype(i), atom_itype(j))
    end do
  end do
  !$omp end parallel do
```

## 3. SIMD Vectorization (`!$omp declare simd`) -- Oliwier, to be implemented
**Concept**: This enables single-core vectorization. We wrap this in standard preprocessor `#ifdef` macros instead of an `if()` clause because SIMD instructions are generated by the compiler at compile-time, not run-time. 

```fortran
  ! Add this directly above your pure functions
#ifdef USE_SIMD
  !$omp declare simd(lj_pair_energy) uniform(itype1, itype2)
#endif
  pure function lj_pair_energy(r2, itype1, itype2) result(e)
    ! ... function logic ...
```
To use this specific switch, `-DUSE_SIMD` should be added to `Makefile` when we want vectorization enabled:

```makefile
FC     = gfortran

# 1. Added -fopenmp to process !$omp directives
# 2. Added -cpp to process #ifdef macros for the SIMD toggle
FFLAGS = -O2 -Wall -Wextra -fopenmp -cpp

# Toggle for SIMD Vectorization in lj_pair_energy
# To turn SIMD off, compile using: make clean && make ENABLE_SIMD=0
ENABLE_SIMD ?= 1
ifeq ($(ENABLE_SIMD), 1)
  FFLAGS += -DUSE_SIMD
endif
```