! monte_carlo.f90
! Module to manage MC step and rotations


module monte_carlo
  use parameters    ! To get phi
  use initial_conf ! To use function unit_vec & cross
  use energy    !To use energy functions for the mc step
  implicit none

contains

subroutine rotate_dihedral(n_carbons, n_atoms, coords, k, delta_phi, explicit_h, coords_new)
    ! Subroutine: rotate_dihedral
    ! Author: ManelDC55
    ! ---------------------------------------------------------------------------
    ! Rotates part of the chain (atoms belonging to carbon k+2 to N) a given 
    ! angle delta_phi around the axis formed by the bond of carbons k and k+1. 
    ! We use the Rodrigues formula to apply the rotation.
    ! --------------------------------------------------------------------------- 

    integer, intent(in) :: n_carbons        ! Total carbon atoms
    integer, intent(in) :: n_atoms          ! Total atoms in the array (C or C+H)
    double precision, intent(in) :: coords(n_atoms, 3) ! Actual coordinates
    integer, intent(in) :: k                ! k carbon index (axis: k -> k+1)
    double precision, intent(in) :: delta_phi ! Applied rotation angles (radians)
    logical, intent(in) :: explicit_h       ! Flag to consider hydrogens 
    double precision, intent(out) :: coords_new(n_atoms, 3)
    
    double precision :: axis(3), pivot(3), v(3), v_rot(3) !rotation axis, center of giration, relative position vector and rotated vector
    double precision :: cos_p, sin_p, dot_uv 
    integer :: i, j

    ! 1. Initialize coords_new by copying all current coordinates

    if (k < 1 .or. k > n_carbons-2) stop "Invalid dihedral index"

    coords_new = coords

    ! 2. Define rotation axis: bond unitary vector C(k) -> C(k+1)
    ! unit_vec is defined on initial-conf.f90 [cite: 63]
    axis = unit_vec(coords(k+1, :) - coords(k, :))
    
    ! The rotation point/pivot is atom k+1
    pivot = coords(k+1, :)

    cos_p = cos(delta_phi)
    sin_p = sin(delta_phi)

    ! 3. Rotate carbons from k+2 to the end of the chain [cite: 31]
    do i = k + 2, n_carbons
      v = coords(i, :) - pivot
      dot_uv = sum(axis * v)
      
      ! Rodrigues formula application
      v_rot = v * cos_p + &
              cross(axis, v) * sin_p + &
              axis * dot_uv * (1.0d0 - cos_p)

      coords_new(i, :) = pivot + v_rot
    end do

    ! 4. Rotate hydrogens only if explicit_h is true [cite: 47, 49]
    if (explicit_h) then
      ! Hydrogens start after the last carbon (n_carbons + 1) [cite: 51]
      do i = n_carbons + 1, n_atoms
        do j = k + 2, n_carbons
          ! Identify if hydrogen i is bonded to a moving carbon j [cite: 30, 62]
          v = coords(i, :) - coords(j, :)
          ! Using a cutoff slightly larger than bond_ch (1.09A) 
          if (vnorm(v) < 1.2d0) then 
            
            v = coords(i, :) - pivot
            dot_uv = sum(axis * v)
            
            v_rot = v * cos_p + &
                    cross(axis, v) * sin_p + &
                    axis * dot_uv * (1.0d0 - cos_p)
            
            coords_new(i, :) = pivot + v_rot
            exit ! Optimization: hydrogen found, move to next atom
          end if
        end do
      end do
    end if

  end subroutine rotate_dihedral

  subroutine mc_step(n_carbons, n_atoms, coords, symbols, explicit_h, beta, max_delta, &
                      E_total, E_lj, E_tors, accepted)
      ! Subroutine: mc_step
      ! Author: ManelDC55 & Team
      ! ---------------------------------------------------------------------------
      ! Performs one Monte Carlo trial move using optimized energy functions:
      ! 1. Selects a random internal bond (k) and a random rotation angle.
      ! 2. Generates a trial configuration (coords_new).
      ! 3. Computes the energy difference (dE) using the colleague's optimized function.
      ! 4. Applies the Metropolis criterion to accept or reject the move.
      ! --------------------------------------------------------------------------- 

      integer, intent(in) :: n_carbons, n_atoms !Number of carbon atoms and total atoms
      double precision, intent(inout) :: coords(n_atoms, 3) !Coordinates of the system
      character(len=2), intent(in) :: symbols(n_atoms)  !Array containing the element label
      logical, intent(in) :: explicit_h !Logical used to consider or not hydrogen atoms
      double precision, intent(in) :: beta           ! 1 / (kb * T)
      double precision, intent(in) :: max_delta      ! Max rotation angle (rad)
      double precision, intent(inout) :: E_total, E_lj, E_tors  !Energy parameters
      logical, intent(out) :: accepted

      double precision :: coords_new(n_atoms, 3) !New coords after rotation
      double precision :: dE, dE_lj, dE_tors !Changes in the energy
      double precision :: random_value, phi !
      integer :: k_bond !Bond where rotation is applied
      
      accepted = .false.

      ! a. Pick random internal bond k (axis: k -> k+1)
      ! We choose k from 1 to n_carbons-2 so k+1 is not the last atom
      call random_number(random_value)
      k_bond = 1 + int(random_value * (n_carbons - 2))

      ! Pick random angle phi in range [-max_delta, max_delta]
      call random_number(random_value)
      phi = (random_value - 0.5d0) * 2.0d0 * max_delta

      ! b. Rotate atoms to generate trial configuration
      call rotate_dihedral(n_carbons, n_atoms, coords, k_bond, phi, explicit_h, coords_new)

      ! c. Compute delta energy (Optimized version from colleague)
      ! Signature: (coords_old, coords_new, nc, k, dE, dE_lj, dE_tors)
      call delta_energy(coords, coords_new, n_carbons, k_bond, dE, dE_lj, dE_tors)

      ! d. Metropolis acceptance criterion
      call random_number(random_value)
      if (dE < 0.0d0 .or. random_value < exp(-beta * dE)) then
        ! Accept: update coordinates and energy components
        coords = coords_new
        E_total = E_total + dE
        E_lj    = E_lj + dE_lj
        E_tors  = E_tors + dE_tors
        accepted = .true.
      end if

    end subroutine mc_step

    subroutine run_monte_carlo(n_steps, n_carbons, n_atoms, coords, symbols, &
                                explicit_h, beta, max_delta)
    ! ---------------------------------------------------------------------------
    ! Subroutine: run_monte_carlo
    ! Author: ManelDC55 & Team
    ! ---------------------------------------------------------------------------
    ! Performs a full Monte Carlo simulation for a polymer chain.
    ! It calculates initial energy, runs the Metropolis loop, and monitors
    ! the evolution of the system's energy and acceptance ratio.
    ! ---------------------------------------------------------------------------

    ! --- Argument List Variables ---
        integer, intent(in) :: n_steps           ! Total number of MC trial moves to perform
        integer, intent(in) :: n_carbons         ! Number of Carbon atoms (United Atoms)
        integer, intent(in) :: n_atoms           ! Total atoms in array (useful if explicit_h=T)
        double precision, intent(inout) :: coords(n_atoms, 3) ! System coordinates (updated if move accepted)
        character(len=2), intent(in) :: symbols(n_atoms)      ! Chemical symbols (C, H, etc.)
        logical, intent(in) :: explicit_h        ! Flag: True to move H atoms, False for United Atom
        double precision, intent(in) :: beta     ! Thermodynamic beta: 1/(kb * T)
        double precision, intent(in) :: max_delta ! Maximum rotation angle in radians

        ! --- Internal State Variables ---
        integer :: istep                         ! Current iteration counter for the loop
        integer :: total_accepted                ! Accumulator for moves that pass the Metropolis test
        integer :: print_interval                ! Frequency of progress updates to the terminal
        double precision :: E_total              ! Current total energy of the system (kcal/mol)
        double precision :: E_lj                 ! Current Lennard-Jones (non-bonded) energy component
        double precision :: E_tors               ! Current Torsional (dihedral) energy component
        double precision :: acceptance_rate      ! Percentage of moves accepted so far
        logical :: accepted_step                 ! Flag returned by mc_step for the current trial

        ! --- Initialize Simulation State ---
        total_accepted = 0
        ! Calculate update frequency (every 10% of the total simulation)
        print_interval = max(1, n_steps / 10)

        ! 1. Establish the Baseline Energy
        ! Before starting moves, we must know the energy of the starting configuration
        call compute_total_energy(coords, n_carbons, E_total, E_lj, E_tors)

        write(*,'(A)')       " [MC Engine] Simulation Initialized"
        write(*,'(A,F15.4)') " [MC Engine] Initial Total Energy (kcal/mol): ", E_total
        write(*,'(A,F10.2)') " [MC Engine] Target Beta (1/kbT):             ", beta
        write(*,'(A)')       " ------------------------------------------------------------"

        ! 2. Main Monte Carlo Loop
        do istep = 1, n_steps
            
            ! Try a single trial move: pick bond -> rotate -> calculate dE -> Metropolis test
            call mc_step(n_carbons, n_atoms, coords, symbols, explicit_h, &
                        beta, max_delta, E_total, E_lj, E_tors, accepted_step)
            
            ! Update global acceptance counter
            if (accepted_step) then
                total_accepted = total_accepted + 1
            end if
            
            ! 3. Progress Monitoring
            ! Provides real-time feedback on energy relaxation and acceptance statistics
            if (mod(istep, print_interval) == 0 .or. istep == n_steps) then
                acceptance_rate = (real(total_accepted) / real(istep)) * 100.0d0
                write(*,'(A,I9,A,F12.4,A,F6.2,A)') &
                    " Step:", istep, " | Total Energy:", E_total, " | Acceptance:", acceptance_rate, "%"
            end if

        end do

        ! 4. Final Simulation Report
        write(*,'(A)') " ------------------------------------------------------------"
        write(*,'(A)') " [MC Engine] Simulation Finished Successfully"
        write(*,'(A,F12.4)') " Final Energy (kcal/mol): ", E_total
        write(*,'(A,F6.2,A)') " Final Acceptance Rate:   ", (real(total_accepted)/real(n_steps))*100.0, "%"

        end subroutine run_monte_carlo

end module 