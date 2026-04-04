! Global module for the parameters
! Author: Itxaso Muñoz-Aldalur
! Contributors: Oliwier Misztal

module parameters
  implicit none
  double precision, parameter:: pi        = 4.0d0 * atan(1.0d0)
  double precision, parameter:: bond_len  = 1.54d0   ! Angstrom, C-C
  double precision, parameter:: bond_ang  = 114.0d0  ! degrees, C-C-C

  ! OpenMP Parallelization Switches
  ! Set to .true. to enable parallelization of the energy calculations
  ! (Note: Indentation not allowed here)
#ifdef _USE_OPENMP
  logical :: omp_total_energy = .true.
  logical :: omp_delta_energy = .true.
#else
  logical :: omp_total_energy = .false.
  logical :: omp_delta_energy = .false.
#endif

  ! Lennard-Jones Parameters for UA calculations
  ! double precision, parameter:: sigma_cc  = 3.73d0   ! Angstrom
  ! double precision, parameter:: eps_cc    = 0.091d0  ! kcal/mol

  double precision, parameter:: kb        = 0.001987d0 ! kcal/(mol·K)

  ! Prepare for MC with explicit hydrogens:

  ! FORCE FIELD PARAMETERS: OPLS-AA for Polyethylene
  ! ! Reference: Saether et al., Macromolecules 2021 (Table 1, SI)

  ! --- Lennard-Jones Parameters (sigma in Angstroms, eps in kcal/mol) ---
  double precision, parameter :: sigma_cc = 3.50d0
  double precision, parameter :: eps_cc   = 0.066d0

  double precision, parameter :: sigma_hh = 2.50d0
  double precision, parameter :: eps_hh   = 0.030d0

  ! Cross-interactions (Explicitly calculated in the paper's Table 1)
  double precision, parameter :: sigma_ch = 2.96d0
  double precision, parameter :: eps_ch   = 0.045d0

  ! --- Torsional Parameters (kcal/mol) ---
  ! These are EFFECTIVE backbone parameters.
  ! They mathematically combine the C-C-C-C, C-C-C-H, and H-C-C-H dihedrals 
  ! into a single rotation matrix to save CPU cycles in the Monte Carlo loop.
  ! Values pre-divided by 2 for the energy equation: U(phi) = sum(K_n/2 * ...)
  
  double precision, parameter :: opls_c1 =  0.8700d0   ! 1.74 / 2
  double precision, parameter :: opls_c2 = -0.0785d0   ! -0.157 / 2
  double precision, parameter :: opls_c3 =  1.5075d0   ! (0.279 + 4*0.366 + 4*0.318) / 2

end module parameters

