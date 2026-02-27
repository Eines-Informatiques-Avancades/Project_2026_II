! Global module for the parameters
module parameters
  implicit none
  double precision, parameter:: pi        = 4.0d0 * atan(1.0d0)
  double precision, parameter:: bond_len  = 1.54d0   ! Angstrom, C-C
  double precision, parameter:: bond_ang  = 114.0d0  ! degrees, C-C-C
  double precision, parameter:: sigma_cc  = 3.73d0   ! Angstrom
  double precision, parameter:: eps_cc    = 0.091d0  ! kcal/mol
  double precision, parameter:: kb        = 0.001987d0 ! kcal/(mol·K)
end module parameters

