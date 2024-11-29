PROGRAM ORBITSIM
  implicit none
  INTEGER, PARAMETER :: dp = kind(0.d0)

  TYPE :: body
    REAL(8) :: mass = 0d0
    REAL(8) :: posi(3) = [0d0,0d0,0d0]
    REAL(8) :: vel(3) = [0d0,0d0,0d0]
  END TYPE

  TYPE(body)         :: system(3)
  INTEGER, PARAMETER :: tend = 3640 
  INTEGER, PARAMETER :: dt = 1

  ! Assume the sun is at the origin with 0 movement
  system(1) = body(mass = 1.9884E30) 

  ! The Earth at aphelion, assume the x-y plane is the ecliptic
  system(2) = body(mass=5.9722E24, posi=[1.521E8_dp, 0d0, 0d0])  

  system(3) = body(&
    mass=7.34767309E22,&
    posi=[system(2)%posi(1) + 384400.0_dp, 0.d0, 0.d0],&
    vel=[1.d022*cos(5.4 * 3.14159 / 180.d0), 0.d0, 1.022*sin(5.4 * 3.14159 / 180.d0)] )

  contains

FUNCTION grav(m1, m2, r) RESULT(fsubg)
  real(8) :: m1, m2
  real(8) :: r(3)
  real(8) :: fsubg(3)
  real(8), parameter :: bigg = 6.6743E11 

  fsubg = bigg * m1 * m2 / r ** 2

END FUNCTION

END PROGRAM
