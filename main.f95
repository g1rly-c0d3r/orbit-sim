PROGRAM ORBITSIM
  use plot
  use body
  implicit none
  INTEGER, PARAMETER :: dp = kind(0.d0)

  TYPE(body_t)       :: system(3)
  INTEGER, PARAMETER :: tend = 3600*24*365*10 ! 10 years in seconds
  INTEGER, PARAMETER :: dt = 3600*24 ! seconds
  REAL(8), PARAMETER :: AU = 1.495978707E11 ! meters
  INTEGER            :: t, i, j
  REAL(8)            :: acel_g(3)
  ! Setup for the sim:
  !                                                ^         ^
  !                                                |         |
  ! (sun) O                                (earth) o  (moon) .
  ! Assume the sun is at the origin with 0 movement
  system(1) = body_t(mass = 1.9884E30) 

  ! The Earth at aphelion, assume the x-y plane is the ecliptic
  system(2) = body_t(mass=5.9722E24,&
    posi=[1.d0 * AU, 0d0, 0d0])
    !vel=[0d0,107E6_dp,0d0])  

  ! the moon, on the far side of the earth from the sun
  system(3) = body_t(&
    mass=7.34767309E22,&
    posi=[system(2)%posi(1) + 384400000.0_dp, 0.d0, 0.d0])
    !vel=[0.d0, 1.022E3*cos(5.4 * 3.14159 / 180.d0), 1.022E3*sin(5.4 * 3.14159 / 180.d0)] )

  
  do t = 1, tend, dt
    ! calculate the acelleration for each timestep
    do i = 1, 3 
      do j = 1, 3 
        acel_g = 0d0
        if (i.ne.j) then
          acel_g = grav(system(j)%mass, system(j)%posi - system(i)%posi)
        end if
        ! update the velocity for each timestep & each body
        system(i)%vel = system(i)%vel + acel_g * dt
      end do
    end do

    do i = 1, 3 
      system(i)%posi = system(i)%posi + system(i)%vel * dt
    end do
    print*, system(1)%posi
    print*, system(2)%posi
    print*, system(3)%posi

    call plot_frame(system, 3, t, [1.521E8 + 5E7, 1.521E8 + 5E7, 2E7])
    write(*, "('t= ', i0, ' / ' i0, ' sec')", advance="yes") t, tend

  end do

  contains

FUNCTION grav(m, r) RESULT(fsubg)
  real(8) :: m
  real(8) :: r(3)
  real(8) :: fsubg(3)
  real(8), parameter :: bigg = 6.6743E-11 
  real(8), parameter :: eps = 0e-4
  real(8) :: rmag

  rmag = sqrt(r(1)**2 + r(2)**2 + r(3)**2)

  fsubg = (r/rmag)* (bigg * m / (rmag ** 2 + eps**2))

END FUNCTION

END PROGRAM
