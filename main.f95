PROGRAM ORBITSIM
  use plot
  use body
  implicit none
  INTEGER, PARAMETER :: dp = kind(0.d0)

  CHARACTER(LEN=30), PARAMETER :: FFMPEG = "/usr/bin/ffmpeg"
  CHARACTER(LEN=20)            :: framerate
  INTEGER, PARAMETER :: N = 5
  TYPE(body_t)       :: system(N)
  INTEGER, PARAMETER :: tend = 3600*24*365*1 ! 10 years in seconds
  INTEGER, PARAMETER :: dt = 3600*1 ! seconds
  REAL(8), PARAMETER :: AU = 1.495978707E11 ! meters
  INTEGER            :: t, i, j
  REAL(8)            :: acel_g(3)
  ! Setup for the sim:
  !                                                ^         ^
  !                                                |         |
  ! (sun) O                                (earth) o  (moon) .
  ! Assume the sun is at the origin with 0 movement
  system(1) = body_t(mass = 1.898E27) 

  ! The Earth at aphelion, assume the x-y plane is the ecliptic
  system(2) = body_t(mass=8.93E22,&
    posi=[421.8E6_dp, 0d0, 0d0], &
    vel=[0d0, 17339.8313193_dp, 0d0])  

  ! the moon, on the far side of the earth from the sun
  system(3) = body_t(&
    mass=4.8E22,&
    posi=[671.1E6_dp, 0.d0, 0.d0], &
    vel=[0.d0, 13.743669E3_dp, 0.d0] )

  system(4) = body_t(&
    mass=1.48E23,&
    posi=[1.0704E9_dp, 0.d0, 0.d0], &
    vel=[0.d0, 10.87934E3_dp, 0.d0] )

  system(5) = body_t(&
    mass=1.08E23,&
    posi=[1.8827E9_dp, 0.d0, 0.d0], &
    vel=[0.d0, 8.203835E3_dp, 0.d0] )

  
  do t = 1, tend, dt
    ! calculate the acelleration for each timestep
    do i = 1, N 
      acel_g = 0d0
      do j = 1, N 
        if (i.ne.j) then
          acel_g = acel_g + grav(system(j)%mass, system(j)%posi - system(i)%posi)
        end if
      end do
     ! update the velocity for each timestep & each body
     system(i)%vel = system(i)%vel + acel_g * dt
    end do

    do i = 1, N 
      system(i)%posi = system(i)%posi + system(i)%vel * dt
    end do

    ! generate the frame for this timestep, the array is the +/- x, y, and z bounds of the window
    call plot_frame(system, N, t, [2.5E9, 2.5E9, 0.5E9])
    write(*, "('t= ', i0, ' / ' i0, ' sec')", advance="yes") t, tend

  end do

  write(framerate, "(i20)") 60

  call EXECUTE_COMMAND_LINE(TRIM(TRIM(FFMPEG)//" -loglevel 24 -y -framerate "//TRIM(ADJUSTL(framerate))&
    //" -pattern_type glob -i 'target/data/*.png' -c:v libx264 -r 200 -f mp4 orbit_sim.mp4;"), wait=.true.)

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
