module plot
  use body
  use plplot
implicit none

private dp
  integer, parameter  :: dp = kind(0.d0)
public plot_frame

contains 

  subroutine plot_frame(system, len, N, bounds)
    type(body_t), intent(in)         :: system(:)
    integer, intent(in)              :: len
    integer, intent(in)              :: N
    real(4), intent(in)              :: bounds(:)
    integer                          :: rc, i, j
    character(len=100)                :: filename
    character(len=10)                :: filenum
    real                             :: x(1), y(1), z(1)

    !turns N the number into a string and reads it into filenum digit by digit
    write(filenum, "(i0.10)") N

    filename = "target/data/hour_"//trim(adjustl(filenum))//".png"
    ! pase the CL arguments & set the output field
    rc = plparseopts(PL_PARSE_FULL)
    rc = plsetopt('o', TRIM(filename))
    call plinit()
    call pladv(0)
    ! set actual plot window size as a fraction of the canvas, xmin, xmax, ymin, ymax
    call plvpor(0.01, 0.99, 0.0, 0.8)
    ! setup the data range
    call plwind(-bounds(1), bounds(1), -bounds(2)+1.0, bounds(2)-1.0)
    ! color map; 15 = white
    call plcol0(15)
    ! setup the frame for the plot, bounds(1) is xlimits, bounds(2) is ylimit, bounds(3) is zlimit, last 2 args are up angle and turn angle
    call plw3d(2.0*bounds(1),2.0*bounds(2), 2.0*bounds(3),&
      -bounds(1), bounds(1), -bounds(2), bounds(2), -bounds(3), bounds(3), 50.0, 60.0)
    call plbox3('bnstu', 'x (1E9 m)', 0.0_pl_test_flt, 0, &
               'bnstu', 'y (1E9 m)', 0.0_pl_test_flt, 0, &
               'bcmnstuv', 'z (1E9 m)', 0.0_pl_test_flt, 0)
    do i = 1, len
    ! turns the position vector of the system into the correct type for plplot
      x = [system(i)%posi(1)]
      y = [system(i)%posi(2)]
      z = [system(i)%posi(3)]

      SELECT CASE (i)
        CASE (1) 
          call plstring3(x,y,z, "O")
        CASE DEFAULT
          call plstring3(x,y,z, "o")
      END SELECT

          
    end do
    call plmtex('t', 1.0_pl_test_flt, 0.5_pl_test_flt, 0.5_pl_test_flt, "Galilean moons orbiting Jupiter")

    call plend()
  end subroutine plot_frame


    !character(len=20)                :: framerate
!write(framerate, "(I20)") N
!    
!    call EXECUTE_COMMAND_LINE(TRIM(TRIM(FFMPEG)//" -loglevel 24 -y -framerate "//TRIM(ADJUSTL(framerate))&
!      //" -pattern_type glob -i 'target/data/*.png' -c:v libx264 -r 200 -f mp4 double_pendulum.mp4;"))
!
end module 
