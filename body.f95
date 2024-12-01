module body

  implicit none
  private dp
    INTEGER, PARAMETER :: dp = kind(0.d0)
  public body_t

TYPE :: body_t
  REAL(8) :: mass = 0d0
  REAL(8) :: posi(3) = [0d0,0d0,0d0]
  REAL(8) :: vel(3) = [0d0,0d0,0d0]
END TYPE

end module body

