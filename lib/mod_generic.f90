module mod_generic

  use, intrinsic :: iso_fortran_env

  implicit none

contains

real(REAL64) function dist3d(x1,y1,z1,x2,y2,z2)

  real(REAL64), intent(in) :: x1
  real(REAL64), intent(in) :: y1
  real(REAL64), intent(in) :: z1
  real(REAL64), intent(in) :: x2
  real(REAL64), intent(in) :: y2
  real(REAL64), intent(in) :: z2

  dist3d = sqrt((x1-x2)**2 + (y1-y2)**2 + (z1-z2)**2)

end function dist3d

end module mod_generic
