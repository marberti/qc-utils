module mod_distance_matrix

  use, intrinsic :: iso_fortran_env
  use :: mod_xyz
  use :: mod_generic

  implicit none

contains

subroutine cmpt_distance_matrix(xyz,dm)

  type(xyz_t), intent(in) :: xyz
  real(REAL64), allocatable, dimension(:,:), intent(out) :: dm
  character(*), parameter :: my_name = "cmpt_distance_matrix"
  integer :: i
  integer :: j
  integer :: n
  real(REAL64) :: d
  integer :: err_n
  character(120) :: err_msg

  n = xyz%n

  allocate(dm(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(ERROR_UNIT,*) my_name//": "//trim(err_msg)
    stop 1
  end if

  dm = 0.0_REAL64
  do i = 1, n
    do j = i+1, n
      d = dist3d(xyz%x(i),xyz%y(i),xyz%z(i),xyz%x(j),xyz%y(j),xyz%z(j))
      dm(i,j) = d
      dm(j,i) = d
    end do
  end do

end subroutine cmpt_distance_matrix

subroutine write_distance_matrix(xyz,dm)

  type(xyz_t), intent(in) :: xyz
  real(REAL64), dimension(:,:), intent(in) :: dm
  character(*), parameter :: my_name = "write_distance_matrix"
  integer, parameter :: col_per_block = 5
  integer :: sz
  integer :: i
  integer :: j
  integer :: k
  integer :: col_to_print
  integer :: col_left

  sz = size(dm,1)
  if (sz /= xyz%n) then
    write(ERROR_UNIT,*) my_name//": incompatible xyz structure and dm"
    stop 1
  end if

  i = 1
  do
    col_left = sz - i + 1
    col_to_print = min(col_per_block,col_left)
    if (col_to_print <= 0) exit
    write(*,'(9X)',advance="no")
    do k = i, i + col_to_print - 1
      write(*,'(4X,I4,2X,A2)',advance="no") k, xyz%el(k)
    end do
    write(*,*)
    do j = i, sz
      write(*,'(1X,I4,2X,A2)',advance="no") j, xyz%el(j)
      do k = i, i + col_to_print - 1
        if (k > j) exit
        write(*,'(1X,F11.6)',advance="no") dm(j,k)
      end do
      write(*,*)
    end do
    i = i + col_per_block
  end do

end subroutine write_distance_matrix

end module mod_distance_matrix
