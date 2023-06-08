module mod_xyz

  use, intrinsic :: iso_fortran_env

  implicit none

  type :: xyz_t
    integer :: n
    character(2), allocatable, dimension(:) :: el
    real(REAL64), allocatable, dimension(:) :: x
    real(REAL64), allocatable, dimension(:) :: y
    real(REAL64), allocatable, dimension(:) :: z
  end type

contains

subroutine read_xyz(fname,xyz)

  character(*), intent(in) :: fname
  type(xyz_t), intent(out) :: xyz
  character(*), parameter :: my_name = "read_xyz"
  integer, parameter :: fnumb = 2000
  integer :: i
  integer :: err_n
  character(120) :: err_msg

  open(unit=fnumb,file=fname,status="old",action="read",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(ERROR_UNIT,*) my_name//": "//trim(err_msg)
    stop 1
  end if

  read(fnumb,*,iostat=err_n,iomsg=err_msg) xyz%n
  if (err_n /= 0) then
    write(ERROR_UNIT,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  read(fnumb,*,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(ERROR_UNIT,*) my_name//": "//trim(err_msg)
    stop 1
  end if

  allocate(xyz%el(xyz%n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(ERROR_UNIT,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  allocate(xyz%x(xyz%n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(ERROR_UNIT,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  allocate(xyz%y(xyz%n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(ERROR_UNIT,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  allocate(xyz%z(xyz%n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(ERROR_UNIT,*) my_name//": "//trim(err_msg)
    stop 1
  end if

  do i = 1, xyz%n
    read(fnumb,*,iostat=err_n,iomsg=err_msg) &
      xyz%el(i), xyz%x(i), xyz%y(i), xyz%z(i)
    if (err_n /= 0) then
      write(ERROR_UNIT,*) my_name//": "//trim(err_msg)
      stop 1
    end if
  end do

  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(ERROR_UNIT,*) my_name//": "//trim(err_msg)
    stop 1
  end if

end subroutine read_xyz

subroutine write_xyz(xyz)

  type(xyz_t), intent(in) :: xyz
  integer :: i

  do i = 1, xyz%n
    write(*,'(1X,A2,3(3X,F11.6))') xyz%el(i), xyz%x(i), xyz%y(i), xyz%z(i)
  end do

end subroutine write_xyz

end module mod_xyz
