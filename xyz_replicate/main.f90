program main

  use, intrinsic :: iso_fortran_env

  implicit none

  character(*), parameter :: prog_name  = "xyz_replicate"
  character(*), parameter :: fname_ctrl = "control"
  integer, parameter :: fnumb_xyz  = 100
  integer, parameter :: fnumb_ctrl = 101
  integer, parameter :: fnumb_out  = 102
  character(120) :: fname_xyz
  character(120) :: fname_out
  character(2) :: el
  real(REAL64) :: x, y, z
  real(REAL64) :: dx, dy, dz
  integer :: indx
  integer :: i
  integer :: an
  integer :: an_r
  real(REAL64), dimension(:,:), allocatable :: c
  character(2), dimension(:), allocatable :: l
  integer, dimension(:), allocatable :: r
  integer :: err_n
  character(120) :: err_msg

  ! check command arguments
  if (command_argument_count() /= 1) then
    write(*,*) "Usage: "//prog_name//" <file.xyz>"
    stop 1
  end if

  call get_command_argument(1,fname_xyz)
  fname_out = trim(fname_xyz)//".replicated"

  ! open files
  open(unit=fnumb_xyz,file=fname_xyz,status="old",action="read",&
    &iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  open(unit=fnumb_ctrl,file=fname_ctrl,status="old",action="read",&
    &iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  open(unit=fnumb_out,file=fname_out,status="replace",action="write",&
    &iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  ! main execution
  ! read xyz
  read(fnumb_xyz,*,iostat=err_n,iomsg=err_msg) an
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if
  read(fnumb_xyz,*,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  allocate(c(3,an),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if
  allocate(l(an),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  do i = 1, an
    read(fnumb_xyz,*,iostat=err_n,iomsg=err_msg) el, x, y, z
    if (err_n /= 0) then
      write(*,*) "Error: "//trim(err_msg)
      stop 1
    end if
    l(i) = el
    c(1,i) = x
    c(2,i) = y
    c(3,i) = z
  end do

  ! read control
  read(fnumb_ctrl,*,iostat=err_n,iomsg=err_msg) dx, dy, dz
   if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if
  read(fnumb_ctrl,*,iostat=err_n,iomsg=err_msg) an_r
   if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  allocate(r(an_r),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if
  
  do i = 1, an_r
    read(fnumb_ctrl,*,iostat=err_n,iomsg=err_msg) r(i)
    if (r(i) > an) then
      write(*,*) "Error: atom in control out of range"
      stop 1
    end if
  end do

  ! write xyz replicated
  write(fnumb_out,*) an + an_r
  write(fnumb_out,*)
  do i = 1, an
    write(fnumb_out,'(A2,3(3X,F11.6))') &
      &l(i), c(1,i), c(2,i), c(3,i)
  end do
  do i = 1, an_r
    indx = r(i)
    write(fnumb_out,'(A2,3(3X,F11.6))') &
      &l(indx), c(1,indx)+dx, c(2,indx)+dy, c(3,indx)+dz
  end do

  ! deallocate
  deallocate(c,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if
  deallocate(l,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if
  deallocate(r,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  ! close files
  close(unit=fnumb_xyz,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  close(unit=fnumb_ctrl,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  close(unit=fnumb_out,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

end program main
