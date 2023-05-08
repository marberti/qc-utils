program main
  use, intrinsic :: iso_fortran_env
  implicit none
  type :: xyz_s
    integer :: an
    character(120) :: comment_line
    character(2), allocatable, dimension(:) :: el
    real(REAL64), allocatable, dimension(:) :: x
    real(REAL64), allocatable, dimension(:) :: y
    real(REAL64), allocatable, dimension(:) :: z
  end type xyz_s
  character(*), parameter :: prog_name = "oniom_resort"
  integer, parameter :: fnumb_out = 100
  character(120) :: exec_name
  character(120) :: fname_in
  character(120) :: fname_out
  type (xyz_s) :: xyz_in
  type (xyz_s) :: xyz_out
  integer :: err_n
  character(120) :: err_msg
  call get_command_argument(0,exec_name)
  if (command_argument_count() /= 1) then
    write(*,*) prog_name//": Bad usage"
    write(*,*) "Usage: "//trim(exec_name)//" <xyz_file>"
    stop 1
  end if
  call get_command_argument(1,fname_in)
  call read_xyz(fname_in,xyz_in)
  call resort_xyz(xyz_in,xyz_out)
  fname_out = trim(fname_in)//".resorted"
  open(file=fname_out,unit=fnumb_out,status="replace",action="write",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) prog_name//": "//trim(err_msg)
    stop 1
  end if
  call write_xyz(fnumb_out,xyz_out)
  close(unit=fnumb_out,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) prog_name//": "//trim(err_msg)
    stop 1
  end if
contains
subroutine read_xyz(fname,xyz)
  character(*), intent(in) :: fname
  type(xyz_s), intent(out) :: xyz
  character(*), parameter :: my_name = "read_xyz"
  integer, parameter :: fnumb = 2000
  integer :: i
  integer :: err_n
  character(120) :: err_msg
  open(file=fname,unit=fnumb,status="old",action="read",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  read(fnumb,*,iostat=err_n) xyz%an
  if (err_n /= 0) then
    write(*,*) my_name//": Bad format in "//trim(fname)
    stop 1
  end if
  read(fnumb,'(A120)',iostat=err_n) xyz%comment_line
  if (err_n /= 0) then
    write(*,*) my_name//": Bad format in "//trim(fname)
    stop 1
  end if
  allocate(xyz%el(xyz%an),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  allocate(xyz%x(xyz%an),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  allocate(xyz%y(xyz%an),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  allocate(xyz%z(xyz%an),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  do i = 1, xyz%an
    read(fnumb,*,iostat=err_n) xyz%el(i), xyz%x(i), xyz%y(i), xyz%z(i)
    if (err_n /= 0) then
      write(*,*) my_name//": Bad format in "//trim(fname)
      stop 1
    end if
  end do
  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if
end subroutine read_xyz
subroutine resort_xyz(xyz_in,xyz_out)
  type(xyz_s), intent(in)  :: xyz_in
  type(xyz_s), intent(out) :: xyz_out
  character(*), parameter :: my_name = "resort_xyz"
  integer :: sn
  integer, allocatable, dimension(:) :: slist
  character(8) :: istr
  integer :: i
  integer :: j
  integer :: out_i
  logical :: flag_skip
  integer :: err_n
  character(120) :: err_msg
  xyz_out%an = xyz_in%an
  xyz_out%comment_line = trim(xyz_in%comment_line)//" (resorted)"
  write(*,*) "How many atoms to sort?"
  read(*,*) sn
  if (sn > xyz_out%an) then
    write(*,*) my_name//": Specified more atoms than effective ones"
    stop 1
  end if
  allocate (slist(sn),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  write(*,*) "List atoms to sort"
  do i = 1, sn
    read(*,*) slist(i)
    if (slist(i) > xyz_out%an) then
      write(istr,'(I8)') slist(i)
      istr = adjustl(istr)
      write(*,*) my_name//": Atom '"//trim(istr)//"' in list out of range"
      stop 1
    end if
  end do
  allocate (xyz_out%el(xyz_out%an),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  allocate (xyz_out%x(xyz_out%an),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  allocate (xyz_out%y(xyz_out%an),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  allocate (xyz_out%z(xyz_out%an),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if
  out_i = 1
  do i = 1, xyz_in%an
    flag_skip = .false.
    do j = 1, sn
      if (i == slist(j)) then
        flag_skip = .true.
        exit
      end if
    end do
    if (flag_skip) cycle
    xyz_out%el(out_i) = xyz_in%el(i)
    xyz_out%x(out_i)  = xyz_in%x(i)
    xyz_out%y(out_i)  = xyz_in%y(i)
    xyz_out%z(out_i)  = xyz_in%z(i)
    out_i = out_i + 1
  end do
  do i = 1, sn
    xyz_out%el(out_i) = xyz_in%el(slist(i))
    xyz_out%x(out_i)  = xyz_in%x(slist(i))
    xyz_out%y(out_i)  = xyz_in%y(slist(i))
    xyz_out%z(out_i)  = xyz_in%z(slist(i))
    out_i = out_i + 1
  end do
end subroutine resort_xyz
subroutine write_xyz(fnumb,xyz)
  integer, intent(in) :: fnumb
  type(xyz_s), intent(in) :: xyz
  character(*), parameter :: my_name = "write_xyz"
  integer :: i
  write(fnumb,*) xyz%an
  write(fnumb,'(A)') trim(xyz%comment_line)
  do i = 1, xyz%an
    write(fnumb,'(A2,3(3X,F11.6))') xyz%el(i), xyz%x(i), xyz%y(i), xyz%z(i)
  end do
end subroutine write_xyz
end program main
