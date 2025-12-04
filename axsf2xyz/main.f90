program main

  use, intrinsic :: iso_fortran_env

  implicit none !Definisci tipo veriabile manualmente

  character(32) :: fname
  character(32) :: prog_name
  integer :: n_frame
  integer :: n_at
  character(2), dimension(:), allocatable :: els
  real(REAL64), dimension(:), allocatable :: xs
  real(REAL64), dimension(:), allocatable :: ys
  real(REAL64), dimension(:), allocatable :: zs
  integer :: err_n
  character(120) :: err_msg

  call get_command_argument(0,prog_name)
  if (command_argument_count() /= 1) then
    write(*,*) "Usage: "//trim(prog_name)//" <file_AXSF>"
    stop 1
  end if
  call get_command_argument(1,fname)
  
  call read1(fname,n_frame,n_at)
  
  allocate(els(n_at),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  allocate(xs(n_frame*n_at),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  allocate(ys(n_frame*n_at),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  allocate(zs(n_frame*n_at),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  call read2(fname,n_frame,n_at,els,xs,ys,zs)
  call write_xyz(fname,n_frame,n_at,els,xs,ys,zs)

  deallocate(els,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  deallocate(xs,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  deallocate(ys,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  deallocate(zs,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

contains

!=============================================================================

subroutine read1(fname,n_frame,n_at)
  
  character(*), intent(in) :: fname
  integer, intent(out) :: n_frame
  integer, intent(out) :: n_at

  integer, parameter :: fnumb = 100
  character(200) :: buff
  character(20) :: buff2

  open(unit=fnumb,file=fname,action="read",status="old",&
          iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  read(fnumb,'(A200)') buff
  read(fnumb,'(A200)') buff
  read(buff,*) buff2, n_frame
  read(fnumb,'(A200)') buff
  n_at = 0
  do
    read(fnumb,'(A200)') buff
    if (buff(1:5) == "ATOMS") then
      exit
    else
      n_at = n_at + 1
    end if
  end do

  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

end subroutine read1

!=============================================================================

subroutine read2(fname,n_frame,n_at,els,xs,ys,zs)

  use :: mod_periodictable

  character(*), intent(in) :: fname
  integer, intent(in) :: n_frame
  integer, intent(in) :: n_at
  character(2), dimension(n_at), intent(out) :: els
  real(REAL64), dimension(n_frame*n_at), intent(out) :: xs
  real(REAL64), dimension(n_frame*n_at), intent(out) :: ys
  real(REAL64), dimension(n_frame*n_at), intent(out) :: zs

  integer, parameter :: fnumb = 100
  character(200) :: buff
  integer :: i
  integer :: j
  integer :: at_numb
  real(REAL64) :: x
  real(REAL64) :: y
  real(REAL64) :: z
  integer :: err_n
  character(120) :: err_msg

  open(unit=fnumb,file=fname,action="read",status="old",&
          iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  read(fnumb,'(A200)') buff
  read(fnumb,'(A200)') buff
  do i = 1, n_frame
    read(fnumb,'(A200)') buff
    do j = 1, n_at
      read(fnumb,*) at_numb, x, y, z
      if (i == 1) then
        els(j) = ptable(at_numb)%ef
      end if
      xs((i-1)*n_at+j) = x
      ys((i-1)*n_at+j) = y
      zs((i-1)*n_at+j) = z
    end do
  end do

  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

end subroutine read2

!=============================================================================

subroutine write_xyz(fname,n_frame,n_at,els,xs,ys,zs)

  character(*), intent(in) :: fname
  integer, intent(in) :: n_frame
  integer, intent(in) :: n_at
  character(2), dimension(n_at), intent(in) :: els
  real(REAL64), dimension(n_frame*n_at), intent(in) :: xs
  real(REAL64), dimension(n_frame*n_at), intent(in) :: ys
  real(REAL64), dimension(n_frame*n_at), intent(in) :: zs

  integer, parameter :: fnumb = 100
  character(32) :: fname_out
  integer :: i
  integer :: j
  integer :: err_n
  character(120) :: err_msg

  fname_out = fname(:len_trim(fname)-4)//"xyz"
  open(unit=fnumb,file=fname_out,action="write",status="replace",&
          iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

  do i = 1, n_frame
    write(fnumb,*) n_at
    write(fnumb,*)
    do j = 1, n_at
      write(fnumb,'(A2,3(3X,F12.6))') els(j), xs((i-1)*n_at+j), ys((i-1)*n_at+j), zs((i-1)*n_at+j)
    end do
  end do
  do i = n_frame - 1, 1, -1
    write(fnumb,*) n_at
    write(fnumb,*)
    do j = 1, n_at
      write(fnumb,'(A2,3(3X,F12.6))') els(j), xs((i-1)*n_at+j), ys((i-1)*n_at+j), zs((i-1)*n_at+j)
    end do
  end do

  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Error: "//trim(err_msg)
    stop 1
  end if

end subroutine write_xyz

!=============================================================================

end program main
