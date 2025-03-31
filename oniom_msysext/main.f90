program main

  use, intrinsic :: iso_fortran_env

  implicit none

  integer, parameter :: fnumb_in = 100
  integer, parameter :: fnumb_out = 101
  character(*), parameter :: fname_out = "modelsys.xyz"
  real(REAL64), parameter :: dist_OH = 0.96_REAL64
  character(120) :: prog_name
  character(120) :: fname_in
  integer :: i
  integer :: j
  integer :: li ! link index
  integer :: n ! number of atoms
  integer :: n_out ! number of atoms in output
  character(2), dimension(:), allocatable :: e ! elements
  character(2), dimension(:), allocatable :: le ! link elements
  integer, dimension(:,:), allocatable :: ln ! link pair (low,high)
  integer, dimension(:), allocatable :: hl ! high level atoms
  real(REAL64), dimension(:,:), allocatable :: c ! atoms' coordinates
  character(1) :: level
  character(2) :: le_tmp
  integer :: ln_tmp
  integer :: lock
  integer :: low_count
  integer :: high_count
  real(REAL64) :: alpha
  real(REAL64) :: xa
  real(REAL64) :: ya
  real(REAL64) :: za
  real(REAL64) :: xb
  real(REAL64) :: yb
  real(REAL64) :: zb
  real(REAL64) :: xl
  real(REAL64) :: yl
  real(REAL64) :: zl
  real(REAL64) :: dist
  character(200) :: buff
  character(8) :: istr
  integer :: err_n
  character(120) :: err_msg

  ! open files ----------------------------------------------------------------
  if (command_argument_count() /= 1) then
    call get_command_argument(0,prog_name)
    write(*,*) "Usage: "//trim(prog_name)//" <file_2oniom>"
    stop 1
  end if

  call get_command_argument(1,fname_in)

  open(unit=fnumb_in,file=fname_in,status='old',action='read',&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot open "//trim(fname_in)
    stop 1
  end if

  open(unit=fnumb_out,file=fname_out,status='replace',action='write',&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot open "//trim(fname_out)
    stop 1
  end if

  ! get number of atoms -------------------------------------------------------

  read(fnumb_in,*,iostat=err_n,iomsg=err_msg) n
  if (err_n /= 0) then
    write(*,*) "Cannot read number of atoms from "//trim(fname_in)
    stop 1
  end if
  read(fnumb_in,*,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Unexpected EOF in "//trim(fname_in)
    stop 1
  end if

  ! allocation ----------------------------------------------------------------

  allocate(e(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot allocate e"
    stop 1
  end if

  allocate(le(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot allocate le"
    stop 1
  end if

  allocate(ln(n,2),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot allocate ln"
    stop 1
  end if

  allocate(hl(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot allocate hl"
    stop 1
  end if

  allocate(c(n,3),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot allocate c"
    stop 1
  end if

  ! read 2oniom ---------------------------------------------------------------

  low_count = 0
  high_count = 0
  li = 0
  do i = 1, n
    read(fnumb_in,'(A200)',iostat=err_n,iomsg=err_msg) buff
    if (err_n /= 0) then
      write(*,*) "Unexpected EOF in "//trim(fname_in)
      stop 1
    end if

    le_tmp = ""
    ln_tmp = 0
    read(buff,*,iostat=err_n,iomsg=err_msg) &
      e(i), lock, c(i,1), c(i,2), c(i,3), level, le_tmp, ln_tmp

    select case (level)
    case ("L","l")
      low_count = low_count + 1
    case ("H","h")
      high_count = high_count + 1
      hl(high_count) = i
    case default
      write(*,*) "Unknown level on atom ",i
      stop 1
    end select

    if (ln_tmp /= 0) then
      li = li + 1
      le(li) = le_tmp
      ln(li,1) = i
      ln(li,2) = ln_tmp
    end if

!    write(*,'(A2,2X,I2,3(2X,F12.6),2X,A,2X,A2,2X,I5)') e(i), lock, c(i,1), c(i,2), c(i,3), level, le_tmp, ln_tmp
  end do

  ! compute model system ------------------------------------------------------

  n_out = high_count + li
  write(fnumb_out,*) n_out
  write(fnumb_out,*)

  do i = 1, li
    j = ln(i,1)
    xa = c(j,1)
    ya = c(j,2)
    za = c(j,3)

    j = ln(i,2)
    xb = c(j,1)
    yb = c(j,2)
    zb = c(j,3)

    dist = sqrt((xb-xa)**2 + (yb-ya)**2 + (zb-za)**2)
    alpha = dist_OH / dist

    xl = xb - alpha*(xb - xa)
    yl = yb - alpha*(yb - ya)
    zl = zb - alpha*(zb - za)

    write(fnumb_out,'(A2,3(2X,F12.6))') le(i), xl, yl, zl
  end do

  do i = 1, high_count
    j = hl(i)
    write(fnumb_out,'(A2,3(2X,F12.6))') e(j), c(j,1), c(j,2), c(j,3)
  end do

  ! output --------------------------------------------------------------------

  write(istr,'(I8)') low_count
  istr = adjustl(istr)
  write(*,*) "Atoms on low level  = "//trim(istr)
  write(istr,'(I8)') high_count
  istr = adjustl(istr)
  write(*,*) "Atoms on high level = "//trim(istr)
  write(istr,'(I8)') li
  istr = adjustl(istr)
  write(*,*) "Link atoms = "//trim(istr)
  write(istr,'(I8)') n_out
  istr = adjustl(istr)
  write(*,*) "Atoms on model system = "//trim(istr)

!  write(*,*) "Link atom list:"
!  do i = 1, li
!    write(*,*) ln(i,1)," -> ",ln(i,2)," as ",le(i)
!  end do

  ! close files ---------------------------------------------------------------

  close(unit=fnumb_in,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot close "//trim(fname_in)
    stop 1
  end if

  open(unit=fnumb_out,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot close "//trim(fname_out)
    stop 1
  end if

  ! deallocation --------------------------------------------------------------

  deallocate(e,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot deallocate e"
    stop 1
  end if

  deallocate(le,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot deallocate le"
    stop 1
  end if

  deallocate(ln,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot deallocate ln"
    stop 1
  end if

  deallocate(hl,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot deallocate hl"
    stop 1
  end if

  deallocate(c,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot deallocate c"
    stop 1
  end if

end program main
