program main

  use, intrinsic :: iso_fortran_env

  implicit none

  type :: xyz
    character(2) :: e
    real(REAL64) :: x
    real(REAL64) :: y
    real(REAL64) :: z
  end type xyz

  character(*), parameter :: fname_1 = "s1.xyz"
  character(*), parameter :: fname_2 = "s2.xyz"
  character(*), parameter :: fname_out = "output.xyz"
  integer, parameter :: fnumb_1 = 101
  integer, parameter :: fnumb_2 = 102
  integer, parameter :: fnumb_out = 103

  type (xyz), dimension(:), allocatable :: s1
  type (xyz), dimension(:), allocatable :: s2
  integer :: s1_n
  integer :: s2_n
  integer, dimension(:), allocatable :: s1_count
  integer, dimension(:), allocatable :: corresp
  real(REAL64) :: dist
  real(REAL64) :: best_dist
  integer :: best_i
  integer :: i
  integer :: j
  integer :: err_n
  character(120) :: err_msg

  ! read input ----------------------------------------------------------------

  ! read first xyz
  open(file=fname_1,unit=fnumb_1,action="read",status="old",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot open "//trim(fname_1)
    stop 1
  end if

  read(fnumb_1,*) s1_n
  read(fnumb_1,*)

  allocate(s1(s1_n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot allocate s1"
    stop 1
  end if

  do i = 1, s1_n
    read(fnumb_1,*) s1(i)%e, s1(i)%x, s1(i)%y, s1(i)%z
  end do

  close(unit=fnumb_1,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot close "//trim(fname_1)
    stop 1
  end if

  ! read second xyz
  open(file=fname_2,unit=fnumb_2,action="read",status="old",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot open "//trim(fname_2)
    stop 1
  end if

  read(fnumb_2,*) s2_n
  read(fnumb_2,*)

  if (s2_n /= s1_n) then
    write(*,*) "Error: different atom number"
    stop 1
  end if

  allocate(s2(s2_n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot allocate s2"
    stop 1
  end if

  do i = 1, s2_n
    read(fnumb_2,*) s2(i)%e, s2(i)%x, s2(i)%y, s2(i)%z
  end do

  close(unit=fnumb_2,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot close "//trim(fname_2)
    stop 1
  end if

  ! init ----------------------------------------------------------------------

  allocate(s1_count(s1_n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot allocate s1_count"
    stop 1
  end if
  s1_count = 0

  allocate(corresp(s1_n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot allocate corresp"
    stop 1
  end if

  ! pre-check -----------------------------------------------------------------
  ! controllare numero di elementi

  ! body ----------------------------------------------------------------------

  do j = 1, s2_n
    best_dist = 1000000000.0_REAL64
    do i = 1, s1_n
      if (s2(j)%e /= s1(i)%e) cycle
      dist = cmp_dist(s1(i),s2(j))
      if (dist < best_dist) then
        best_dist = dist
        best_i = i
      end if
    end do

    s1_count(best_i) = s1_count(best_i) + 1
    corresp(j) = best_i
  end do

  do i = 1, s1_n
    if (s1_count(i) /= 1) then
      write(*,*) "Error: there is no one-to-one correspondence between s1 and s2"
      stop 1
    end if
  end do

  do i = 1, s1_n
    write(*,*) i, " -> ", corresp(i)
  end do

  ! write output --------------------------------------------------------------

  open(file=fname_out,unit=fnumb_out,action="write",status="replace",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot open "//trim(fname_out)
    stop 1
  end if

  write(fnumb_out,*) s1_n
  write(fnumb_out,*)
  do i = 1, s1_n
    do j = 1, s1_n
      if (corresp(j) == i) then
        write(fnumb_out,'(A2,3(1X,F12.6))') &
          adjustr(s2(j)%e), s2(j)%x, s2(j)%y, s2(j)%z
        exit
      end if
    end do
  end do

  close(unit=fnumb_out,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot close "//trim(fname_out)
    stop 1
  end if

  ! deallocation --------------------------------------------------------------

  deallocate(s1,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot deallocate s1"
    stop 1
  end if

  deallocate(s2,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot deallocate s2"
    stop 1
  end if

  deallocate(s1_count,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot deallocate s1_count"
    stop 1
  end if

  deallocate(corresp,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) then
    write(*,*) "Cannot deallocate corresp"
    stop 1
  end if

contains

!==============================================================================

real(REAL64) function cmp_dist(a,b)

  type (xyz), intent(in) :: a
  type (xyz), intent(in) :: b

  cmp_dist = sqrt((a%x - b%x)**2 + (a%y - b%y)**2 + (a%z - b%z)**2)

end function cmp_dist

!==============================================================================

end program main
