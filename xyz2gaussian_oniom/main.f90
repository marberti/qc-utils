program main

  use, intrinsic :: iso_fortran_env

  implicit none

  type :: link_t
    integer :: l
    integer :: h
    character(20) :: atype
  end type link_t

  type :: label_t
    character(20) :: from
    character(20) :: to
  end type label_t

  character(*), parameter :: prog_name = "xyz2gaussian_oniom"
  integer, parameter :: xyz_fnumb_in  = 2000
  integer, parameter :: xyz_fnumb_out = 2001
  character(120) :: xyz_fname_in
  character(120) :: xyz_fname_out
  integer :: high_n
  integer :: link_n
  type(link_t), allocatable, dimension(:) :: link_list
  integer :: label_n
  type(label_t), allocatable, dimension(:) :: label_list
  integer :: an
  character(120) :: comment_line
  character(2)  :: el
  character(20) :: el_out
  real(REAL64) :: x
  real(REAL64) :: y
  real(REAL64) :: z
  integer :: oniom_opt
  character(1) :: oniom_level
  integer :: i
  integer :: j
  logical :: flag_skip
  integer :: err_n
  character(120) :: err_msg

  if (command_argument_count() /= 1) then
    call help(prog_name)
    stop 1
  end if
  call get_command_argument(1,xyz_fname_in)
  xyz_fname_out = trim(xyz_fname_in)//".2oniom"

  open(file=xyz_fname_in,unit=xyz_fnumb_in,status="old",action="read",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) prog_name//": "//trim(err_msg)
    stop 1
  end if
  open(file=xyz_fname_out,unit=xyz_fnumb_out,status="replace",action="write",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) prog_name//": "//trim(err_msg)
    stop 1
  end if

  call read_control(high_n,link_n,link_list,label_n,label_list)

  read(xyz_fnumb_in,*) an
  write(xyz_fnumb_out,*) an
  read(xyz_fnumb_in,'(A120)') comment_line
  write(xyz_fnumb_out,'(A)') trim(comment_line)

  do i = 1, an
    read(xyz_fnumb_in,*) el, x, y, z
    flag_skip = .false.
    el_out = el
    do j = 1, label_n
      if (trim(el) == trim(label_list(j)%from)) then
        el_out = label_list(j)%to
        exit
      end if
    end do
    if (i > an - high_n) then
      oniom_opt = 0
      oniom_level = "H"
    else
      oniom_opt = -1
      oniom_level = "L"
    end if
    do j = 1, link_n
      if (i == link_list(j)%l) then
        oniom_opt = 0
        write(xyz_fnumb_out,'(A20,3X,I2,3(3X,F11.6),3X,A1,3X,A20,3X,I6)') &
          el_out, oniom_opt, x, y, z, oniom_level,&
          link_list(j)%atype, link_list(j)%h
        flag_skip = .true.
        exit
      end if
    end do
    if (flag_skip.eqv..true.) cycle
    write(xyz_fnumb_out,'(A20,3X,I2,3(3X,F11.6),3X,A1)') &
      el_out, oniom_opt, x, y, z, oniom_level
  end do

  close(unit=xyz_fnumb_in,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) prog_name//": "//trim(err_msg)
    stop 1
  end if
  close(unit=xyz_fnumb_out,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) prog_name//": "//trim(err_msg)
    stop 1
  end if

contains

subroutine help(prog_name)

  character(*), intent(in) :: prog_name

  write(*,*) "Usage: "//prog_name//" <xyz_file>"

end subroutine help

subroutine read_control(high_n,link_n,link_list,label_n,label_list)

  character(*), parameter :: my_name = "read_control"
  integer, intent(out) :: high_n
  integer, intent(out) :: link_n
  type(link_t), allocatable, dimension(:), intent(out) :: link_list
  integer, intent(out) :: label_n
  type(label_t), allocatable, dimension(:), intent(out) :: label_list
  character(*), parameter :: fname = "control"
  integer, parameter :: fnumb = 2010
  character(200) :: buff
  character(200) :: key
  character(8) :: str
  integer :: err_n
  character(120) :: err_msg

  open(file=fname,unit=fnumb,status="old",action="read",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    call make_control_template()
    stop 1
  end if

  high_n = -1
  link_n = -1
  do
    read(fnumb,'(A200)',iostat=err_n) buff
    if (err_n /= 0) exit
    read(buff,*) key

    select case (key)
    case ("highlevel")
      read(buff,*,iostat=err_n) str, high_n
      if (err_n /= 0) then
        write(*,*) my_name//": Unable to read 'highlevel' atoms"
        stop 1
      end if
      if (high_n < 0) then
        write(*,*) my_name//": 'highlevel' cannot be lesser than zero"
        stop 1
      end if

    case ("linkatoms")
      read(buff,*,iostat=err_n) str, link_n
      if (err_n /= 0) then
        write(*,*) my_name//": Unable to read 'linkatoms' number"
        stop 1
      end if
      if (link_n < 0) then
        write(*,*) my_name//": 'linkatoms' cannot be lesser than zero"
        stop 1
      end if
      allocate(link_list(link_n),stat=err_n,errmsg=err_msg)
      if (err_n /= 0) then
        write(*,*) my_name//": "//trim(err_msg)
        stop 1
      end if
      do i = 1, link_n
        read(fnumb,*,iostat=err_n) &
          link_list(i)%l, link_list(i)%h, link_list(i)%atype
        if (err_n /= 0) then
          write(*,*) my_name//": Bad format in 'linkatoms' line"
          stop 1
        end if
      end do

    case ("labels")
      read(buff,*,iostat=err_n) str, label_n
      if (err_n /= 0) then
        write(*,*) my_name//": Unable to read 'labels' number"
        stop 1
      end if
      if (label_n < 0) then
        write(*,*) my_name//": 'labels' cannot be lesser than zero"
        stop 1
      end if
      allocate(label_list(label_n),stat=err_n,errmsg=err_msg)
      if (err_n /= 0) then
        write(*,*) my_name//": "//trim(err_msg)
        stop 1
      end if
      do i = 1, label_n
        read(fnumb,*,iostat=err_n) &
          label_list(i)%from, label_list(i)%to
        if (err_n /= 0) then
          write(*,*) my_name//": Bad format in 'labels' line"
          stop 1
        end if
      end do

    case default
      write(*,*) my_name//": Unexpected string '"//trim(buff)//"'"
      stop 1

    end select
  end do

  if ((high_n == -1).or.(link_n == -1)) then
    write(*,*) my_name//": Missing mandatory info from 'control' file"
    stop 1
  end if

  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if

end subroutine read_control

subroutine make_control_template()

  character(*), parameter :: my_name = "make_control_template"
  character(*), parameter :: fname = "control"
  integer, parameter :: fnumb = 2020
  integer :: err_n
  character(120) :: err_msg

  open(file=fname,unit=fnumb,status="new",action="write",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if

  write(fnumb,'(A)') "highlevel <int>       "//&
    "# Put last <int> atoms on high level"
  write(fnumb,'(A)') "linkatoms <int>       "//&
    "# Number of link atoms. <int> lines follow"
  write(fnumb,'(A)') "<int1> <int2> <char>  "//&
    "# Link <int1> (low) to <int2> (high) as <char>"
  write(fnumb,'(A)') "labels <int>          "//&
    "# Number of labels (optional). <int> lines follow"
  write(fnumb,'(A)') "<char1> <char2>       "//&
    "# Substitute each atom <char1> with <char2>"

  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) then
    write(*,*) my_name//": "//trim(err_msg)
    stop 1
  end if

  write(*,*) "Written '"//trim(fname)//"' template file"

end subroutine make_control_template

end program main
