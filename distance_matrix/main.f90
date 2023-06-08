program main

  use, intrinsic :: iso_fortran_env
  use :: mod_xyz
  use :: mod_distance_matrix

  implicit none

  character(*), parameter :: my_name = "main"
  type(xyz_t) :: xyz
  integer :: i
  character(120) :: fname
  real(REAL64), allocatable, dimension(:,:) :: dm

  if (command_argument_count() /= 1) then
    write(ERROR_UNIT,*) my_name//": argument 'file.xyz' expected"
    stop 1
  end if

  call get_command_argument(1,fname)

  call read_xyz(fname,xyz)
  call cmpt_distance_matrix(xyz,dm)

  write(*,*) trim(fname)//":"
  call write_xyz(xyz)
  write(*,*)
  write(*,*) "Distance Matrix:"
  call write_distance_matrix(xyz,dm)

end program main
