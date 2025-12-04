FC=gfortran
EXE=axsf2xyz

.PHONY: default
default:
	$(FC) -c mod_periodictable.f90
	$(FC) -c main.f90
	$(FC) -o $(EXE) mod_periodictable.o main.o

