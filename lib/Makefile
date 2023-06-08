FC = gfortran

.PHONY: default
default:
	$(FC) -c mod_generic.f90
	$(FC) -c mod_xyz.f90
	$(FC) -c mod_distance_matrix.f90
	ar cr libqc.a mod_generic.o mod_xyz.o mod_distance_matrix.o

