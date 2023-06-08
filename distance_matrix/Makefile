FC = gfortran
EXE = distance_matrix.x

MODDIR = ../lib

.PHONY: default
default:
	$(FC) -J$(MODDIR) -c main.f90
	$(FC) -J$(MODDIR) -o $(EXE) main.o ../lib/libqc.a

