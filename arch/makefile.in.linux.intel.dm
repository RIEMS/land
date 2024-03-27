
#  Options for Linux with Intel Fortran MPI

CPP = cpp
CPPFLAGS =
CC = icc
F90 = ifort
MF90 = mpif90 -f90=$(F90)
F90FLAGS = -g -fpp -free -fpe0
LDFLAGS =
LIBS =
RM = rm -f

MPP_BUILD = YES
CPPFLAGS += -DMPP_LAND

NETCDF_INC = -I/$(NETCDF)/include
NETCDF_LIB = -L/$(NETCDF)/lib -lnetcdf -lnetcdff
