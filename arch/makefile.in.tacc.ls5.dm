
#  Options for Linux with Intel Fortran MPI

CPP = cpp
CPPFLAGS = -P -traditional -DMPP_LAND # -DSPATIAL_SOIL
CC = cc
F90 = mpif90
F90FLAGS = -g -free -convert big_endian -fpe0
LDFLAGS =
LIBS =
RM = rm -f

MPP_BUILD = YES
MPP_INC = -I../mpp
MPP_LIB = ../mpp/mpp_land.o ../mpp/cpl_wrf.o

JASPER_INC = -I/usr/include/jasper
JASPER_LIB = -L/usr/lib -ljpeg -ljasper

NETCDF_INC = -I${NETCDF}/include
NETCDF_LIB = -L${NETCDF}/lib -lnetcdf -lnetcdff

BZIP2_USE = NO
BZIP2_INC  = -I/usr/include
BZIP2_LIB = -L/usr/lib64 -lbz2
