
#  Options for Linux with pgf90 MPI

CPP = cpp
CPPFLAGS = -P -traditional
CC = cc
F90 = mpif90
F90FLAGS = -g -Mfree -Kieee
LDFLAGS =
LIBS =
RM = rm -f

MPP_BUILD = YES
CPPFLAGS += -DMPP_LAND
MPP_INC = -I../mpp
MPP_LIB = ../mpp/mpp_land.o ../mpp/cpl_wrf.o

JASPER_INC = -I/usr/include/jasper
JASPER_LIB = -L/usr/lib -ljpeg -ljasper

NETCDF_INC = -I/usr/local/netcdf4-pgi/include
NETCDF_LIB = -L/usr/local/netcdf4-pgi/lib -lnetcdf -lnetcdff

BZIP2_USE = NO
BZIP2_INC = -I/usr/include
BZIP2_LIB = -lbz2
