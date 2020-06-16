
#  Options for TACC Lonestar 5

CPP = cpp
CPPFLAGS = 
CC = icc
F90 = mpif90
F90FLAGS = -g -fpp -free -fpe0
LDFLAGS =
LIBS =
RM = rm -f

MPP_BUILD = YES
CPPFLAGS += -DMPP_LAND
MPP_INC = -I../mpp
MPP_LIB = ../mpp/mpp_land.o ../mpp/cpl_wrf.o

JASPER_INC = -I/usr/include/jasper
JASPER_LIB = -L/usr/lib -ljpeg -ljasper

NETCDF_INC = -I${TACC_NETCDF_DIR}/include
NETCDF_LIB = -L${TACC_NETCDF_DIR}/lib -lnetcdf -lnetcdff

BZIP2_USE = NO
BZIP2_INC  = -I/usr/include
BZIP2_LIB = -L/usr/lib64 -lbz2
