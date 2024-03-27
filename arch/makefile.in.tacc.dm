
#  Options for TACC Lonestar 5

CPP = cpp
CPPFLAGS =
CC = icc
F90 = ifort
MF90 = mpif90
F90FLAGS = -g -fpp -free -fpe0
LDFLAGS =
LIBS =
RM = rm -f

MPP_BUILD = YES
CPPFLAGS += -DMPP_LAND

NETCDF_INC = -I${TACC_NETCDF_DIR}/include
NETCDF_LIB = -L${TACC_NETCDF_DIR}/lib -lnetcdf -lnetcdff
