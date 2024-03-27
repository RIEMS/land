
#  Options for Linux with pgf90 MPI

CPP = cpp
CPPFLAGS = -P -traditional
CC = pgcc
F90 = pgf90
MF90 = mpif90
F90FLAGS = -g -Mfree -Kieee
LDFLAGS =
LIBS =
RM = rm -f

MPP_BUILD = YES
CPPFLAGS += -DMPP_LAND

NETCDF_INC = -I/usr/local/netcdf4-pgi/include
NETCDF_LIB = -L/usr/local/netcdf4-pgi/lib -lnetcdf -lnetcdff
