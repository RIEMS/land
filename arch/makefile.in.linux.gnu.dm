
#  Options for Linux with gfortran

CPP = cpp
CPPFLAGS = -traditional
CC = gcc
F90 = gfortran
MF90 = mpif90
F90FLAGS = -g -cpp -ffree-form -ffree-line-length-none -fbounds-check -fno-range-check -fno-underscoring
LDFLAGS =
LIBS =
RM = rm -f

MPP_BUILD = YES
CPPFLAGS += -DMPP_LAND

NETCDF_INC = -I/usr/include
NETCDF_LIB = -L/usr/lib -lnetcdf -lnetcdff
