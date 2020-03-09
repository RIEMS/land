
#  Options for Linux with gfortran

CPP = cpp
CPPFLAGS = -traditional -D_GFORTRAN_
CC = gcc
F90 = mpif90
F90FLAGS = -g -cpp -ffree-form -ffree-line-length-none -fconvert=big-endian -fbounds-check -fno-range-check -fno-underscoring
LDFLAGS =
LIBS =
RM = rm -f

MPP_BUILD = YES
CPPFLAGS += -DMPP_LAND
MPP_INC = -I../mpp
MPP_LIB = ../mpp/mpp_land.o ../mpp/cpl_wrf.o

JASPER_INC = -I/usr/include/jasper
JASPER_LIB = -L/usr/lib -ljpeg -ljasper

NETCDF_INC = -I/usr/include
NETCDF_LIB = -L/usr/lib -lnetcdf -lnetcdff

BZIP2_USE = NO
BZIP2_INC = -I/usr/include
BZIP2_LIB = -lbz2
