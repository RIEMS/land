
#  Options for Linux with gfortran

MPPFLAG = YES
F90            =       mpif90
F90FLAGS       =       -g -cpp -ffree-form -ffree-line-length-none -fconvert=big-endian -fbounds-check -fno-range-check -fno-underscoring
MODFLAGS       =       -I../mpp
HYDRO_LIB = ../mpp/mpp_land.o ../mpp/cpl_wrf.o
LDFLAGS        =
CPP            =       /usr/bin/cpp
CPPFLAGS       =       -P -traditional -D_GFORTRAN_
LIBS           =
LIBJASPER      =       -ljpeg -L/usr/lib -ljasper
INCJASPER      =       -I/usr/include/jasper
NETCDFMOD      =       -I/usr/include
NETCDFLIB      =       -L/usr/lib -lnetcdf -lnetcdff
BZIP2          =       NO
BZIP2_LIB      =       -lbz2
BZIP2_INCLUDE  =       -I/usr/include
RM             =       rm -f
CC             =       /usr/bin/gcc
