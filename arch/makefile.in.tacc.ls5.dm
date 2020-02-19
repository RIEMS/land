
#  Options for Linux with Intel Fortran MPI

MPPFLAG        =       YES
CC             =       icc
F90            =       mpif90
F90FLAGS       =       -g -free -convert big_endian -fpe0
MODFLAGS       =       -I../mpp
HYDRO_LIB      =       ../mpp/mpp_land.o ../mpp/cpl_wrf.o
LDFLAGS        =
CPP            =       cpp
CPPFLAGS       =       -P -traditional -DMPP_LAND # -DSPATIAL_SOIL
LIBS           =
LIBJASPER      =      -ljasper
INCJASPER      =      -I/usr/include/jasper
NETCDFMOD      =      -I${NETCDF}/include
NETCDFLIB      =      -L${NETCDF}/lib -lnetcdf -lnetcdff
BZIP2          =       NO
BZIP2_INCLUDE  =       -I/usr/include
BZIP2_LIB      =       -L/usr/lib64 -lbz2
RM             =       rm -f
