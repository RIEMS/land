
#  Options for Linux with Intel Fortran MPI

MPPFLAG        =       YES
CPP            =       cpp
CPPFLAGS       =       -P -traditional -DMPP_LAND # -DSPATIAL_SOIL
CC             =       cc
F90            =       mpif90
F90FLAGS       =       -g -free -convert big_endian -fpe0
MODFLAGS       =       -I../mpp
HYDRO_LIB      =       ../mpp/mpp_land.o ../mpp/cpl_wrf.o
LDFLAGS        =
LIBS           =
LIBJASPER      =      -ljasper
INCJASPER      =      -I/usr/include/jasper
NETCDFMOD      =      -I/glade/u/apps/ch/opt/netcdf/4.4.1.1/intel/17.0.1/include
NETCDFLIB      =      -L/glade/u/apps/ch/opt/netcdf/4.4.1.1/intel/17.0.1/lib -lnetcdf -lnetcdff
BZIP2          =       NO
BZIP2_INCLUDE  =       -I/usr/include
BZIP2_LIB      =       -L/usr/lib64 -lbz2
RM             =       rm -f
