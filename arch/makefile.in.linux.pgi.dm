
#  Options for Linux with pgf90 MPI

MPPFLAG        =       YES
CPP            = cpp
CPPFLAGS       = -P -traditional -DMPP_LAND
CC             =      cc
F90            = mpif90
F90FLAGS       =      -g -Mfree -byteswapio -Kieee
MODFLAGS       = -I../mpp
HYDRO_LIB      =       ../mpp/mpp_land.o ../mpp/cpl_wrf.o
LDFLAGS        =
LIBS           =
LIBJASPER      =      -ljpeg -L/usr/lib -ljasper
INCJASPER      =      -I/usr/include
NETCDFMOD      =      -I/usr/local/netcdf4-pgi/include
NETCDFLIB      =      -L/usr/local/netcdf4-pgi/lib -lnetcdf -lnetcdff
BZIP2          =      NO
BZIP2_LIB      =      -lbz2
BZIP2_INCLUDE  =      -I/usr/include
RM             =      rm -f

