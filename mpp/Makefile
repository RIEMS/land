# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../makefile.in

OBJS =  cpl_wrf.o mpp_land.o 

all:	$(OBJS)

.F90.o:
	$(F90) -o $(@) $(CPPFLAGS) $(F90FLAGS) -c $(*).F90
	@echo ""

clean:
	rm -f *.o *.mod *.smod *.stb *~
