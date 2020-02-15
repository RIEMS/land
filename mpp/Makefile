# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../makefile.in

OBJS =  cpl_wrf.o mpp_land.o 

all:	$(OBJS)

.F90.o:
	$(COMPILERF90) -o $(@) $(CPPFLAGS) $(F90FLAGS) $(FREESOURCE) -c $(*).F90
	@echo ""

clean:
	rm -f *.o *.mod *.smod *.stb *~
