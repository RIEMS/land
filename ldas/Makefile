# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../makefile.in

OBJS_NoahMP = module_ldas_noahmp.o

OBJS = \
	main_ldas.o \
	module_ldas_io_netcdf.o

CPPHRLDAS = -D_HRLDAS_OFFLINE_ $(MOD_OPT)

all:	$(OBJS_NoahMP) $(OBJS)

NoahMP : $(OBJS_NoahMP) $(OBJS)

.F90.o:
	$(COMPILERF90) -o $(@) -c $(CPPFLAGS) $(CPPHRLDAS) $(NETCDFMOD) $(F90FLAGS) $(FREESOURCE) \
	       -I../mpp -I. -I../phys -I../share $(*).F90
	@echo ""

#
# Dependencies:
#

main_ldas.o:	$(OBJS_NoahMP)
$(OBJS_NoahMP):	module_ldas_io_netcdf.o

# This command cleans up object files, etc.
clean:
	$(RM) *.o *.mod *.smod *.stb *~
