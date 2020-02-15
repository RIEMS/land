# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../makefile.in

OBJS_NoahMP = module_ldas_noahmp.o

OBJS = \
	main_ldas.o \
	module_ldas_io_netcdf.o

all: $(OBJS_NoahMP) $(OBJS)

NoahMP : $(OBJS_NoahMP) $(OBJS)

.F90.o:
	$(F90) -o $(@) -c $(CPPFLAGS) $(NETCDFMOD) $(F90FLAGS) \
	       -I../mpp -I. -I../phys -I../share $(*).F90
	@echo ""

#
# Dependencies:
#

main_ldas.o: $(OBJS_NoahMP)
$(OBJS_NoahMP):	module_ldas_io_netcdf.o

# This command cleans up object files, etc.
clean:
	$(RM) *.o *.mod *.smod *.stb *~
