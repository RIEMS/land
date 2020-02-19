# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../makefile.in

OBJS_NOAHMP = module_ldas_noahmp.o

OBJS = \
	main_ldas.o \
	module_ldas_io_netcdf.o

all: $(OBJS_NOAHMP) $(OBJS)

NOAHMP : $(OBJS_NOAHMP) $(OBJS)

.F90.o:
	$(F90) -o $(@) -c $(CPPFLAGS) $(NETCDF_INC) $(F90FLAGS) -I../mpp -I. -I../phys -I../share $(*).F90
	@echo ""

#
# Dependencies:
#

main_ldas.o: $(OBJS_NOAHMP)
$(OBJS_NOAHMP):	module_ldas_io_netcdf.o

# This command cleans up object files, etc.
clean:
	$(RM) *.o *.mod *.smod *.stb *~
