# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../makefile.in

OBJS = \
	module_wrf_utilities.o \
	kwm_string_utilities.o \
	module_model_constants.o \
	module_date_utilities.o \
	module_ra_gfdleta.o

all: $(OBJS)

.F90.o:
	$(F90) -o $(@) $(CPPFLAGS) $(F90FLAGS) -c $(*).F90
	@echo ""

#
# Dependencies:
#

#
# This command cleans up object (etc.) files:
#

clean:
	$(RM) *.o *.mod *.smod *.stb *~

