# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../makefile.in

OBJS = \
	module_sf_noahmpdrv.o \
	module_sf_noahmplsm.o \
	module_sf_noahmp_glacier.o \
	module_sf_noahmp_groundwater.o \
	module_sf_biogeochem.o \
	module_sf_gecros.o \
	module_sf_urban.o \
	module_sf_bep.o \
	module_sf_bem.o \
	module_sf_bep_bem.o

all: $(OBJS)

.F90.o:
	$(F90) -o $(@) -c $(CPPFLAGS) $(F90FLAGS) -I../share $(*).F90
	@echo ""

#
# Dependencies:
#
module_sf_noahmplsm.o: module_sf_gecros.o module_sf_biogeochem.o
module_sf_noahmpdrv.o: module_sf_noahmplsm.o module_sf_noahmp_glacier.o module_sf_noahmp_groundwater.o \
                       module_sf_urban.o module_sf_bep.o module_sf_bep_bem.o
module_sf_noahmp_groundwater.o: module_sf_noahmplsm.o
module_sf_bep.o: module_sf_urban.o
module_sf_bep_bem.o: module_sf_urban.o module_sf_bem.o

#
# This command cleans up object (etc) files:
#

clean:
	$(RM) *.o *.mod *.smod *.stb *~
