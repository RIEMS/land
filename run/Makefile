.SUFFIXES:
.SUFFIXES: .o .f

include ../makefile.in

OBJS_NOAHMP = \
	../ldas/module_ldas_noahmp.o \
	../ldas/main_ldas.o

ifeq ($(MPP_BUILD), YES)
OBJS_MPP = \
	../mpp/cpl_wrf.o \
	../mpp/mpp_land.o
else
OBJS_MPP =
endif

OBJS = \
	../ldas/module_ldas_config.o \
	../ldas/module_ldas_io_netcdf.o \
	../phys/module_sf_noahmpdrv.o \
	../phys/module_sf_noahmplsm.o \
	../phys/module_sf_noahmp_glacier.o \
	../phys/module_sf_noahmp_groundwater.o \
	../phys/module_sf_gecros.o \
	../phys/module_sf_urban.o \
	../phys/module_sf_bep.o \
	../phys/module_sf_bem.o \
	../phys/module_sf_bep_bem.o \
	../share/module_wrf_utilities.o \
	../share/module_model_constants.o \
	../share/module_date_utilities.o \
	../share/kwm_string_utilities.o \
	../share/module_ra_gfdleta.o

CMD = ldas.exe
all: $(CMD)

### default we create the exe based on NoahMP
$(CMD): $(OBJS)
	$(MF90) -o $(@) $(OBJS) $(OBJS_MPP) $(OBJS_NOAHMP) $(NETCDF_LIB)


# This command cleans up
clean:
	$(RM) $(CMD) *~
