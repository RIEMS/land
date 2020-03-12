.SUFFIXES:
.SUFFIXES: .o .f

ifeq ($(WRF_HYDRO_RAPID),1)
include ${TAO_DIR}/bmake/tao_common
endif

include ../makefile.in

ifeq ($(WRF_HYDRO_RAPID),1)
PHDF5_INC=-I ${TACC_HDF5_INC}
RAPID_MACRO = ${TAO_FORTRAN_LIB} ${TAO_LIB} ${PETSC_LIB} ${PHDF5_INC} \
        -Wl,-rpath,${TACC_HDF5_LIB} -L${TACC_HDF5_LIB} -lhdf5 -lz
RAPID_LIB =  -lrapid
else
RAPID_MACRO =
RAPID_LIB =
endif

OBJS_NOAHMP = ../ldas/module_ldas_noahmp.o

OBJS = \
	../ldas/main_ldas.o \
	../ldas/module_ldas_config.o \
	../ldas/module_ldas_io_netcdf.o \
	../phys/module_sf_noahmpdrv.o \
	../phys/module_sf_noahmplsm.o \
	../phys/module_sf_noahmp_glacier.o \
	../phys/module_sf_noahmp_groundwater.o \
	../phys/module_sf_bgc.o \
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
	@echo ""
	echo "${TAO_FORTRAN_LIB} ${TAO_LIB} ${PETSC_LIB} ${PHDF5_INC} -Wl,-rpath,${TACC_HDF5_LIB} -L${TACC_HDF5_LIB} -lhdf5 -lz"
# We have to include the modules built in ../ldas
	$(F90) -o $(@) -I../ldas -I../phys $(OBJS) $(OBJS_NOAHMP) $(MPP_LIB) $(RAPID_LIB) $(NETCDF_LIB) $(RAPID_MACRO)

	@echo ""

# Template to create the exe file based on different land model. Such as NoahMP
NOAHMP: $(OBJS)
	@echo ""
	$(F90) -o $(CMD) -I../ldas -I../phys $(OBJS) $(OBJS_NOAHMP) $(MPP_LIB) $(NETCDF_LIB)
	@echo ""

# This command cleans up
clean:
	$(RM) $(CMD) *~
