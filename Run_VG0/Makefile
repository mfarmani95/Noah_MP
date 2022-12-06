# Makefile 
#
#INCLUDES=
.SUFFIXES:
.SUFFIXES: .o .f

include ../macros

MAINOBJ1 = ../IO_code/Noah_driver.o

OBJS = \
        ../Noah_code/module_Noahlsm_mixedRE.o \
	../Noah_code/module_Noahlsm.o \
	../Noah_code/module_sf_noahmp_groundwater.o \
	../Noah_code/module_sf_urban.o \
	../Noah_code/module_sf_bep.o \
	../Noah_code/module_sf_bem.o \
	../Noah_code/module_sf_bep_bem.o \
	../Noah_code/module_Noahlsm_utility.o \
	../Noah_code/module_date_utilities.o \
	../IO_code/module_Noah_NC_output.o \
	../IO_code/module_Noahlsm_gridded_input.o \
	../IO_code/module_wrf_error.o \
	../IO_code/wrf_debug.o

CMD = Noah
all:	$(CMD)

Noah: $(OBJS) $(MAINOBJ1)
	@echo ""
#	$(COMPILER90) -o $(@) $(OBJS) $(MAINOBJ1) -L/home/u4/niug/local/lib -lnetcdff
	$(COMPILER90) -o $(@) $(OBJS) $(MAINOBJ1) -L/opt/ohpc/pub/apps/netcdf-fortran/4.5.3/lib -lnetcdff
	@echo ""

# This command cleans up

clean:
	rm ../IO_code/*mod ../IO_code/*.o
	rm ../Noah_code/*mod ../Noah_code/*.o
	rm $(CMD)
