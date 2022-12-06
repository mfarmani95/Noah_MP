# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F
include ../macros

OBJS = \
	module_date_utilities.o \
	module_Noahlsm_utility.o \
        module_Noahlsm_mixedRE.o \
	module_Noahlsm.o \
        module_sf_noahmp_groundwater.o \
        module_sf_urban.o \
        module_sf_bep.o \
        module_sf_bem.o \
        module_sf_bep_bem.o

all:	$(OBJS)

.F.o:
	@echo ""
	$(RM) $(*).f
	$(CPP) $(CPPFLAGS) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(FREESOURCE) $(*).f
#	$(RM) $(*).f
	@echo ""

#
# Dependencies:
#

#module_Noahlsm.o:	module_Noahlsm_param_init.o


	$ cp *.mod ../IO_code/.

#
# This command cleans up object (etc) files:
#

clean:
	$(RM) *.o *.mod *.stb *~

