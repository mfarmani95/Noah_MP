# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS = \
	module_Noah_NC_output.o \
	module_Noahlsm_gridded_input.o \
	Noah_driver.o \
        module_wrf_error.o \
        wrf_debug.o

all:	$(OBJS)

.F.o:
	@echo ""
	$(RM) $(*).f
	$(CPP) $(CPPFLAGS) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(FREESOURCE) $(*).f
	$(RM) $(*).f
	@echo ""
#
# Dependencies:
#

Noah_driver.o:  ../Noah_code/module_Noahlsm.o
Noah_driver.o:  ../Noah_code/module_date_utilities.o
Noah_driver.o:  ../Noah_code/module_Noahlsm_utility.o
Noah_driver.o:  module_Noahlsm_gridded_input.o

# This command cleans up object files
clean:
	$(RM) *.o *.mod *.stb *~

