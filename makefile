####################################################################
####################################################################
#                                                                  #
#  This makefile may have to be modified for your particular       #
#  system and computing environment.  Most likely to change        #
#  from system to system are:                                      #
#       COMPILER90 : the command to invoke the Fortran 90 compiler #
#       FREESOURCE : the Fortran 90 option to specify free-form    #
#                    source code                                   #
#       MODFLAG    : the Fortran 90 option to indicate other       #
#                    directories to search for modules             #
#                                                                  #
####################################################################
####################################################################
#                                                                  #
# For Linux with Portland Group Compilers, try                     #
# (filling in the appropriate path to the compiler, if necessary)  #
#    COMPILER90 = $(PATH_TO_PORTLAND_GROUP_COMPILER)/pgf90         #
#    FREESOURCE =       -Mfree                                     #
#    F90FLAGS   =       -c                                         #
#    MODFLAG    =       -I                                         #
#    LDFLAGS    =                                                  #
#    CPP        =       cpp                                        #
#    CPPFLAGS   =       -C -P -traditional                         #
#                                                                  #
####################################################################
####################################################################
#                                                                  #
# For Linux with Intel Fortran compiler, try                       #
# (filling in the appropriate path to the compiler, if necessary)  #
#                                                                  #
#    COMPILER90 = $(PATH_TO_INTEL_COMPILER)/ifort                  #
#    FREESOURCE =       -FR                                        #
#    F90FLAGS   =       -c                                         #
#    MODFLAG    =       -I                                         #
#    LDFLAGS    =                                                  #
#    CPP        =       cpp                                        #
#    CPPFLAGS   =       -C -P -traditional                         #
#                                                                  #
####################################################################
####################################################################
#                                                                  #
# For Linux with AbSoft compiler, try                              #
# (filling in the appropriate path to the compiler, if necessary)  #
#                                                                  #
#    COMPILER90 = $(PATH_TO_ABSOFT_COMPILER)/f90                   #
#    FREESOURCE =       -f free                                    #
#    F90FLAGS   =       -c                                         #
#    MODFLAG    =       -p                                         #
#    LDFLAGS    =                                                  #
#    CPP        =       cpp                                        #
#    CPPFLAGS   =       -C -P -traditional                         #
#                                                                  #
####################################################################
####################################################################
#

.IGNORE:

default: macros
	@( make -f Makefile)
	@rm -f .tmpfile

macros:
	@echo ".IGNORE:" 						>> macros
	@echo "RM		=	rm -f"				>> macros
	@uname -a > .tmpfile
	@grep OSF .tmpfile ; \
	if [ $$? = 0 ] ; then echo "Compiling for Compaq"				; \
		echo "COMPILER90=	f90"				>> macros	; \
		echo "FREESOURCE=	-free"				>> macros	; \
		echo "F90FLAGS  =       -c -convert big_endian"		>> macros	; \
		echo "MODFLAG	=	-I"				>> macros	; \
		echo "LDFLAGS	=	-fast"				>> macros	; \
		echo "CPP	=       cpp"				>> macros	; \
		echo "CPPFLAGS	=       -C -P -traditional"		>> macros	; \
		echo "LIBS 	=	"			 	>> macros ;\
	else grep Linux .tmpfile 							; \
	if [ $$? = 0 ] ; then echo "Compiling for Linux"				; \
		echo "COMPILER90=	ifort"				>> macros	; \
		echo "FREESOURCE=	-FR"				>> macros	; \
		echo "F90FLAGS  =       -c -convert big_endian" 	>> macros	; \
		echo "MODFLAG	=	-I"				>> macros	; \
		echo "LDFLAGS	=	" 				>> macros	; \
		echo "CPP	=       cpp"				>> macros	; \
		echo "CPPFLAGS	=       -C -P -traditional"		>> macros	; \
		echo "LIBS 	=	" 				>> macros ;\
	else grep AIX .tmpfile								; \
	if [ $$? = 0 ] ; then echo "Compiling for IBM"					; \
		echo "COMPILER90=	xlf"				>> macros	; \
		echo "FREESOURCE=	-qfree=f90"			>> macros	; \
		echo "F90FLAGS  =       -c"		                >> macros	; \
		echo "MODFLAG	=	-I"				>> macros	; \
		echo "LDFLAGS	=	" 				>> macros	; \
		echo "CPP	=       cpp"				>> macros	; \
		echo "CPPFLAGS	= 	-C -P -traditional"		>> macros	; \
	else grep SunOS .tmpfile							; \
	if [ $$? = 0 ] ; then echo "Compiling for Sun"					; \
		echo "COMPILER90=	f90"				>> macros	; \
		echo "FREESOURCE=	-free"				>> macros	; \
		echo "F90FLAGS  =       -c -I. -I/home/sn21/niu/NCARSOFTWARE/netcdf-3.5/include">> macros	; \
		echo "MODFLAG	=	-M"				>> macros	; \
		echo "LDFLAGS	=	-fast -stackvar -O3 -dalign -parallel" 		>> macros	; \
		echo "CPP	=       cpp"				>> macros	; \
		echo "CPPFLAGS	= 	-C -P -traditional"		>> macros	; \
	else grep IRIX .tmpfile								; \
	if [ $$? = 0 ] ; then echo "Compiling for SGI"					; \
		echo "COMPILER90=	f90"				>> macros	; \
		echo "FREESOURCE=	-freeform"			>> macros	; \
		echo "F90FLAGS  =       -c -I."				>> macros	; \
		echo "MODFLAG	=	-I"				>> macros	; \
		echo "LDFLAGS	=	" 				>> macros	; \
		echo "CPP	=       cpp"				>> macros	; \
		echo "CPPFLAGS	= 	-C -P -traditional"		>> macros	; \
	else echo "Do not know how to compile for the `cat .tmpfile` machine."		; \
	fi ; \
	fi ; \
	fi ; \
	fi ; \
	fi
	@rm -f .tmpfile

clean:	macros
	@echo "RM = rm -f" >> macros
	@( make -f Makefile clean )
	rm -f  *~
