#
#  This file contains the default FORTRAN and C compile flags
#  for SunOS platforms
#
# set the optimization level, to compile and link debug invoke the Makefile
# with "DEBUG=1" in the command line.
# 

	ifdef DEBUG
         FLEVEL =  -g -C
         CLEVEL =  -g -C
	 ADD_DEBUG = _debug
	else
	 FLEVEL = -O2
	 CLEVEL = -O
	 ADD_DEBUG = 
	endif

# FORTRAN section

	FC = f77
	LNKCMD = f77
	FFLAGS += $(FLEVEL)  -col120
	FFLAGS += -e -ftrap=no%inexact,no%underflow  

# C section (vs. natural methods)

	CC = cc 
	CFLAGS += $(CLEVEL)

# LD section

	FLDFLAGS += $(CLEVEL) 
	FLDFLAGS +=-ftrap=no%inexact,no%underflow 

# c++ section

	CXX = g++
	CXXFLAGS +=

# RCS section

	CO = co
	COFLAGS +=

# CPP section

	CPPFLAGS += -DIRIX

# archiver section

	AR = ar
	ARFLAGS += rv

# GCC flag needed for dependency creation

	CPP = gcc -x c -traditional -MM
	CPP_CXX = g++ -x c -traditional -MM

# Tell me where XLIB and X-header files are located

	X_INC +=  /usr/dt/include /usr/openwin/share/include
	X_LIB +=  /usr/dt/lib /usr/openwin/lib 

# tell me where you keep libtcl.a  and the includes

         TCL_LIB += /usr/local/lib
         TCL_INC += /usr/local/include


# DEFAULT_INCLUDES defines the default search path for include files

        CTMP = $(CLAS_PACK)

        DEFAULT_INCLUDES = $(addprefix -I, \
		$(CTMP)/include $(CTMP)/inc_derived $(TCL_INC) $(X_INC))

# End of flags.IRIX64.mk

