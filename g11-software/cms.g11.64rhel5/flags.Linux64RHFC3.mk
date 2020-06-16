#
#  This file contains the default FORTRAN and C compile flags
#  for Linux platforms
#  $Id: flags.Linux64RHFC3.mk,v 1.1 2005/04/21 13:02:05 holtrop Exp $
#
# set the optimization level, to compile and link debug invoke the Makefile
# with "DEBUG=1" in the command line.
# 
	ifdef DEBUG
         FLEVEL +=  -g
         CLEVEL +=  -g 
	 CXXLEVEL += -g
	 CPPFLAGS += -DDEBUG
	 ADD_DEBUG = _debug
	else
         ifndef NOOPTIMIZE
	   FLEVEL += -O3
	   CLEVEL += -O3
         endif
	 ADD_DEBUG = 
	endif

	ifdef GPROF
         FLEVEL += -pg
         CLEVEL += -pg         
	 ifdef DEBUG
         ADD_DEBUG = _debug_prof
	 else
         ADD_DEBUG = _prof
	 endif
        endif

# FORTRAN section

	FC = g77
	LNKCMD = g77
	FFLAGS += $(FLEVEL)
	FFLAFS += -fno-f2c
	FFLAGS += -m64 -fno-automatic -finit-local-zero
	FFLAGS += -ffixed-line-length-none
	FFLAGS += -fno-second-underscore
#
# Note: The -fno-f2c option is very important for mixed FORTRAN/C environment
# where there are C functions that return a float to some FORTRAN code. The "f2c" 
# calling convention wants all routines to use double only. CLAS code is not
# consistend in this.
#

# C section (vs. natural methods)

	CC = gcc
	CFLAGS += $(CLEVEL) -m64 -fwritable-strings

# LD section

	FLDFLAGS += $(CLEVEL)
	FLDFLAGS += -m64 -fno-automatic -finit-local-zero
	FLDFLAGS += -ffixed-line-length-none
	FLDFLAGS += -fno-second-underscore


# c++ section

	CXX = g++
	CXXFLAGS += $(CXXLEVEL) -m64

# SL section:

DLLFLAGS= -O -Wall -fPIC
SOFLAGS=  -shared
SL_SUF = so
SLLD=g++

# RCS section

	CO = co
	COFLAGS =

# CPP section

	CPPFLAGS += -DLinux  -DLinux64

# archiver section

	AR = ar
	ARFLAGS = rv

# flags needed for dependency creation

	CPP = g77 -MM
        CPP_CXX = g++ -MM

# Tell the Makefile where the libraries are (X, TCL and CERNLIB)

	X_LIB += /usr/X11R6/lib64/
        X_INC += /usr/X11R6/include

# tell me where you keep the tcl includes

        ifndef TCL_INC
         TCL_INC = /usr/include
        endif

        TCL_VER = TCL80

# DEFAULT_INCLUDES defines the default search path for include files

	CTMP = $(CLAS_PACK)

	DEFAULT_INCLUDES += $(addprefix -I, \
               $(CTMP)/include $(CTMP)/inc_derived $(TCL_INC) $(X_INC))
#
#	DEFAULT_INCLUDES += -I/usr/include/g++
#



