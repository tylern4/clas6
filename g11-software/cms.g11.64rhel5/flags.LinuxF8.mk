#
#  This file contains the default FORTRAN and C compile flags
#  for Linux platforms
#  $Id: flags.LinuxF8.mk,v 1.1 2008/07/10 14:51:48 ungaro Exp $
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

	FC = gfortran
	LNKCMD = gfortran
	FFLAGS += $(FLEVEL)
	FFLAFS += -fno-f2c
	FFLAGS += -fno-automatic
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
	CFLAGS += $(CLEVEL) 

# LD section

	FLDFLAGS += $(CLEVEL)
	FLDFLAGS += -fno-automatic
	FLDFLAGS += -ffixed-line-length-none
	FLDFLAGS += -fno-second-underscore


# c++ section

	CXX = g++
	CXXFLAGS += $(CXXLEVEL) 

# SL section:

DLLFLAGS= -O -Wall -fPIC
SOFLAGS=  -shared
SL_SUF = so
SLLD=g++

# RCS section

	CO = co
	COFLAGS =

# CPP section

	CPPFLAGS += -DLinux  

# archiver section

	AR = ar
	ARFLAGS = rv

# flags needed for dependency creation

	CPP = g77 -MM
        CPP_CXX = g++ -MM

# Tell the Makefile where the libraries are (X, TCL and CERNLIB)

	X_LIB += /usr/lib/X11/..
        X_INC += /usr/X11/include

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



