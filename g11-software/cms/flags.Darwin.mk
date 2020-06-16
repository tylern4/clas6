#
#  This file contains the default FORTRAN and C compile flags
#  for Linux platforms
#  $Id: flags.Darwin.mk,v 1.1 2007/08/22 02:40:04 holtrop Exp $
#
# set the optimization level, to compile and link debug invoke the Makefile
# with "DEBUG=1" in the command line.
# 

	ifdef DEBUG
         FLEVEL +=  -g -fbounds-check
         CLEVEL +=  -g 
	 CXXLEVEL += -g
	 CPPFLAGS += -DDEBUG
	 ADD_DEBUG = _debug
	else
         ifndef NOOPTIMIZE
	   FLEVEL += -O2
	   CLEVEL += -O2
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

#	FC = gfortran
#	LNKCMD = gfortran
#	FFLAGS += $(FLEVEL)
#	FFLAGS += -fno-automatic -ff2c

	FC = g77
	LNKCMD = g77
	FFLAGS += $(FLEVEL)
	FFLAGS += -fno-automatic -finit-local-zero
	FFLAGS += -ffixed-line-length-none
	FFLAGS += -fno-second-underscore

# C section (vs. natural methods)

	CC = gcc
	CFLAGS += $(CLEVEL)

# LD section

	FLDFLAGS += $(CLEVEL)

# c++ section

	CXX = g++
	CXXFLAGS += $(CXXLEVEL)

# SL section:

DLLFLAGS= -O -Wall -fPIC
SOFLAGS=  -dynamiclib -single_module -undefined dynamic_lookup -install_name $@ 
SL_SUF = so
SLLD=g++-4

# RCS section

	CO = co
	COFLAGS =

# CPP section

	CPPFLAGS += -DLinux -DDarwin

# archiver section

	AR = ar
	ARFLAGS = -r -v

# flags needed for dependency creation

	CPP = /sw/bin/gcc-4 -MM
        CPP_CXX = /sw/bin/g++-4 -MM

# Tell the Makefile where the libraries are (X, TCL and CERNLIB)

	X_LIB += /usr/lib/X11/..
        X_INC += /usr/X11R6/include
	X_INC += /sw/include

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



