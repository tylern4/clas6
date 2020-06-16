#
#  This file contains the default FORTRAN and C compile flags
#  for Linux platforms
#  $Id: flags.LinuxRHFC8.mk,v 1.3 2008/09/09 21:14:10 turonski Exp $
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
	   FLEVEL += -O
	   CLEVEL += -O
	   CXXLEVEL += -O
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

	ifdef GFORTRAN
	 FC = gfortran
	 LNKCMD = gfortran
	 FFLAGS += $(FLEVEL)
	 FFLAFS += -fno-f2c
#	 FFLAFS += -m32
	 FFLAGS += -fno-automatic 
#	 FFLAFS += -fPIC
	 FFLAGS += -ffixed-line-length-none
	 FFLAGS += -fno-second-underscore
	 FFLAGS += -funroll-loops
	 FFLAGS += -fomit-frame-pointer
	else	
	 FC = g77
	 LNKCMD = g77
	 FFLAGS += $(FLEVEL)
	 FFLAFS += -fno-f2c 
#	 FFLAGS += -m32
	 FFLAGS += -fno-automatic
	 FFLAGS += -finit-local-zero
#	 FFLAGS += -fPIC
	 FFLAGS += -ffixed-line-length-none
	 FFLAGS += -fno-second-underscore
	endif
#
# Note: The -fno-f2c option is very important for mixed FORTRAN/C environment
# where there are C functions that return a float to some FORTRAN code. The "f2c" 
# calling convention wants all routines to use double only. CLAS code is not
# consistend in this.
#

# C section (vs. natural methods)

	CC = gcc
	CFLAGS += $(CLEVEL) -Wall

# LD section

	FLDFLAGS += $(CLEVEL)
	FLDFLAGS += -fno-automatic
	FLDFLAGS += -finit-local-zero
	FLDFLAGS += -ffixed-line-length-none
	FLDFLAGS += -fno-second-underscore


# c++ section

	CXX = g++
	CXXFLAGS += $(CXXLEVEL) -Wall

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

	ifdef GFORTRAN
	 CPP = gfortran -MM
	else
	 CPP = g77 -MM
	endif
        CPP_CXX = g++ -MM

# Tell the Makefile where the libraries are (X, TCL and CERNLIB)

#	X_LIB += /usr/lib/X11/..
#	X_INC += /usr/X11/include
	X_LIB += /usr/X11R6/lib/
	X_INC += /usr/X11R6/include

# tell me where you keep the tcl includes

#	TCL_INC = /home/clasg9/include
        ifndef TCL_INC
         TCL_INC = /usr/include
        endif

        TCL_VER = tcl8.4

# DEFAULT_INCLUDES defines the default search path for include files

	CTMP = $(CLAS_PACK)

	DEFAULT_INCLUDES += $(addprefix -I, \
               $(CTMP)/include $(CTMP)/inc_derived $(TCL_INC) $(X_INC))
        ifdef USE_ROOT_SYSTEM 
	 DEFAULT_INCLUDES += -I$(shell $(ROOTSYS)/bin/root-config --incdir)
        endif
#
#	DEFAULT_INCLUDES += -I/usr/include/g++
#



