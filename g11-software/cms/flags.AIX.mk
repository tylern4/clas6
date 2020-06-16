#
#  This file contains the default FORTRAN and C compile flags
#  for SunOS platforms
#
#
# set the optimization level, to compile and link debug invoke the Makefile
# with "DEBUG=1" in the command line.
# 

ifdef DEBUG
  FLEVEL =  -g -C
  CLEVEL =  -g -C
  # CPPFLAGS += -DDEBUG # get rid of this for now, causes trouble
  ADD_DEBUG = _debug
else
  FLEVEL = -O2
  CLEVEL = -O2
  ADD_DEBUG = 
endif

# FORTRAN section

FC = f77
LNKCMD = f77
FFLAGS += $(FLEVEL)
FFLAGS += -qfixed=132 -qextname

# C section (vs. natural methods)

CC = cc
CFLAGS += $(CLEVEL)

# LD section

FLDFLAGS += $(CLEVEL)

# c++ section

CXX = CC_r4
CXXFLAGS +=

# RCS section

CO = co
COFLAGS +=

# CPP section on AIX you need to add -WF, before any CPP flags....

CPPFLAGS += -DAIX -DAIX_SOURCE

# archiver section

AR = ar
ARFLAGS += rv

# GCC flag needed for dependency creation

CPP = gcc -x c -traditional -MM
CPP_CXX = g++ -MM

# tell me TCL and X libraries are

X_LIB += /usr/lib/Motif1.2 /usr/lib/X11R5

TCL_LIB += /usr/local/lib
TCL_INC += /usr/local/include

# tell me where you put Hv, define this in offsite.cshrc file

#HV_LOCATION = /net/fs1/site/clas/u1/Hv/Hv

# DEFAULT_INCLUDES defines the default search path for include files

CTMP = $(CLAS_PACK)

DEFAULT_INCLUDES += $(addprefix -I, \
                 $(CTMP)/include $(CTMP)/inc_derived $(TCL_INC) $(X_INC))

# on AIX the -I prefix is not sufficient, so you need to tell the
# Makefile to ADD -WF, before the -I.  These flags are only invoked
# when compiling the code.

	INC_FLAG = -WF,

#



