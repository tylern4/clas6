#
#  This file contains the default FORTRAN and C compile flags
#  for HP-UX platforms
#
#
# set the optimization level, to compile and link debug invoke the Makefile
# with "DEBUG=1" in the command line.
# 

ifdef DEBUG
  FLEVEL +=  -g -C
  CLEVEL +=  -g -C
  CXXLEVEL += -g -C
  CPPFLAGS += -DDEBUG
  ADD_DEBUG = _debug
else
  CLEVEL = -O
  FLEVEL = -O2
  ADD_DEBUG =
endif

# FORTRAN section

FC = fort77
LNKCMD = fort77
FFLAGS += $(FLEVEL)
FFLAGS += -cpp +es +ppu -K

# C section (vs. natural methods)

CC = cc
CFLAGS += $(CLEVEL)
CFLAGS +=  -Aa -Ae

# LD section

FLDFLAGS += $(FLEVEL)
FLDFLAGS += +FPZO

# c++ section

CXX = aCC
CXXFLAGS += $(CXXLEVEL) +eh

# SL section

DLLFLAGS = -z +Z
SOFLAGS = -b -z
SL_SUF = sl
SLLD = CC
USEDYNAMIC = -Bdynamic
USESTATIC = -Bstatic 

# RCS section

CO = co
COFLAGS +=

# CPP section

CPPFLAGS += -DHPUX

# archiver section

AR = ar
ARFLAGS += rv

# CPP flag needed for dependency creation

CPP = gcc -x c -traditional -MM
CPP_CXX = g++ -MM

#  define location  X and any other special directories that hold
#  libraries and includes for HP-UX

X_LIB += /usr/lib/Motif1.2_R6 /usr/lib/X11R6
X_INC += /usr/include/Motif1.2

#  define location of  TCL libraries and includes for HP-UX

ifndef TCL_LIB 
  TCL_LIB = /apps/tcl/lib
endif
ifndef TCL_INC
  TCL_INC = /apps/tcl/include
endif

# tell me where you put Hv, define this in the offsite.cshrc file

#HV_LOCATION = /net/fs1/site/clas/u1/Hv/Hv

# DEFAULT_INCLUDES defines the default search path for include files

CTMP = $(CLAS_PACK)

DEFAULT_INCLUDES += $(addprefix -I, \
               $(CTMP)/include $(CTMP)/inc_derived $(TCL_INC) $(X_INC))

#








