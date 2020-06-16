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
 CXXLEVEL = -g
 CPPFLAGS += -DDEBUG
 ADD_DEBUG = _debug
else

ifdef ONLINE
 FLEVEL = -fast -KPIC
 CLEVEL = -fast -KPIC
 CXXLEVEL = -fast -KPIC
 ADD_DEBUG = 
else
 FLEVEL = -O2
 CLEVEL = -O
 CXXLEVEL = -O
 ADD_DEBUG = 
endif

endif

# ONLINE section. the fortran compiler is changed here for the
# online to accomodate the ipc software.

ifdef ONLINE
 FC = f77
 LNKCMD = CC=f77; export CC; $(RTHOME)/bin/rtlink -cxx -mt
 ADD_ONLINE = _online
else
 FC = f77
 LNKCMD = f77
 ADD_ONLINE = 
endif

# FORTRAN section

FFLAGS += $(FLEVEL) 
FFLAGS += -e -ftrap=no%inexact,no%underflow  

# C section (vs. natural methods)

CC = cc 
CFLAGS += $(CLEVEL)

# LD section

FLDFLAGS += $(CLEVEL) 
FLDFLAGS +=-ftrap=no%inexact,no%underflow 

# c++ section

CXX = CC 
CXXFLAGS = $(CXXLEVEL)
ifdef WS50
 CXXFLAGS += -compat
endif

#CXXFLAGS += -ptr/$(TOP_DIR)/slib/$(OS_NAME)

# SL section:

DLLFLAGS = -KPIC
SOFLAGS =  $(CXXLEVEL) -G
SL_SUF = so
SLLD = CC
USEDYNAMIC = -Bdynamic
USESTATIC = -Bstatic

# RCS section

CO = co
COFLAGS +=

# CPP section

CPPFLAGS += -DSunOS -D_SUNOS_SOURCE

# archiver section

AR = ar
ARFLAGS += rv

# GCC flag needed for dependency creation

CPP = gcc -x c -traditional -MM
CPP_CXX = g++ -MM

# Tell me where XLIB and X-header files are located

X_INC +=  /usr/dt/include /usr/openwin/share/include
X_LIB +=  /usr/dt/lib /usr/openwin/lib 

# tell me where you keep libtcl.a  and the includes

TCL_LIB += /apps/tcl/lib
TCL_INC += /apps/tcl/include

#MYSQL_INCLUDE_PATH = /u/apps/mysql/include/mysql
#MYSQL_LIB_PATH = /u/apps/mysql/lib/mysql

MYSQL_INCLUDE_PATH = /group/clas/mysql/SunOSws6/include
MYSQL_LIB_PATH = /group/clas/mysql/SunOSws6/lib


# tell me where you put Hv

#HV_LOCATION = /net/fs1/site/clas/u1/Hv/Hv

# DEFAULT_INCLUDES defines the default search path for include files

CTMP = $(CLAS_PACK)

DEFAULT_INCLUDES += $(addprefix -I, \
              $(CTMP)/include $(CTMP)/inc_derived $(TCL_INC) $(X_INC))

# End of flags.SunOS.mk













































