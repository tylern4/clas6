#
#  This file contains the default FORTRAN and C compile flags
#  for Linux platforms
#
# set the optimization level, to compile and link debug invoke the Makefile
# with "DEBUG=1" in the command line.
# 

	ifdef DEBUG
         FLEVEL +=  -g -fdebug-kludge
         CLEVEL +=  -g 
	 CXXLEVEL += -g
	 CPPFLAGS += -DDEBUG
	 ADD_DEBUG = _debug
	else
	 FLEVEL += -O2
	 CLEVEL += -O2
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

# Get the version information.  RedHat 5.x has some issues that need 
# to be dealt with.

	LINUX_VERSION = $(shell $(CLAS_CMS)/linux_version)
	LINUX_DIST =  $(shell $(CLAS_CMS)/linux_distrib)

# Find out if egcs is used for g++
EGCS  = $(shell g++ --version | awk -F- '{if($$1=="egcs")printf("egcs")}')

# FORTRAN section

	FC = g77
	LNKCMD = g77
	FFLAGS += $(FLEVEL)
	FFLAGS += -m486 -fno-automatic -finit-local-zero
	FFLAGS += -ffixed-line-length-none
	FFLAGS += -fno-second-underscore

# C section (vs. natural methods)

	CC = gcc
ifeq "$(EGCS)" "egcs"
	CC = egcs
endif
	CFLAGS += $(CLEVEL) -fwritable-strings

# LD section

	FLDFLAGS += $(CLEVEL)
	FLDFLAGS += -m486 -fno-automatic -finit-local-zero
	FLDFLAGS += -ffixed-line-length-none
	FLDFLAGS += -fno-second-underscore


# c++ section

	CXX = g++
ifeq "$(EGCS)" "egcs"
	CXXFLAGS += $(CXXLEVEL) -DEGCS -fno-rtti
else
	CXXFLAGS += $(CXXLEVEL) -fhandle-exceptions
endif

# SL section:

DLLFLAGS= -O -Wall -fPIC
SOFLAGS=  -shared
SL_SUF = so
SLLD=g++

# ROOT SECTION:EGCS

#ifeq "$(EGCS)" "egcs"
#  ROOT_FLAGS = -fno-rtti -fno-exceptions
#endif
ifeq "$(LINUX_DIST)" "Debian"
  ROOT_FLAGS =
endif

# RCS section

	CO = co
	COFLAGS =

# CPP section

	CPPFLAGS += -DLinux
	ifeq ($(LINUX_VERSION),5)
	CPPFLAGS += -DRedHat5
	endif

# archiver section

	AR = ar
	ARFLAGS = rv

# flags needed for dependency creation

	CPP = g77 -MM
        CPP_CXX = g++ -MM

# Tell the Makefile where the libraries are (X, TCL and CERNLIB)

	X_LIB += /usr/X11R6/lib
        X_INC += /usr/X11R6/include

# tell me where you keep libtcl.a  and the includes

#	ifndef TCL_LIB 
#          TCL_LIB = /usr/lib
#        endif
        ifndef TCL_INC
         TCL_INC = /usr/include
        endif

        TCL_VER = TCL75


# DEFAULT_INCLUDES defines the default search path for include files

	CTMP = $(CLAS_PACK)

	DEFAULT_INCLUDES += $(addprefix -I, \
               $(CTMP)/include $(CTMP)/inc_derived $(TCL_INC) $(X_INC))
#
#	DEFAULT_INCLUDES += -I/usr/include/g++
#



