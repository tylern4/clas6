#
#  This file contains the default FORTRAN and C compile flags
#  for OSF1 platforms
#
# set the optimization level, to compile and link debug invoke the Makefile
# with "DEBUG=1" in the command line.
# 

	ifdef DEBUG
         CLEVEL =  -g 
         FLEVEL =  -g 
         CPPFLAGS += -DDEBUG
	 ADD_DEBUG = _debug
	else
	 CLEVEL = -O0
	 FLEVEL = -O0
	 ADD_DEBUG = 
	endif

# FORTRAN section

	FC = f77
	LNKCMD = f77
	FFLAGS += $(FLEVEL)
	FFLAGS += -extend_source
	FFLAGS += -noautomatic
	FFLAGS += -vms
        FFLAGS += -fpe2

# C section (vs. natural methods)

	CC = gcc
	CFLAGS += $(CLEVEL)

# LD section

	FLDFLAGS += $(CLEVEL)
	FLDFLAGS += -extend_source
	FLDFLAGS += -noautomatic
	FLDFLAGS += -vms
#	FLDFLAGS += -nofor_main
	FLDFLAGS += -fpe2

# c++ section

	CXX = g++
	CXXFLAGS =

# RCS section

	CO = co
	COFLAGS =

# CPP section

	CPPFLAGS += -DLinux
	CPPFLAGS += -DOSF1

# archiver section

	AR = ar
	ARFLAGS = rv

# flags needed for dependency creation

	CPP = gcc -x c -traditional -MM
	CPP_CXX = g++ -x c -traditional -MM

# Tell the Makefile where the libraries are (X, TCL and CERNLIB)
# this is my current guess

	X_LIB += /usr/shlib/X11

# tell me where you keep libgcc.a

	GCC_LIB = /usr/local/lib/gcc-lib/alpha-dec-osf4.0/2.7.2.2
	LIB_PATH += $(GCC_LIB)

# tell me where you keep libtcl.a  and the includes

	ifndef TCL_LIB 
          TCL_LIB = /usr/local/lib
        endif
        ifndef TCL_INC
         TCL_INC = /usr/local/include
        endif

# tell me where you put Hv, define this if offsite.cshrc file

#HV_LOCATION = /net/fs1/site/clas/u1/Hv/Hv

# DEFAULT_INCLUDES defines the default search path for include files

#	CTMP = $(TOP_DIR)/packages
	CTMP = $(CLAS_PACK)

	DEFAULT_INCLUDES += $(addprefix -I, \
	     $(CTMP)/include $(CTMP)/inc_derived $(CTMP)/gsim/include \
	     $(TCL_INC) $(X_INC))
#




