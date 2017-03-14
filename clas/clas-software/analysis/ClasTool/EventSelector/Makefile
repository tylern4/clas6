#   Makefile for CLASTOOL code.
#  
# Notice:
#
# At CEBAF you must execute the following before this will compile and link.
#
# SETUP YOUR CLAS ENVIRONMENT (source /group/clas/builds/../packages/cms/jlab.csh )
# SETUP YOUR ROOT ENVIRONMENT (use root       )
#
#
# REWRITE:
#
#	Maurik.Holtrop@unh.edu	May-12-2004
# 
#
# NOTES:
#
# SRC_CLASS: contains the source files (*.cc) of the CLASSES. A dictionary is made.
# SRC_FILES: contains the source files (*.cc) for the FUNCTIONS and PROGRAMS. No dictionary.
# INC_FILES: contains include files. This is only used to detect if we need to remake the Makefile_depends.
#
# The depends file will only be used if you keep the last include: 'include Makefile_depends'
# This scheme will still try to generate one (in Makefile_bottom).
#

SRC_CLASS = TCTEventSelector.cc TCTCompositeParticle.cc TCTParticle.cc TCTVector.cc  TCTVariable.cc TCTPhotonSelector.cc TCTStringUtils.cc TCTOperations.cc TCTTextReader.cc
INC_FILES = TCTEventSelector.h TCTCompositeParticle.h TCTParticle.h TCTVector.h TCTVariable.h TCTPhotonSelector.h TCTStringUtils.h TCTOperations.h TCTTextReader.h

CERNLIB   = -L$(CERNROOT)/lib -lpawlib -lpacklib -lkernlib -lmathlib -lg2c

include ../Makefile_top

INCLUDES +=  -I$(CLASTOOL)/../include  -I$(CLAS_PACK)/include
INCLUDES +=  -I/penta4/obrab/utilities/eloss/ -I/penta4/obrab/utilities/g11pcor

#
# Defininf NOSAFE during compilation creates somewhat faster code, but
# no idiot checking is done.
#

ifdef NOSAFE 
CXXFLAGS+= -DNOSAFE
endif

CREATED_LIB =   libCTEventSelector.a
CREATED_SHLIB = libCTEventSelector.so

OTHERLOADLIBS = -L$(ROOTSYS)/lib -lPhysics -lEG \
         -L$(localslib) -lClasBanks -lVirtualReader -lDSTReader \
         -L$(CLAS_LIB) -L$(TOP_DIR)/lib/$(OS_NAME)  
#	$(CERNLIB)

all: shlib

include ../Makefile_bottom

include Makefile_depends

