#! gnumake

#---------------------------------------------------
#  Makefile for bos2ntuples
#---------------------------------------------------
include $(CLAS_CMS)/Rules.make

ifdef USEBOSIO
BOSLIB= -lbosio$(ADD_DEBUG)
else
BOSLIB = -lfputil$(ADD_DEBUG) -lbos$(ADD_DEBUG) -lfpack$(ADD_DEBUG)
endif
ROOTCFLAGS	= $(shell root-config --prefix=$(ROOTSYS) --cflags)
ROOTLIBS	= $(shell root-config  --libs)
ROOTGLIBS	= $(shell root-config  --glibs)
ROOTINCLUDE	= $(shell root-config  --incdir)
MYSQL_LIB_PATH = $(MYSQL_ROOT_PATH)/lib/mysql
MYSQL_INCLUDE_PATH = $(MYSQL_ROOT_PATH)/include
ifndef MAP
	MAPLIB = -lcaldbMap$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -L$(MYSQL_LIB_PATH) -lmysqlclient -lz $(TCLLIB) 
INCLUDE += -I$(MYSQL_INCLUDE_PATH)
else
	MAPLIB = -lmapmanager$(ADD_DEBUG)
endif

STDLIBS= -L$(LOCAL_LIB) -L$(CLAS_LIB)  -ltcl -lclasEvent$(ADD_DEBUG) -leloss$(ADD_DEBUG) -lmomentum_correction$(ADD_DEBUG) -lg10pcor$(ADD_DEBUG) -lPcor$(ADD_DEBUG) -lvkTAGR$(ADD_DEBUG) -lplib$(ADD_DEBUG) -lpid$(ADD_DEBUG) -lec$(ADD_DEBUG) -lcc$(ADD_DEBUG) -lsc$(ADD_DEBUG) -ltrk$(ADD_DEBUG) -ldc$(ADD_DEBUG) -llac$(ADD_DEBUG) -lst$(ADD_DEBUG)  -lpid$(ADD_DEBUG) -ltag$(ADD_DEBUG) -ltagM$(ADD_DEBUG) -lscaler$(ADD_DEBUG) -lvertex$(ADD_DEBUG) -lbankdefs$(ADD_DEBUG) -lclasutil$(ADD_DEBUG)   -lc_bos_io$(ADD_DEBUG) -lrecutl$(ADD_DEBUG) -litape$(ADD_DEBUG) $(BOSLIB) -lc_bos_io$(ADD_DEBUG) $(MAPLIB)  -lonline_dummy$(ADD_DEBUG) -lc_cern$(ADD_DEBUG) -lpp$(ADD_DEBUG) -lpwaUtil$(ADD_DEBUG) $(ROOTLIBS) $(ROOTGLIBS) 

#use the newest cern library
CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -L$(CERNPATH)  -lpacklib -lmathlib -lkernlib
MATHLIB= -lm $(ROOTLIBS) $(ROOTGLIBS) 

FINCLUDE := $(FINCLUDE) -I./include
INCLUDE := $(INCLUDE) -I./include -I$(ROOTINCLUDE) 

fOBJS= $(patsubst %.f,$(OBJDIR)/%.o,$(fSRCS))
FOBJS= $(patsubst %.F,$(OBJDIR)/%.o,$(FSRCS))
OBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(SRCS))

CCOBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(CCSRCS))

CCCOBJS= $(patsubst %.C,$(OBJDIR)/%.o,$(CCCSRCS))

p3piNtpSRCS= p3piNtp.cc TNtupleUtil.cc
p3piNtpOBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(p3piNtpSRCS))

3piNtpSRCS= 3piNtp.cc TNtupleUtil.cc
3piNtpOBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(3piNtpSRCS))

4piNtpSRCS= 4piNtp.cc TNtupleUtil.cc
4piNtpOBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(4piNtpSRCS))

#
##
#

p3piNtp: $(p3piNtpOBJS)
	$(C++) $(CDBG) $(CFLAGS) $(INCLUDE) -o $@ $^ $(STDLIBS) $(CERNLIB) $(FLIB) $(FLIB_NATIVE) $(MATHLIB)

3piNtp: $(3piNtpOBJS)
	$(C++) $(CDBG) $(CFLAGS) $(INCLUDE) -o $@ $^ $(STDLIBS) $(CERNLIB) $(FLIB) $(FLIB_NATIVE) $(MATHLIB)

4piNtp: $(4piNtpOBJS)
	$(C++) $(CDBG) $(CFLAGS) $(INCLUDE) -o $@ $^ $(STDLIBS) $(CERNLIB) $(FLIB) $(FLIB_NATIVE) $(MATHLIB)

all:  p3piNtp 3piNtp 4piNtp

