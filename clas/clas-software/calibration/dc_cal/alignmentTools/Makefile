#! gnumake

#---------------------------------------------------
# The D. P. Weygand Makefile
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
ROOTINCLUDE	= $(ROOTSYS)/include

STDLIBS= -L$(LOCAL_LIB) $(ROOTLIBS) $(ROOTGLIBS) 

#use the newest cern library
CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -L$(CERNPATH)  -lpacklib -lmathlib -lkernlib
MATHLIB= -lm $(ROOTLIBS) $(ROOTGLIBS) 

FINCLUDE := $(FINCLUDE) -I./include
INCLUDE := $(INCLUDE) -I./include -I$(ROOTINCLUDE) 
CCCSRCS:= 

CCSRCS:= 

fSRCS:= 

FSRCS:= 

SRCS:= 

fOBJS= $(patsubst %.f,$(OBJDIR)/%.o,$(fSRCS))
FOBJS= $(patsubst %.F,$(OBJDIR)/%.o,$(FSRCS))
OBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(SRCS))

CCOBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(CCSRCS))

CCCOBJS= $(patsubst %.C,$(OBJDIR)/%.o,$(CCCSRCS))

all: plotResiVsLayer getMeansOfResi install scripts

plotResiVsLayerSRCS= plotResiVsLayer.cc
plotResiVsLayerOBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(plotResiVsLayerSRCS))

plotResiVsLayer: $(plotResiVsLayerOBJS)
	$(C++) $(CDBG) $(CFLAGS) $(INCLUDE) -o $@ $^ $(STDLIBS) $(CERNLIB) $(FLIB) $(FLIB_NATIVE) $(MATHLIB)

getMeansOfResiSRCS= getMeansOfResi.cc
getMeansOfResiOBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(getMeansOfResiSRCS))

getMeansOfResi: $(getMeansOfResiOBJS)
	$(C++) $(CDBG) $(CFLAGS) $(INCLUDE) -o $@ $^ $(STDLIBS) $(CERNLIB) $(FLIB) $(FLIB_NATIVE) $(MATHLIB)

scripts: 
	cp *.sh $(LOCAL_BIN)

LIBS= $(LOCAL_LIB)/lib$(ADD_DEBUG).a
lib: $(LIBS)

$(LIBS): $(OBJS) $(FOBJS) $(fOBJS) $(CCOBJS) $(CCCOBJS)
	$(VDIR) $(LOCAL_LIB)
	ar rv $@ $?

install: plotResiVsLayer getMeansOfResi
	$(VDIR) $(LOCAL_BIN)
	/usr/bin/install  plotResiVsLayer $(LOCAL_BIN)
	/usr/bin/install  getMeansOfResi  $(LOCAL_BIN)

env:
	@echo EXE $(EXE)
	@echo LIBS $(LIBS)
	@echo CC $(CC)
	@echo FC $(FC)
	@echo OS_NAME $(OS_NAME)
	@echo INCLUDE $(INCLUDE)
	@echo BOSPATH $(BOSPATH)
	@echo FLIB $(FLIB)
	@echo OBJS $(OBJS)
	@echo CFLAGS $(CFLAGS)
	@echo FFLAGS $(FFLAGS)
	@echo CCLAGS $(CCFAGS)
	@echo INCLUDE $(INCLUDE)
	@echo LOCAL_LIB $(LOCAL_LIB)
	@echo LOCAL_BIN $(LOCAL_BIN)
	@echo LOCAL_INCLUDE $(LOCAL_INCLUDE)
	@echo OBJDIR $(OBJDIR)
	@echo STDLIBS $(STDLIBS)
	@echo ROOTINCLUDE $(ROOTINCLUDE)
	@echo ROOTLIBS $(ROOTLIBS)
	@echo ROOTGLIBS $(ROOTGLIBS)
