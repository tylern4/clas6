#! gnumake

CC = g++
#---------------------------------------------------
# The D. P. Weygand Makefile
#---------------------------------------------------
include $(CLAS_CMS)/Rules.make

CC = g++

ifdef MAP
ADD_MAP= _map
endif

ifdef USEBOSIO
BOSLIB = -lbosio$(ADD_DEBUG)
else
BOSLIB = -lfputil$(ADD_DEBUG) -lbos$(ADD_DEBUG) -lfpack$(ADD_DEBUG)
endif

RECLIB = -L$(LOCAL_LIB) -L$(CLAS_LIB)  -litape$(ADD_DEBUG) -ltrk$(ADD_DEBUG) -lpid$(ADD_DEBUG) -ltag$(ADD_DEBUG) -ltagM$(ADD_DEBUG) -llac$(ADD_DEBUG) -lseb$(ADD_DEBUG) -lst$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -lsc$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG)  -ldc$(ADD_DEBUG) -lec$(ADD_DEBUG) -lcc$(ADD_DEBUG)  -lsc$(ADD_DEBUG) -lvertex$(ADD_DEBUG) -lbankdefs$(ADD_DEBUG) -lrecutl$(ADD_DEBUG) -lonline_dummy$(ADD_DEBUG)  $(BOSLIB)

ifndef MAP
        RECLIB += -lcaldbMap$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -L$(MYSQL_LIB_PATH) -lmysqlclient -lz
else
        RECLIB += -lmapmanager$(ADD_DEBUG)
endif

TCLLIB_Linux        := /usr/lib/libtcl8.3.so
TCLLIB_LinuxRHEL3   := /usr/lib/libtcl8.3.so
TCLLIB_LinuxRHEL4   := /usr/lib/libtcl8.3.so
TCLLIB_Linux64RHFC3   := /usr/lib/libtcl8.3.so
TCLLIB_LinuxRHFC8   := /usr/lib/libtcl8.4.so
TCLLIB:= $(TCLLIB_$(OS_NAME))

#use the newest cern library
CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -L$(CERNPATH) -lpacklib
MATHLIB= -lm

ROOTLIBS = `$(ROOTSYS)/bin/root-config --glibs`

FINCLUDE := $(FINCLUDE) -I./include
INCLUDE := $(INCLUDE) -I./include -I`$(ROOTSYS)/bin/root-config --incdir`

CCSRCS:= 

FSRCS:= 

SRCS:=  

FOBJS= $(patsubst %.F,$(OBJDIR)/%.o,$(FSRCS))
OBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(SRCS))

CCOBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(CCSRCS))

g12_monitorSRCS= g12_monitor.cc
g12_monitorOBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(g12_monitorSRCS))
g12_monitorDictSRCS= g12_monitorDict.cc
g12_monitorDictOBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(g12_monitorDictSRCS))

g12_monitor: $(g12_monitorOBJS) $(g12_monitorDictOBJS)
	$(CC) $(CDBG) $(CFLAGS) $(INCLUDE) -o $@ $^ $(RECLIB) $(CERNLIB) $(ROOTLIBS) $(FLIB) $(FLIB_NATIVE) $(MATHLIB) $(TCLLIB)

g12_monitorDict.cc: g12_monitor.h g12_monitorLinkDef.h
	$(ROOTSYS)/bin/rootcint -f $@ -c $^

all: g12_monitor

install:  g12_monitor
	$(VDIR) $(LOCAL_BIN)
	mv g12_monitor $(LOCAL_BIN)
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
