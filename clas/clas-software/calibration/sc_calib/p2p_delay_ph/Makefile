#! gnumake

#---------------------------------------------------
# The D. P. Weygand Makefile
#---------------------------------------------------
#
#Last updated 6/24/04 by Rob Fersch (fersch@jlab.org)
#
#
include $(CLAS_CMS)/Rules.make

ifdef USEBOSIO
BOSLIB= -lbosio$(ADD_DEBUG)
else
BOSLIB = -lfputil$(ADD_DEBUG) -lbos$(ADD_DEBUG) -lfpack$(ADD_DEBUG)
endif

STDLIBS= -L$(LOCAL_LIB) -L$(CLAS_LIB) -lpid$(ADD_DEBUG) -lec$(ADD_DEBUG) -lcc$(ADD_DEBUG) -lsc$(ADD_DEBUG) -ltrk$(ADD_DEBUG) \
       -ldc$(ADD_DEBUG) -llac$(ADD_DEBUG) -lst$(ADD_DEBUG) -lpid$(ADD_DEBUG) -ltag$(ADD_DEBUG) -lscaler$(ADD_DEBUG) \
       -lvertex$(ADD_DEBUG) -lbankdefs$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) -lrecutl$(ADD_DEBUG) \
       -litape$(ADD_DEBUG) $(BOSLIB) -lc_bos_io$(ADD_DEBUG) -lonline_dummy$(ADD_DEBUG) -lc_cern$(ADD_DEBUG)  

INCLUDE += -I$(MYSQL_INCLUDE_PATH)

ifndef MAP
	STDLIBS += -lcaldbMap$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -L$(MYSQL_LIB_PATH) -lmysqlclient  -ldl
else
	STDLIBS += -lmapmanager$(ADD_DEBUG)	
endif

STDLIBS += -lz -lnsl -ltcl

#use the newest cern library
CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -L$(CERNPATH) -lpacklib 
MATHLIB= -lm

FINCLUDE := $(FINCLUDE) -I./include
INCLUDE := $(INCLUDE) -I./include

CCSRCS:= 

FSRCS:= 

SRCS:=  

FOBJS= $(patsubst %.F,$(OBJDIR)/%.o,$(FSRCS))
OBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(SRCS))

CCOBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(CCSRCS))

hist_offsetsSRCS= hist_offsets.c
hist_offsetsOBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(hist_offsetsSRCS))

hist_offsets: $(hist_offsetsOBJS)
	$(CC) $(CDBG) $(CFLAGS) $(INCLUDE) -o $@ $^ $(STDLIBS) $(CERNLIB) $(FLIB) $(FLIB_NATIVE) $(MATHLIB)

fit_offsetsSRCS= fit_offsets.c
fit_offsetsOBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(fit_offsetsSRCS))

fit_offsets: $(fit_offsetsOBJS)
	$(CC) $(CDBG) $(CFLAGS) $(INCLUDE) -o $@ $^ $(STDLIBS) $(CERNLIB) $(FLIB) $(FLIB_NATIVE) $(MATHLIB)

pair_monSRCS= pair_mon.c
pair_monOBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(pair_monSRCS))

pair_mon: $(pair_monOBJS)
	$(CC) $(CDBG) $(CFLAGS) $(INCLUDE) -o $@ $^ $(STDLIBS) $(CERNLIB) $(FLIB) $(FLIB_NATIVE) $(MATHLIB)

all: lib  hist_offsets fit_offsets pair_mon

LIBS= $(LOCAL_LIB)/libhist_offsets$(ADD_DEBUG).a
lib: $(LIBS)

$(LIBS): $(OBJS) $(FOBJS) $(CCOBJS)
	$(VDIR) $(LOCAL_LIB)
	ar rv $@ $?
install:  hist_offsets
	$(VDIR) $(LOCAL_BIN)
	mv hist_offsets $(LOCAL_BIN)
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
