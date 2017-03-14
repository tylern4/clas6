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

STDLIBS= -L$(CLAS_LIB) -lpid$(ADD_DEBUG) -ltag$(ADD_DEBUG) -lec$(ADD_DEBUG) -lcc$(ADD_DEBUG) -lsc$(ADD_DEBUG) \
	-ltrk$(ADD_DEBUG) -ldc$(ADD_DEBUG) -llac$(ADD_DEBUG) -lst$(ADD_DEBUG) -lpid$(ADD_DEBUG) \
	-lscaler$(ADD_DEBUG) -lvertex$(ADD_DEBUG) -lbankdefs$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) \
	-lc_bos_io$(ADD_DEBUG) -lrecutl$(ADD_DEBUG) -lrecsis$(ADD_DEBUG) -litape$(ADD_DEBUG) $(BOSLIB) -lc_bos_io$(ADD_DEBUG) \
	-lonline_dummy$(ADD_DEBUG) -lc_cern$(ADD_DEBUG)

INCLUDE += -I$(MYSQL_INCLUDE_PATH)

ifndef MAP
	STDLIBS += -lcaldbMap$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -L$(MYSQL_LIB_PATH) -lmysqlclient  -ldl
else
	STDLIBS += -lmapmanager$(ADD_DEBUG)	
endif


ifeq ($(OS_NAME),Darwin)
    STDLIBS +=  -lg2c  -L/Developer/SDKs/MacOSX10.4u.sdk/usr/lib/gcc/i686-apple-darwin8/4.0.1/ -lstdc++ -lgcc_eh -ltcl
endif

ifeq ($(findstring Linux,$(OS_NAME)),Linux)
  ifdef GFORTRAN
#    STDLIBS += -L/usr/lib/ -lgfortran -lstdc++ -lz -lnsl -ltcl
    STDLIBS += -lgfortran -lstdc++ -lz -lnsl -ltcl
  else
  STDLIBS +=  -lstdc++ -lz -lnsl -ltcl
  endif
endif


ifeq "$(OS_NAME)" "SunOS"
OTHER = /usr/local/lib
STDLIBS += -lsocket -lnsl -L$(OTHER)/ -ltcl 
endif	


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

sc_monSRCS= sc_mon.c
sc_monOBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(sc_monSRCS))

sc_mon: $(sc_monOBJS)
	$(CC) $(CDBG) $(CFLAGS) $(INCLUDE) -o $@ $^ $(STDLIBS) $(CERNLIB) $(FLIB) $(FLIB_NATIVE) $(MATHLIB)

all: lib  sc_mon

LIBS= $(LOCAL_LIB)/libsc_mon$(ADD_DEBUG).a
lib: $(LIBS)

$(LIBS): $(OBJS) $(FOBJS) $(CCOBJS)
	$(VDIR) $(LOCAL_LIB)
	ar rv $@ $?

install:  sc_mon
	$(VDIR) $(LOCAL_BIN)
	mv sc_mon $(LOCAL_BIN)
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
