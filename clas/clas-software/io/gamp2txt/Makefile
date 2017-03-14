#! gnumake

#---------------------------------------------------
# The D. P. Weygand et. al. Makefile 
#---------------------------------------------------
include Rules.make

ifdef DBG
CCFLAGS := $(CCFLAGS) -g
CFLAGS := $(CFLAGS) -g
endif

STDLIBS=  -lm


RECLIB = -L$(CLAS_LIB)  -lscaler$(ADD_DEBUG) -lst$(ADD_DEBUG) -lpid$(ADD_DEBUG) -lsc$(ADD_DEBUG) -lec$(ADD_DEBUG) -lcc$(ADD_DEBUG) -lst$(ADD_DEBUG) -ltag$(ADD_DEBUG)  -lrecutl$(ADD_DEBUG) -lclasutil$(ADD_DEBUG)  -lonline_dummy$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) -lmapmanager$(ADD_DEBUG) -lfputil$(ADD_DEBUG) -lbos$(ADD_DEBUG)  -lfpack$(ADD_DEBUG) -lc_cern$(ADD_DEBUG)

# the bosio version :
#RECLIB = -L$(TOP_DIR)/lib/$(OS_NAME) -L$(CLAS_LIB) -lst$(ADD_DEBUG) -lpid$(ADD_DEBUG) -lsc$(ADD_DEBUG) -lec$(ADD_DEBUG) -lcc$(ADD_DEBUG) -lst$(ADD_DEBUG) -ltag$(ADD_DEBUG) -lrecsis$(ADD_DEBUG) -lrecutl$(ADD_DEBUG) -lclasutil$(ADD_DEBUG)  -lonline_dummy$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) -lmapmanager$(ADD_DEBUG) -lbosio$(ADD_DEBUG) -lc_cern$(ADD_DEBUG)

# clean this up and put the bos stuff at the end in this order:
# c_bos_io bos fputil fpack
# or c_bos_io bos_io
# -lrecsis$(ADD_DEBUG)

CERNPATH= $(CERN_ROOT)/lib
CERNLIB= 

MISCLIB_SunOS :=  -L/usr/lib -lm -lc -lnsl -lsocket
MISCLIB_HP-UX :=  -L/usr/lib -lm -lc -lf
MISCLIB_AIX :=  -L/usr/lib -lxlf90 -lm -lc 
MISCLIB_Linux :=  -L/usr/lib  -lm -lc /usr/lib/libf2c.a
MISCLIB_OSF1 :=  -L/usr/lib -lm -lc -lfor -lUfor

MISCLIB := $(MISCLIB_$(OS_NAME))

XLIB= -L/lib 
RPCLIB= -L/usr/lib -lrpcsvc
TCLLIB= -L/usr/local/lib -ltcl

LIBS= $(RECLIB) $(CERNLIB) $(STDLIBS) $(XLIB) $(MISCLIB) $(STDLIBS) $(RECUTIL) $(CERNLIB)

ifdef PROF	
	CFLAGS := $(CFLAGS) -pg
endif

PROGRAM= txt2part 

SRCS= txt2part.c
FSRC= 

OBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(SRCS))
FOBJ =${FSRC:.F=.o}

ASRCS= gamp2txt.cc

AOBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(ASRCS))

$(PROGRAM): $(OBJS)
	$(CC) $(CDBG) $(CFLAGS) -o $@ $(OBJS) $(LIBS) $(FLIB)

gamp2txt: $(AOBJS)
	$(C++) $(CDBG) $(CFLAGS) -o $@ $(AOBJS) $(LIBS) $(FLIB)


env:
	@echo CC $(CC)
	@echo INCLUDE $(INCLUDE)
	@echo BOSPATH $(BOSPATH)
	@echo FLIB $(FLIB)
	@echo FINCLUDE $(FINCLUDE)
	@echo LIBS $(LIBS)
	@echo OBJS $(OBJS)
	@echo FOBJ $(FOBJ)
	@echo FLIB $(FLIB)
	@echo RECLIB $(RECLIB)
	@echo TCLLIB $(TCLLIB)
	@echo CDBG $(CDBG)
	@echo CFLAGS $(CFLAGS)
	@echo OBJDIR $(OBJDIR)


















