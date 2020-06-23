#! gnumake
#DBG = 1
#---------------------------------------------------
# The D. P. Weygand et. al. Makefile 
#---------------------------------------------------
include $(CLAS_CMS)/Rules.make


INCLUDE := -I../../include -I$(HOME)/mytools/source $(INCLUDE)

RECLIB = -L$(TOP_DIR)/lib/$(OS_NAME) -L$(CLAS_LIB) -ldc$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) -lmapmanager$(ADD_DEBUG) -lfputil$(ADD_DEBUG) -lfpack$(ADD_DEBUG) -lrecutl$(ADD_DEBUG) -lbos$(ADD_DEBUG)  -lfpack$(ADD_DEBUG)

CFLAGS += -O
CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -lc_cern -L$(CERNPATH) -lpacklib

MISCLIB_SunOS :=  -L/usr/lib -lm -lc -lnsl -lsocket
MISCLIB_HP-UX :=  -L/usr/lib -lm -lc -lf
MISCLIB_AIX :=  -L/usr/lib -lxlf90 -lm -lc 
MISCLIB_Linux :=  -lm -lc
MISCLIB_LinuxRH6 :=  -lm -lc
MISCLIB_OSF1 :=  -L/usr/lib -lm -lc -lfor -lUfor

MISCLIB := $(MISCLIB_$(OS_NAME))

XLIB= -L/lib 
RPCLIB= -L/usr/lib -lrpcsvc
TCLLIB= -L/usr/local/lib -ltcl

LIBS= $(RECLIB) $(XLIB) $(MISCLIB) $(STDLIBS) $(RECLIB) $(CERNLIB) $(MISCLIB)

#PROGRAM=

exe: aligndc

SRCS= aligndc.c dcg_tools.c amoeba.c filltrack.c fmatrix.c nrutil.c
FOBJ =${FSRC:.F=.o}

MAPSRC = mapdiff.c nrutil.c
MAPOBS = $(patsubst %.c,$(OBJDIR)/%.o,$(MAPSRC))

OBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(SRCS)) $(patsubst %.F,$(OBJDIR)/%.o,$(FSRC))

$(PROGRAM): $(OBJS)
	$(FC) -v -o $@ $(CDBG) $(FINCLUDE) $^ $(FOBJ) $(LIBS)

aligndc: $(OBJS)
	$(CC) $(CDBG) $(CFLAGS) -o $@ $^ $(LIBS) $(FLIB)
	mv $@ $(TOP_DIR)/bin/$(OS_NAME)/

mapdiff: $(MAPSRC)
	$(CC) $(CDBG) $(CFLAGS) -o $@ $^ $(INCLUDE) $(LIBS) $(FLIB)
	mv $@ $(TOP_DIR)/bin/$(OS_NAME)/

map2latex: map2latex.c  nrutil.c
	$(CC) $(CDBG) $(CFLAGS) -o $@ $^ $(INCLUDE) $(LIBS) $(FLIB)
	mv $@ $(TOP_DIR)/bin/$(OS_NAME)/

testsize: test.c
	$(CC) $(CDBG) $(CFLAGS) -o $@ $^ $(INCLUDE) $(LIBS) $(FLIB)

scripts: 
	cp *.pl $(TOP_DIR)/bin/$(OS_NAME)/
	
env:
	@echo CC $(CC)
	@echo INCLUDE $(INCLUDE)
	@echo BOSPATH $(BOSPATH)
	@echo FLIB $(FLIB)
	@echo FINCLUDE $(FINCLUDE)
	@echo LIBS $(LIBS)
	@echo OBJS $(OBJS)
	@echo OBJDIR $(OBJDIR)
	@echo FOBJ $(FOBJ)
	@echo FLIB $(FLIB)
	@echo RECLIB $(RECLIB)
	@echo TCLLIB $(TCLLIB)
	@echo CDBG $(CDBG)
	@echo CFLAGS $(CFLAGS)
	@echo CERN_ROOT $(CERN_ROOT)
	@echo CERNLIB $(CERNLIB)


















