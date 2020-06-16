#! gnumake

#---------------------------------------------------
# The D. P. Weygand et. al. Makefile 
#---------------------------------------------------
include $(CLAS_CMS)/Rules.make


INCLUDE :=  $(INCLUDE) -I../../include

BOSLIB = -lfputil$(ADD_DEBUG) -lbos$(ADD_DEBUG) -lfpack$(ADD_DEBUG)

ifdef USEBOSIO
BOSLIB = -lbosio$(ADD_DEBUG)
endif

RECLIB = -L$(LOCAL_LIB) -L$(CLAS_LIB)  -litape$(ADD_DEBUG) -ltrk$(ADD_DEBUG) -lpid$(ADD_DEBUG) -ltag$(ADD_DEBUG) -ltagM$(ADD_DEBUG) -llac$(ADD_DEBUG) -lseb$(ADD_DEBUG) -lst$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -lsc$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG)  -ldc$(ADD_DEBUG) -lec$(ADD_DEBUG) -lcc$(ADD_DEBUG)  -lsc$(ADD_DEBUG) -lvertex$(ADD_DEBUG) -lbankdefs$(ADD_DEBUG) -lrecutl$(ADD_DEBUG) -lonline_dummy$(ADD_DEBUG)  -ldl $(BOSLIB)

INCLUDE += -I$(MYSQL_INCLUDE_PATH)	

ifndef MAP
	RECLIB += -lcaldbMap$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -L$(MYSQL_LIB_PATH) -lmysqlclient

	ifeq "$(OS_NAME)" "LinuxRH6"
            RECLIB +=  -lz  
	endif

	ifeq "$(OS_NAME)" "LinuxRH7"
            RECLIB +=  -lz
	endif

	ifeq "$(OS_NAME)" "LinuxRH8"
            RECLIB +=  -lz
	endif

	ifeq "$(OS_NAME)" "LinuxRH9"
            RECLIB +=  -lz
	endif

	ifeq "$(OS_NAME)" "SunOS"
	RECLIB += -lsocket -lnsl
	endif	
 	
else
	RECLIB += -lmapmanager$(ADD_DEBUG)	
endif

CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -L$(CERNPATH) -lmathlib -lkernlib -lpacklib 

MISCLIB_SunOS :=  -L/usr/lib -lm -lc -lnsl -lsocket
MISCLIB_HP-UX :=  -L/usr/lib -lm -lc -lf
MISCLIB_AIX :=  -L/usr/lib -lxlf90 -lm -lc 
MISCLIB_Linux :=   -lm -lc
MISCLIB_LinuxRH6 :=   -lm -lc
MISCLIB_LinuxRH7 :=   -lm -lc
MISCLIB_LinuxRH8 :=   -lm -lc
MISCLIB_LinuxRH9 :=   -lm -lc
MISCLIB_OSF1 :=  -L/usr/lib -lm -lc -lfor -lUfor

MISCLIB := $(MISCLIB_$(OS_NAME))

XLIB= -L/lib 
RPCLIB= -L/usr/lib -lrpcsvc
TCLLIB=  -ltcl

LIBS= $(RECLIB) $(XLIB) $(RECUTIL) $(CERNLIB)  $(STDLIBS)  $(MISCLIB) $(CLIB) $(TCLLIB)

PROGRAM= a1
all: exe install
exe: a1c$(ADD_DEBUG)

SRCS= a1.c analysis.c faults.c
FSRC= 

FOBJ =${FSRC:.F=.o}
OBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(SRCS))

$(PROGRAM): $(OBJS)
	$(C++) -v -o $@ $(CDBG) $(FINCLUDE) $^ $(FOBJ) $(LIBS) $(FLIB)

a1c$(ADD_DEBUG): $(OBJS)
	$(C++) $(CDBG) $(CFLAGS) -o $@ $^ $(LIBS) $(FLIB)

xa1c$(ADD_DEBUG): $(OBJS)
	$(C++) $(CDBG) $(FFLAGS) -o $@ $^ $(FLIB)

install: a1c$(ADD_DEBUG)
	mv -f a1c$(ADD_DEBUG) $(TOP_DIR)/bin/$(OS_NAME)

env:
	@echo FC $(FC)
	@echo CC $(CC)
	@echo INCLUDE $(INCLUDE)
	@echo BOSPATH $(BOSPATH)
	@echo FLIB $(FLIB)
	@echo FLIB_NATIVE $(FLIB_NATIVE)
	@echo FINCLUDE $(FINCLUDE)
	@echo LOCAL_LIB $(LOCAL_LIB)
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
	@echo FLIBRARY $(FLIBRARY)
	@echo FLIB_Linux $(FLIB_Linux)
	@echo OS_NAME $(OS_NAME)
	@echo CLIB $(CLIB)
