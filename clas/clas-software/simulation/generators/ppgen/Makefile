#! gnumake

#---------------------------------------------------
# The D. P. Weygand Makefile
#---------------------------------------------------
include $(CLAS_CMS)/Rules.make





INCLUDE := -I../../include -I../../inc_derived   $(INCLUDE) 

PWA = -L$(CLAS_LIB) -lpp

RECLIB = -L$(TOP_DIR)/lib/$(OS_NAME) -L$(CLAS_LIB) -lpid$(ADD_DEBUG) -ltag$(ADD_DEBUG) -llac$(ADD_DEBUG) -lseb$(ADD_DEBUG) -lst$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -lsc$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG)  -ldc$(ADD_DEBUG) -lec$(ADD_DEBUG) -lcc$(ADD_DEBUG) -ltrk$(ADD_DEBUG)  -ldc$(ADD_DEBUG)  -lc_cern$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) -lsc$(ADD_DEBUG)  -lmapmanager$(ADD_DEBUG)  -lfputil$(ADD_DEBUG) -lfpack$(ADD_DEBUG) -lrecutl$(ADD_DEBUG) -lonline_dummy$(ADD_DEBUG)  -lclasutil$(ADD_DEBUG)  -lbos$(ADD_DEBUG)  -lfpack$(ADD_DEBUG)


CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -L$(CERNPATH) -lpacklib

MISCLIB_SunOS :=  -L/usr/lib -lm -lc -lnsl -lsocket
MISCLIB_HP-UX :=  -L/usr/lib -lm -lc -lf
MISCLIB_AIX :=  -L/usr/lib -lxlf90 -lm -lc 
MISCLIB_Linux :=  -L/usr/lib  -lm 
MISCLIB_OSF1 :=  -L/usr/lib -lm -lc -lfor -lUfor

MISCLIB := $(MISCLIB_$(OS_NAME))

XLIB= -L/lib 
RPCLIB= -L/usr/lib -lrpcsvc
TCLLIB= -L/usr/local/lib -ltcl

LIBS= $(RECLIB) $(XLIB) $(RECUTIL) $(CERNLIB)  $(STDLIBS)  $(MISCLIB)


EXE_default:=
EXE_IRIX:= $(EXE_default)
EXE_SunOS:= $(EXE_default)
EXE_HP-UX:= $(EXE_default)
EXE_AIX:= $(EXE_default)
EXE_Linux = $(EXE_default)

EXE:= $(EXE_$(OS_NAME))
ifndef EXE
EXE:= $(EXE_default)
endif
all: lib
exe: $(EXE)


ppgen: $(OBJDIR)/ppgen.o 
	$(C++) -g -o $@ $^ $(PWA) $(LIBS) $(FLIB)

makebos: $(OBJDIR)/makebos.o
	$(C++) -g -o $@ $^ $(PWA) $(LIBS) $(FLIB)


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
	@echo INCLUDE $(INCLUDE)
