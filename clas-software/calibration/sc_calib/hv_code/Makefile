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

STDLIBS= -L$(TOP_DIR)/lib/$(OS_NAME) -L$(CLAS_LIB) -L$(MYSQL_LIB_PATH) -lsc$(ADD_DEBUG) -lc_cern$(ADD_DEBUG) -lpid$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) -lec$(ADD_DEBUG) -lcc$(ADD_DEBUG) -lrecutl$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -lonline_dummy -lfputil$(ADD_DEBUG) -lbos$(ADD_DEBUG) -lfpack$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG)  -lst 

INCLUDE += -I$(MYSQL_INCLUDE_PATH)

STDLIBS += -lcaldbMap$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -lmysqlclient$(ADD_DEBUG) -ldl$(ADD_DEBUG)

 ifeq "$(OS_NAME)" "LinuxRH6"
	STDLIBS +=  -lz  
  endif

 ifeq "$(OS_NAME)" "LinuxRH7"
	STDLIBS +=  -lz -lnsl 
  endif 

 ifeq "$(OS_NAME)" "LinuxRHEL3"
	STDLIBS +=  -lz -lnsl 
  endif 

 ifeq "$(OS_NAME)" "LinuxRH8"
	STDLIBS +=  -lz -lnsl -ltcl$(ADD_DEBUG) 
  endif 

 ifeq "$(OS_NAME)" "LinuxRH9"
	STDLIBS +=  -lz -lnsl -ltcl$(ADD_DEBUG) 
  endif 

  ifeq "$(OS_NAME)" "SunOS"
	STDLIBS += -lsocket -lnsl
  endif	


#use the newest cern library
CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -L$(CERNPATH) -lpacklib 
MATHLIB= -lm

FINCLUDE := $(FINCLUDE) -I./include
INCLUDE := $(INCLUDE) -I./include



MISCLIB_SunOS :=  -L/usr/lib -lm -lc -lnsl -lsocket
MISCLIB_HP-UX :=  -L/usr/lib -lm -lc -lf
MISCLIB_AIX :=  -L/usr/lib -lxlf90 -lm -lc 
MISCLIB_Linux :=  -L/usr/lib  -lm -lc 
MISCLIB_LinuxRHEL3 :=  -L/usr/lib  -lm -lc 

MISCLIB := $(MISCLIB_$(OS_NAME))

LIBS= $(STDLIBS) $(BOSLIBS) $(CERNLIB) $(FLIB) $(MISCLIB) $(MATHLIB) $(STDLIBS)

OBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(SRCS))

CCOBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(CCSRCS))

hvSRCS= hv.c
hvOBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(hvSRCS))

hv.exe: $(hvOBJS)
	$(CC) $(CDBG) $(CFLAGS) $(INCLUDE) -o $@ $^ $(STDLIBS) $(CERNLIB) $(FLIB) $(FLIB_NATIVE) $(MATHLIB)

all: hv.exe

env:
	@echo CC $(CC)
	@echo F77 $(F77)
	@echo INCLUDE $(INCLUDE)
	@echo BOSPATH $(BOSPATH)
	@echo FLIB $(FLIB)
	@echo LIBS $(LIBS)
	@echo CERNLIB\\t$(CERNLIB)







