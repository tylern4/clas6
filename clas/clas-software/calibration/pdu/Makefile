#! gnumake

#---------------------------------------------------
# The D. P. Weygand Makefile
#---------------------------------------------------
include $(CLAS_CMS)/Rules.make
ifdef USEBOSIO
BOSLIBS= -lbosio$(ADD_DEBUG)
else
BOSLIBS = -lfputil$(ADD_DEBUG) -lbos$(ADD_DEBUG) -lfpack$(ADD_DEBUG)
endif

STDLIBS= -L$(LOCAL_LIB) -L$(CLAS_LIB)  -lbankdefs$(ADD_DEBUG) -lc_cern$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) -lrecutl$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) -litape

ifndef MAP
	INCLUDE += -I$(MYSQL_INCLUDE_PATH)
	STDLIBS += -lcaldbMap$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -L$(MYSQL_LIB_PATH) -lmysqlclient  -ldl
else
	STDLIBS += -lmapmanager$(ADD_DEBUG) -lclasutil$(ADD_DEBUG)	
endif

#use the newest cern library
CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -L$(CERNPATH) -lpacklib 
MATHLIB= -lm

INCLUDE := -I../../include $(INCLUDE)

MISCLIB_SunOS :=  -L/usr/lib -lm -lc -lnsl -lsocket
MISCLIB_HP-UX :=  -L/usr/lib -lm -lc -lf
MISCLIB_AIX :=  -L/usr/lib -lxlf90 -lm -lc 
MISCLIB_Linux :=   -lm -lc -ldl -lnsl
MISCLIB_LinuxRH6 :=   -lm -lc -lz -ldl -lnsl -ltcl
MISCLIB_LinuxRH7 :=   -lm -lc -lz -ldl -lnsl -ltcl
MISCLIB_LinuxRHFC8 :=   -lm -lc -lz -ldl -lnsl -ltcl
MISCLIB_LinuxRHFC9 :=   -lm -lc -lz -ldl -lnsl -ltcl
MISCLIB_LinuxRHEL3 :=   -lm -lc -lz -ldl -lnsl -ltcl
MISCLIB_OSF1 :=  -L/usr/lib -lm -lc -lfor -lUfor

MISCLIB := $(MISCLIB_$(OS_NAME))

LIBS= $(STDLIBS) $(BOSLIBS) $(CERNLIB) $(MISCLIB) 

PROGRAM = pdu$(ADD_DEBUG)
SRCS = pdu.c pduInit.c pdu_hbk.c CountHits.c FlagBadWires.c
OTHER = Makefile
OBJS =  $(patsubst %.c,$(OBJDIR)/%.o,$(SRCS))

$(PROGRAM): $(OBJS) $(OTHER)
	$(FC) $(CDBG) $(CFLAGS) -o $(PROGRAM) $(OBJS) $(LIBS)	

install: $(PROGRAM)
	mv -f $(PROGRAM) $(CLAS_BIN)
env:
	@echo 'CC:      '$(CC)
	@echo 'F77:     '$(F77)
	@echo 'INCLUDE: '$(INCLUDE)
	@echo 'BOSPATH: '$(BOSPATH)
	@echo 'FLIB:    '$(FLIB)
	@echo 'LIBS:    '$(LIBS)
	@echo 'CERNLIB: '$(CERNLIB)
	@echo 'SRCS:    '$(SRCS)
	@echo 'OBJS:    '$(OBJS)




