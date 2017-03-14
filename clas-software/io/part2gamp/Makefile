#! gnumake

include $(CLAS_CMS)/Rules.make

RECLIB = -L$(TOP_DIR)/lib/$(OS_NAME) -L$(CLAS_LIB) -lscaler$(ADD_DEBUG) -lpid$(ADD_DEBUG) -ltag$(ADD_DEBUG) -llac$(ADD_DEBUG) -lseb$(ADD_DEBUG) -lst$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -lsc$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG)  -ldc$(ADD_DEBUG) -lec$(ADD_DEBUG) -lcc$(ADD_DEBUG) -ltrk$(ADD_DEBUG)  -ldc$(ADD_DEBUG)  -lc_bos_io$(ADD_DEBUG) -lsc$(ADD_DEBUG)  -lmapmanager$(ADD_DEBUG)  -lfputil$(ADD_DEBUG) -lfpack$(ADD_DEBUG) -lrecutl$(ADD_DEBUG) -lonline_dummy$(ADD_DEBUG) -lc_cern$(ADD_DEBUG) -lclasutil$(ADD_DEBUG)  -lbos$(ADD_DEBUG)  -lfpack$(ADD_DEBUG)

#use the newest cern library
CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -L$(CERNPATH) -lpacklib 

INCLUDE := -I../../include -I../../inc_derived $(INCLUDE)

MISCLIB_SunOS :=  -L/usr/lib -lm -lc -lnsl -lsocket
MISCLIB_HP-UX :=  -L/usr/lib -lm -lc -lf
MISCLIB_AIX :=  -L/usr/lib -lxlf90 -lm -lc 
MISCLIB_Linux :=  -L/usr/lib  -lm -lc /usr/lib/libf2c.a
MISCLIB_OSF1 :=  -L/usr/lib -lm -lc -lfor -lUfor
 
MISCLIB := $(MISCLIB_$(OS_NAME))

XLIB= -L/lib 
RPCLIB= -L/usr/lib -lrpcsvc
TCLLIB= -L/usr/local/lib -ltcl

LIBS= $(STDLIBS) $(RECLIB) $(CERNLIB) $(FLIB) $(MISCLIB) 

CSRCS= part2gamp.cc
COBJS= $(patsubst %.cc,$(OBJDIR)/%.o,$(CSRCS))

part2gamp: $(COBJS)
	$(C++) $(CDBG) $(CFLAGS) $(INCLUDE) -o $@ $^ $(LIBS)

countgamp: countgamp.cc
	$(C++) $(CDBG) countgamp.cc -o countgamp

install: bos2hb 
	mv -f bos2hb $(CLAS_BIN)

env:
	@echo CC $(CC)
	@echo F77 $(F77)
	@echo INCLUDE $(INCLUDE)
	@echo BOSPATH $(BOSPATH)
	@echo FLIB $(FLIB)
	@echo LIBS $(LIBS)
	@echo CERNLIB\\t$(CERNLIB)





