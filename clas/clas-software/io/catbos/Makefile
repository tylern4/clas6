
include $(CLAS_CMS)/Rules.make

MISCLIB_SunOS :=  -L/usr/lib -lm -lc -lnsl -lsocket
MISCLIB_HP-UX :=  -L/usr/lib -lm -lc -lf
MISCLIB_AIX :=  -L/usr/lib -lxlf90 -lm -lc 
MISCLIB_Linux :=  -L/usr/lib  -lm -lc
MISCLIB_OSF1 :=  -L/usr/lib -lm -lc -lfor -lUfor
MISCLIB := $(MISCLIB_$(OS_NAME))

LIBPP = -L$(CLAS_LIB) -lpp$(ADD_DEBUG)
RECLIB = -L$(TOP_DIR)/lib/$(OS_NAME) -L$(CLAS_LIB) -lpid$(ADD_DEBUG) \
-ltag$(ADD_DEBUG) -llac$(ADD_DEBUG) -lseb$(ADD_DEBUG) -lst$(ADD_DEBUG) \
-lclasutil$(ADD_DEBUG) -lsc$(ADD_DEBUG) -lc_cern$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) \
-ldc$(ADD_DEBUG) -lec$(ADD_DEBUG) -lcc$(ADD_DEBUG) -ltrk$(ADD_DEBUG) \
-ldc$(ADD_DEBUG)  -lc_bos_io$(ADD_DEBUG) -lsc$(ADD_DEBUG)  -lmapmanager$(ADD_DEBUG) \
-lfputil$(ADD_DEBUG) -lfpack$(ADD_DEBUG) \
-lrecutl$(ADD_DEBUG) -lonline_dummy$(ADD_DEBUG)  -lclasutil$(ADD_DEBUG)\
-lbos$(ADD_DEBUG)  -lfpack$(ADD_DEBUG)

LIBS= $(STDLIBS) $(RECLIB) $(CERN) $(CERNLIB) $(FLIB) $(MISCLIB) 

exe: catbos$(ADD_DEBUG) splitbos$(ADD_DEBUG)

catbosSRCS= catbos.c
catbosOBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(catbosSRCS))

splitbosSRCS= splitbos.c
splitbosOBJS= $(patsubst %.c,$(OBJDIR)/%.o,$(splitbosSRCS))


catbos$(ADD_DEBUG): $(catbosOBJS)
	$(FC) $(CDBG) $(CFLAGS) -o $@ $^ $(LIBS) $(FLIB)

splitbos$(ADD_DEBUG): $(splitbosOBJS)
	$(FC) $(CDBG) $(CFLAGS) -o $@ $^ $(LIBS) $(FLIB)

install: catbos$(ADD_DEBUG) splitbos$(ADD_DEBUG)
	mv -f catbos$(ADD_DEBUG) splitbos$(ADD_DEBUG) $(CLAS_BIN)

clean:
	@rm catbos$(ADD_DEBUG) $(catbosOBJS) splitbos$(ADD_DEBUG) $(splitbosOBJS)

env:
	@echo CERN $(CERN)
	@echo CC $(CC)
	@echo INCLUDE $(INCLUDE)
	@echo FLIB $(FLIB)
	@echo FINCLUDE $(FINCLUDE)
	@echo LIBS $(LIBS)
	@echo OBJS $(OBJS)
	@echo OBJDIR $(OBJDIR)
	@echo FOBJ $(FOBJ)
	@echo FLIB $(FLIB)
	@echo RECLIB $(RECLIB)
	@echo CDBG $(CDBG)
	@echo CFLAGS $(CFLAGS)
	@echo CERN_ROOT $(CERN_ROOT)
	@echo CERNLIB $(CERNLIB)
