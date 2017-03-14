#! gnumake

include $(CLAS_CMS)/rules.make

BOSLIBS= -L$(TOP_DIR)/lib/$(OS_NAME) -L$(CLAS_LIB) -lc_cern$(ADD_DEBUG) -lpid -lcc$(ADD_DEBUG) -lec$(ADD_DEBUG) -lsc$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) -lrecutl$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -lonline_dummy -lfputil$(ADD_DEBUG) -lbos$(ADD_DEBUG) -lfpack$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) -lmapmanager -lst -lvertex$(ADD_DEBUG) 

#use the newest cern library
CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -L$(CERNPATH) -lpacklib 
MATHLIB= -lm

INCLUDE := -I../../include $(INCLUDE)

MISCLIB_SunOS :=  -L/usr/lib -lm -lc -lnsl -lsocket
MISCLIB_HP-UX :=  -L/usr/lib -lm -lc -lf
MISCLIB_AIX :=  -L/usr/lib -lxlf90 -lm -lc 
MISCLIB_Linux :=  -L/usr/lib  -lm -lc /usr/lib/libf2c.a

MISCLIB := $(MISCLIB_$(OS_NAME))

LIBS= $(STDLIBS) $(BOSLIBS) $(CERNLIB) $(FLIB) $(MISCLIB) 

sc_delay_el: sc_delay_el.o
	$(CC) -o $@ sc_delay_el.o $(LIBS)

env:
	@echo CC $(CC)
	@echo F77 $(F77)
	@echo INCLUDE $(INCLUDE)
	@echo BOSPATH $(BOSPATH)
	@echo FLIB $(FLIB)
	@echo LIBS $(LIBS)
	@echo CERNLIB\\t$(CERNLIB)







