#! gnumake

#---------------------------------------------------
# The D. P. Weygand Makefile
#---------------------------------------------------
# THIS MAKEFILE NOW WORKS ON SUN AND LINUX
# J. Santoro
#----------------------------------------



include $(CLAS_CMS)/Rules.make

BOSLIBS= -L$(TOP_DIR)/lib/$(OS_NAME) -L$(CLAS_LIB) -lsc$(ADD_DEBUG) -lc_cern$(ADD_DEBUG) -lpid -lc_bos_io$(ADD_DEBUG) \
-lec$(ADD_DEBUG) -lcc$(ADD_DEBUG) -lrecutl$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -lonline_dummy -lfputil$(ADD_DEBUG)     \
-lbos$(ADD_DEBUG) -lfpack$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) -lmapmanager -lst 

#use the newest cern library
CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -L$(CERNPATH)  -lpawlib  -ljetset74  -lkernlib -lpacklib -lmathlib
MATHLIB= -lm
FLIB := -lgcc
INCLUDE := -I../../../include $(INCLUDE)

MISCLIB_SunOS :=  -L/usr/lib -lm -lc -lnsl -lsocket
MISCLIB_HP-UX :=  -L/usr/lib -lm -lc -lf
MISCLIB_AIX :=  -L/usr/lib -lxlf90 -lm -lc 
MISCLIB_Linux :=  -L/usr/lib  -lm -lc  -lc_nonshared -lg -lg++ 

MISCLIB := $(MISCLIB_$(OS_NAME))

LIBS= $(STDLIBS) $(BOSLIBS) $(CERNLIB) $(FLIB) $(MISCLIB) $(MATHLIB) 


hscan_gains.exe: hscan_gains.o
	g77 -o $@ hscan_gains.o $(LIBS)

min_gains_main.exe: min_gains_main.o min_gains.o
	g77 -o $@ min_gains_main.o min_gains.o $(LIBS)

ALL= min_gains_main.exe hscan_gains.exe

all: $(ALL)



env:
	@echo CC $(CC)
	@echo F77 $(F77)
	@echo INCLUDE $(INCLUDE)
	@echo BOSPATH $(BOSPATH)
	@echo FLIB $(FLIB)
	@echo LIBS $(LIBS)
	@echo CERNLIB\\t$(CERNLIB)









