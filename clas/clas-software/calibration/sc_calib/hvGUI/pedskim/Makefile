#### my makefile ######

INCLUDE = -I. -I$(CLAS_PACK)/inc_derived -I$(CLAS_PACK)/include -I$(HOME)/packages/inc_derived

ifdef USEBOSIO
BOSLIB= -lbosio$(ADD_DEBUG)
else
BOSLIB = -lfputil -lbos -lfpack
endif


STDLIBS= -L$(LOCAL_LIB) -L$(CLAS_LIB) -lpid$(ADD_DEBUG) -lec$(ADD_DEBUG) -lcc$(ADD_DEBUG) -lsc$(ADD_DEBUG) \
	-ltrk$(ADD_DEBUG) -ldc$(ADD_DEBUG) -llac$(ADD_DEBUG) -lst$(ADD_DEBUG) -ltag$(ADD_DEBUG) \
	-lscaler$(ADD_DEBUG) -lvertex$(ADD_DEBUG) -lbankdefs$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) \
	-lrecutl$(ADD_DEBUG) \
	$(BOSLIB) -lonline_dummy$(ADD_DEBUG) -lc_cern$(ADD_DEBUG)

INCLUDE += -I$(MYSQL_INCLUDE_PATH)

ifndef MAP
STDLIBS += -lcaldbMap -lclasutil -L$(MYSQL_LIB_PATH) -lmysqlclient  -ldl
else
STDLIBS += -lmapmanager$(ADD_DEBUG)
endif
STDLIBS +=  -L/usr/lib -lz -lc -lnsl -ltcl

ifdef GFORTRAN
  STDLIBS += -L/usr/lib/ -lgfortran -lstdc++
else
  STDLIBS += -L/usr/lib/ -lstdc++ -lg2c
endif


#use the newest cern library
CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -L$(CERNPATH) -lpacklib -lkernlib -lmathlib
MATHLIB= -lm

INCLUDE  := $(INCLUDE) 

CCFLAGS=-fPIC

################################
TOF_FORWARD_COSMIC_filter.o: TOF_FORWARD_COSMIC_filter.c 
	echo Include Dirs are:$(INCLUDE)
	echo Making the object file
	gcc -c -o $@ $< $(CDBG) $(CFLAGS) $(CCFLAGS) $(INCLUDE) $(XINCLUDE)	

TOF_FORWARD_COSMIC_filter: TOF_FORWARD_COSMIC_filter.o
	echo LINKING......
	gcc $(CDBG) $(CCFLAGS) $(INCLUDE) -o $@ $^ -L$(LOCAL_LIB) $(STDLIBS) $(CERNLIB) $(MAPLIB)

all: TOF_FORWARD_COSMIC_filter

clean:
	rm -r TOF_FORWARD_COSMIC_filter TOF_FORWARD_COSMIC_filter.o
