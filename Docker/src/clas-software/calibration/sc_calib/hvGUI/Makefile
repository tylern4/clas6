################# A MAKEFILE FOR tofHVGUI ####################################

LOCAL_INCLUDE = -I. -I$(CLAS_PACK)/inc_derived -I$(CLAS_PACK)/include
INCLUDE:= $(LOCAL_INCLUDE)

ifdef USEBOSIO
BOSLIB= -lbosio$(ADD_DEBUG)
else
BOSLIB = -lfputil -lbos -lfpack
endif

MAPLIBDIR = MapUtils
LOCAL_LIB = $(HOME)/lib/$(OS_NAME)
MAPLIB = -L$(MAPLIBDIR) -lMapUtils
STDLIBS= -L$(CLAS_LIB)  -L$(LOCAL_LIB) -lpid -lsc -ltrk -ldc -lpid  \
                        -lscaler -lvertex -lbankdefs -lclasutil -lc_bos_io -lrecutl $(BOSLIB) -lc_bos_io -lonline_dummy -lc_cern
                        

ROOTLIB= -L$(ROOTSYS)/lib -lNew -lCint -lCore -lGpad -lGraf -lGraf3d  \
                          -lGui -lHist -lHistPainter -lPhysics -lPostscript  \
                          -lTree -lRint -lMinuit  \
                          -lTreePlayer -lTreeViewer -lMatrix

SLIB= -L$(CLAS_SLIB)

INCLUDE += -I$(MYSQL_INCLUDE_PATH)

ifndef MAP
STDLIBS += -lcaldbMap -lclasutil -L$(MYSQL_LIB_PATH) -lmysqlclient  -ldl
else
STDLIBS += -lmapmanager$(ADD_DEBUG)
endif
STDLIBS +=  -L/usr/lib -lz -lc -ltcl -lnsl

ifdef GFORTRAN
  STDLIBS += -L/usr/lib/ -lgfortran -lstdc++
else
  STDLIBS += -L/usr/lib/ -lstdc++ -lg2c
endif

#use the newest cern library
CERNPATH= $(CERN_ROOT)/lib
CERNLIB= -L$(CERNPATH) -lpacklib -lkernlib -lmathlib
MATHLIB= -lm

INCLUDE  := $(INCLUDE) -I./include
INCLUDE  := $(INCLUDE) -I$(ROOTSYS)/include

CCFLAGS=-fPIC
########################################################################################################################

mainTOFHVGUISRCS= MainWindow.cc MyDict.cc
mainTOFHVGUIOBJS= $(patsubst %.cxx,%.o,$(mainTOFHVGUISRCS))

tofHVcalib: $(mainTOFHVGUIOBJS)
	g++ $(CDBG) $(CCFLAGS) $(INCLUDE) -o $@ $^ -L$(LOCAL_LIB) $(STDLIBS) $(CERNLIB) $(ROOTLIB) $(SLIB) $(MAPLIB)


all: tofHVcalib MyDict.cc

MyDict.cc:
	@rm -rf MyDict.*
	rootcint MyDict.cc -c MainWindow.h HistoWindow.h doHV.h

clean:
	@rm tofHVcalib MyDict.*
