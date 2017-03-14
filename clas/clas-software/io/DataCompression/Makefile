# Makefile to build CompressCLASdata
# Author: Mike Williams
CLASINC    = -I$(CLAS_PACK)/include -I$(CLAS_PACK)/inc_derived
ROOTFLAGS  = $(shell $(ROOTSYS)/bin/root-config --cflags)
CXXFLAGS   = -O -Wall  
CLASFLAGS  = $(CXXFLAGS) $(ROOTFLAGS) $(CLASINC)
LD         = g++

#DATALIBS   = ./libCLASdata.so 
DATALIBS   = $(CLAS_SLIB)/libCLASdata.so 
CLASLIBS   = -L$(TOP_DIR)/lib/$(OS_NAME) -L$(CLAS_LIB) -leloss -lvertex -lpid -ltag -llac -lseb -lst -lclasutil -lsc -lc_bos_io -ldc -lec -lcc -ltrk -ldc  -lc_bos_io -lsc -lmapmanager -lfputil -lfpack -lrecutl -lonline_dummy -lc_cern -lclasutil -lbos -lfpack -lbankdefs -lvertex -lbankdefs
CERNLIBS   = -L$(CERN_ROOT)/lib -lpacklib -lnsl
ROOTLIBS     = $(shell $(ROOTSYS)/bin/root-config --libs)
FLIBS      = -lg2c

INCLUDE    = -I./CLASdata -I./BOSutils $(CLASINC) $(ROOTFLAGS)
LIBS       = ./BOSutils/BOS.o ./BOSutils/SetCLASdata.o $(DATALIBS) $(CLASLIBS) $(CERNLIBS) $(FLIBS) $(ROOTLIBS)

all: libCLASdata.so CompressCLASdata CLASdatadump install

lib: all

clean:; @rm -f *.o */*.o *.so */*.so */*Dict*

CompressCLASdata: CompressCLASdata.o
	@echo "Building CompressCLASdata ..."
	g++ -O CompressCLASdata.o $(LIBS) -o CompressCLASdata
	@echo "done."

CompressCLASdata.o: CompressCLASdata.cc ./BOSutils/BOS.o ./BOSutils/SetCLASdata.o ./libCLASdata.so
	@echo "Building CompressCLASdata.o ..."
	g++ -O $(CXXFLAGS) $(INCLUDE) -c -o CompressCLASdata.o CompressCLASdata.cc
	@echo "done."

CLASdatadump: CLASdatadump.o
	@echo "Building CLASdatadump ..."
	g++ -O CLASdatadump.o $(LIBS) -o CLASdatadump
	@echo "done."

CLASdatadump.o: CLASdatadump.cc ./BOSutils/BOS.o ./BOSutils/SetCLASdata.o $(CLAS_SLIB)/libCLASdata.so
	@echo "Building CLASdatadump.o ..."
	g++ -O $(CXXFLAGS) $(INCLUDE) -c -o CLASdatadump.o CLASdatadump.cc
	@echo "done."

./BOSutils/SetCLASdata.o: ./BOSutils/SetCLASdata.h ./BOSutils/SetCLASdata.C
	@cd BOSutils && $(MAKE) SetCLASdata.o

./BOSutils/BOS.o: ./BOSutils/BOS.h ./BOSutils/BOS.C
	@cd BOSutils && $(MAKE) BOS.o

./libCLASdata.so: ./CLASdata/CLASdata.h  ./CLASdata/CLASdata.C 
	@cd CLASdata && $(MAKE) lib

install:
	cp CLASdatadump $(CLAS_BIN)/.
	cp CompressCLASdata $(CLAS_BIN)/.
