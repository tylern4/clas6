# Makefile to build the libraries
# Author: Mike Williams

CLASINC    = -I$(CLAS_PACK)/include -I$(CLAS_PACK)/inc_derived 
ROOTFLAGS  = $(shell $(ROOTSYS)/bin/root-config --cflags)
CXXFLAGS   = -O -Wall  
CLASFLAGS  = $(CXXFLAGS) $(ROOTFLAGS) $(CLASINC)
LD         = g++
INCLUDE    = -I../CLASdata $(CLASINC) $(ROOTFLAGS)
OBJS       = BOS.o SetCLASdata.o

all: $(OBJS)

lib: $(OBJS)

clean:; @rm -f $(OBJS)
	@rm -f *.o

BOS.o: BOS.h BOS.C
	@echo "Building BOS.o ..."
	g++ $(CLASFLAGS) -c -o BOS.o BOS.C
	@echo "done."

SetCLASdata.o: SetCLASdata.h SetCLASdata.C ../CLASdata/libCLASdata.so
	g++ $(INCLUDE) -c -o SetCLASdata.o SetCLASdata.C

