#! gnumake
# Makefile to compile the sync utility
# Author: Paul Mattione (pmatt@jlab.org), 03/23/10

INCLUDE       = -I. -I$(CLAS_PACK)/inc_derived -I$(CLAS_PACK)/include
CXXFLAGS      = -O -Wall
FLAGS         = $(CXXFLAGS) $(INCLUDE)

CLASLIBS       = -L$(CLAS_LIB) -lvertex -lpid -ltag -llac -lseb -lst -lclasutil -lsc -lc_bos_io -ldc -lec -lcc -ltrk -ldc -lc_bos_io -lsc -ltcl -lcaldbMap -lrecutl -lonline_dummy -lc_cern -lclasutil -lbankdefs -lvertex -lbankdefs -lbosio
CERNLIBS       = -L$(CERN_ROOT)/lib/ -lc_cern -lkernlib -lpacklib -lmathlib -lnsl
MYSQLLIBS      = -L$(MYSQL_LIB_PATH) -lmysqlclient -ldl
TOPLIBS        = $(CLASLIBS) $(CERNLIBS) $(MYSQLLIBS)

ifdef GFORTRAN
  TOPLIBS += -L/usr/lib/ -lg2c -lgfortran -lstdc++
else
  TOPLIBS += -L/usr/lib/ -lg2c
endif

all: sync

exe: sync

sync: sync.cc
	@echo "Building sync ..."
	g++ -O -Wall sync.cc $(TOPLIBS) $(INCLUDE) -o sync
	@mv sync $(TOP_DIR)/bin/$(OS_NAME)/
	@echo "done."





































































