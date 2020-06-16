PROG     = ntp_filter
EXE      = $(PROG)_$(OSNAME)
#EXE      = $(HOME)/bin/$(OSNAME)/$(PROG)
SOURCES  = $(wildcard *.[f])
INC      = $(wildcard *.[inc])
OBJ      = $(shell echo "$(SOURCES)" | sed s/f\ /o\ /g) 

CERNLIBS = -lmathlib -lphtools  -lgeant321 -lpacklib -lkernlib -lpawlib
CLASLIBS = -lc_bos_io -lrecutl  -lfputil -lbos -lfpack -lcc -lmapmanager
LOCLIBS  = -lPcor -lstdc++ -leloss

#include $(CLAS_ROOT)/CMD/include.mk
#include ~/include/include.mk
include include.mk

%.o:    %.f $(INC)
	$(COMPILER) $(F77OPT) -c $< -o $@

all:    $(EXE)

$(EXE): $(OBJ) $(INC)
	$(COMPILER) $(F77OPT) -o $(EXE) $(OBJ)  $(LOCLIBS) $(LIBS)

clean:
	rm -f  *.o core

delete:
	rm -f $(EXE)

