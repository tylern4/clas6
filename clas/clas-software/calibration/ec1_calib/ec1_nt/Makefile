PROG     = ec1_nt
#EXE      = $(HOME)/bin/$(OSNAME)/$(PROG)
EXE      = $(PROG)_$(OSNAME)
SOURCES  = $(wildcard *.[f])
INC      = $(wildcard *.[inc])
OBJ      = $(shell echo "$(SOURCES)" | sed s/f\ /o\ /g) 

CERNLIBS = -lmathlib -lkernlib -lpacklib
CLASLIBS = -lc_bos_io -lrecutl -lclasutil -llac -lmapmanager -lfputil -lbos -lfpack -lc_bos_io

include ./include.mk

%.o:    %.f $(INC)
	$(COMPILER) $(F77OPT) -c $< -o $@

all:    $(EXE)

$(EXE): $(OBJ) $(INC)
	$(COMPILER) $(F77OPT) -o $(EXE) $(OBJ) $(LIBS)

clean:
	rm -f  *.o core

delete:
	rm -f $(EXE)

