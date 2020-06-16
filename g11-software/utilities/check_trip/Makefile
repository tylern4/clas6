PROG     =  check_trip
EXE      = $(PROG)_$(OSNAME)
SOURCES  = $(wildcard *.[f])
INC      = $(wildcard *.[inc])
OBJ      = $(shell echo "$(SOURCES)" | sed s/f\ /o\ /g) 

CERNLIBS = -lmathlib -lphtools  -lgeant321 -lpacklib -lkernlib -lpawlib
CLASLIBS = -lc_bos_io -lrecutl  -lfputil -lbos -lfpack -lcc 

#include $(CLAS_ROOT)/CMD/include.mk
include include.mk

%.o:    %.f $(INC)
	$(COMPILER) $(F77OPT) -c $< -o $@

all:    $(EXE)

$(EXE): $(OBJ) $(INC)
	$(COMPILER) $(F77OPT) -o $(EXE) $(OBJ) $(LIBS)

clean:
	rm -f  *.o core $(EXE)

delete:
	rm -f $(EXE)

