#
# if DEBUG then make exe in current directory
#
ifdef DEBUG
     EXE    = $(PROG)_$(OSNAME)_debug
endif
# SunOS
ifeq (SunOS, $(OSNAME))
  SYSLIBS  = -L/usr/openwin/lib -lnsl -lsocket -ldl -lm -lc 
  COMPILER = f77 
  ifdef DEBUG
     F77OPT = -C -g -e -ftrap=no%inexact,no%underflow
  else
     F77OPT = -e -ftrap=no%inexact,no%underflow -O2
  endif
# next line is valid only for ULTRA2 processors (AIACESUN)
  F77OPT += -xtarget=ultra2 -xcache=16/32/1:4096/64/1
endif

# HP-UX
ifeq (HP-UX, $(OSNAME))
  SYSLIBS  = -L/usr/lib -lf -lm  -lcl -lc
  COMPILER = fort77
  ifdef DEBUG
    F77OPT  = -DHP_UX +ppu +es +E1 -C -g
  else
    F77OPT  = -DHP_UX +ppu +es +E1 -O2
  endif 
endif

# OSF1
ifeq (OSF1, $(OSNAME))
  SYSLIBS  = -L/usr/lib -lm -lc
  COMPILER = f77
  F77OPT   = -O0 -extend_source -noautomatic -vms -fpe2 -DOSF1
endif

# Linux
ifeq (Linux, $(OSNAME))
#  SYSLIBS  = -L/usr/lib -lm -lc
  SYSLIBS  = 
  COMPILER = f77
  F77OPT   = -O2 -m486 -fno-automatic 
  F77OPT  += -ffixed-line-length-none -fno-second-underscore -DLinux
endif

LIBS     = $(SYSLIBS) -L$(CLAS_LIB) $(CLASLIBS) -L$(CERN_ROOT)/lib $(CERNLIBS)

