#
#
#
ifndef PHY_DIR
  PHY_DIR=$(TOP_DIR)
endif

OBJDIR   = $(PHY_DIR)/obj/$(OS_NAME)/tdc_calib/
LIBDIR   = $(PHY_DIR)/lib/$(OS_NAME)/
EXEDIR   = $(PHY_DIR)/bin/$(OS_NAME)/
#==========================================
#============ Packages ====================
#
#======== Compilers =====
CPP_SunOS = CC -g
CPP_AIX   = xlC  
### CPP_AIX   = f77  -bnoquiet 
###
###
###

CPP_HP-UX = CC +a1
CPP_Linux = g++ -g -Wall
CPP:= $(CPP_$(OSNAME))
###
###
###
F77_SunOS = f77
F77_AIX   = f77 
F77_HP-UX = fort77
F77_Linux = g77 -v
F77 = $(F77_$(OSNAME))
ifdef GFORTRAN
  F77 = gfortran -v
endif

CC_SunOS  = cc
CC_HP-UX  = cc
CC_Linux  = gcc
CC = $(CC_$(OSNAME))
# ===========================
#  Fortran Compiler Flags ===
FPLAT_FLAGS_SunOS = -O2 -e -DSunOS
FPLAT_FLAGS_HP-UX = -O2 -cpp +es +ppu -K -DHPUX
FPLAT_FLAGS_Linux = -DLinux -m486 -fno-automatic -finit-local-zero \
                    -ffixed-line-length-none -fno-second-underscore
ifdef GFORTRAN
  FPLAT_FLAGS_Linux = -DLinux -fno-automatic -ffixed-line-length-none -fno-second-underscore
endif

ROOTCFLAGS   := $(shell $(ROOTSYS)/bin/root-config --cflags)
ROOTLIBS     := $(shell $(ROOTSYS)/bin/root-config --libs)
ROOTGLIBS    := $(shell $(ROOTSYS)/bin/root-config --glibs)
ROOTINCLUDE  := $(shell $(ROOTSYS)/bin/root-config --cflags)

FPLAT_FLAGS = $(FPLAT_FLAGS_$(OSNAME))
#
#
#=========== Misc Libs
MISCLIBS_SunOS = -R/opt/SUNWspro/lib -L/opt/SUNWspro/lib -lV77 -lF77 -lM77 \
				-lm -lsocket -lgen -lsunmath 
MISCLIBS_HP-UX = -lm    
MISCLIBS_AIX = -L/usr/local/lib -L/usr/lib/Motif1.2 -L/usr/lib/X11R5 -lm \
   -lg -lxlf90 -lxlf -lc
MISCLIBS_Linux =  -L/usr/X11R6/lib -lm -ldl -lXpm -lX11 -lm -ldl -rdynamic
ifdef GFORTRAN
  MISCLIBS_Linux += -lgfortran
else
  MISCLIBS_Linux += -lg2c
endif
MISCLIBS = $(MISCLIBS_$(OSNAME))

#
#
SUPLIBS_SunOS = -L/lib -L/usr/lib -lrpcsvc -lc -lnsl

SUPLIBS = $(SUPLIBS_$(OSNAME))
#
#
CERNLIB_SunOS = -L/site/cernlib/sun4_solaris2/97a/lib -lpacklib -lkernlib -lmathlib
CERNLIB_HP-UX = -L/site/cernlib/hp_ux102/97a/lib -lpacklib -lkernlib -lmathlib
CERNLIB_AIX =  -L$(CERN_ROOT)/lib -lpacklib -lkernlib -lmathlib
CERNLIB_Linux = -L$(CERN_ROOT)/lib -lmathlib -lpacklib -lkernlib 

CERNLIBS = $(CERNLIB_$(OSNAME))
#
#
ADD_DEBUG = 
BOSLIBS_Local = -L$(CLAS_LIB) -lc_bos_io$(ADD_DEBUG) -lrecutl \
	      -lfputil$(ADD_DEBUG) -lbos$(ADD_DEBUG) -lfpack$(ADD_DEBUG) -lbos \
	       -lbosio -lst -lcc -lseb -lsc -ltrk -ldc 
#
#
#================
#Work with CalDB|
#================
#
#
INCLUDE += -I$(MYSQL_INCLUDE_PATH)

ifndef MAP
BOSLIBS_Local += -lcaldbMap$(ADD_DEBUG) -lclasutil$(ADD_DEBUG) -L$(MYSQL_LIB_PATH) -lmysqlclient -ldl
else
BOSLIBS_Local += -lmapmanager$(ADD_DEBUG)	
endif

ifeq "$(OSNAME)" "Linux"
BOSLIBS_Local +=  -lz -lnsl -ltcl$(ADD_DEBUG) -lc_bos_io
endif

ifeq "$(OS_NAME)" "SunOS"
OTHER = /usr/local/lib
BOSLIBS_Local += -lsocket -lnsl -L$(OTHER)/ -ltcl 
endif	
#
#=============================================================
#

BOSLIBS_HP-UX = $(BOSLIBS_Local)
BOSLIBS_AIX = $(BOSLIBS_Local)
BOSLIBS_Linux = $(BOSLIBS_Local)
BOSLIBS = $(BOSLIBS_Local)
#

#BOSLIBS  = -L$(CLAS_LIB)  -lrecutl$(ADD_DEBUG) \
#                -lfpack -lfputil -lclasutil \
#         -lbos$(ADD_DEBUG) -lc_bos_io$(ADD_DEBUG) -lmapmanager$(ADD_DEBUG)

#
#
#
# Makefile for making BOSIO LIB
#
ACP = gcc -M
#

#OBJDIR = $(PHY_DIR)/$(OSNAME)/

#@echo $(OBJDIR)

ROOTINCLUDE = -I$(ROOTSYS)/include 
PACKINCLUDE = -I../../../include
CLASINCLUDE = -I$(CLAS_PACK)/include

ADLIBS = -L$(CLAS_LIB)/ -l c_bos_io 
SINCS = $(PACKINCLUDE) $(ROOTINCLUDE) $(CLASINCLUDE)
SLIBS = -lmath 

PRIVLIBS = -L$(PHY_DIR)/lib/$(OS_NAME)

.PHONY: all 

$(OBJDIR)/%.o: %.cxx
	@test -d $(OBJDIR) || mkdir -p $(OBJDIR) ;
	$(ACP) $< > $(OBJDIR)/$*.o.depend $(SINCS)
	$(CPP) -c -o $@ $< $(SINCS)
#	mv $*.o $(OBJDIR)$*.o
#	mv $*.o.depend $(OBJDIR)$*.o.depend

$(OBJDIR)/%.o: $(ADPACK)/%.cxx
	@test -d $(OBJDIR) || mkdir -p $(OBJDIR) ;
	$(ACP) $< > $(OBJDIR)/$*.o.depend $(SINCS)
	$(CPP) -c -o $@ $< $(SINCS)
#	mv $*.o $(OBJDIR)$*.o
#	mv $*.o.depend $(OBJDIR)$*.o.depend

$(OBJDIR)/%.o: %.F
	@test -d $(OBJDIR) || mkdir -p $(OBJDIR) ;
	$(F77)  $(FPLAT_FLAGS) -c $< -o $(OBJDIR)/$@ $(SINCS)

$(OBJDIR)/%.o: $(ADPACK)/%.F
	@test -d $(OBJDIR) || mkdir -p $(OBJDIR) ;
	$(F77)  $(FPLAT_FLAGS) -c $< -o $@ $(SINCS)

$(OBJDIR)/%.o: %.c
	@test -d $(OBJDIR) || mkdir -p $(OBJDIR) ;
	$(CC)  -c $< -o $(OBJDIR)/$@ $(SINCS)

$(OBJDIR)/%.o: $(ADPACK)/%.c
	@test -d $(OBJDIR) || mkdir -p $(OBJDIR) ;
	$(CC)   -c $< -o $@ $(SINCS)

.PRECIOUS: %


LIBRARY    = ./

#
MAIN_SOURCE       = $(wildcard *.cxx) $(wildcard *.c) $(wildcard *.F) 
PACK_SOURCE       = $(wildcard $(ADPACK)/*.cxx) $(wildcard $(ADPACK)/*.F) $(wildcard $(ADPACK)/*.c)
#
PACK_OBJS1         = $(PACK_SOURCE:.cxx=.o) 
PACK_OBJS2         = $(PACK_OBJS1:.c=.o) 
PACK_OBJ           = $(PACK_OBJS2:.F=.o) 
#
MAIN_OBJS1         = $(MAIN_SOURCE:.cxx=.o)
MAIN_OBJS2         = $(MAIN_OBJS1:.c=.o)
MAIN_OBJ           = $(MAIN_OBJS2:.F=.o)
#
# 
#
#
MAIN_OBJECTS      = $(addprefix $(OBJDIR)/,$(MAIN_OBJ))
PACK_OBJECTS      = $(addprefix $(OBJDIR)/,$(notdir $(PACK_OBJ)))
#

LIBRARYS = $(ROOTLIBS) $(PRIVLIBS) $(BOSLIBS) $(MISCLIBS) $(SUPLIBS) $(CERNLIBS)
#
all: tdc_calib

#
tdc_calib:  $(MAIN_OBJECTS) $(PACK_OBJECTS)
	@test -d $(EXEDIR) || mkdir -p $(EXEDIR) ;
	@echo -------------------------------------------------
	@echo -- Linking code...
	@echo -------------------------------------------------
	$(CPP) -o $(EXEDIR)/$@ $(MAIN_OBJECTS) \
                     $(LIBRARYS) -lc_bos_io -ltcl $(PACK_OBJECTS) 
	@echo -------------------------------------
	@echo Compilation done..
#
#=========== Help Section ====
#


help:
	@echo ---------------------------------------
	@echo Makefile for SC TDC Calibration
	@echo Type "make tdc_calib" to Compile code
	@echo Type "make show" to see enviroments
	@echo ---------------------------------------



show:
	@echo _______________________________________
	@echo   $(OSNAME) platform..
	@echo _______________________________________
	@echo MAIN SOURCE FILES = $(MAIN_SOURCE)
	@echo _______________________________________
	@echo PACK SOURCE FILES = $(PACK_SOURCE)
	@echo _______________________________________
	@echo MAIN OBJ = $(MAIN_OBJ)
	@echo _______________________________________
	@echo PACK OBJ = $(PACK_OBJ)	
	@echo _______________________________________
	@echo PACK OBJECTS = $(PACK_OBJECTS)	
	@echo _______________________________________
	@echo MAIN OBJECTS = $(MAIN_OBJECTS)	
	@echo _______________________________________
	@echo ROOT LIBS = $(ROOTLIBS)	
	@echo _______________________________________
	@echo PRIV LIBS = $(PRIVLIBS)	
	@echo _______________________________________
	@echo BOS LIBS  = $(BOSLIBS)	
	@echo _______________________________________
	@echo MISC LIBS = $(MISCLIBS)	
	@echo _______________________________________
	@echo SUPL LIBS = $(SUPLLIBS)	
	@echo _______________________________________
	@echo CERN LIBS = $(CERNLIBS)	
	@echo _______________________________________
	@echo COMPILERS: CPP = $(CPP), ACPP = $(ACP)

