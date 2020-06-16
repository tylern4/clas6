#
# this make file enters each sub-directory in tree and either
# invokes the "generic.mk lib" or if a Makefile is found in the
# subdirectory invokes  that Makefile.  Sub-directory called "cms"
# is ignored.
#

ifndef OS_NAME
 OS_NAME = $(shell $(CLAS_CMS)/uname_clas)
endif

# include the link list to make definition of packages to build easier

include $(CLAS_CMS)/link.mk
include $(CLAS_CMS)/link.$(OS_NAME).mk

# make of list of directories and/or files you do not want to enter

NO_MAKE = CVS cms bin lib Makefile scrips scripts_cooking_scripts recon libpp include inc_derived utilities Dispatcher Xpwa ccm dcm dct0
NO_MAKE += tests bankdefs draw dstmaker ecm fit kinefit lcm phyana pwa2000 pwatape reccal rtm scm tcm gsimDisp
NO_MAKE += $(wildcard *.* *~)

# make a complete list of the directory

DIRE = $(wildcard *)

# remove the do not go there files/directories

ALLLIBS0 = $(filter-out $(NO_MAKE), $(DIRE))

ALLLIBS = bankdefs $(ALLLIBS0) # put bankdefs at beginning of list

# these libraries are needed to build recsis

# Add Map explicitly and remove mapmanager from list of libraries since
# library name does not match directory name.
RECSIS_LIB0 = $(RECLIBS1) $(RECLIBS2) $(RECLIBS3) Map
RECSIS_LIB = $(filter-out mapmanager, $(RECSIS_LIB0))

# these libraries are needed to build GSIM

GSIM_LIB = $(filter-out mapmanager, $(GSIM_INT_LIBS0)) Map

# these libraries are needed to build ced

CED_LIB = ced Hv

# these libraries are needed to build celeg

CELEG_LIB = celeg

# these libraries are needed to build psg

PSG_LIB = psg

# these files are needed to build clasbank.ddl, clas_index.inc clas_offsets.inc

DDL_DIR = bankdefs 

# these libraries are needed to build fdump

FDUMP_LIB = fputil bos clasutil fpack bosio

# this library is needed to build PHOTON_BEAM

PHOTON_LIB = photon_beam 

ifeq (Linux, $(OS_NAME))
   RECSIS_LIB += patches
   GSIM_LIB += patches
endif
ifeq (AIX, $(OS_NAME))
   RECSIS_LIB += patches
   GSIM_LIB += patches
endif

# targets

all: $(ALLLIBS) 

recsis_lib:  $(RECSIS_LIB)

ntuplemakers_lib: $(RECSIS_LIB)

user_ana_lib: $(RECSIS_LIB)

gsim_lib:  $(GSIM_LIB)

celeg_lib: $(CELEG_LIB)

psg_lib: $(PSG_LIB)

fdump_lib: $(FDUMP_LIB)

photon_beam : $(PHOTON_LIB)

ced_lib : $(CED_LIB)

ddl_file : $(DDL_DIR)

help: 
	@echo ALLLIBS = $(ALLLIBS)
	@echo DDL_DIR = $(DDL_DIR)
	@echo DIRE = $(DIRE)
	@echo NO_MAKE = $(NO_MAKE)
	@echo RECSIS_LIB = $(RECSIS_LIB)
	@echo RECSIS_LIB0 = $(RECSIS_LIB0)
	@echo RECLIBS1 = $(RECLIBS1)
	@echo RECLIBS2 = $(RECLIBS2)
	@echo RECLIBS3 = $(RECLIBS3)
	@echo RECSIS_MD_LIBS = $(RECSIS_MD_LIBS)

$(ALLLIBS):
	if test -d $@; then \
	    $(MAKE) -e -C $@ lib; \
	fi

.PHONY : $(ALLLIBS) help all recsis gsim celeg user_ana ntuplemakers






