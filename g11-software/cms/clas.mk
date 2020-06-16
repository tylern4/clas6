#
# user/developer CLAS.mk Use this makefile if you are working on more
# than one package simultaneously.  For example, if you want to work on
# trk, bos and dc  do the following:
#                                       cvs checkout trk
#                                       cvs checkout dc
#                                       cvs checkout bos
#                                       cvs checkout user
#                                       make -f $CLAS_CMS/clas.mk
#
# note: you need the user package to build the binary.
#
# The binaries and libraries are deposited in /home/$USERNAME/lib/$OS_NAME and 
# /home/$USERNAME/bin/$OS_NAME.  You can change this by defining an 
# enviroment variable: 
#                          setenv TOP_DIR /scratch/$USERNAME
# before invoking the make.

ifndef CLAS_CMS
  export CLAS_CMS = $(shell pwd)/cms
  @echo  defined CLAS_CMS as $(CLAS_CMS)
endif

ifndef OS_NAME
 OS_NAME = $(shell $(CLAS_CMS)/uname_test)
endif

ifndef TOP_DIR
  export TOP_DIR = /home/$(USERNAME)
endif

all: lib exe

lib:
	$(MAKE) -e -f $(CLAS_CMS)/clas_lib.mk

exe: lib
	$(MAKE) -e -f $(CLAS_CMS)/clas_bin.mk

recsis: lib
	$(MAKE) -e -f $(CLAS_CMS)/clas_bin.mk recsis

user: lib
	cd user; $(MAKE) exe

user_ana: lib
	cd user_ana; $(MAKE) exe

user_anaNM: lib
	cd user_anaNM; $(MAKE) exe

EventStore: lib
	cd EventStore; $(MAKE) exe

ced: lib
	cd ced; $(MAKE) exe

utilities: lib
	cd utilities; $(MAKE) exe

gsim_bat: lib
	$(MAKE) -e -f $(CLAS_CMS)/clas_bin.mk gsim_bat

gsim_int: lib
	$(MAKE) -e -f $(CLAS_CMS)/clas_bin.mk gsim_int

celeg: lib
	$(MAKE) -e -f $(CLAS_CMS)/clas_bin.mk celeg

fdump: lib
	$(MAKE) -e -f $(CLAS_CMS)/clas_bin.mk fdump

clean: 
	find . -name "$(OS_NAME)/*.d" -exec rm  {} \;
	rm $(TOP_DIR)/lib/$(OS_NAME)/*

.PHONY : lib exe recsis user gsim_bat gsim_int fdump clean




