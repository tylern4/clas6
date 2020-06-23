#
#
#
#
#

PHYSANA_OPTION=

ifdef DEBUG
  ADD_DEBUG=_debug
  PHYSANA_OPTION=DEBUG=1 ADD_DEBUG=_debug  
endif

ifndef CLAS_CMS
  export CLAS_CMS = $(shell pwd)/cms
  @echo  defined CLAS_CMS as $(CLAS_CMS)
endif

ifndef OS_NAME
   export OS_NAME = $(shell $(CLAS_CMS)/uname_test)
endif

ifndef TOP_DIR
  export TOP_DIR = $(HOME)
endif

MAKE = make

all:
	cd physana; $(MAKE) $(PHYSANA_OPTION) p2p_delay_el$(ADD_DEBUG)

help:
	@echo Makefile for p2p_delay_el Program
	@echo Make = $(MAKE)










