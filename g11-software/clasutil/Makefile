#
# Individual package makefile, calls $CLAS_CMS/Makefile with the
# appropriate arguments
#
#   get the operating system name

ifndef OS_NAME
 OS_NAME = $(shell $(CLAS_CMS)/uname_clas)
endif

# get the directory make has been invoked from, this is used to NAME the
# binary and executable

MAIN_DIRECTORY = $(notdir $(shell pwd))

# TOP_DIR points to the final location of binary and library tree:
# $(TOP_DIR)/$(OS_NAME)/bin/       $(TOP_DIR)/$(OS_NAME)/lib/
#
# USERS should set the enviroment TOP_DIR to a directory of their choice
# to override the default relative path

ifndef TOP_DIR  
 TOP_DIR = /home/$(USERNAME)
endif

# CLAS_CMS points to the location of the included makefiles.  
# Default is to get this from the enviroment.

ifndef CLAS_CMS
 CLAS_CMS = ../
endif

all: lib exe

lib:
	$(MAKE) -e -f $(CLAS_CMS)/Makefile lib

exe:
	@echo "no exe to be made for $(MAIN_DIRECTORY)"

clean:
	$(MAKE) -e -f $(CLAS_CMS)/Makefile clean

help:
	$(MAKE) -e -f $(CLAS_CMS)/Makefile help

.PHONY : lib exe clean help

# get the defined link lists, link.$(OS_NAME).mk contains the machine
# dependent part of the link list.

include $(CLAS_CMS)/flags.$(OS_NAME).mk

