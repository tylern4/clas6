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
 TOP_DIR = $(HOME)
endif

# CLAS_CMS points to the location of the included makefiles.  
# Default is to get this from the enviroment.

ifndef CLAS_CMS
 CLAS_CMS = ../
endif
INCLUDE_ALL += ./include
DEFAULT_INCLUDES += ./include 

all: lib

lib: $(FORCE)
	$(MAKE) -e -f $(CLAS_CMS)/Makefile lib INCLUDE_FILES="include/dummy.inc" 

clean: $(FORCE)
	$(MAKE) -e -f $(CLAS_CMS)/Makefile clean

help: $(FORCE)
	$(MAKE) -e -f $(CLAS_CMS)/Makefile help

bat: $(FORCE)
	$(MAKE) -e -f $(CLAS_CMS)/Makefile exe \
		INCLUDE_FILES="include/dummy.inc" \
		REQUIRED_OBJS="$(CLAS_LIB)/gsim_bat.o" \
		LIBNAMES="$(GSIM_BAT_LIBS) \
				$(MAIN_DIRECTORY)$(ADD_DEBUG) \
				$(GSIM_BAT_MD_LIBS)" \
		SHARED_LIBS="$(GSIM_BAT_SHARED_LIBS)" \

int: $(FORCE)
	$(MAKE) -e -f $(CLAS_CMS)/Makefile exe \
		INCLUDE_FILES="include/dummy.inc" \
		REQUIRED_OBJS="$(CLAS_LIB)/gsim_int.o" \
		LIBNAMES="$(GSIM_INT_LIBS) \
				$(MAIN_DIRECTORY)$(ADD_DEBUG) \
				$(GSIM_INT_MD_LIBS)" \
		SHARED_LIBS="$(GSIM_INT_SHARED_LIBS)" \

FORCE = DUMMY
DUMMY:

# get the defined link lists, link.$(OS_NAME).mk contains the machine
# dependent part of the link list.

include $(CLAS_CMS)/flags.$(OS_NAME).mk
include $(CLAS_CMS)/link.mk
include $(CLAS_CMS)/link.$(OS_NAME).mk


