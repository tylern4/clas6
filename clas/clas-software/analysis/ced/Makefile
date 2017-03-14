#
# CED Makefile
#
# TOP_DIR points to the final location of binary and library tree:
# $(TOP_DIR)/$(OS_NAME)/bin/       $(TOP_DIR)/$(OS_NAME)/lib/
#
# USERS should set the enviroment TOP_DIR to a directory of their choice
# to override the default relative path (default= /home/$USERNAME

# CLAS_CMS points to the location of the included makefiles.  
# Default is to get this from the enviroment.

ifndef CLAS_CMS
 CLAS_CMS = ../cms
endif

# new includes  

DEFAULT_INCLUDES += -I../Hv/src -I$(CLAS_PACK)/Hv/src

# new CPPFLAGS

CPPFLAGS += -DNO_ET

# define the link list

CREATED_C = ced.c

# add the location of Hv library to the X_LIB variable
# which will then make it part of the LIB_PATH search

X_LIB += $(HV_LOCATION)/$(OS_NAME)

# define the link list, which is kept in $CLAS_CMS/link.*.mk

LIBNAMES=$(MAIN_DIRECTORY)$(ADD_DEBUG) $(CED_LIBS) $(CED_MD_LIBS)
LIBNAMES+= Hv$(ADD_DEBUG)

SHARED_LIBS=$(CED_SHARED_LIBS)

# get the generic Makefile and predefined link lists;  
# link.$(OS_NAME).mk contains the machine
# dependent part of the link list.

include $(CLAS_CMS)/Makefile

# LD section

ifeq "$(OS_NAME)" "OSF1" 
 FLDFLAGS += -nofor_main
 LIBNAMES+= gcc
endif

env:
	@echo OS_NAME $(OS_NAME)
	@echo HV_LOCATION $(HV_LOCATION)
	@echo CED_LIBS $(CED_LIBS)
	@echo CED_MD_LIBS $(CED_MD_LIBS)
	@echo MAIN_DIRECTORY $(MAIN_DIRECTORY)
	@echo CED_SHARED_LIBS $(CED_SHARED_LIBS)
	@echo LIBNAMES $(LIBNAMES)
