#
# Individual package makefile, includes $CLAS_CMS/Makefile with the appropriate arguments
#
# TOP_DIR points to the final location of binary and library tree:
# $(TOP_DIR)/bin/$(OS_NAME)       $(TOP_DIR)/lib/$(OS_NAME)
#
# USERS should set the enviroment TOP_DIR to a directory of their choice
# to override the default relative path (default= /home/$USERNAME

# CLAS_CMS points to the location of the included makefiles.  
# Default is to get this from the enviroment.

ifndef CLAS_CMS
 CLAS_CMS = ../
endif
#export TOP_DIR = $(shell pwd)

# define the link list
CREATED_F=fsgen_main.F

CERNLIB=  mathlib graflib packlib lepto651 jetset74 kernlib 

LIBNAMES = fsgen$(ADD_DEBUG) scaler bosio$(ADD_DEBUG) c_cern$(ADD_DEBUG) $(CERNLIB)
ifeq "$(OS_NAME)" "SunOS"
LIBNAMES += nsl socket
endif
#
LIBNAMES += $(RECSIS_LIBS1) $(MAIN_DIRECTORY)$(ADD_DEBUG) $(RECSIS_LIBS2) \
            $(RECSIS_MD_LIBS) $(RECSIS_LIBS3) $(CERNLIB)
SHARED_LIBS=$(RECSIS_SHARED_LIBS)

#
include $(CLAS_CMS)/Makefile

