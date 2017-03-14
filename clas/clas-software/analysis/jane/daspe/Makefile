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
export TOP_DIR = ../

# define the link list
CREATED_F=daspe_main.F

CERNLIB =  mathlib graflib packlib   kernlib

LIBNAMES =daspe $(CERNLIB)

#
include $(CLAS_CMS)/Makefile






