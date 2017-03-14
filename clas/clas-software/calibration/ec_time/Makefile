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
#include makefmap
# define the link list
CREATED_F=ectimes4.F
F_SOURCES=ectimes4.F

CERNLIB=  mathlib graflib packlib kernlib 

LIBNAMES += ec_time$(ADD_DEBUG) scaler$(ADD_DEBUG) bosio$(ADD_DEBUG) recutl \
	ec$(ADD_DEBUG) mysqlclient caldbMap clasutil$(ADD_DEBUG) c_cern$(ADD_DEBUG) $(CERNLIB) \
	$(BOSIO_MD_LIBS)

#LIBNAMES += $(MAIN_DIRECTORY)$(ADD_DEBUG)  $(RECSIS_LIBS2) \
#            $(RECSIS_MD_LIBS) $(RECSIS_LIBS3) bosio$(ADD_DEBUG) c_cern$(ADD_DEBUG) $(CERNLIB)

SHARED_LIBS+=$(RECSIS_SHARED_LIBS)

#
include $(CLAS_CMS)/Makefile

fout_map: fout_map.F
	f77 -o  $(TOP_DIR)/bin/$(OS_NAME)/fout_map fout_map.F
