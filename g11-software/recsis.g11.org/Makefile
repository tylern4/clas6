#
# Individual package makefile, calls $CLAS_CMS/Makefile with the
# appropriate arguments

# TOP_DIR points to the final location of binary and library tree:
# $(TOP_DIR)/$(OS_NAME)/bin/       $(TOP_DIR)/$(OS_NAME)/lib/

# USERS should set the enviroment TOP_DIR to a directory of their choice
# to override the default relative path

# CLAS_CMS points to the location of the included makefiles.  
# Default is to get this from the enviroment.

ifndef CLAS_CMS
 CLAS_CMS = ../
endif

CREATED_F=recsis.F

COMPILED_OBJS=$(CLAS_LIB)/rec_work$(ADD_DEBUG).o

LIBNAMES= $(RECSIS_LIBS1) $(MAIN_DIRECTORY)$(ADD_DEBUG) 
LIBNAMES+= $(RECSIS_LIBS2) $(RECSIS_MD_LIBS) $(RECSIS_LIBS3)

SHARED_LIBS=$(RECSIS_SHARED_LIBS)
ifdef ONLINE
 INCLUDE_ALL += $(CLON_SOURCE)/clas/bosio/bosio.s $(CLON_INC) $(CODA)/common/include $(RTHOME)/include
endif
include $(CLAS_CMS)/Makefile
