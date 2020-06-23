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

# define the link list

REQUIRED_OBJS=$(CLAS_LIB)/recsis$(ADD_DEBUG).o $(CLAS_LIB)/rec_work$(ADD_DEBUG).o

LIBNAMES=$(RECSIS_LIBS1) $(MAIN_DIRECTORY)$(ADD_DEBUG) $(RECSIS_LIBS2) vertex\
             $(RECSIS_MD_LIBS) $(RECSIS_LIBS3)

SHARED_LIBS=$(RECSIS_SHARED_LIBS)


PACK_NAMES += ec sc cc seb trk lac

INCLUDE_ALL += $(addprefix $(CLAS_PACK)/, $(PACK_NAMES))

DEFAULT_INCLUDES += $(addprefix -I,$(INCLUDE_ALL))

# get the generic Makefile and predefined link lists;  

include $(CLAS_CMS)/Makefile


