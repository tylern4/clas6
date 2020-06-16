#
# Individual package makefile, calls $CLAS_CMS/Makefile with the
# appropriate arguments
#

# TOP_DIR points to the final location of binary and library tree:
# $(TOP_DIR)/$(OS_NAME)/bin/       $(TOP_DIR)/$(OS_NAME)/lib/
#
# USERS should set the enviroment TOP_DIR to a directory of their choice
# to override the default relative path

# CLAS_CMS points to the location of the included makefiles.  
# Default is to get this from the enviroment.

ifndef CLAS_CMS
 CLAS_CMS = ../
endif


# and gsim/include to the include search path

CLAS_INCLUDES += -I../gsim/include -I$(CLAS_PACK)/gsim/include

# define the link list

LIBNAMES=$(GSIM_BAT_LIBS) $(GSIM_BAT_MD_LIBS) 
LIBNAMES += tag

SHARED_LIBS=$(GSIM_BAT_SHARED_LIBS)

# Check for local copies of *.F and *.c files, and add them to the 
# list of objects that are required. This is needed because with duplicate
# nemes of routines that are already in the libgsim.a library, those will
# be the routines used instead of the local ones.

LOCAL_SOURCES_F += $(filter-out gsim_bat.F, $(wildcard *.F))
LOCAL_SOURCES_C += $(wildcard *.c)
CREATED_F += gsim_bat.F $(LOCAL_SOURCES_F)
CREATED_C += $(LOCAL_SOURCES_C)

# get the mother of all make files


ifdef USE_STATIC_LIBS
  ADD_ONLINE = _static
endif


include $(CLAS_CMS)/Makefile

