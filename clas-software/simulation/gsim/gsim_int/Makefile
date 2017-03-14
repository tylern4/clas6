#
# Individual package makefile, calls $CLAS_CMS/Makefile with the
# appropriate arguments

# TOP_DIR points to the final location of binary and library tree:
# $(TOP_DIR)/$(OS_NAME)/bin/       $(TOP_DIR)/$(OS_NAME)/lib/

# USERS should set the enviroment TOP_DIR to a directory of their choice
# to override the default relative path

# CLAS_CMS points to the location of the included makefiles.  
# Default is to get this from the enviroment.

# ifeq ("$(OS_NAME)","Linux") 
#  NO_MOTIF=1     <<< that flag should be set on the command line!!!!
# endif

ifdef NO_MOTIF
 CPPFLAGS += -DNO_MOTIF
endif

ifndef CLAS_CMS
 CLAS_CMS = ../
endif

# and gsim/include to the include search path

CLAS_INCLUDES += -I$(CLAS_PACK)/gsim/include -I$(CLAS_PACK)/gsim

# define the link list

LIBNAMES= $(GSIM_INT_LIBS) $(GSIM_INT_MD_LIBS)

SHARED_LIBS=$(GSIM_INT_SHARED_LIBS)

# Check for local copies of *.F and *.c files, and add them to the 
# list of objects that are required. This is needed because with duplicate
# nemes of routines that are already in the libgsim.a library, those will
# be the routines used instead of the local ones.

LOCAL_SOURCES_F += $(filter-out gsim_int.F, $(wildcard *.F))
LOCAL_SOURCES_C += $(wildcard *.c)
CREATED_F+=gsim_int.F $(LOCAL_SOURCES_F)
CREATED_C+= $(LOCAL_SOURCES_C)

# Are we making a static exec ?

ifdef USE_STATIC_LIBS
  ADD_ONLINE = _static
endif

# get the mother of all make files

include $(CLAS_CMS)/Makefile

#end of gsim_int makefile





