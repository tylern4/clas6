#
# Individual package makefile, calls $CLAS_CMS/Makefile with the
# appropriate arguments
#
# CLAS_CMS points to the location of the included makefiles.  
# Default is to get this from the enviroment.

ifndef CLAS_CMS
 CLAS_CMS = ../
endif

# for gthadr.F set path for include files

#INCLUDE_ALL += gsim/include
CLAS_INCLUDES += -I$(CLAS_PACK)/gsim/include  

include $(CLAS_CMS)/Makefile
