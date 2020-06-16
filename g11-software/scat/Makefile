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

#OS_NAME = $(shell $(CLAS_CMS)/uname_clas)

ifndef CLAS_CMS
 CLAS_CMS = ../
endif

# additions to  the include search path

CLAS_INCLUDES += 

# Check for local copies of *.F and *.c files, and add them to the 
# list of objects that are required. This is needed because with duplicate
# nemes of routines that are already in the libgsim.a library, those will
# be the routines used instead of the local ones.

SCAT_SOURCES = j_svc.c j_xdr.c active.c reg.c svt.c j_clnt.c j_xdr.c
SCAT_SOURCES += scat.c spack.c sinfo.c lst.c evout.c svt.c

SCATBOS_SOURCES = scatbos.c evout.c

LIBRARY1 = scat
LIBRARY2 = scatBos

all: lib

lib:
	$(MAKE) -f $(CLAS_CMS)/Makefile C_SOURCES="$(SCAT_SOURCES)" \
                   MAIN_DIRECTORY=$(LIBRARY1) lib
	$(MAKE) -f $(CLAS_CMS)/Makefile C_SOURCES="$(SCATBOS_SOURCES)" \
                   CREATED_C=rec_work.c \
                   MAIN_DIRECTORY=$(LIBRARY2) lib

exe:
	@echo "No executible for the scat package"

help:
	$(MAKE) -f $(CLAS_CMS)/Makefile C_SOURCES="$(SCAT_SOURCES)" \
                   MAIN_DIRECTORY=$(LIBRARY1) help
	$(MAKE) -f $(CLAS_CMS)/Makefile C_SOURCES="$(SCATBOS_SOURCES)" \
                   MAIN_DIRECTORY=$(LIBRARY2) help

clean:
	$(MAKE) -f $(CLAS_CMS)/Makefile clean

# end of scat makefile


