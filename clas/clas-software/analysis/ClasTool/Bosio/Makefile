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

# list of routines I don't want to compile in the shared library.
# in particular fpackf.F, namind.F, nlist.F  
# call those hexa to char conversion routines 
# ucoth and uhtoc from the kernlib cern library,
# I don't want to link the cernlib everytime I link bosio.
# If we really wanna use them, may be we should extract them.
# E.A

#
#BOSIO_FOR_OUT = "fpackf.F namind.F nlist.F" 
# get the generic Makefile and predefined link lists;  

SRC_FILES = $(wildcard *.c)
SRC_FORTRAN = $(wildcard *.F)
OBJ_FILES  = $(SRC_FILES:.c=.o)
OBJ_FILES += $(SRC_FORTRAN:.F=.o)

all: lib

shlib: $(OBJ_FILES)
	ar r $(CLASTOOL)/slib/$(OS_NAME)/libbosio.a $(OBJ_FILES)

lib: $(OBJ_FILES)
	ar r $(CLASTOOL)/slib/$(OS_NAME)/libbosio.a $(OBJ_FILES)

clean:
	@echo 
	@echo '****>>>>> Cleaning distribution'
	@echo 
	@rm -rf *.o
	@rm -rf libbosio.a
#=====================================================================
%.o:%.c
	gcc -c -w -D $(OS_NAME) $<
%.o:%.F
	gfortran -c $<



