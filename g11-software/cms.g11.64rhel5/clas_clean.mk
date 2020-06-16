#
# this make file enters each sub-directory in tree and either
# invokes the "generic.mk lib" or if a Makefile is found in the
# subdirectory invokes  that Makefile.  Sub-directory called "cms"
# is ignored.
#

ifndef OS_NAME
 OS_NAME = $(shell $(CLAS_CMS)/uname_clas)
endif

FORCE = DUMMY

# make of list of directories and/or files you do not want to enter

NO_MAKE = CVS cms bin lib Makefile
NO_MAKE += $(wildcard *.* *~)

# make a complete list of the directory

DIRE = $(wildcard *)

# remove the do not go there files/directories

ALLLIBS = $(filter-out $(NO_MAKE), $(DIRE))

all: $(ALLLIBS)

$(ALLLIBS): 
	cd $@; ln -s $(CLAS_CMS)/Makefile . 2> /dev/null; $(MAKE) -e clean;

.PHONY : $(ALLLIBS) all





