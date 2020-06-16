# bankdefs makefile
# $Id: Makefile,v 1.29 2000/07/05 16:01:44 marki Exp $
# makes include files for fortran bos access: clas_index.inc and
#       clas_offsets.inc
# makes include file with cvs tag: cvs_tagname.inc
# makes libbankdefs.a

ifndef OS_NAME
	OS_NAME = $(shell $(CLAS_CMS)/uname_clas)
endif

ifdef DEBUG
	ADD_DEBUG = _debug
endif

ALL_DDL = $(wildcard *.ddl)

NO_CAT = clasbanks.ddl *clasbanks*.ddl

DDL_FILES = $(filter-out $(NO_CAT), $(ALL_DDL))

ddl: lib

lib: ../inc_derived/clas_index.inc ../inc_derived/clas_offsets.inc \
    ../inc_derived/cvs_tagname.inc libbankdefs

clasbanks.ddl : $(DDL_FILES)
#	If there is a lockfile present, another process may be making the
#	header file. In that case don't make it.
	test -f lockfile2 || $(MAKE) cat_ddl_files
#	Wait for the lockfile to disappear
	$(MAKE) wait2

../inc_derived/clas_index.inc ../inc_derived/clas_offsets.inc: clasbanks.ddl \
    $(CLAS_SCRIPTS)/bos_param
	@test -d ../inc_derived || mkdir -p ../inc_derived
	$(SHELL) -c '$(CLAS_SCRIPTS)/bos_param clasbanks.ddl'

../inc_derived/cvs_tagname.inc: head.ddl $(CLAS_SCRIPTS)/set_tag_param
	@test -d ../inc_derived || mkdir -p ../inc_derived
	$(SHELL) -c '$(CLAS_SCRIPTS)/set_tag_param head.ddl'

../inc_derived/clasbanks.h: $(CLAS_SCRIPTS)/clasbanks.perl $(DDL_FILES)
	@test -d ../inc_derived || mkdir ../inc_derived
#	If there is a lockfile present, another process may be making the
#	header file. In that case don't make it.
	test -f lockfile || $(MAKE) run_perl_script
#	Wait for the lockfile to disappear
	$(MAKE) wait

cat_ddl_files:
	date > lockfile2
	$(SHELL) -c 'cat $(DDL_FILES) > clasbanks.ddl'
	$(RM) lockfile2

run_perl_script:
	date > lockfile
	$(CLAS_SCRIPTS)/clasbanks.perl
	$(RM) lockfile

libbankdefs: ../inc_derived/clasbanks.h
	$(RM) clasbanks.h # remove any stale local versions
	$(MAKE) -f $(CLAS_CMS)/Makefile lib

wait:
#	Test for the lockfile. If it's there, go to sleep. If not go to the
#	next command.
	@if test -f lockfile; then sleep 15; fi
#	This time throw an error if lockfile still there
	@if test -f lockfile; then $(MAKE) lockfile_error; fi

wait2:
#	Test for the lockfile. If it's there, go to sleep. If not go to the
#	next command.
	@if test -f lockfile2; then sleep 15; fi
#	This time throw an error if lockfile still there
	@if test -f lockfile2; then $(MAKE) lockfile_error; fi

lockfile_error:
	@echo bankdefs/lockfile exists, cannot continue make
	exit 1

exe:
	@echo nothing to be done for bankdefs

clean:
	$(RM) -r ../inc_derived
	$(RM) clasbanks.cc clasbanks.ddl
	$(RM) $(TOP_DIR)/lib/$(OS_NAME)/libbankdefs.a
	$(RM) $(TOP_DIR)/lib/$(OS_NAME)/libbankdefs_debug.a
	$(RM) -r depends/$(OS_NAME)
# end of bankdefs makefile

.PHONY : clean exe ddl lib wait libbankdefs
