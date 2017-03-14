
ifdef GFORTRAN
 ROOTLIBS = -lgfortran $(shell $(ROOTSYS)/bin/root-config --libs)
else
 ROOTLIBS = -lg2c $(shell $(ROOTSYS)/bin/root-config --libs)
endif

LIBNAMES += gamecock$(ADD_DEBUG) sc$(ADD_DEBUG) st$(ADD_DEBUG) c_bos_io$(ADD_DEBUG) \
      fputil$(ADD_DEBUG) bos$(ADD_DEBUG) fpack$(ADD_DEBUG) recutl$(ADD_DEBUG) \
            caldbMap$(ADD_DEBUG) c_cern$(ADD_DEBUG) clasutil$(ADD_DEBUG) \
	  packlib mysqlclient

ifeq "$(shell uname)" "Linux"
  LIBNAMES += tcl nsl z m
endif

make_tw_histos:
	$(MAKE) exe EXE=$(localbin)/$@$(ADD_DEBUG) CREATED_C="make_tw_histos.c" LNKCMD="g++ -g" SHARED_LIBS="$(ROOTLIBS)"


include $(CLAS_CMS)/Makefile

