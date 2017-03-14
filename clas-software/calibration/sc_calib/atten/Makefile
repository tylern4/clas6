ifdef GFORTRAN
 ROOTLIBS = -lgfortran $(shell $(ROOTSYS)/bin/root-config --libs)
else
 ROOTLIBS = -lg2c $(shell $(ROOTSYS)/bin/root-config --libs)
endif

LIBNAMES += sc$(ADD_DEBUG) c_bos_io$(ADD_DEBUG) fputil$(ADD_DEBUG) bos$(ADD_DEBUG) fpack$(ADD_DEBUG) \
   gamecock$(ADD_DEBUG) caldbMap$(ADD_DEBUG) c_cern$(ADD_DEBUG) clasutil$(ADD_DEBUG) \
   recutl$(ADD_DEBUG) recsis$(ADD_DEBUG) packlib mysqlclient recsis$(ADD_DEBUG)

ifeq "$(shell uname)" "Linux"
  LIBNAMES += tcl nsl z m
endif

tof_calib:
	$(MAKE) exe EXE=$(localbin)/$@$(ADD_DEBUG) CREATED_C="tof_calib.c" LNKCMD="g++ -g" SHARED_LIBS="$(ROOTLIBS)"


include $(CLAS_CMS)/Makefile

