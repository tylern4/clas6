LIBNAMES= $(MAIN_DIRECTORY)$(ADD_DEBUG) ec$(ADD_DEBUG) tag$(ADD_DEBUG) vertex$(ADD_DEBUG) bankdefs$(ADD_DEBUG) pid$(ADD_DEBUG) lac$(ADD_DEBUG) seb$(ADD_DEBUG) st$(ADD_DEBUG) sc$(ADD_DEBUG) dc$(ADD_DEBUG) cc$(ADD_DEBUG) ec$(ADD_DEBUG) \
 trk$(ADD_DEBUG) recutl$(ADD_DEBUG) c_bos_io$(ADD_DEBUG) fputil$(ADD_DEBUG) \
 bos$(ADD_DEBUG) fpack$(ADD_DEBUG) mapmanager$(ADD_DEBUG) \
 clasutil$(ADD_DEBUG) c_cern$(ADD_DEBUG) \
 mathlib kernlib packlib $(RECSIS_MD_LIBS) \
 $(CXX_LIBS)

SHARED_LIBS=$(RECSIS_SHARED_LIBS) $(CXX_SHARED_LIBS)

CREATED_CC = nt10maker.cc

include $(CLAS_CMS)/Makefile


ifeq ($(OS_NAME),AIX)
        CXX_LIBS += C
        LIB_PATH += /usr/lib
endif

ifeq ($(OS_NAME),HP-UX)
	LNKCMD = CC
        CXX_LIBS += C
        LIB_PATH += /opt/CC/lib/eh
endif


#
# For all things Linux.
# 
ifeq ($(findstring Linux,$(OS_NAME)),Linux)
        CXX_LIBS += stdc++
endif

ifeq ($(OS_NAME),SunOS)
        CXX_LIBS += Crun Cstd
endif

ifeq ($(OS_NAME),SunOSws6)
        CXX_LIBS += Crun Cstd
endif
