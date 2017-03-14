ifndef MAP
    CALIB_LIB = caldbMap
    MYSQL_LIBS = mysqlclient
else
    CALIB_LIB = mapmanager
endif


LLIBNAMES = $(MAIN_DIRECTORY) ec bankdefs  \
 pid tag lac seb st sc dc cc ec trk \
 recutl c_bos_io fputil bos fpack $(CALIB_LIB) \
 clasutil c_cern

LIBNAMES = $(addsuffix $(ADD_DEBUG), $(LLIBNAMES))

LIBNAMES += mathlib kernlib packlib $(RECSIS_MD_LIBS) \
 $(CXX_LIBS) $(MYSQL_LIBS)

SHARED_LIBS = $(RECSIS_SHARED_LIBS) $(CXX_SHARED_LIBS)

CREATED_CC = gpp.cc

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
# Generic Linuxes.
#
ifeq ($(findstring Linux,$(OS_NAME)),Linux)
        CXX_LIBS += stdc++ z
endif

ifeq ($(OS_NAME), Linux64RHFC6)
        CXX_LIBS += stdc++ 
        LIB_PATH += /usr/lib64
endif

ifeq ($(OS_NAME),SunOS)
        CXX_LIBS += Cstd Crun
endif







