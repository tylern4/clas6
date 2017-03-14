LIBNAMES= $(MAIN_DIRECTORY) bankdefs  \
 c_bos_io fputil recutl bos fpack mapmanager \
 clasutil c_cern mathlib kernlib packlib $(RECSIS_MD_LIBS) \
 $(CXX_LIBS)

SHARED_LIBS=$(RECSIS_SHARED_LIBS) $(CXX_SHARED_LIBS)

CREATED_CC = elast_gen_wr.cc

include $(CLAS_CMS)/Makefile


ifeq ($(OS_NAME),AIX)
        CXX_LIBS += C
        LIB_PATH += /usr/lib
endif

ifeq ($(OS_NAME),HP-UX)
        CXX_LIBS += C
        LIB_PATH += /opt/CC/lib/eh
endif

ifeq ($(OS_NAME),Linux)
        CXX_SHARED_LIBS += /usr/lib/libstdc++.so.2.7.2.8
endif

ifeq ($(OS_NAME),SunOS)
        CLAS_INCLUDES += -I/apps/SUNWspro/SC4.2/include/CC
        CXX_LIBS += C
        LIB_PATH += /apps/SUNWspro/SC4.2/lib
endif
