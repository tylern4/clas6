LIBNAMES= $(MAIN_DIRECTORY) recutl c_cern mathlib kernlib packlib


#CREATED_C = xxx.c
CREATED_F = spp_int.F
#CREATED_CC = nt20maker.cc

SHARED_LIBS=$(RECSIS_SHARED_LIBS) $(CXX_SHARED_LIBS)

include $(CLAS_CMS)/Makefile

CXXFLAGS+=$(shell root-config --cflags) -I$(HOME)/source_codes/include -I$(HOME)/source_codes/eg1a/filter_dst/ -I$(HOME)/source_codes/eg1b/dst_eg1b/ 
LIBS+=-L$(HOME)/CLAS/lib/$(OSNAME)/ 
SHARED_LIBS+=$(shell root-config --glibs)

ifeq ($(OS_NAME),AIX)
        CXX_LIBS += Cx
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
