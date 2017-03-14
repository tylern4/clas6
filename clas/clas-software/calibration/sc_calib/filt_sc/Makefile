OS_NAME:=$(shell $(CLAS_CMS)/uname_clas)
CREATED_C = filt_highsc.c

LOCAL_INCLUDES = -I. -I../../include -I../../inc_derived
LIBNAMES =  pid$(ADD_DEBUG) sc$(ADD_DEBUG) \
ec$(ADD_DEBUG) cc$(ADD_DEBUG) st$(ADD_DEBUG) lac$(ADD_DEBUG) tag$(ADD_DEBUG) \
vertex$(ADD_DEBUG) scaler$(ADD_DEBUG) \
clasutil$(ADD_DEBUG) recutl$(ADD_DEBUG) c_bos_io$(ADD_DEBUG) \
mapmanager$(ADD_DEBUG) bankdefs$(ADD_DEBUG) \
itape$(ADD_DEBUG) fputil$(ADD_DEBUG) fpack$(ADD_DEBUG) bos$(ADD_DEBUG) \
c_cern$(ADD_DEBUG) online_dummy$(ADD_DEBUG) packlib kernlib 

include $(CLAS_CMS)/Makefile

ifeq ($(OS_NAME),AIX)
  SHARED_LIBS += -lC
endif

ifeq ($(OS_NAME),HP-UX)
  MD_LIBS += -lm
  SHARED_LIBS += -lC
endif

ifeq ($(OS_NAME),Linux)
  MD_LIBS += -lstdc++ -ldl -lnsl
endif

ifeq ($(OS_NAME),LinuxRH6)
  MD_LIBS += -lstdc++ -ldl -lnsl
endif

ifeq ($(OS_NAME),LinuxRH7)
  MD_LIBS += -lstdc++ -ldl -lnsl
endif

ifeq ($(OS_NAME),LinuxRH8)
  MD_LIBS += -lstdc++ -ldl -lnsl
endif

ifeq ($(OS_NAME),LinuxRH9)
  MD_LIBS += -lstdc++ -ldl -lnsl
endif

ifeq ($(OS_NAME),SunOS)
  MD_LIBS += -lsocket -lnsl
  SHARED_LIBS += -lc -lCstd -lCrun
endif
