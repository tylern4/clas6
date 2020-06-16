OS_NAME=$(shell $(CLAS_CMS)/uname_clas)
CREATED_C = filter_tagr.c
LOCAL_INCLUDES = -I. -I../../include -I../../inc_derived
LIBNAMES =  pid trk sc ec cc st lac tag vertex scaler clasutil recutl c_bos_io \
	mapmanager bankdefs itape fputil fpack bos c_cern packlib kernlib

include $(CLAS_CMS)/Makefile

ifeq ($(OS_NAME),AIX)
  SHARED_LIBS += -lC
endif

ifeq ($(OS_NAME),HP-UX)
  MD_LIBS += -lm
  SHARED_LIBS += -lC
endif

ifeq ($(OS_NAME),Linux)
  MD_LIBS += -lstdc++
endif

ifeq ($(OS_NAME),LinuxRH6)
  MD_LIBS += -lstdc++
endif

ifeq ($(OS_NAME),LinuxRH7)
  MD_LIBS += -lnsl -lstdc++
endif

ifeq ($(OS_NAME),LinuxRH8)
  MD_LIBS += -lnsl -lstdc++
endif

ifeq ($(OS_NAME),LinuxRH9)
  MD_LIBS += -lnsl -lstdc++
endif

ifeq ($(OS_NAME),LinuxRHEL3)
  MD_LIBS += -lnsl -lstdc++
endif


ifeq ($(OS_NAME),SunOS)
  MD_LIBS += -lsocket -lnsl
  SHARED_LIBS += -lC
endif
