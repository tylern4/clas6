OS_NAME=$(shell $(CLAS_CMS)/uname_clas)
CREATED_C = filter_tagr.c
LOCAL_INCLUDES = -I. -I../../tag -I../../include -I../../inc_derived 

CLAS_LIBS =  pid sc ec cc st lac tag vertex scaler trk\
	    recutl itape c_bos_io bankdefs fputil bos fpack c_cern

ifndef MAP
	LIB_TMP = caldbMap$(ADD_DEBUG) clasutil$(ADD_DEBUG) mysqlclient

	ifeq "$(OS_NAME)" "LinuxRH6"
		LIB_TMP += z
	endif
	ifeq "$(OS_NAME)" "LinuxRH7"
		LIB_TMP += z
	endif
	ifeq "$(OS_NAME)" "LinuxRH8"
		LIB_TMP += z
	endif
	ifeq "$(OS_NAME)" "LinuxRH9"
		LIB_TMP += z
	endif
	ifeq "$(OS_NAME)" "LinuxRHEL3"
		LIB_TMP += z 
	endif
	ifeq "$(OS_NAME)" "LinuxRHFC8"
		LIB_TMP += z 
	endif
	ifeq "$(OS_NAME)" "SunOS"
		LIB_TMP += socket nsl
	endif
	ifeq "$(OS_NAME)" "SunOSws6"
		LIB_TMP += socket nsl
	endif
	SHARED_LIBS += -ltcl$(TCL_VERSION)
else
	LIB_TMP = mapmanager$(ADD_DEBUG) clasutil$(ADD_DEBUG) 
endif

LIBNAMES += $(addsuffix $(ADD_DEBUG), $(CLAS_LIBS))
LIBNAMES += $(LIB_TMP)
LIBNAMES += packlib kernlib

include $(CLAS_CMS)/Makefile

ifeq ($(OS_NAME),AIX)
  SHARED_LIBS += -lC
endif

ifeq ($(OS_NAME),HP-UX)
  MD_LIBS += -lm -lcxx
  SHARED_LIBS += -lC
  LIB_PATH += /opt/CC/lib/eh
endif

ifeq ($(OS_NAME),Linux)
  MD_LIBS += -lstdc++ -ldl -lnsl
endif

ifeq ($(OS_NAME),LinuxRH6)
  MD_LIBS += -lstdc++ -ldl -lnsl
  CXXFLAGS += -g
  EGCS_FLAG += -g
  CFLAGS += -g
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
ifeq ($(OS_NAME),LinuxRHEL3)
  MD_LIBS += -lstdc++ -ldl -lnsl
endif
ifeq ($(OS_NAME),LinuxRHFC8)
  MD_LIBS += -lstdc++ -ldl -lnsl
endif

ifeq ($(OS_NAME),SunOS)
  MD_LIBS += -lsocket -lnsl
  SHARED_LIBS += -lC
endif

ifeq ($(OS_NAME),SunOSws6)
  MD_LIBS += -lsocket -lnsl
  SHARED_LIBS += -lCrun -lCstd -ldl
endif
