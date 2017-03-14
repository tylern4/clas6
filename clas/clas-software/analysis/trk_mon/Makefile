ifndef OS_NAME
 OS_NAME:=$(shell $(CLAS_CMS)/uname_clas)
endif

CREATED_C = trk_mon.c
LOCAL_INCLUDES =  -I. -I../../include -I../../inc_derived -I$(MYSQL_INCLUDE_PATH) -I$(CLAS_PACK)/c_sql

BOSLIB = fputil bos fpack

ifdef USEBOSIO
BOSLIB = bosio
endif

CLAS_LIBS = trk_mon trk pid dc sc ec cc st lac tag vertex scaler recutl c_bos_io bankdefs itape $(BOSLIB) c_cern online_dummy c_sql

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
	ifeq "$(OS_NAME)" "SunOS"
		LIB_TMP += socket nsl
	endif
	ifeq "$(OS_NAME)" "SunOSws6"
		LIB_TMP += socket nsl
	endif

	SHARED_LIBS += -ltcl$(TCL_VERSION)

else
	LIB_TMP = mapmanager clasutil 

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

ifeq ($(OS_NAME),Darwin)
  MD_LIBS += -L/Developer/SDKs/MacOSX10.4u.sdk/usr/lib/gcc/i686-apple-darwin8/4.0.1/ -lstdc++ -lgcc_eh
endif


ifeq ($(findstring Linux,$(OS_NAME)),Linux)
  MD_LIBS += -lstdc++ -ldl -lnsl
endif

ifeq ($(OS_NAME),LinuxRH6)
  CXXFLAGS += -g
  EGCS_FLAG += -g
  CFLAGS += -g
endif

ifeq ($(OS_NAME),SunOS)
  MD_LIBS += -lsocket -lnsl
  SHARED_LIBS += -lCrun -lCstd
endif

ifeq ($(OS_NAME),SunOSws6)
  MD_LIBS += -lsocket -lnsl
  SHARED_LIBS += -lCrun -lCstd -ldl
endif
