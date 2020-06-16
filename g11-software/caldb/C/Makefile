#
# Makefile for caldb
#
LOCAL_INCLUDES += -I$(MYSQL_INCLUDE_PATH)
LIB_PATH += $(MYSQL_LIB_PATH)
LIBNAMES  = caldbC clasutil mysqlclient
SHARED_LIBS = -ltcl -ldl
ifeq "$(OS_NAME)" "LinuxRH6"
  LIBNAMES += z
endif
ifeq "$(OS_NAME)" "LinuxRH7"
  LIBNAMES += z
endif
ifeq "$(OS_NAME)" "LinuxRH8"
  LIBNAMES += z
endif
ifeq "$(OS_NAME)" "LinuxRH9"
  LIBNAMES += z
endif
CREATED_C += maintest.c
LIBRARY = $(locallib)/libcaldbC$(ADD_DEBUG).a
#
include $(CLAS_CMS)/Makefile
#
LIBRARY = $(locallib)/libcaldbC$(ADD_DEBUG).a
#
# end of Makefile
