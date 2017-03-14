CREATED_CC = trk_mon_lite.cc

#LOCAL_INCLUDES += -I$(CLAS_PACK)/caldb/C

#LIB_PATH += /usr/lib

LIB_NAMES = $(MAIN_DIRECTORY) dc c_bos_io bankdefs itape online_dummy

ifndef USEBOSIO
  LIB_NAMES += fputil bos fpack
else
  LIB_NAMES += bosio
endif

LIBNAMES += $(addsuffix $(ADD_DEBUG), $(LIB_NAMES))

ifndef MAP
  LOCAL_INCLUDES += -I$(MYSQL_INCLUDE_PATH)
# LIB_PATH += $(MYSQL_LIB_PATH)
  LIBNAMES += caldbC$(ADD_DEBUG) caldbMap$(ADD_DEBUG) mysqlclient dl
else
  LIBNAMES += mapmanager$(ADD_DEBUG)
endif
LIBNAMES += clasutil$(ADD_DEBUG) # needs to be after caldbMap

LIBNAMES += tcl
LIBNAMES += c_cern packlib mathlib kernlib
LIBNAMES += stdc++

include $(CLAS_CMS)/Makefile
