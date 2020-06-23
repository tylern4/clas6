#
# Individual package makefile, calls $CLAS_CMS/Makefile with the
# appropriate arguments
#

# TOP_DIR points to the final location of binary and library tree:
# $(TOP_DIR)/$(OS_NAME)/bin/       $(TOP_DIR)/$(OS_NAME)/lib/
#
# USERS should set the enviroment TOP_DIR to a directory of their choice
# to override the default relative path

# CLAS_CMS points to the location of the included makefiles.  
# Default is to get this from the enviroment.

OS_NAME = $(shell $(CLAS_CMS)/uname_clas)

ifndef CLAS_CMS
 CLAS_CMS = ../
endif

# additions to  the include search path

CLAS_INCLUDES += 

# individual source listings for a read/write library and a read only
# library

RW_SOURCES = map_create.c map_add_item.c map_add_item1.c map_rem_item.c \
          map_rem_sub.c \
          map_next_sub.c map_next_item.c map_put_int.c map_put_float.c \
          map_put_char.c \
          map_get_int.c map_get_float.c map_get_char.c map_fnd_int.c \
          map_fnd_float.c \
          map_fillname.c map_find_pred.c \
          map_error.c map_syserror.c map_warn.c map_log_mess.c \
	  map_read.c map_read_buffer.c map_write.c map_write_buffer.c map_add_sub.c \
          map_overwrite.c map_find_same.c map_time_pred.c map_dump.c \
          map_find_item.c map_stat_item.c map_open_close.c \
          map_find_sub.c map_fix_item.c map_fix_sub.c map_fix_head.c \
	  map_stat_head.c map_rem_arr.c  map_endianConvert.c \
          map_add_item_.c map_put_int_.c map_put_flo_.c map_get_int_.c \
          map_get_flo_.c map_fnd_int_.c map_fnd_flo_.c map_stat_item_.c \
          map_rem_item_.c map_rem_sub_.c map_rem_arr_.c map_dump_.c \
          map_log_mess_.c \
	  mapf_add_item.c mapf_get_float.c mapf_get_int.c mapf_put_float.c \
	  mapf_put_int.c mapf_rem_item.c mapf_stat_item.c \
	  mapf_get_char.c mapf_put_char.c

RO_SOURCES = map_next_sub.c map_next_item.c \
          map_get_int.c map_get_float.c map_get_char.c map_fnd_int.c \
          map_fnd_float.c \
          map_fillname.c map_find_pred.c \
          map_error.c map_syserror.c map_warn.c map_log_mess.c \
	  map_read.c map_read_buffer.c map_endianConvert.c \
          map_find_same.c map_time_pred.c map_dump.c \
          map_find_item.c map_stat_item.c \
#     map_open_close_ro.c \
          map_find_sub.c \
	  map_stat_head.c \
          map_get_int_.c \
          map_get_flo_.c map_fnd_int_.c map_fnd_flo_.c map_stat_item_.c \
          map_dump_.c map_log_mess_.c \
	  mapf_get_float.c mapf_get_int.c \
	  mapf_stat_item.c map_readonly_stub.c \
	  mapf_get_char.c mapf_put_char.c

LIBRARY1 = map_manager
LIBRARY2 = map_manager_ro

all: lib

lib:
	$(MAKE) -f  $(CLAS_CMS)/Makefile C_SOURCES="$(RW_SOURCES)" \
                   MAIN_DIRECTORY=$(LIBRARY1) lib
	$(MAKE) -f  $(CLAS_CMS)/Makefile C_SOURCES="$(RO_SOURCES)" \
                   MAIN_DIRECTORY=$(LIBRARY2) lib

help:
	$(MAKE) -f $(CLAS_CMS)/Makefile C_SOURCES="$(RW_SOURCES)" \
                   MAIN_DIRECTORY=$(LIBRARY1) help
	$(MAKE) -f $(CLAS_CMS)/Makefile C_SOURCES="$(RO_SOURCES)" \
                   MAIN_DIRECTORY=$(LIBRARY2) help

exe:
	@echo "Executibles for the Map package are found in the "
	@echo "uitilities package "

clean:
	$(MAKE) -f $(CLAS_CMS)/Makefile clean

# end of scat makefile


