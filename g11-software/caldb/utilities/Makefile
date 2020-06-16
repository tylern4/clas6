# This Makefile assumes that each file in the directory contains a
# main program that should be made into an executable. It also assumes
# that only the mapmanager library is necessary to resolve references.
LOCAL_INCLUDES += -I$(MYSQL_INCLUDE_PATH)

ifndef MAP
  LIBNAMES += caldbMap$(ADD_DEBUG)
else
  LIBNAMES += mapmanager$(ADD_DEBUG)
endif

  LIBNAMES += caldbC$(ADD_DEBUG) clasutil$(ADD_DEBUG) mysqlclient

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
  ifeq "$(OS_NAME)" "SunOS"
      SHARED_LIBS += -lsocket -lnsl
  endif 
  SHARED_LIBS += -ltcl$(TCL_VERSION) -ldl

# make a list of all local C source files
c_src_files_0 = $(wildcard *.c)

# exclude certain programs if calibration database being used
  c_src_files = $(c_src_files_0)

# make a list of executables to make by stripping the ".c" from the source
# file names
c_main_routines = $(foreach file, $(c_src_files), $(subst .c,,$(file)))

# default target: make all of the executables
real_all: $(c_main_routines)

# multiple target, each an executable to be made, with the rule to make them
$(c_main_routines):
	@echo making $@
	$(MAKE) exe EXE=$(localbin)/$@$(ADD_DEBUG) CREATED_C=$@.c

# include mother
include $(CLAS_CMS)/Makefile

help_local:
	@echo c_main_routines = $(c_main_routines)
	@echo c_src_file = $(c_src_files)
