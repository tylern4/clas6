ifndef OS_NAME
 OS_NAME = $(shell $(CLAS_CMS)/uname_clas)
endif

ifneq (,$(findstring $(OS_NAME),LinuxRH6 LinuxRH7 LinuxRH8 LinuxRH9 LinuxRHEL3))
  LINUX_DIR = patches.$(OS_NAME)
endif

# for patches.Linux/gthadr.F the path for include files has to be set,
# therefore use Makefiles in patches.$(OS_NAME)

all: lib

lib: $(LINUX_DIR) $(FORCE)
	if test -d patches.$(OS_NAME); then \
	    $(MAKE) -e -C patches.$(OS_NAME) lib; \
	fi

patches.$(OS_NAME):
	test -d $@ || ln -s patches.Linux $@

clean: $(FORCE)
	cd patches.$(OS_NAME); $(MAKE) -e clean

help: $(FORCE)
	cd patches.$(OS_NAME); $(MAKE) -e help

FORCE = DUMMY
DUMMY :
