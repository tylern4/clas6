MISCLIB_SunOS := m c nsl socket
MISCLIB_HP-UX :=  m c f
MISCLIB_AIX :=  xlf90 m c 
MISCLIB_Linux := m c
MISCLIB_OSF1 := m c for Ufor

MISCLIB = $(MISCLIB_$(OS_NAME))

LIBNAMES = c_bos_io$(ADD_DEBUG) fputil$(ADD_DEBUG) \
	bos$(ADD_DEBUG) fpack$(ADD_DEBUG) c_bos_io$(ADD_DEBUG) packlib \
	$(MISCLIB) # use these library for linking

# multiple target, each an executable to be made, with the rule to make them
bosdump:
	$(MAKE) exe EXE=$(localbin)/$@$(ADD_DEBUG) CREATED_C="bosdump.c" LOCAL_INCLUDES="-I. -I../../include"

# include mother
include $(CLAS_CMS)/Makefile
