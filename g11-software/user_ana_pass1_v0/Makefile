# define the link list

ifdef MAP
  RECLIBS3 += mysqlclient z
  LIB_PATH += $(MYSQL_LIB_PATH)
endif

LIB_PATH += /apps/gcc/lib/
LIB_PATH += /apps/lib/

REQUIRED_OBJS=$(CLAS_LIB)/recsis$(ADD_DEBUG).o \
	$(CLAS_LIB)/rec_work$(ADD_DEBUG).o

LIBNAMES= $(RECSIS_LIBS1) $(MAIN_DIRECTORY)$(ADD_DEBUG) ana$(ADD_DEBUG) \
	$(RECSIS_LIBS2) $(RECSIS_MD_LIBS) $(RECSIS_LIBS3) stdc++ bankdefs


#LIBNAMES= $(RECSIS_LIBS1) $(MAIN_DIRECTORY)$(ADD_DEBUG) ana$(ADD_DEBUG) \
#       $(RECSIS_LIBS2) $(RECSIS_MD_LIBS) $(RECSIS_LIBS3)

SHARED_LIBS=$(RECSIS_SHARED_LIBS)


PACK_NAMES += ec sc cc seb trk lac icf ana

INCLUDE_ALL += $(addprefix ../, $(PACK_NAMES))
INCLUDE_ALL += $(addprefix $(CLAS_PACK)/, $(PACK_NAMES))

DEFAULT_INCLUDES += $(addprefix -I,$(INCLUDE_ALL))

# get the generic Makefile and predefined link lists;  

include $(CLAS_CMS)/Makefile
# end of makefile
