
# define the link list

#ifdef MAP
#  RECLIBS3 += mysqlclient 
#  LIB_PATH += $(MYSQL_LIB_PATH)
#endif

#LIB_PATH += /apps/gcc/lib/
#LIB_PATH += /apps/lib/


CREATED_C += sync.c
C_SOURCES += sync.c

#LIBRARY =   


LIBNAMES= $(RECSIS_LIBS1) $(MAIN_DIRECTORY)$(ADD_DEBUG) ana$(ADD_DEBUG) \
        $(RECSIS_LIBS2) $(RECSIS_MD_LIBS) $(RECSIS_LIBS3)  bankdefs$(ADD_DEBUG) \
	recutl$(ADD_DEBUG) 

#SHARED_LIBS=$(RECSIS_SHARED_LIBS)

#INCLUDE_ALL += $(addprefix ../, $(PACK_NAMES))
#INCLUDE_ALL += $(addprefix $(CLAS_PACK)/, $(PACK_NAMES))

#DEFAULT_INCLUDES += $(addprefix -I,$(INCLUDE_ALL))

# get the generic Makefile and predefined link lists;  

include $(CLAS_CMS)/Makefile
# end of makefile

