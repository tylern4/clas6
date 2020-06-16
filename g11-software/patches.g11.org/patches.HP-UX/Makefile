#---------------------
# Makefile to make generic library and binary APF 12/31/1996
#
# If Makefile file is invoked in directory "top":                      
# Makefile will look for *.F and *.c in all subdirectories of top/
# and compile them into a library called "libtop.a" and link the
# code into a binary called "top".  
#
# The intent is for a Makefile file that is robust for large projects, yet 
# simple for small single user projects.  The Makefile supports shared disks
# by locating the compiled libraries and linked executibles in directories
# named $(TOP_DIR)/lib/$(OS_NAME) and $(TOP_DIR)/bin/$(OS_NAME), where 
# $(TOP_DIR) is to be defined by the user.  In order to support multiple
# platforms one must supply the platform specific compile and link flags
# in a file called:  $(CLAS_CMS)/flags.$(OS_NAME).mk, where $(CLAS_CMS)
# is a directory that containing these files.  
#
# Attempts have been made to comment each section so the user understands
# enough of what is going on, so that they can make any modifications that
# are neccessary.  However, with correct use, no modifications should be
# needed.
#
# For example, let's say we have a directory "prime" which has this Makefile
# and a subdirectory called "code" which contains FORTRAN and C code. 
# The file code/prime.F contains the "main" routine and the rest of the
# code contains sub-routines.  To make a Library call "libprime.a" simply
# enter:
#           make lib
#
# To make the executible, named "prime",  
# enter:
#           make exe CREATED_F=prime.F \
#                    LIBNAMES="prime mathlib kernlib packlib clasutil tcl"
#
# To make your life easier, you can "hardwire" the CREATED_F and LIBNAMES
# variable by editting this Makefile.  Thereafter you need only
# enter:
#           make exe
#
# NOTE that with the exception of the "main" routine there is no need to
# tell make which source to compile or what it depends on.  The Makefile
# figures that out for you.  Any source code that you DO NOT wish to
# compile needs to have its suffix change to something other than .F or .c
#
# Another variable of some importance is the LIB_PATH variable which is 
# defined to be the search path the linker uses to find the libraries defined
# in the LIBNAMES variable.
#
# BUGS/TO DO LIST:
# 
# 1) c++ support 
#
# 2) automatic determination of LD launch, currently hardwired to use
#    FC on the link, whereas sometimes CC is appropriate.
#
######################################################################
#  Variable       definition
# * -> user setable variables (defaults usually provided)
#
#  SHELL          shell used for spawned processes (/bin/sh)
# *TOP_DIR        Directory you want the libraries and binaries deposited
#  locallib       $(TOP_DIR)/$(OS_NAME)/lib    libraries deposited here
#  localbin       $(TOP_DIR)/$(OS_NAME)/bin    binaries depostied here
#  localdep       $(TOP_DIR)/package/depends/$(OS_NAME) 
#                                 dependency files deposited here
#  OS_NAME        result of the "uname" command which return the Machine Type
#  MAIN_DIRECTORY Directory which Makefile is executed from
# *CLAS_CMS        points to the directory contain flags.$(OS_NAME).mk and
#                 other *.mk files needed to work
# *CERNLIB        points to the directory holding CERN libraries
# *CLASLIB        points the CLAS library suite
# *TCL_LIB        points to the directory holding TCL libraries
# *X_LIB          points to the directory holding X libraries
#
#  EXE             path and name of executible created
#  LIBRARY         path and name of library created
#  LIBSED
#  DEPSED
#  SOURCE_FILES    complete list of all *.F and *.c in sub-directories of 
#                  MAIN_DIRECTORY, include directory prefix
#  SUB_DIRECTORIES list of subdirectories that contain source code
#  INCLUDE_FILES   complete list of *.inc and *.h files in subdirectories of
#                  MAIN_DIRECTORY, including directory prefix
#  LOCAL_INC_DIR   list of directories that contain *.inc and *.h files
#  LOCAL_INCLUDES  list of directories with "-I" prefix added so this can be
#                  passed to the CPP program
#  FORTRAN_SOURCES
#  NOTDIR_F_SOURCES
#  C_SOURCES
#  NOTDIR_C_SOURCES
#  OBJS
# *CREATED_F       FORTRAN source files (usually the main program) that you
#                  want *.o files created and placed in $locallib
#  CREATED_OBJS
#  CREATED_OBJS_F
# *REQUIRED_OBJS   name of *.o files that ARE NOT to be created but in 
#                  existence (ie. they exist in $CLAS_LIB).
# *CREATED_C       name of C source files that you want *.o files created
#  CREATED_OBJS_C
#  SYSTEM_FLAGS    name of include file that hold the machine dependent flags
#  FFLAGS          FORTRAN compile flags
#  CFLAGS          C compile flags
#  FLDFLAGS        ld linker flags
# *CLAS_INCLUDE    cpp include search path
#  LIBS
# *LIBNAMES        names of ALL static libraries you wish to link against
# *LIB_PATH        ld search path for libraries
#  VPATH_DIR       ld search path for libraries properly formatted for ld
#  RAWDEPS         file names for dependencies (*.d)
#  DEPS            path+file names for dependencies
#
#########################################################
#---------------start of Makefile------------------ 
#########################################################
#
#---------------------
# Set up the library name and directory names, libraries and binaries
# are deposited in directories, locallib and localbin defined below.
#---------------------

SHELL = /bin/sh        # try to use the csh and you'll get killed by CPU costs
OS_NAME = $(shell uname)

MAIN_DIRECTORY = $(notdir $(shell pwd))

ifdef LOGNAME
 USERNAME = $(LOGNAME)
else
 USERNAME = $(USER)
endif

# EDIT THE FOLLOWING THE LINE---tell MAKE where to deposit the libraries and
# binaries

ifeq ($(HOST), jlabh1)
 TOP_DIR = /tmp/$(USERNAME)
endif
ifeq ($(HOST), jlaba1)
 TOP_DIR = /tmp/$(USERNAME)
endif
ifndef TOP_DIR
 TOP_DIR = /scratch/$(USERNAME)
endif

locallib = $(TOP_DIR)/lib/$(OS_NAME)
localbin = $(TOP_DIR)/bin/$(OS_NAME)
localdep = ./depends/$(OS_NAME)

# EDIT THE FOLLOWING LINE----tell MAKE where to find the included Makefiles

#CLAS_CMS = $(CLAS_CMS)

#------------------------------------
# Define the Main program routine CREATED_F or CREATED_C and
# define the library link list, LIBNAMES should contain the complete
# link list.  Please see the files <link.mk>  and
# <link.OS_NAME.mk> for the default link list for RECSIS, GSIM, FDUMP...
#------------------------------------

CREATED_F += 
CREATED_C +=
REQUIRED_OBJS += 

#LIBNAMES  = $(MAIN_DIRECTORY)
#LIBNAMES  += $(RECSIS_LIBS) $(RECSIS_MD_LIBS) 

LIBS = $(addprefix -l, $(LIBNAMES))

#---------------------
# EDIT THE FOLLOWING LINE
# Set up variables that point to directories holding the libraries
# These are used by the LINKER to find those libraries named in the 
# LIBNAMES variable...see LIBNAMES variable above...
#---------------------

#LIB_PATH = $(locallib) $(CTEST_EXE) $(CERN_ROOT)/lib $(TCL_LIB) $(X_LIB)
LIB_PATH = $(locallib) $(CERN_ROOT)/lib $(TCL_LIB) $(X_LIB)
LIB_PATH +=

#---------------------
# Set up the targets, the basename is needed for CLAS directories
# that are name clas.lib.  The ".lib" suffix is removed.
#---------------------

EXE = $(localbin)/$(basename $(MAIN_DIRECTORY))
LIBRARY = $(locallib)/lib$(addsuffix $(ADD_DEBUG), \
			$(basename $(MAIN_DIRECTORY))).a

#----------------------------------------
# List of subdirectories containing FORTRAN and/or C source code
#----------------------------------------

SOURCE_FILES = $(wildcard *.[cF] */*.[cF])
SUB_DIRECTORIES = $(sort $(dir $(SOURCE_FILES)))

INCLUDE_FILES = $(wildcard *.inc */*.inc  *.h */*.h)
LOCAL_INCLUDES = $(addprefix -I,$(sort $(dir $(INCLUDE_FILES))))

#----------------------------------------------
# build up the list of source and object files
#----------------------------------------------

FORTRAN_SOURCES = $(filter-out %.c, $(SOURCE_FILES))
NOTDIR_F_SOURCES = $(notdir $(FORTRAN_SOURCES))

C_SOURCES = $(filter-out %.F, $(SOURCE_FILES))
NOTDIR_C_SOURCES = $(notdir $(C_SOURCES))

# OBJS variable contains the object filenames for both F and C source

OBJS  = $(NOTDIR_F_SOURCES:%.F=$(LIBRARY)(%.o))  \
	$(NOTDIR_C_SOURCES:%.c=$(LIBRARY)(%.o))

#------------------------------------
# Required OBJECTS, usually the MAIN routine that starts it all
# use the CREATED variable to name those object files you wish to
# CREATE and use the REQUIRED_OBJS to list those object files
# that are not to be created but in existence (like recsis.o)
#------------------------------------

ifneq ($(strip $(CREATED_F)),)
 CREATED_OBJS = $(CREATED_F:.F=.o)
 CREATED_OBJS_F = $(addprefix $(locallib)/,$(CREATED_OBJS))
 REQUIRED_OBJS += $(CREATED_OBJS_F)
endif

ifneq ($(strip $(CREATED_C)),)
 CREATED_OBJS = $(CREATED_C:.c=.o)
 CREATED_OBJS_C = $(addprefix $(locallib)/,$(CREATED_OBJS))
 REQUIRED_OBJS += $(CREATED_OBJS_C)
endif

#---------------------------------------
# Machine dependent flags and libraries
#---------------------------------------

SYSTEM_FLAGS = $(CLAS_CMS)/flags.$(OS_NAME).mk
include $(SYSTEM_FLAGS)
include $(CLAS_CMS)/link.mk
include $(CLAS_CMS)/link.$(OS_NAME).mk

#---------------------------------------
#add your own compile/link flags here if you need them.  To wipe out the
#default flags change the "+=" to "="
#---------------------------------------

	FFLAGS+=
	CFLAGS+=
	FLDFLAGS+=

#-----------------------------------------
# include file search path, $DEFAULT_INCLUDES is defined in the
# $(CLAS_CMS)/flags.$(OS_NAME).mk file---for AIX you need to send -WF,-I
# instead of -I
#-----------------------------------------

	CLAS_INCLUDES +=  $(LOCAL_INCLUDES)
	CLAS_INCLUDES +=  $(DEFAULT_INCLUDES)

#--------------------------------------------------------
# force loading of these routines 
#--------------------------------------------------------

#UNDEF = -Wl,-u,hbook1_,-u,hropen_

#-----------------------------------------
# Prepare libraries and their search path, add any new directories into
# the VPATH_TMP variable below.
#-----------------------------------------
empty:=
space:= $(empty) $(empty)
VPATH_TMP  = $(strip $(LIB_PATH))
VPATH_DIR  = $(subst $(space),:,$(strip $(LIB_PATH)))
LIBS_DIR  = -L$(subst :, -L,$(VPATH_DIR))

vpath %.a $(strip $(VPATH_DIR)):/usr/local/lib:/usr/lib:
VPATH =$(patsubst %/,%:,$(SUB_DIRECTORIES))
       
#-----------------------------------------
# variables needed for automatic dependency generation.
# The LIBSED and DEPSED variable, have "\" in front of "/" so these variables
# can be fed to sed.  
# The DEPS variable contains all the dependency filenames
#-----------------------------------------

RAWDEPS  = $(NOTDIR_F_SOURCES:.F=.d) $(NOTDIR_C_SOURCES:.c=.d)
DEPS = $(addprefix $(localdep)/,$(RAWDEPS))
LIBSED := $(subst /,\\/,$(LIBRARY))
DEPSED := $(subst /,\\/,$(localdep))

#---------
# actions
#---------
all: DUMMY

lib: mklib  $(LIBRARY)
   
exe: mkbin $(CREATED_OBJS_F) $(CREATED_OBJS_C) $(EXE) 
   
.PHONY : all mkall mklib mkbin exe lib clean helpme
	@

FORCE = DUMMY
DUMMY :

helpme: $(FORCE)
	echo VPATH_DIR = $(VPATH_DIR)
	echo VPATH_TMP = $(VPATH_TMP)
	echo USERNAME = $(USERNAME)
	echo locallib = $(locallib)
	echo LIBRARY = $(LIBRARY)
	echo LIBSED = $(LIBSED)
	echo LIBS = $(LIBS)

realclean: $(FORCE)
	rm -R ./depends
	rm -R $(TOP_DIR)/lib
	rm -R $(TOP_DIR)/bin

mkall: mklib mkbin

mklib:
	@test -d $(locallib) || mkdir -p $(locallib) ;
	@test -f $(LIBRARY)  || ar r $(LIBRARY) ;

mkbin:
	@test -d $(localbin) || mkdir -p $(localbin)

$(CREATED_OBJS_F): $(locallib)/%.o: %.F  $(SYSTEM_FLAGS)
			$(FC) $(FFLAGS) $(CPPFLAGS) \
			$(addprefix $(INC_FLAG),$(CLAS_INCLUDES)) -c \
			$< -o $@ 

$(CREATED_OBJS_C): $(locallib)/%.o: %.c  $(SYSTEM_FLAGS)
			$(CC) $(CFLAGS) $(CPPFLAGS) $(CLAS_INCLUDES) -c \
			$< -o $@ 

$(LIBRARY): $(OBJS) $(LIB) $(SYSTEM_FLAGS)
	@$(AR) ts $(LIBRARY)

$(EXE):  $(CREATED_OBJS_F) $(CREATED_OBJS_C) $(LIBS)
	$(FC)  $(UNDEF) \
	$(addprefix $(INC_FLAG), $(CLAS_INCLUDES)) \
	$(FLDFLAGS) \
	$(REQUIRED_OBJS) $(LIBS_DIR) $(LIBS) \
	-o $(EXE)

#------------------------------------------------------------
# Pattern rules
#------------------------------------------------------------
#  automatic generation of depends files  SOURCE.d  loated in 
#  localdep/*.d. 
#
#  include all the local generated dependency files, these are created and
#  updated automatically for you.

$(localdep)/%.d: %.c $(SYSTEM_FLAGS)
	@test -d $(localdep) || mkdir -p $(localdep) ;
	$(SHELL) -c '$(CPP) $(CPPFLAGS) \
	$(CLAS_INCLUDES) \
	$< \
	| sed \
	-e '\"s/$*\\.o:/$(LIBSED)\($*.o\):/g\"' \
	-e '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' > $@'

$(localdep)/%.d: %.F  $(SYSTEM_FLAGS)
	@test -d $(localdep) || mkdir -p $(localdep) ;
	$(SHELL) -c '$(CPP) $(CPPFLAGS) \
	$(CLAS_INCLUDES) \
	$< \
	| sed \
	-e  '\"s/$*\\.F\\.o:/$(LIBSED)\($*.o\):/g\"' \
	-e  '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' > $@'

$(LIBRARY)(%.o): %.F $(SYSTEM_FLAGS)
	$(FC)  $(FFLAGS) $(CPPFLAGS) \
	$(addprefix $(INC_FLAG),$(CLAS_INCLUDES)) -c \
	$<
	@$(AR) r $@ $%
	@$(RM) $%

$(LIBRARY)(%.o): %.c  $(SYSTEM_FLAGS)
	$(CC) $(CFLAGS)  $(CPPFLAGS) \
		$(CLAS_INCLUDES) -c \
		$<
	@$(AR) r $@ $%
	@$(RM) $%

%.o: %.F
	$(FC)  $(FFLAGS) $(CPPFLAGS) $(CLAS_INCLUDES) -c $<

%.o: %.c
	$(CC) $(CFLAGS)  $(CPPFLAGS) $(CLAS_INCLUDES) -c $<

#-------------------------------------------------
#  Dependencies for each source file
#-------------------------------------------------

ifdef DEPS
      include $(DEPS)
endif







