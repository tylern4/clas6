######################################################################
#
# 
#
#
#
#
#


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
#  DLINKLIB        path and name of Dynamically linked library
#                                E.A.  10/11/98
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
#  SOFLAGS         Dynamically linked libraries flag E.A. 11/10/98
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

# delete ALL predefined SUFFIXES to save time

.SUFFIXES:
#.SUFFIXES: .c .F .C .ddl .inc

ifndef OS_NAME
 OS_NAME = $(shell $(CLAS_CMS)/uname_clas)
endif

MAIN_DIRECTORY = $(notdir $(shell pwd))

ifdef LOGNAME
 USERNAME = $(LOGNAME)
else
 USERNAME = $(USER)
endif

# EDIT THE FOLLOWING THE LINE---tell MAKE where to deposit the libraries and
# binaries

ifndef TOP_DIR
 TOP_DIR = $(HOME)/
endif

locallib = $(TOP_DIR)/lib/$(OS_NAME)
localbin = $(TOP_DIR)/bin/$(OS_NAME)
tempobj = $(TOP_DIR)/obj/$(OS_NAME)
localdic = $(TOP_DIR)/dic
localinc = $(TOP_DIR)/inc_derived

# EDIT THE FOLLOWING LINE----tell MAKE where to find the included Makefiles

#CLAS_CMS = $(CLAS_CMS)

#------------------------------------
# Define the Main program routine CREATED_F or CREATED_C and
# define the library link list, LIBNAMES should contain the complete
# link list.  Please see the files <link.mk>  and
# <link.OS_NAME.mk> for the default link list for RECSIS, GSIM, FDUMP...
#------------------------------------

#CREATED_F += 
#CREATED_C +=
#REQUIRED_OBJS += 

#LIBNAMES  = $(MAIN_DIRECTORY)$(ADD_DEBUG)
#LIBNAMES  += $(RECSIS_LIBS) $(RECSIS_MD_LIBS)

LIBS = $(addprefix -l, $(LIBNAMES))
MD_LIBS = $(addprefix -l, $(MD_LIBNAMES))

#---------------------
# EDIT THE FOLLOWING LINE
# Set up variables that point to directories holding the libraries
# These are used by the LINKER to find those libraries named in the 
# LIBNAMES variable...see LIBNAMES variable above...
#---------------------

LIB_PATH += $(locallib)  $(CLAS_LIB) $(CERN_ROOT)/lib $(TCL_LIB) $(X_LIB)

#----------------------------------------
# List of subdirectories containing FORTRAN and/or C source code
#----------------------------------------

SOURCE_FILES = $(wildcard *.[cF])
SOURCE_FILES += $(wildcard *.cc)
SUB_DIRECTORIES = $(sort $(dir $(SOURCE_FILES)))

#INCLUDE_FILES = $(wildcard ../*/*.inc ../*/*.h)
INCLUDE_FILES = $(wildcard *.inc *.h */*.h */*.inc ../include/*.inc ../include/*.h ../inc_derived/*.inc ../inc_derived/*.h)
LOCAL_INCLUDES = -I. $(addprefix -I,$(sort $(dir $(INCLUDE_FILES))))

LOCAL_HEADERS = $(wildcard *.h)

#----------------------------------------------
# build up the list of source and object files
#----------------------------------------------

CC_SOURCES = $(filter-out %.F %.c , $(SOURCE_FILES))
NOTDIR_CC_SOURCES = $(notdir $(CC_SOURCES))
NOTDIR_CC_OBJS0 =$(NOTDIR_CC_SOURCES:.cc=.o)
CC_OBJS = $(addprefix $(tempobj)/,$(notdir $(NOTDIR_CC_OBJS0)))

DIC_SOURCES0 = $(NOTDIR_CC_SOURCES:.cc=Cint.cc)
DIC_SOURCES = $(addprefix $(localdic)/,$(notdir $(DIC_SOURCES0)))
DIC_OBJS0 = $(NOTDIR_CC_SOURCES:.cc=Cint.o)
DIC_OBJS = $(addprefix $(tempobj)/,$(notdir $(DIC_OBJS0)))
DIC_HEADER = $(NOTDIR_CC_SOURCES:.cc=Cint.h)

#---------------------
# Set up the targets, the basename is needed for CLAS directories
# that are name clas.lib.  The ".lib" suffix is removed.
#---------------------

SL = $(localbin)/libT$(basename $(MAIN_DIRECTORY)).so

#---------------------------------------
# Machine dependent flags and  default link lists
#---------------------------------------

SYSTEM_FLAGS = $(CLAS_CMS)/flags.$(OS_NAME).mk
include $(SYSTEM_FLAGS)
include $(CLAS_CMS)/link.mk
include $(CLAS_CMS)/link.$(OS_NAME).mk

#---------------------------------------
#add your own compile/link flags here if you need them.  To wipe out the
#default flags change the "+=" to "="
#---------------------------------------

#FFLAGS+=
#CFLAGS+=
#FLDFLAGS+=

#-----------------------------------------
# include file search path, $DEFAULT_INCLUDES is defined in the
# $(CLAS_CMS)/flags.$(OS_NAME).mk file---for AIX you need to send -WF,-I
# instead of -I
#-----------------------------------------

CLAS_INCLUDES +=  $(LOCAL_INCLUDES)
CLAS_INCLUDES +=  $(DEFAULT_INCLUDES)

#-----------------------------------------
# Root includes
#-----------------------------------------

ROOTFLAGS	= -I$(ROOTSYS)/include

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

TEST = $(strip $(VPATH_DIR)):/usr/local/lib:/usr/lib
vpath %.a $(strip $(VPATH_DIR)):/usr/local/lib:/usr/lib
VPATH =$(patsubst %/,%:,$(SUB_DIRECTORIES))

#---------
# actions
#---------
all: sl

sl: mkbin  $(SL) 

.PHONY : all sl mkbin clean help mkall

help: 
	@echo CC = $(CC)
	@echo CFLAGS = $(CFLAGS)
	@echo CLAS_INCLUDES = $(CLAS_INCLUDES)
	@echo CLEVEL = $(CLEVEL)
	@echo CPP = $(CPP)
	@echo CPPFLAGS = $(CPPFLAGS)
	@echo CXXFLAGS = $(CFLAGS)
	@echo SOFLAGS=$(SOFLAGS)
	@echo LIBNAMES = $(LIBNAMES)
	@echo LIB = $(LIB)
	@echo LIBS = $(LIBS)
	@echo LIB_PATH = $(LIB_PATH)
	@echo NOTDIR_CC_SOURCES = $(NOTDIR_CC_SOURCES)
	@echo OS_NAME = $(OS_NAME)
	@echo SHARED_LIBS = $(SHARED_LIBS)
	@echo VPATH = $(VPATH)
	@echo VPATH_DIR = $(VPATH_DIR)
	@echo vpath = $(vpath)
	@echo SOURCE_FILES = $(SOURCE_FILES)
	@echo CC_SOURCES = $(CC_SOURCES)
	@echo NOTDIR_CC_SOURCES = $(NOTDIR_CC_SOURCES)
	@echo CC_OBJS = $(CC_OBJS)
	@echo NOTDIR_CC_OBJS0 = $(NOTDIR_CC_OBJS0)
	@echo SLLD = $(SLLD)
	@echo DIC_SOURCES0 = $(DIC_SOURCES0)
	@echo DIC_SOURCES = $(DIC_SOURCES)
	@echo DIC_OBJS = $(DIC_OBJS)
	@echo LOCAL_HEADERS = $(LOCAL_HEADERS)


clean:
	rm $(DIC_SOURCES)
	rm $(localinc)/$(DIC_HEADER)
	rm $(localinc)/$(LOCAL_HEADERS)
	rm $(DIC_OBJS)
	rm $(CC_OBJS)


mkall: mkbin 

mkbin:
	@test -d $(localbin) || mkdir -p $(localbin)


$(SL): $(CC_OBJS) $(DIC_OBJS)
	$(SLLD) $(addprefix $(INC_FLAG), $(CLAS_INCLUDES)) \
	 $(SOFLAGS) $(FLDFLAGS) \
	$(CC_OBJS) $(DIC_OBJS) \
	$(LIBS_DIR) $(LIBS) $(SHARED_LIBS) $(MD_LIBS) \
	-o $(SL)

$(CC_OBJS): $(NOTDIR_CC_SOURCES)
	$(CXX) $(CXXFLAGS) $(ROOTFLAGS) $(addprefix $(INC_FLAG), $(CLAS_INCLUDES)) \
	-c $(localdic)/$(NOTDIR_CC_SOURCES) -o $(CC_OBJS)

$(DIC_OBJS): $(DIC_SOURCES)
	$(CXX) $(CXXFLAGS) $(ROOTFLAGS) $(addprefix $(INC_FLAG), $(CLAS_INCLUDES)) \
	-c $(DIC_SOURCES) -o $(DIC_OBJS)

$(DIC_SOURCES): $(LOCAL_HEADERS)
	@echo "Generating root dictionnary"
	rootcint -f $(DIC_SOURCES0) -c $(LOCAL_HEADERS)
	mv $(DIC_SOURCES0) $(localdic)/
	mv $(DIC_HEADER) $(localinc)/
