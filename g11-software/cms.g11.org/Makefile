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
#  localslib       $(TOP_DIR)/$(OS_NAME)/slib   shared libraries deposited here
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
#  SLIBRARY         path and name of shared library created
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
# RCS ID: $Id: Makefile,v 1.96 2003/01/23 19:17:15 marki Exp $
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

ifeq ($(OS_NAME),SunOS4) 
  AR = /usr/5bin/ar
else
  AR = ar
endif

locallib = $(TOP_DIR)/lib/$(OS_NAME)
localslib = $(TOP_DIR)/slib/$(OS_NAME)
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

#CREATED_F += 
#CREATED_C +=
#REQUIRED_OBJS += 

#LIBNAMES  = $(MAIN_DIRECTORY)$(ADD_DEBUG)
#LIBNAMES  += $(RECSIS_LIBS) $(RECSIS_MD_LIBS)

# Add the dummy library to the link list if DUMMY_OUT_LIBS is defined.
ifdef DUMMY_OUT_LIBS
	LIBNAMES += dummy
endif
# Filter out the real libraries if they are on the dummy list. If no dummies
# are defined, this has no effect.
LIBNAMES_WO_DUMMIES = $(filter-out $(DUMMY_OUT_LIBS), $(LIBNAMES))
# Add the "-l" prefix to the library names
LIBS = $(addprefix -l, $(LIBNAMES_WO_DUMMIES))
MD_LIBS = $(addprefix -l, $(MD_LIBNAMES))

ifdef USE_ROOT_SYSTEM
 ROOT_LIBS = $(shell ${ROOTSYS}/bin/root-config --glibs)

 LIBS += $(ROOT_LIBS)
endif

#---------------------
# EDIT THE FOLLOWING LINE
# Set up variables that point to directories holding the libraries
# These are used by the LINKER to find those libraries named in the 
# LIBNAMES variable...see LIBNAMES variable above...
#---------------------

LIB_PATH +=  $(localslib) $(locallib)  $(CLAS_SLIB) $(CLAS_LIB) $(CERN_ROOT)/lib $(TCL_LIB) $(X_LIB)

#----------------------------------------
# List of subdirectories containing FORTRAN and/or C source code
#----------------------------------------

SOURCE_FILES += $(wildcard *.[cF])
SOURCE_FILES += $(wildcard *.cc)
SUB_DIRECTORIES = $(sort $(dir $(SOURCE_FILES)))

#INCLUDE_FILES = $(wildcard ../*/*.inc ../*/*.h)
TMP_INCLUDE_FILES += $(wildcard *.inc *.h */*.h */*.inc ../include/*.inc ../include/*.h ../inc_derived/*.inc ../inc_derived/*.h)
ifdef USE_ROOT_SYSTEM
INCLUDE_FILES = $(filter-out %Cint.h %LinfDef.h,$(TMP_INCLUDE_FILES))
else
INCLUDE_FILES = $(TMP_INCLUDE_FILES)
endif

LOCAL_INCLUDES += -I. $(addprefix -I,$(sort $(dir $(INCLUDE_FILES))))

#----------------------------------------------
# build up the list of source and object files
#----------------------------------------------

FORTRAN_SOURCES += $(filter-out %.c %.cc $(CREATED_F), $(SOURCE_FILES))
NOTDIR_F_SOURCES = $(notdir $(FORTRAN_SOURCES))

C_SOURCES += $(filter-out %.F %.cc $(CREATED_C), $(SOURCE_FILES))
NOTDIR_C_SOURCES = $(notdir $(C_SOURCES))

CC_SOURCES += $(filter-out %.F %.c %_Cint.cc $(CREATED_CC), $(SOURCE_FILES))
NOTDIR_CC_SOURCES = $(notdir $(CC_SOURCES))

ifdef USE_ROOT_SYSTEM
LINKDEF_INCLUDES = $(wildcard *_LinkDef.h)
CINT_SOURCES += $(subst _LinkDef.h,_Cint.cc,$(LINKDEF_INCLUDES))
NOT_DIR_CINT_SOURCES = $(notdir $(CINT_SOURCES))

CINT_HEADERS += $(subst _LinkDef.h,_Cint.h,$(LINKDEF_INCLUDES))
NOT_DIR_CINT_HEADERS += $(notdir $(CINT_HEADERS))
else
CINT_SOURCES =
NOT_DIR_CINT_SOURCES = 
CINT_HEADERS =
NOT_DIR_CINT_HEADERS =

endif

LIB_SOURCES = $(NOTDIR_F_SOURCES) $(NOTDIR_C_SOURCES) $(NOTDIR_CC_SOURCES) $(NOT_DIR_CINT_SOURCES)

LIB_BASES = $(basename $(LIB_SOURCES))
SLIB_BASES = $(basename $(LIB_SOURCES))

# OBJS variable contains the object filenames for both F and C source

tempobj = $(TOP_DIR)/obj/$(OS_NAME)/$(basename $(MAIN_DIRECTORY))$(ADD_ONLINE)$(ADD_DEBUG)

OBJS  = $(addprefix $(tempobj)/,$(addsuffix .o,$(LIB_BASES)))
oparen := (
cparen := )
LIB_MEMBERS = $(addprefix $(LIBRARY)$(oparen),$(addsuffix .o$(cparen),$(LIB_BASES)))
SLIB_MEMBERS = $(addprefix $(SLIBRARY)$(oparen),$(addsuffix .o$(cparen),$(SLIB_BASES)))

#---------------------
# Set up the targets, the basename is needed for CLAS directories
# that are name clas.lib.  The ".lib" suffix is removed.
#---------------------

ifndef EXE
 EXE = $(localbin)/$(basename $(MAIN_DIRECTORY))$(ADD_ONLINE)$(ADD_DEBUG)
endif

ifneq ($(strip $(OBJS)),)
ifndef LIBRARY
LIBRARY = $(locallib)/lib$(addsuffix $(ADD_DEBUG), \
			$(basename $(MAIN_DIRECTORY))).a
endif
ifndef SLIBRARY
SLIBRARY = $(localslib)/lib$(addsuffix $(ADD_DEBUG), \
			$(basename $(MAIN_DIRECTORY))).so
endif
SLIBRARY_NAME=lib$(addsuffix $(ADD_DEBUG), $(basename $(MAIN_DIRECTORY))).so
OBJS_TO_AR = $(tempobj)/$(basename $(MAIN_DIRECTORY))$(ADD_DEBUG).tmp
OBJS_TO_SLIB = $(tempobj)/$(basename $(MAIN_DIRECTORY))$(ADD_DEBUG).stmp
endif

#------------------------------------
# Required OBJECTS, usually the MAIN routine that starts it all
# use the CREATED variable to name those object files you wish to
# CREATE and use the REQUIRED_OBJS to list those object files
# that are not to be created but in existence (like recsis.o)
#------------------------------------

ifneq ($(strip $(CREATED_F)),)
 CREATED_OBJS_F0 = $(CREATED_F:.F=$(ADD_DEBUG).o)
 CREATED_OBJS_F = $(addprefix $(locallib)/,$(notdir $(CREATED_OBJS_F0)))
 REQUIRED_OBJS += $(CREATED_OBJS_F)
endif

ifneq ($(strip $(CREATED_C)),)
 CREATED_OBJS_C0 = $(CREATED_C:.c=$(ADD_DEBUG).o)
 CREATED_OBJS_C = $(addprefix $(locallib)/,$(notdir $(CREATED_OBJS_C0)))
 REQUIRED_OBJS += $(CREATED_OBJS_C)
endif

ifneq ($(strip $(CREATED_CC)),)
 CREATED_OBJS_CC0 = $(CREATED_CC:.cc=$(ADD_DEBUG).o)
 CREATED_OBJS_CC = $(addprefix $(locallib)/,$(notdir $(CREATED_OBJS_CC0)))
 REQUIRED_OBJS += $(CREATED_OBJS_CC)
endif

REQUIRED_OBJS += $(COMPILED_OBJS)

ifdef MAP
  CPPFLAGS += -DMAP
endif

#---------------------------------------
# Machine dependent flags and default link lists
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
CLAS_ROOT_INCLUDES = $(filter-out /usr/include%, $(CLAS_INCLUDES))
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

#-----------------------------------------
# variables needed for automatic dependency generation.
# The LIBSED and DEPSED variable, have "\" in front of "/" so these variables
# can be fed to sed.  
# The DEPS variable contains all the dependency filenames
#-----------------------------------------
LIBDEB = $(locallib)/lib$(addsuffix _debug, \
			$(basename $(MAIN_DIRECTORY))).a
RAWDEPS  = $(NOTDIR_F_SOURCES:.F=$(ADD_DEBUG).d) $(NOTDIR_C_SOURCES:.c=$(ADD_DEBUG).d) $(NOTDIR_CC_SOURCES:.cc=$(ADD_DEBUG).d)
DEPS = $(addprefix $(localdep)/, $(RAWDEPS))
LIBSED    := $(subst /,\\/,$(locallib)/lib$(basename $(MAIN_DIRECTORY))$(ADD_DEBUG).a)
DEPSED := $(subst /,\\/,$(localdep))

MAIN_DEPS = $(addprefix $(localdep)/, $(CREATED_C:.c=$(ADD_DEBUG).dd)  $(CREATED_CC:.cc=$(ADD_DEBUG).dd) $(CREATED_F:.F=$(ADD_DEBUG).dd))

#---------
# actions
#---------
all: lib exe

lib: mklib  $(LIBRARY) $(CREATED_OBJS_F) $(CREATED_OBJS_C) $(CREATED_OBJS_CC)

slib: mkslib  $(SLIBRARY) $(CREATED_OBJS_F) $(CREATED_OBJS_C) $(CREATED_OBJS_CC)

exe: mkbin $(CREATED_OBJS_F) $(CREATED_OBJS_C) $(EXE) 
	@echo $(ONLINE_MESSAGE)

.PHONY : all mkall mklib mkbin exe lib clean help archive createso

help:
	@echo AR = $(AR)
	@echo CC = $(CC)
	@echo CFLAGS = $(CFLAGS)
	@echo CLAS_INCLUDES = $(CLAS_INCLUDES)
	@echo DEFAULT_INCLUDES $(DEFAULT_INCLUDES)
	@echo CLEVEL = $(CLEVEL)
	@echo CPP = $(CPP)
	@echo CPP_CXX = $(CPP_CXX)
	@echo CPPFLAGS = $(CPPFLAGS)
	@echo CREATED_OBJS_C = $(CREATED_OBJS_C)
	@echo CREATED_OBJS_CC = $(CREATED_OBJS_CC)
	@echo CREATED_OBJS_F = $(CREATED_OBJS_F)
	@echo CXX = $(CXX)
	@echo CXXFLAGS = $(CXXFLAGS)
	@echo DEPS $(DEPS)
	@echo EXE = $(EXE)
	@echo FC = $(FC)
	@echo FFLAGS = $(FFLAGS)
	@echo FLDFLAGS=$(FLDFLAGS)
	@echo INC_FLAG = $(INC_FLAG)
	@echo INCLUDE_FILES = $(INCLUDE_FILES)
	@echo LIB = $(LIB)
	@echo LIB_BASES = $(LIB_BASES)
	@echo LIB_MEMBERS = "$(LIB_MEMBERS)"
	@echo SLIB_MEMBERS = "$(SLIB_MEMBERS)"
	@echo LIB_PATH = $(LIB_PATH)
	@echo LIB_SOURCES = $(LIB_SOURCES)
	@echo LIB_SOURCE_BASES = $(LIB_SOURCE_BASES)
	@echo LIBNAMES = $(LIBNAMES)
	@echo LIBRARY = $(LIBRARY)
	@echo LIBS = $(LIBS)
	@echo LIBS_DIR = $(LIBS_DIR)
	@echo LIBSED = $(LIBSED)
	@echo LNKCMD = $(LNKCMD)
	@echo localdep = $(localdep)
	@echo locallib = $(locallib)
	@echo localslib = $(localslib)
	@echo LOCAL_INCLUDES = $(LOCAL_INCLUDES)
	@echo MAIN_DIRECTORY = $(MAIN_DIRECTORY)
	@echo MD_LIBS = $(MD_LIBS)
	@echo NOTDIR_CC_SOURCES = $(NOTDIR_CC_SOURCES)
	@echo NOTDIR_C_SOURCES = $(NOTDIR_C_SOURCES)
	@echo NOTDIR_F_SOURCES = $(NOTDIR_F_SOURCES)
	@echo OS_NAME = $(OS_NAME)
	@echo OBJS = $(OBJS)
	@echo OBJS_TO_AR = $(OBJS_TO_AR)
	@echo OBJS_TO_SLIB = $(OBJS_TO_SLIB)
	@echo RAWDEPS $(RAWDEPS)
	@echo RECSIS_LIBS1 = $(RECSIS_LIBS1)
	@echo RECSIS_LIBS2 = $(RECSIS_LIBS2)
	@echo RECSIS_LIBS3 = $(RECSIS_LIBS3)
	@echo SHARED_LIBS = $(SHARED_LIBS)
	@echo SLIBRARY = $(SLIBRARY)
	@echo SOURCE_FILES = $(SOURCE_FILES)
	@echo UNDEF = $(UNDEF)
	@echo VPATH = $(VPATH)
	@echo VPATH_DIR = $(VPATH_DIR)
	@echo vpath = $(vpath)

xhelp: 
	@echo CINT_SOURCES = $(CINT_SOURCES)
	@echo CINT_HEADERS = $(CINT_HEADERS)
	@echo LINKDEF_INCLUDES = $(LINKDEF_INCLUDES)
	@echo NOTDIR_CC_SOURCES = $(NOTDIR_CC_SOURCES)
	@echo DEPS = $(DEPS)
	@echo DEPSED = $(DEPSED)
clean: 
	-rm  depends/$(OS_NAME)/*.d* 
	-rm  $(LIBRARY)
	-rm  $(SLIBRARY)
	-rm  $(EXE)
	-rm  $(LIBDEB)
	-rm  $(EXE)_debug
	-rm  $(CREATED_OBJS_C)
	-rm  $(CREATED_OBJS_F)
	-rm  $(CREATED_OBJS_CC)
ifdef USE_ROOT_SYSTEM
	-rm  $(CINT_SOURCES) $(CINT_HEADERS)
endif

mkall: mklib mkbin

mklib:
	@test -d $(locallib) || mkdir -p $(locallib)
	@test -d $(tempobj) || mkdir -p $(tempobj)
ifneq ($(strip $(LIBRARY)),)
	@test -f $(LIBRARY)  || $(AR) r $(LIBRARY)
endif
	@$(RM) $(OBJS_TO_AR)

mkslib:
	@test -d $(localslib) || mkdir -p $(localslib)
	@test -d $(tempobj) || mkdir -p $(tempobj)
	@$(RM) $(OBJS_TO_SLIB)

mkbin:
	@test -d $(localbin) || mkdir -p $(localbin)
	@test -d $(tempobj) || mkdir -p $(tempobj)

$(CREATED_OBJS_F): $(locallib)/%$(ADD_DEBUG).o: %.F
			test -d $(dir $@) || mkdir -p $(dir $@)
			$(FC) $(FFLAGS) $(CPPFLAGS) \
			$(addprefix $(INC_FLAG),$(CLAS_INCLUDES)) -c \
			$*.F -o $@ 

$(CREATED_OBJS_C): $(locallib)/%$(ADD_DEBUG).o: %.c
			test -d $(dir $@) || mkdir -p $(dir $@)
			$(CC) $(CFLAGS) $(CPPFLAGS) $(CLAS_INCLUDES) -c \
			$*.c -o $@ 

$(CREATED_OBJS_CC): $(locallib)/%$(ADD_DEBUG).o: %.cc
			test -d $(dir $@) || mkdir -p $(dir $@)
			$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(CLAS_INCLUDES) -c \
			$*.cc -o $@ 

$(LIBRARY): $(LIB_MEMBERS) archive

$(SLIBRARY): $(SLIB_MEMBERS) createso


archive:
	@if [ -f $(OBJS_TO_AR) ]; then \
	echo Makefile: archiving files `cat $(OBJS_TO_AR)`; \
	echo " " >> $(OBJS_TO_AR); \
	$(AR) r $(LIBRARY) `cat $(OBJS_TO_AR) | $(CLAS_CMS)/one_line.pl`; \
	$(RM) -r $(tempobj); \
	$(AR) ts $(LIBRARY); \
	fi

createso:
	@if [ -f $(OBJS_TO_SLIB) ]; then \
	echo Creating shared library from `cat $(OBJS_TO_SLIB)`; \
	echo " " >> $(OBJS_TO_SLIB); \
	$(SLLD) $(SOFLAGS) -o $(SLIBRARY) `cat $(OBJS_TO_SLIB) | $(CLAS_CMS)/one_line.pl`; \
	echo $(SLLD) $(SOFLAGS) -o $(SLIBRARY) `cat $(OBJS_TO_SLIB) | $(CLAS_CMS)/one_line.pl`; \
	$(RM) -r $(tempobj); \
	fi

#
# the LNKCMD command below has replaced the FC command because
# of the need to use separate commands for compiling and linking
# for the online version of recsis. in all other cases the 
# LNKCMD is identical to the old FC command. see flags.SunOS.mk
# or the file for your platform for the definition.
#                                          gilfoyle, 1/28/98
$(EXE): $(LIBS) $(REQUIRED_OBJS)
	$(LNKCMD)  $(UNDEF) \
	$(FLDFLAGS) \
	$(REQUIRED_OBJS) $(LIBS_DIR) $(LIBS) $(SHARED_LIBS) $(MD_LIBS) \
	-o $(EXE)

#------------------------------------------------------------
# Pattern rules
#------------------------------------------------------------
#  automatic generation of depends files  SOURCE.d  loated in 
#  localdep/*.d. 
#
#  include all the local generated dependency files, these are created and
#  updated automatically for you.

$(localdep)/%$(ADD_DEBUG).d: %.F
	@test -d $(localdep) || mkdir -p $(localdep)
	@echo Makefile: making dependency file for $<
	@( echo $(LIBRARY) ; echo $(localdep) ; echo $* ; \
	    echo $(ADD_DEBUG) ; $(CPP) $(CPPFLAGS) $(CLAS_INCLUDES) $< ) \
	    | $(CLAS_CMS)/depends.pl > $@

$(localdep)/%$(ADD_DEBUG).d: %.c
	@test -d $(localdep) || mkdir -p $(localdep)
	@echo Makefile: making dependency file for $<
	@( echo $(LIBRARY) ; echo $(localdep) ; echo $* ; \
	    echo $(ADD_DEBUG) ; $(CPP) $(CPPFLAGS) $(CLAS_INCLUDES) $< ) \
	    | $(CLAS_CMS)/depends.pl > $@

$(localdep)/%$(ADD_DEBUG).d: %.cc
	@test -d $(localdep) || mkdir -p $(localdep)
	@echo Makefile: making dependency file for $<
	@( echo $(LIBRARY) ; echo $(localdep) ; echo $* ; \
	    echo $(ADD_DEBUG) ; $(CPP_CXX) $(CPPFLAGS) $(CLAS_INCLUDES) $< ) \
	    | $(CLAS_CMS)/depends.pl > $@

$(localdep)/%$(ADD_DEBUG).dd: %.c
	@test -d $(localdep) || mkdir -p $(localdep) ;
	@echo Makefile: making dependency file for $<
	@$(CPP_CXX) $(CPPFLAGS) $(CLAS_INCLUDES) $< \
	| $(CLAS_CMS)/depends_object.pl $(localdep) $(locallib) $(ADD_DEBUG) \
	> $@

$(localdep)/%$(ADD_DEBUG).dd: %.cc
	@test -d $(localdep) || mkdir -p $(localdep)
	@echo Makefile: making dependency file for $<
	@$(CPP_CXX) $(CPPFLAGS) $(CLAS_INCLUDES) $< \
	| $(CLAS_CMS)/depends_object.pl $(localdep) $(locallib) $(ADD_DEBUG) \
	> $@

$(localdep)/%$(ADD_DEBUG).dd: %.F
	@test -d $(localdep) || mkdir -p $(localdep) ;
	@echo Makefile: making dependency file for $<
	@$(CPP_CXX) $(CPPFLAGS) $(CLAS_INCLUDES) $< \
	| $(CLAS_CMS)/depends_object.pl $(localdep) $(locallib) $(ADD_DEBUG) \
	> $@

$(LIBRARY)(%.o): %.F
	$(FC)  $(FFLAGS) \
		$(addprefix $(INC_FLAG),$(CPPFLAGS)) \
		$(addprefix $(INC_FLAG),$(CLAS_INCLUDES)) -c \
		$<  -o $(tempobj)/$*.o
	@echo "$(tempobj)/$*.o" >> $(OBJS_TO_AR)

$(LIBRARY)(%.o): %.c  
	$(CC) $(CFLAGS)  $(CPPFLAGS) \
		$(CLAS_INCLUDES) -c \
		$< -o $(tempobj)/$%
	@echo "$(tempobj)/$*.o" >> $(OBJS_TO_AR)

$(LIBRARY)(%.o): %.cc  
	@echo Library make for $(@) from $(<)
	$(CXX) $(CXXFLAGS)  $(CPPFLAGS) \
		$(CLAS_INCLUDES) -c \
		$< -o $(tempobj)/$*.o
	@echo "$(tempobj)/$*.o" >> $(OBJS_TO_AR)

$(SLIBRARY)(%.o): %.F
	$(FC)  $(FFLAGS) \
		$(addprefix $(INC_FLAG),$(CPPFLAGS)) \
		$(addprefix $(INC_FLAG),$(CLAS_INCLUDES)) -c \
		$<  -o $(tempobj)/$*.o
	@echo "$(tempobj)/$*.o" >> $(OBJS_TO_SLIB)

$(SLIBRARY)(%.o): %.c  
	$(CC) $(CFLAGS)  $(CPPFLAGS) \
		$(CLAS_INCLUDES) -c \
		$< -o $(tempobj)/$%
	@echo "$(tempobj)/$*.o" >> $(OBJS_TO_SLIB)

$(SLIBRARY)(%.o): %.cc  
	$(CXX) $(CXXFLAGS)  $(CPPFLAGS) \
		$(CLAS_INCLUDES) -c \
		$< -o $(tempobj)/$*.o
	@echo "$(tempobj)/$*.o" >> $(OBJS_TO_SLIB)


%.o: %.F
	$(FC)  $(FFLAGS) \
	$(addprefix $(INC_FLAG), $(CPPFLAGS)) \
	$(addprefix $(INC_FLAG), $(CLAS_INCLUDES)) -c $<

%.o: %.c
	$(CC) $(CFLAGS)  $(CPPFLAGS) $(CLAS_INCLUDES) -c $<

%.o: %.cc
	$(CXX) $(CXXFLAGS)  $(CPPFLAGS) $(CLAS_INCLUDES) -c $<


ifdef USE_ROOT_SYSTEM
$(CINT_SOURCES): %_Cint.cc: %.h %_LinkDef.h
	rootcint -f $(subst .h,.cc,$@) -c $(CLAS_ROOT_INCLUDES) -D__cplusplus $<
endif

#-------------------------------------------------
#  Dependencies for each source file
#-------------------------------------------------
ifneq ($(strip $(DEPS)),)
    include $(DEPS)
endif
ifneq ($(strip $(MAIN_DEPS)),)
    include $(MAIN_DEPS)
endif







