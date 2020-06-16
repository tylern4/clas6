#---------------------
# Makefile to make generic library	 and binary APF 12/31/1996
#	
# If Makefile file is invoked in directory "t	op":                      
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
##############################
# ROOT & SHARED  # E.A. oct 98
##############################
#
# To generate a shared library:
# make lib SHARED=1
# >> $(TOP_DIR)/slib/<platform>/lib<package>.$(SL_SUF)
# to generate a shared library containing a ROOT dictionnary:
# make lib ROOT=1
# >> $(TOP_DIR)/slib/<platform>/libROOT<package>.$(SL_SUF)
# To generate a shared executable:
# make exe SHARED=1
# >> $(TOP_DIR)/slib/<platform>/<package>
# To generate a shared executable containing a ROOT dictionnary:
# make exe ROOT=1
# >> $(TOP_DIR)/slib/<platform>/ROOT<package>
# When generating a dictionnary, you need a Link Definition header, (see ROOT doc)
# the name of this file should have the format:
# <Aname>_LinkDef.h
# dictionnary sources are put in $(TOP_DIR)/src_derived/<package>Cint.cc
# dictionnary headers are put in $(TOP_DIR)/inc_derived/<package>Cint.h
#
# shared created object are put in $(TOP_DIR)/obj/
# root dictionnary source associated with created obj are put in 
# $(TOP_DIR)/src_derived/<package>CreatedCint.cc
# root dictionnary header associated with created obj are put in 
# $(TOP_DIR)/inc_derived/<package>CreatedCint.h
#
######################################################################
#  Variable       definition
# * -> user setable variables (defaults usually provided)
#
#  SHELL          shell used for spawned processes (/bin/sh)
# *TOP_DIR        Directory you want the libraries and binaries deposited
#  locallib       $(TOP_DIR)/$(OS_NAME)/lib    libraries deposited here
#  localslib      $(TOP_DIR)$(OS_NAME)/slib    shared libraries deposited here
#  localbin       $(TOP_DIR)/$(OS_NAME)/bin    binaries depostied here
#  localdep       $(TOP_DIR)/package/depends/$(OS_NAME) 
#                                 dependency files deposited here
# 
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
#  SHARED          path and name of shared library
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
#  SOFLAGS         shared libraries flag E.A. 11/10/98
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

ifndef CppSuf
  CppSuf=cc
endif

ifndef OS_NAME
 OS_NAME = $(shell $(CLAS_CMS)/uname_clas)
endif

ifndef MAIN_DIRECTORY
  MAIN_DIRECTORY = $(notdir $(shell pwd))
endif

ifndef EXENAME
 EXENAME=$(MAIN_DIRECTORY)
endif

ifdef LOGNAME
 USERNAME = $(LOGNAME)
else
 USERNAME = $(USER)
endif

ifndef LOCAL_PACK
 LOCAL_PACK = ../
endif

# EDIT THE FOLLOWING THE LINE---tell MAKE where to deposit the libraries and
# binaries

ifndef TOP_DIR
 TOP_DIR = $(HOME)/
endif

#  -- ar patch for SunOS4 ----
#

ifeq ($(OS_NAME),SunOS4) 
  AR = /usr/5bin/ar
else
  AR = ar
endif

#
# ---------  working directories.....
#

locallib = $(TOP_DIR)/lib/$(OS_NAME)
localbin = $(TOP_DIR)/bin/$(OS_NAME)
tempobj = $(TOP_DIR)/obj/$(OS_NAME)
tempsrc = ./ROOTCintDerived
localdep = ./depends/$(OS_NAME)
localslib = $(TOP_DIR)/slib/$(OS_NAME)
localincd = ./ROOTCintDerived


################################
# starting point of shared stuff,
# below is a listing of all the good reason we have to used shared stuff:
# forcing shared stuff can also be done directly by using the SHARED flag
################################

# if we are generating ROOT dictionnary,
# we need to use ROOT include files, and libraries

ifdef ROOTDIC
   ROOT=1
   SHARED=1
endif

# EDIT THE FOLLOWING LINE----tell MAKE where to find the included Makefiles

#CLAS_CMS = $(CLAS_CMS)

#---------------------
# EDIT THE FOLLOWING LINE
# Set up variables that point to directories holding the libraries
# These are used by the LINKER to find those libraries named in the 
# LIBNAMES variable...see LIBNAMES variable above...
#---------------------

#LIB_PATH += $(locallib) $(CLAS_LIB) $(CERN_ROOT)/lib $(TCL_LIB) $(X_LIB)

# to be abble to include root shared libs.
# if you don't put $(ROOTSYS)/lib path before $X_LIB path linking will crash
# when trying to resolve XpmFree in libGX11 root library
# probably there is another Xpm library hiding somewhere that doesn't
# contain XpmFree function.. order of PATH & libs is really important !!  E.A

ifdef ROOT
  LIB_PATH += $(ROOTSYS)/lib
endif

LIB_PATH +=$(localslib) $(locallib) $(CLAS_SLIB) $(CLAS_LIB) $(CERN_ROOT)/lib $(TCL_LIB) $(X_LIB)

#----------------------------------------
# List of subdirectories containing FORTRAN and/or C source code
#----------------------------------------

SOURCE_FILES = $(wildcard *.[cF])
SOURCE_FILES += $(wildcard *.$(CppSuf))
SUB_DIRECTORIES = $(sort $(dir $(SOURCE_FILES)))

#INCLUDE_FILES = $(wildcard ../*/*.inc ../*/*.h)
INCLUDE_FILES = $(wildcard */*.h */*.inc)

LOCAL_INCLUDES += -I. $(addprefix -I,$(sort $(dir $(INCLUDE_FILES))))

#if using root dictionnary generator, the directory $TOP_DIR/inc_derived doesn't exist yet,
# but will be created and will be used.

#ifdef ROOT
#  LOCAL_INCLUDES += -I$(ROOTSYS)/include
#endif

#ifdef ROOTDIC
#  LOCAL_INCLUDES += -I$(localincd)
#endif

#---------- List of sources & objects needed for the root dictionnary:
# everything is doubled between dictionnary related to the library you want to build,
# and dictionnary related to the created objects.
# this is so, because created objects could be code that reference
# code in static libraries, that cannot be put in a shared library 
# but can be integrated in one executable. typicall exemple is the CERN lib

ifdef ROOTDIC
# First defined the names of ROOTCINT derived source and header 
# so that they don't get caught they are left in the local directory
# if rootcint previously crashed..
ROOTCINT_OPTIONS =

ROOTCINT_SRC0 = $(basename $(MAIN_DIRECTORY))Cint.cc
ROOTCINT_DICH0 = $(basename $(MAIN_DIRECTORY))Cint.h
ROOTCINT_CREATED_SRC0 = $(basename $(EXENAME))CreatedCint.cc
ROOTCINT_CREATED_DICH0 = $(basename $(EXENAME))CreatedCint.h

LOCINC_FILES = $(wildcard *.h)
ROOTCINT_HEADERS0 = $(filter-out %_LinkDef.h $(ROOTCINT_CREATED_HEADERS), $(LOCINC_FILES))
ROOTCINT_HEADERS = $(filter-out $(ROOTCINT_DICH0) $(ROOTCINT_CREATED_DICH0), $(ROOTCINT_HEADERS0))
ROOTCINT_LNKDF = $(filter-out $(ROOTCINT_CREATED_LNKDF), $(wildcard *_LinkDef.h))

# if there is a linkdef for library: (as detected)
ifneq ($(strip $(ROOTCINT_LNKDF)),)
  ROOTCINT_PREF = ROOT
  ROOTCINT_SRC = $(tempsrc)/$(ROOTCINT_SRC0)
  ROOTCINT_OBJ = $(tempobj)/$(basename $(MAIN_DIRECTORY))Cint.o
  ROOTCINT_DICH = $(localincd)/$(ROOTCINT_DICH0)
  CINTDEPS = $(localdep)/$(MAIN_DIRECTORY)_Cint.d
endif

# if there is a linkdef for created OBJS (this one has to be given by the user)
ifneq ($(strip $(ROOTCINT_CREATED_LNKDF)),)
  ROOTCINT_PREF = ROOT
  ROOTCINT_CREATED_SRC = $(tempsrc)/$(ROOTCINT_CREATED_SRC0)
  ROOTCINT_CREATED_OBJ = $(tempobj)/$(basename $(EXENAME))CreatedCint.o
  ROOTCINT_CREATED_DICH = $(localincd)/$(ROOTCINT_CREATED_DICH0)
  CREATEDCINTDEPS = $(localdep)/$(MAIN_DIRECTORY)_CreatedCint.d
endif
endif

#----------------------------------------------
# build up the list of source and object files
#----------------------------------------------

ifndef FORTRAN_SOURCES
  FORTRAN_SOURCES = $(filter-out %.c %.$(CppSuf) $(CREATED_F), $(SOURCE_FILES))
endif
NOTDIR_F_SOURCES = $(notdir $(FORTRAN_SOURCES))
NOTDIR_F_OBJS0 =$(NOTDIR_F_SOURCES:.F=.o)
NOTDIR_F_SHOBJS0 =$(NOTDIR_F_SOURCES:.F=_sh.o)

ifndef C_SOURCES
  C_SOURCES = $(filter-out %.F %.$(CppSuf) $(CREATED_C), $(SOURCE_FILES))
endif
NOTDIR_C_SOURCES = $(notdir $(C_SOURCES))
NOTDIR_C_OBJS0 =$(NOTDIR_C_SOURCES:.c=.o)
NOTDIR_C_SHOBJS0 =$(NOTDIR_C_SOURCES:.c=_sh.o)

ifndef CC_SOURCES
  CC_SOURCES0 = $(filter-out %.F %.c $(CREATED_CC), $(SOURCE_FILES))
# the line below is to remove remaining MAIN_DIRETORYCint.cc from an old crashed rootcint.

 CC_SOURCES = $(filter-out $(ROOTCINT_SRC0) $(ROOTCINT_CREATED_SRC0), $(CC_SOURCES0))

###############################
endif

NOTDIR_CC_SOURCES = $(notdir $(CC_SOURCES))
NOTDIR_CC_OBJS0 =$(NOTDIR_CC_SOURCES:.$(CppSuf)=.o)
NOTDIR_CC_SHOBJS0 =$(NOTDIR_CC_SOURCES:.$(CppSuf)=_sh.o)


# OBJS variable contains the object filenames for F, C and CC source 
# for static libraries depencies.                                            

OBJS  = $(subst :,,$(patsubst %.F, $(LIBRARY):(:%.o:):, $(NOTDIR_F_SOURCES)) \
        $(patsubst %.c, $(LIBRARY):(:%.o:):, $(NOTDIR_C_SOURCES)) \
        $(patsubst %.$(CppSuf), $(LIBRARY):(:%.o:):, $(NOTDIR_CC_SOURCES)))

# list of object filenames for F, C and CC source 
# for shared library dependencies.

SLIB_F_OBJS = $(addprefix $(tempobj)/$(basename $(MAIN_DIRECTORY))_,$(NOTDIR_F_SHOBJS0))
SLIB_C_OBJS = $(addprefix $(tempobj)/$(basename $(MAIN_DIRECTORY))_,$(NOTDIR_C_SHOBJS0))
SLIB_CC_OBJS = $(addprefix $(tempobj)/$(basename $(MAIN_DIRECTORY))_,$(NOTDIR_CC_SHOBJS0))
CLEAN_OBJS = $(addprefix $(tempobj)/$(basename $(MAIN_DIRECTORY))_,$(NOTDIR_CC_SHOBJS0))

#------------------------------------
# Required OBJECTS, usually the MAIN routine that starts it all
# use the CREATED variable to name those object files you wish to
# CREATE and use the REQUIRED_OBJS to list those object files
# that are not to be created but in existence (like recsis.o)
# statics and shared.
#------------------------------------

ifneq ($(strip $(CREATED_F)),)  
 CREATED_OBJS_F = $(addprefix $(locallib)/,$(notdir $(CREATED_F:.F=$(ADD_DEBUG).o)))
 SH_CREATED_OBJS_F = $(addprefix $(localslib)/$(basename $(MAIN_DIRECTORY))_,$(notdir $(CREATED_F:.F=$(ADD_DEBUG).o)))
endif

ifneq ($(strip $(CREATED_C)),)
 CREATED_OBJS_C = $(addprefix $(locallib)/,$(notdir $(CREATED_C:.c=$(ADD_DEBUG).o)))
 SH_CREATED_OBJS_C = $(addprefix $(localslib)/$(basename $(MAIN_DIRECTORY))_,$(notdir $(CREATED_C:.c=$(ADD_DEBUG).o)))
endif

ifneq ($(strip $(CREATED_CC)),)
 CREATED_OBJS_CC = $(addprefix $(locallib)/,$(notdir $(CREATED_CC:.$(CppSuf)=$(ADD_DEBUG).o)))
 SH_CREATED_OBJS_CC = $(addprefix $(localslib)/$(basename $(MAIN_DIRECTORY))_,$(notdir $(CREATED_CC:.$(CppSuf)=$(ADD_DEBUG).o)))
endif

ifdef SHARED
 REQUIRED_OBJS += $(SH_CREATED_OBJS_F)
 REQUIRED_OBJS += $(SH_CREATED_OBJS_C)
 REQUIRED_OBJS += $(SH_CREATED_OBJS_CC)
else
 REQUIRED_OBJS += $(CREATED_OBJS_F)
 REQUIRED_OBJS += $(CREATED_OBJS_C)
 REQUIRED_OBJS += $(CREATED_OBJS_CC)
endif

REQUIRED_OBJS += $(COMPILED_OBJS)

#---------------------
# Set up the targets, the basename is needed for CLAS directories
# that are name clas.lib.  The ".lib" suffix is removed.
#---------------------

EXE = $(localbin)/$(basename $(EXENAME))$(ADD_ONLINE)$(ADD_DEBUG)

ifneq ($(strip $(OBJS)),)
LIBRARY = $(locallib)/lib$(addsuffix $(ADD_DEBUG), \
			$(basename $(MAIN_DIRECTORY))).a
STATICLINK = -l$(addsuffix $(ADD_DEBUG),$(basename $(MAIN_DIRECTORY)))
endif

# shared equivalents.

ifneq ($(strip $(REQUIRED_OBJS)),)
 SHAREDEXE = $(localbin)/$(ROOTCINT_PREF)$(basename $(EXENAME))$(ADD_ONLINE)$(ADD_DEBUG)_sh
endif

ifneq ($(strip $(SLIB_F_OBJS) $(SLIB_C_OBJS) $(SLIB_CC_OBJS)),)
SHAREDLIB0 = $(addprefix $(ROOTCINT_PREF), $(addsuffix $(ADD_DEBUG), \
			$(basename $(MAIN_DIRECTORY)))).$(SL_SUF)

SHAREDLIB = $(localslib)/lib$(SHAREDLIB0)
SLIBRARY_NAME = lib$(SHAREDLIB0)
SHAREDLINK = -l$(SHAREDLIB0:.$(SL_SUF)=)
endif

#---------------------------------------
# Machine dependent flags and  default link lists
#---------------------------------------

SYSTEM_FLAGS = $(CLAS_CMS)/flags.$(OS_NAME).mk
include $(SYSTEM_FLAGS)
include $(CLAS_CMS)/link.mk
include $(CLAS_CMS)/link.$(OS_NAME).mk

#--------------------------------------
# For LINUX, you really do want to link using the g++ linker,
# the g77 is not equivalent, and won't find the correct g++ libraries.
# Note that the g++ libraries to include would be very sensitive to the
# specific version of g++ you are using (gcc,egcs,comercial cc,...)
#
# The new make system got improved, now this seems ok (10/28/99)
# 
#-------------------------------------
#ifeq ($(OS_NAME),Linux)
#   LNKCMD = g++
#
#endif

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

# ROOT Libraries:
# ROOT_SH_LIBS are pure Root shared libs (containers, tree, histo,..) platform independent.
# ROOT_MD_LIBS root required static libs - platform dependent.
# ROOT_SHARED_LIBS root required shared libs - platform dependent.

ifdef ROOT
MD_LIBNAMES += $(ROOT_MD_LIBS)
SHARED_LIBS += $(ROOT_SH_LIBS) $(ROOT_SHARED_LIBS)
CXXFLAGS += $(ROOT_FLAGS)
SOFLAGS += $(ROOT_FLAGS)
endif

CLAS_LIBNAMES_D = $(addsuffix $(ADD_DEBUG),$(CLAS_LIBNAMES))
CLAS_LIBS= $(addprefix -l, $(CLAS_LIBNAMES_D))

LIBS = $(addprefix -l, $(CLAS_LIBNAMES_D) $(LIBNAMES))

MD_LIBS = $(addprefix -l, $(MD_LIBNAMES))

CLAS_SHARED_LIBS_D = $(addsuffix $(ADD_DEBUG),$(CLAS_SHARED_LIBS))
CLAS_SHAREDLIBS = $(addprefix -l, $(CLAS_SHARED_LIBS_D))

# for futur compatibility :-)
SHAREDLIBS= $(SHARED_LIBS)

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

#
# new architecture, LOCAL_PACK defines the path to get back to the local packages
# (for exemple ../ when the Makefile file is IN the tag package, 
#  ../../ when the Makefile file is IN the clasroot/clasbos package )
# then PACK_INCLUDES defines the path from there to get an include path (for exemple clasroot/bosbank
# to get something in clasroot/bosbank.. )
#
# compilator looks first in $LOCAL_PACK/PACK_INCLUDES and if this directory doesn't exists, it looks
# in $CLAS_PACK/PACK_INCLUDES
#

INCLUDES += $(LOCAL_INCLUDES)
ifneq ($(strip $(PACK_INCLUDES)),)
  INCLUDES += $(addprefix -I$(LOCAL_PACK)/,$(PACK_INCLUDES))
  INCLUDES += $(addprefix -I$(CLAS_PACK)/,$(PACK_INCLUDES))
endif
INCLUDES +=  $(DEFAULT_INCLUDES)

ifdef ROOT
 INCLUDES += -I$(ROOTSYS)/include
endif

ifdef ROOTDIC
ROOTCINT_INCLUDES += $(LOCAL_INCLUDES)
ifneq ($(strip $(CINT_INCLUDES)),)
 ROOTCINT_INCLUDES += $(addprefix -I$(LOCAL_PACK)/,$(CINT_INCLUDES))
 ROOTCINT_INCLUDES += $(addprefix -I$(CLAS_PACK)/,$(CINT_INCLUDES))
endif
endif

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
#VPATH_TMP  = $(strip $(LIB_PATH))
VPATH_DIR  = $(subst $(space),:,$(strip $(LIB_PATH)))
LIBS_DIR  = -L$(subst :, -L,$(VPATH_DIR))

#TEST = $(strip $(VPATH_DIR)):/usr/local/lib:/usr/lib

#vpath %.a $(strip $(VPATH_DIR)):/usr/local/lib:/usr/lib
vpath %.a $(strip $(VPATH_DIR))


#VPATH =$(patsubst %/,%:,$(SUB_DIRECTORIES))

#-----------------------------------------
# variables needed for automatic dependency generation.	
# The LIBSED and DEPSED variable, have "\" in front of "/" so these variables
# can be fed to sed.  
# The DEPS variable contains all the dependency filenames
#-----------------------------------------
#LIBDEB = $(locallib)/lib$(addsuffix _debug, \
#			$(basename $(MAIN_DIRECTORY))).a
#LIBRDEB = $(locallib)/libROOT$(addsuffix _debug, \
#			$(basename $(MAIN_DIRECTORY))).a
#SLIBDEB = $(localslib)/lib$(addsuffix _debug, \
#			$(basename $(MAIN_DIRECTORY))).$(SL_SUF)
#SLIBRDEB =  $(localslib)/libROOT$(addsuffix _debug, \
#			$(basename $(MAIN_DIRECTORY))).$(SL_SUF)
#####

RAWDEPS  = $(addprefix lib$(MAIN_DIRECTORY)., $(NOTDIR_F_SOURCES:.F=.d) $(NOTDIR_C_SOURCES:.c=.d) $(NOTDIR_CC_SOURCES:.$(CppSuf)=.d))
DEPS = $(addprefix $(localdep)/, $(RAWDEPS))

LIBSED    := $(subst /,\\/,$(locallib)/lib$(basename $(MAIN_DIRECTORY)).a)
LIBDEBSED := $(subst /,\\/,$(locallib)/lib$(addsuffix _debug,$(basename $(MAIN_DIRECTORY))).a)
LIBRSED    := $(subst /,\\/,$(locallib)/libROOT$(basename $(MAIN_DIRECTORY)).a)
LIBRDEBSED := $(subst /,\\/,$(locallib)/libROOT$(addsuffix _debug,$(basename $(MAIN_DIRECTORY))).a)

RAWCREATEDDEPS= $(addprefix $(MAIN_DIRECTORY)_, $(CREATED_F:.F=.d) $(CREATED_C:.c=.d) $(CREATED_CC:.$(CppSuf)=.d))
CREATEDDEPS = $(addprefix $(localdep)/, $(RAWCREATEDDEPS))

#CREATEDSED    := $(subst /,\\/,$(locallib)/lib$(basename $(MAIN_DIRECTORY)).a)
#CREATEDDEBSED := $(subst /,\\/,$(locallib)/lib$(addsuffix _debug,$(basename $(MAIN_DIRECTORY))).a)
#CREATEDRSED    := $(subst /,\\/,$(locallib)/libROOT$(basename $(MAIN_DIRECTORY)).a)
#CREATEDDEBSED := $(subst /,\\/,$(locallib)/libROOT$(addsuffix _debug,$(basename $(MAIN_DIRECTORY))).a)

TMPOBJSED    := $(subst /,\\/,$(tempobj)/)
LIBPSED      := $(subst /,\\/,$(locallib)/)
SLIBPSED    := $(subst /,\\/,$(localslib)/)
CINTSRCSED    :=$(subst /,\\/,$(ROOTCINT_SRC))
CINTDICHSED    :=$(subst /,\\/,$(ROOTCINT_DICH))
CREATEDCINTSRCSED :=$(subst /,\\/,$(ROOTCINT_CREATED_SRC))
CREATEDCINTDICHSED  :=$(subst /,\\/,$(ROOTCINT_CREATED_DICH))

TMPCINTCC :=tmpCint
TMPCREATEDCINTCC :=tmpCreatedCint
TMPCINTSED :=$(subst /,\\/,$(localdep)/$(TMPCINTCC).$(CppSuf))
TMPCREATEDCINTSED :=$(subst /,\\/,$(localdep)/$(TMPCREATEDCINTCC).$(CppSuf))


DEPSED := $(subst /,\\/,$(localdep))

#---------
# actions
#---------

ifdef SHARED
  libtarget=shlib
  exetarget=shexe
else
  libtarget=staticlib
  exetarget=staticexe
endif

all: lib exe

lib: $(libtarget)

exe: $(exetarget)

staticlib: mklib  $(LIBRARY) $(CREATED_OBJS_F) $(CREATED_OBJS_C) $(CREATED_OBJS_CC)

staticexe: mkbin $(CREATED_OBJS_F) $(CREATED_OBJS_C) $(CREATED_OBJS_CC) $(EXE)
	@echo $(ONLINE_MESSAGE)

shlib: mkslib $(SLIB_F_OBJS) $(SLIB_C_OBJS) $(SLIB_CC_OBJS) $(SHAREDLIB)

shexe: mkbin $(SH_CREATED_OBJS_F) $(SH_CREATED_OBJS_C) $(SH_CREATED_OBJS_CC) $(SHAREDEXE) 


.PHONY : all mkall mklib mkbin exe lib clean help mkslib

help: 
#------------------------------ OPTIONS
	@echo DEBUG = $(DEBUG)
	@echo ROOT = $(ROOT)
	@echo ROOTDIC = $(ROOTDIC)
	@echo ONLINE = $(ONLINE)
	@echo SHARED = $(SHARED)

#------------------------------ SYSTEM flags / commands
	@echo CppSuf = $(CppSuf)
	@echo OS_NAME = $(OS_NAME)
	@echo CLAS_CMS = $(CLAS_CMS)
	@echo SYSTEM_FLAGS = $(SYSTEM_FLAGS)
	@echo CC = $(CC)
	@echo CFLAGS = $(CFLAGS)
	@echo CLEVEL = $(CLEVEL)
	@echo CPP = $(CPP)
	@echo CPPFLAGS = $(CPPFLAGS)
	@echo CXX = $(CXX)
	@echo CXXFLAGS = $(CXXFLAGS)
	@echo FC = $(FC)
	@echo FFLAGS = $(FFLAGS)
	@echo FLDFLAGS = $(FLDFLAGS)
	@echo SOFLAGS = $(SOFLAGS)
	@echo DLLFLAGS = $(DLLFLAGS)
	@echo SLLD = $(SLLD)
	@echo LNKCMD = $(LNKCMD)
	@echo ROOT_FLAGS = $(ROOT_FLAGS)
#------------------------------ Includes / libraries
	@echo PACK_INCLUDES $(PACK_INCLUDES)
	@echo LOCAL_INCLUDES $(LOCAL_INCLUDES)
	@echo CINT_INCLUDES $(CINT_INCLUDES)
	@echo DEFAULT_INCLUDES $(DEFAULT_INCLUDES)
	@echo ROOTCINT_INCLUDES $(ROOTCINT_INCLUDES)
	@echo INCLUDES = $(INCLUDES)
	@echo RECSIS_LIBS1 = $(RECSIS_LIBS1)
	@echo RECSIS_LIBS2 = $(RECSIS_LIBS2)
	@echo RECSIS_LIBS3 = $(RECSIS_LIBS3)
	@echo TCL_LIB = $(TCL_LIB)
	@echo TCL_INC = $(TCL_INC)
	@echo X_LIB = $(X_LIB)
	@echo X_INC = $(X_INC)

#	@echo VPATH = $(VPATH)
#	@echo vpath = $(vpath)
	@echo LIB = $(LIB)
	@echo LIB_PATH = $(LIB_PATH)
	@echo SLIB_PATH = $(SLIB_PATH)
#	@echo VPATH_TMP = $(VPATH_TMP)
	@echo VPATH_DIR = $(VPATH_DIR)
	@echo LIBS_DIR = $(LIBS_DIR)
	@echo CLAS_LIBNAMES = $(CLAS_LIBNAMES)
	@echo LIBNAMES = $(LIBNAMES)
	@echo LIBS = $(LIBS)
	@echo CLAS_SHARED_LIBS = $(CLAS_SHARED_LIBS)
	@echo SHARED_LIBS = $(SHARED_LIBS)
	@echo SHAREDLIBS = $(SHAREDLIBS)
	@echo MD_LIBNAMES = $(MD_LIBNAMES)
	@echo MD_LIBS = $(MD_LIBS)
	@echo ROOT_SH_LIBS = $(ROOT_SH_LIBS)
#-------------------------------dependencies
	@echo LIBDEB = $(LIBDEB)
	@echo DLLDEB = $(DLLDEB)
	@echo RAWDEPS = $(RAWDEPS)
	@echo DEPS = $(DEPS)
	@echo LIBSED = $(LIBSED)
	@echo SLIBSED = $(SLIBSED)
	@echo RAWCREATEDDEPS = $(RAWCREATEDDEPS)
	@echo CREATEDDEPS = $(CREATEDDEPS)
	@echo TMPOBJSED = $(TMPOBJSED)
	@echo LIBPSED= $(LIBPSED=)
	@echo SLIBPSED = $(SLIBPSED)
	@echo CINTSRCSED = $(CINTSRCSED)
	@echo CINTDICHSED  = $(CINTDICHSED)
	@echo CREATEDCINTSRCSED = $(CREATEDCINTSRCSED)
	@echo CREATEDCINTDICHSED = $(CREATEDCINTDICHSED)
	@echo TMPCINTCC = $(TMPCINTCC)
	@echo TMPCREATEDCINTCC = $(TMPCREATEDCINTCC)
	@echo TMPCINTSED= $(TMPCINTSED=)
	@echo TMPCREATEDCINTSED = $(TMPCREATEDCINTSED)

#------------------------------ targets/obj/src
	@echo EXENAME = $(EXENAME)
	@echo MAIN_DIRECTORY = $(MAIN_DIRECTORY)
	@echo EXE = $(EXE)
	@echo LIBRARY = $(LIBRARY)
	@echo SHAREDEXE = $(SHAREDEXE)
	@echo SHAREDLIB = $(SHAREDLIB)
	@echo NOTDIR_CC_SOURCES = $(NOTDIR_CC_SOURCES)
	@echo NOTDIR_C_SOURCES = $(NOTDIR_C_SOURCES)
	@echo NOTDIR_F_SOURCES = $(NOTDIR_F_SOURCES)
	@echo REQUIRED_OBJS = $(REQUIRED_OBJS)
	@echo CREATED_OBJS_F = $(CREATED_OBJS_F)
	@echo CREATED_OBJS_C = $(CREATED_OBJS_C)
	@echo CREATED_OBJS_CC = $(CREATED_OBJS_CC)
	@echo SH_CREATED_OBJS_F = $(SH_CREATED_OBJS_F)
	@echo SH_CREATED_OBJS_C = $(SH_CREATED_OBJS_C)
	@echo SH_CREATED_OBJS_CC = $(SH_CREATED_OBJS_CC)
	@echo NOTDIR_F_OBJS0 = $(NOTDIR_F_OBJS0)
	@echo NOTDIR_C_OBJS0 = $(NOTDIR_C_OBJS0)
	@echo NOTDIR_CC_OBJS0 = $(NOTDIR_CC_OBJS0)
	@echo SLIB_F_OBJS = $(SLIB_F_OBJS)
	@echo SLIB_C_OBJS = $(SLIB_C_OBJS)
	@echo SLIB_CC_OBJS = $(SLIB_CC_OBJS)
	@echo libtarget = $(libtarget)
	@echo exetarget = $(exetarget)
	@echo SHAREDLINK = $(SHAREDLINK)
	@echo STATICLINK = $(STATICLINK)
#------------------------------------ root cint
	@echo LOCINC_FILES = $(LOCINC_FILES)
	@echo ROOTCINT_OPTIONS = $(ROOTCINT_OPTIONS)
	@echo ROOTCINT_HEADERS = $(ROOTCINT_HEADERS)
	@echo ROOTCINT_CREATED_INCLUDES = $(ROOTCINT_CREATED_INCLUDES)
	@echo ROOTCINT_LNKDF = $(ROOTCINT_LNKDF)
	@echo ROOTCINT_SRC0 = $(ROOTCINT_SRC0)
	@echo ROOTCINT_SRC = $(ROOTCINT_SRC)
	@echo ROOTCINT_OBJ = $(ROOTCINT_OBJ)
	@echo ROOTCINT_DICH0 = $(ROOTCINT_DICH0)
	@echo ROOTCINT_DICH = $(ROOTCINT_DICH)
	@echo ROOTCINT_CREATED_HEADERS = $(ROOTCINT_CREATED_HEADERS)
	@echo ROOTCINT_CREATED_H = $(ROOTCINT_CREATED_H)
	@echo ROOTCINT_CREATED_LNKDF = $(ROOTCINT_CREATED_LNKDF)
	@echo ROOTCINT_CREATED_SRC0 = $(ROOTCINT_CREATED_SRC0)
	@echo ROOTCINT_CREATED_SRC = $(ROOTCINT_CREATED_SRC)
	@echo ROOTCINT_CREATED_OBJ = $(ROOTCINT_CREATED_OBJ)
	@echo ROOTCINT_CREATED_DICH0 = $(ROOTCINT_CREATED_DICH0)
	@echo ROOTCINT_CREATED_DICH = $(ROOTCINT_CREATED_DICH)
#--------------------------------------- help commands
	@echo "TARGETS are :"
	@echo "  clean : cleans everything"
	@echo "  cleantargets: cleans libs, exes, adn created objs"
	@echo "  cleancint: cleans generated dictionnaries"
	@echo "  all : makes all"
	@echo "  lib : lib -> see OPTIONS"
	@echo "  exe : exe -> see OPTIONS"
	@echo "  staticlib : static library"
	@echo "  staticexe : static exe"
	@echo "  shlib : shared lib"
	@echo "  shexe : shared exe"
	@echo
	@echo "OPTIONS are:"
	@echo "  SHARED = 1 -> make shared lib, objs and exe"
	@echo "  ROOT = 1 -> link with ROOT libs, and use ROOT include path"
	@echo "  ROOTDIC =1 -> generate a ROOT cint dictionnary and integrate it in the library"
	@echo "     (This later forces SHARED =1 & ROOT = 1)"
	@echo "  NODEP =1 -> No dependies check (faster, riskier)"


clean: cleanlocalcint cleancint cleantargets cleanshobjs

cleantargets:
	-$(RM)  depends/$(OS_NAME)/*.d 
	-$(RM)  $(LIBRARY)
	-$(RM)  $(SHAREDLIB)
	-$(RM)  $(EXE)
	-$(RM)  $(SHAREDEXE)
	-$(RM)  $(CREATED_OBJS_F)
	-$(RM)  $(CREATED_OBJS_C)	
	-$(RM)  $(CREATED_OBJS_CC)
	-$(RM)  $(SH_CREATED_OBJS_F)
	-$(RM)  $(SH_CREATED_OBJS_C)
	-$(RM)  $(SH_CREATED_OBJS_CC)

cleanshobjs:
	-$(RM) $(tempobj)/$(basename $(MAIN_DIRECTORY))_*
#	-$(RM) $(SLIB_F_OBJS)
#	-$(RM) $(SLIB_C_OBJS)
#	-$(RM) $(SLIB_CC_OBJS)

cleanlocalcint:
	-$(RM) $(ROOTCINT_SRC0)
	-$(RM) $(ROOTCINT_DICH0)
	-$(RM) $(ROOTCINT_CREATED_SRC0)
	-$(RM) $(ROOTCINT_CREATED_DICH0)

cleancint:
	-$(RM)  $(ROOTCINT_SRC)
	-$(RM)  $(ROOTCINT_DICH)
	-$(RM)  $(ROOTCINT_CREATED_SRC)
	-$(RM)  $(ROOTCINT_CREATED_DICH)

mkall: mklib mkbin mkslib

mklib:
	@test -d $(locallib) || mkdir -p $(locallib) ;
	@test -d $(tempobj) || mkdir -p $(tempobj) ;
ifneq ($(strip $(LIBRARY)),)
	@test -f $(LIBRARY)  || $(AR) r $(LIBRARY) ;
endif

mkslib:
	@test -d $(localincd) || mkdir -p $(localincd) ;
	@test -d $(tempsrc) || mkdir -p $(tempsrc) ;
	@test -d $(localslib) || mkdir -p $(localslib) ;
	@test -d $(tempobj) || mkdir -p $(tempobj)

mkbin:
	@test -d $(localbin) || mkdir -p $(localbin)

### kept ("created") static objects---------------

$(CREATED_OBJS_F): $(locallib)/%$(ADD_DEBUG).o: %.F
			@test -d $(dir $@) || mkdir -p $(dir $@)
			$(FC) $(FFLAGS) $(CPPFLAGS) \
			$(addprefix $(INC_FLAG),$(INCLUDES)) -c \
			$*.F -o $@ 

$(CREATED_OBJS_C): $(locallib)/%$(ADD_DEBUG).o: %.c
			@test -d $(dir $@) || mkdir -p $(dir $@)
			$(CC) $(CFLAGS) $(CPPFLAGS)\
                        $(INCLUDES) -c \
			$*.c -o $@ 

$(CREATED_OBJS_CC): $(locallib)/%$(ADD_DEBUG).o: %.$(CppSuf)
			@test -d $(dir $@) || mkdir -p $(dir $@)
			$(CXX) $(CXXFLAGS) $(CPPFLAGS) \
                        $(INCLUDES) -c \
			$*.$(CppSuf) -o $@

### kept ("created") shared objects---------------

$(SH_CREATED_OBJS_F): $(localslib)/$(basename $(MAIN_DIRECTORY))_%$(ADD_DEBUG).o: %.F
			@test -d $(dir $@) || mkdir -p $(dir $@)
			$(FC)  $(FFLAGS) $(DLLFLAGS) $(CPPFLAGS) \
			$(addprefix $(INC_FLAG),$(INCLUDES)) -c \
			$*.F -o $(localslib)/%$(ADD_DEBUG).o;

$(SH_CREATED_OBJS_C): $(localslib)/$(basename $(MAIN_DIRECTORY))_%$(ADD_DEBUG).o: %.c
			@test -d $(dir $@) || mkdir -p $(dir $@)
			$(CC) $(CFLAGS) $(DLLFLAGS) $(CPPFLAGS)\
                        $(INCLUDES) -c \
			$*.c -o $@

$(SH_CREATED_OBJS_CC): $(localslib)/$(basename $(MAIN_DIRECTORY))_%$(ADD_DEBUG).o: %.$(CppSuf)
			@test -d $(dir $@) || mkdir -p $(dir $@)
			$(CXX) $(CXXFLAGS) $(DLLFLAGS) $(CPPFLAGS) \
                        $(INCLUDES) -c \
			$*.$(CppSuf) -o $@

### temporary shared objects ----------------

$(SLIB_F_OBJS): $(tempobj)/$(basename $(MAIN_DIRECTORY))_%_sh.o: %.F
			@test -d $(dir $@) || mkdir -p $(dir $@)
			$(FC) $(FFLAGS) $(DLLFLAGS) $(CPPFLAGS) \
			$(addprefix $(INC_FLAG),$(INCLUDES)) -c \
			$*.F -o $@

$(SLIB_C_OBJS): $(tempobj)/$(basename $(MAIN_DIRECTORY))_%_sh.o: %.c
			@test -d $(dir $@) || mkdir -p $(dir $@)
			$(CC) $(CFLAGS) $(DLLFLAGS) $(CPPFLAGS) $(INCLUDES) -c \
			$*.c -o $@;

$(SLIB_CC_OBJS): $(tempobj)/$(basename $(MAIN_DIRECTORY))_%_sh.o: %.$(CppSuf)
			@test -d $(dir $@) || mkdir -p $(dir $@)
			$(CXX) $(CXXFLAGS) $(DLLFLAGS) $(CPPFLAGS) $(INCLUDES) -c \
			$*.$(CppSuf) -o $@

#### ROOT CINT Dictionnary source & object:------------------------------------------
# root dictionnary is mainly used as loaded shared library -- ROOTCINT_OBJ are shared objects
# although stricktly speaking you could compile an all static executable that would launch roocint
# interactively and therfore would use the dictionnary. 

$(ROOTCINT_OBJ): $(ROOTCINT_SRC) $(ROOTCINT_DICH)
			$(CXX) $(CXXFLAGS) $(DLLFLAGS) $(CPPFLAGS) -I$(localincd)/ $(INCLUDES) -c \
			$(ROOTCINT_SRC) -o $(ROOTCINT_OBJ)

$(ROOTCINT_SRC) $(ROOTCINT_DICH): $(ROOTCINT_HEADERS) $(ROOTCINT_LNKDF)
			@echo "Generating library dictionnary"
			-rootcint -f $(ROOTCINT_SRC0) -c $(ROOTCINT_INCLUDES) \
			-D__cplusplus $(ROOTCINT_OPTIONS) $(ROOTCINT_HEADERS) $(ROOTCINT_LNKDF) || \
			$(RM) $(ROOTCINT_SRC0) $(ROOTCINT_DICH0); 
			mv $(ROOTCINT_SRC0) $(ROOTCINT_SRC)
			mv $(ROOTCINT_DICH0) $(ROOTCINT_DICH)

$(ROOTCINT_CREATED_OBJ): $(ROOTCINT_CREATED_SRC) $(ROOTCINT_CREATED_DICH)
			$(CXX) $(CXXFLAGS) $(DLLFLAGS) $(CPPFLAGS) -I$(localincd)/ $(INCLUDES) -c \
			$(ROOTCINT_CREATED_SRC) -o $(ROOTCINT_CREATED_OBJ)

$(ROOTCINT_CREATED_SRC) $(ROOTCINT_CREATED_DICH): $(ROOTCINT_CREATED_HEADERS) $(ROOTCINT_CREATED_LNKDF)
			@echo "Generating executable dictionnary"
			-rootcint -f $(ROOTCINT_CREATED_SRC0) -c $(ROOTCINT_INCLUDES) \
			-D__cplusplus $(ROOTCINT_CREATED_HEADERS) $(ROOTCINT_CREATED_LNKDF) || \
			$(RM) $(ROOTCINT_CREATED_SRC0) $(ROOTCINT_CREATED_DICH0);
			mv $(ROOTCINT_CREATED_SRC0) $(ROOTCINT_CREATED_SRC);
			mv $(ROOTCINT_CREATED_DICH0) $(ROOTCINT_CREATED_DICH)

#### final targets---------------------------

$(LIBRARY): $(OBJS) $(LIB)
	$(AR) ts $(LIBRARY)

$(SHAREDLIB): $(SLIB_F_OBJS) $(SLIB_C_OBJS) $(SLIB_CC_OBJS) $(ROOTCINT_OBJ)
	$(SLLD)  $(UNDEF)  $(SOFLAGS) \
	$(SLIB_F_OBJS) $(SLIB_C_OBJS) $(SLIB_CC_OBJS) $(ROOTCINT_OBJ)\
	-o $(SHAREDLIB)

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
	$(REQUIRED_OBJS) $(LIBS_DIR) $(STATICLINK) \
	$(CLAS_SHAREDLIBS) $(CLAS_LIBS) \
	$(LIBS) $(SHAREDLIBS) $(MD_LIBS) \
	-o $(EXE)

# "RECSIS" & others static libs are in LIBS
# MD_LIBS
# shared libs are in SHARED_LIBDS

#$(SHAREDEXE): $(LIBS) $(REQUIRED_OBJS) $(SHAREDLIB) $(ROOTCINT_CREATED_OBJ)
#	$(LNKCMD) $(UNDEF) \

$(SHAREDEXE): $(LIBS) $(REQUIRED_OBJS) $(SHAREDLIB) $(ROOTCINT_CREATED_OBJ)
	$(SLLD) $(UNDEF) \
	$(CXXFLAGS) \
	$(REQUIRED_OBJS) $(ROOTCINT_CREATED_OBJ) \
	$(LIBS_DIR) $(SHAREDLINK) $(CLAS_SHAREDLIBS) $(CLAS_LIBS) \
	$(LIBS) $(SHAREDLIBS) $(MD_LIBS)\
	-o $(SHAREDEXE)
	ldd $(SHAREDEXE)

#------------------------------------------------------------
# Pattern rules
#------------------------------------------------------------
#  automatic generation of depends files  SOURCE.d  loated in 
#  localdep/*.d. 
#
#  include all the local generated dependency files, these are created and
#  updated automatically for you.

# libraries dependencies:

$(localdep)/lib$(MAIN_DIRECTORY).%.d: %.F
	@test -d $(localdep) || mkdir -p $(localdep) ;
	$(SHELL) -c '$(CPP) $(CPPFLAGS) \
	$(INCLUDES) \
	$< \
	| sed \
        -e  '\"s/$*\\.F\\.o:/$(LIBSED)\($*.o\)\ $(LIBRSED)\($*.o\)\ $(TMPOBJSED)$(MAIN_DIRECTORY)_$*_sh.o:/g\"' \
        -e  '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' > $@'; \
        $(SHELL) -c '$(CPP) $(CPPFLAGS) \
        $(INCLUDES) \
        $< \
        | sed \
        -e  '\"s/$*\\.F\\.o:/$(LIBDEBSED)\($*.o\)\ $(LIBRDEBSED)\($*.o\)\ $(TMPOBJSED)$(MAIN_DIRECTORY)_$*_sh_debug.o:/g\"' \
        -e  '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' >> $@'

$(localdep)/lib$(MAIN_DIRECTORY).%.d: %.c
	@test -d $(localdep) || mkdir -p $(localdep) ;
	$(SHELL) -c '$(CPP) $(CPPFLAGS) \
	$(INCLUDES) \
	$< \
	| sed \
	-e '\"s/$*\\.o:/$(LIBSED)\($*.o\)\ $(LIBRSED)\($*.o\)\ $(TMPOBJSED)$(MAIN_DIRECTORY)_$*_sh.o:/g\"' \
	-e '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' > $@'; \
	$(SHELL) -c '$(CPP) $(CPPFLAGS) \
	$(INCLUDES) \
	$< \
	| sed \
	-e '\"s/$*\\.o:/$(LIBDEBSED)\($*.o\)\ $(LIBRDEBSED)\($*.o\)\ $(TMPOBJSED)$(MAIN_DIRECTORY)_$*_sh_debug.o:/g\"' \
	-e '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' >> $@'

$(localdep)/lib$(MAIN_DIRECTORY).%.d: %.$(CppSuf)
	@test -d $(localdep) || mkdir -p $(localdep) ;
	$(SHELL) -c '$(CPP_CXX) $(CPPFLAGS) \
	$(INCLUDES) \
	$< \
	| sed \
	-e '\"s/$*\\.o:/$(LIBSED)\($*.o\)\ $(LIBRSED)\($*.o\)\ $(TMPOBJSED)$(MAIN_DIRECTORY)_$*_sh.o:/g\"' \
	-e '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' > $@'; \
	$(SHELL) -c '$(CPP_CXX) $(CPPFLAGS) \
	$(INCLUDES) \
	$< \
	| sed \
	-e '\"s/$*\\.o:/$(LIBDEBSED)\($*.o\)\ $(LIBRDEBSED)\($*.o\)\ $(TMPOBJSED)$(MAIN_DIRECTORY)_$*_sh_debug.o:/g\"' \
	-e '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' >> $@'

#created objects dependencies:

$(localdep)/$(MAIN_DIRECTORY)_%.d: %.F
	@test -d $(localdep) || mkdir -p $(localdep) ;
	$(SHELL) -c '$(CPP) $(CPPFLAGS) \
	$(INCLUDES) \
	$< \
	| sed \
        -e  '\"s/$*\\.F\\.o:/$(LIBPSED)$*.o\ $(SLIBPSED)$(MAIN_DIRECTORY)_$*.o:/g\"' \
        -e  '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' > $@'; \
        $(SHELL) -c '$(CPP) $(CPPFLAGS) \
        $(INCLUDES) \
        $< \
        | sed \
        -e  '\"s/$*\\.F\\.o:/$(LIBPSED)$*_debug.o\ $(SLIBPSED)$(MAIN_DIRECTORY)_$*_debug.o:/g\"' \
        -e  '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' >> $@'

$(localdep)/$(MAIN_DIRECTORY)_%.d: %.c
	@test -d $(localdep) || mkdir -p $(localdep) ;
	$(SHELL) -c '$(CPP) $(CPPFLAGS) \
	$(INCLUDES) \
	$< \
	| sed \
        -e  '\"s/$*\\.o:/$(LIBPSED)$*.o\ $(SLIBPSED)$(MAIN_DIRECTORY)_$*.o:/g\"' \
	-e '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' > $@'; \
	$(SHELL) -c '$(CPP) $(CPPFLAGS) \
	$(INCLUDES) \
	$< \
	| sed \
        -e  '\"s/$*\\.o:/$(LIBPSED)$*_debug.o\ $(SLIBPSED)$(MAIN_DIRECTORY)_$*_debug.o:/g\"' \
	-e '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' >> $@'

$(localdep)/$(MAIN_DIRECTORY)_%.d: %.$(CppSuf)
	@test -d $(localdep) || mkdir -p $(localdep) ;
	$(SHELL) -c '$(CPP_CXX) $(CPPFLAGS) \
	$(INCLUDES) \
	$< \
	| sed \
        -e  '\"s/$*\\.o:/$(LIBPSED)$*.o\ $(SLIBPSED)$(MAIN_DIRECTORY)_$*.o:/g\"' \
	-e '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' > $@'; \
	$(SHELL) -c '$(CPP_CXX) $(CPPFLAGS) \
	$(INCLUDES) \
	$< \
	| sed \
        -e  '\"s/$*\\.o:/$(LIBPSED)$*_debug.o\ $(SLIBPSED)$(MAIN_DIRECTORY)_$*_debug.o:/g\"' \
	-e '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' >> $@'

#CINT dependencies:

$(localdep)/$(MAIN_DIRECTORY)_Cint.d: $(ROOTCINT_HEADERS)
	@test -d $(localdep) || mkdir -p $(localdep);
	ls $(ROOTCINT_HEADERS)  \
	| sed -e 's/^/#include\ "/' -e 's/$$/"/' \
	> $(localdep)/$(TMPCINTCC).$(CppSuf)
	$(SHELL) -c '$(CPP_CXX) $(CPPFLAGS) \
	$(INCLUDES) $(localdep)/$(TMPCINTCC).$(CppSuf) \
	| sed \
	-e '\"s/$(TMPCINTCC).o:/$(CINTSRCSED)\ $(CINTDICHSED):/\"' \
	-e  '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' \
	-e '\"s/\ $(TMPCINTSED)/\ /\"' \
	> $(localdep)/$(MAIN_DIRECTORY)_Cint.d'
#	$(RM) $(localdep)/$(TMPCINT).$(CppSuf)

$(localdep)/$(MAIN_DIRECTORY)_CreatedCint.d: $(ROOTCINT_CREATED_HEADERS)
	@test -d $(localdep) || mkdir -p $(localdep);
	ls $(ROOTCINT_CREATED_HEADERS) \
	| sed -e 's/^/#include\ "/' -e 's/$$/"/' \
	> $(localdep)/$(TMPCREATEDCINTCC).$(CppSuf)
	$(SHELL) -c '$(CPP_CXX) $(CPPFLAGS) \
	$(INCLUDES) $(localdep)/$(TMPCREATEDCINTCC).$(CppSuf) \
	| sed \
	-e '\"s/$(TMPCREATEDCINTCC).o:/$(CREATEDCINTSRCSED)\ $(CREATEDCINTDICHSED):/\"' \
	-e  '\"s/:/\ $(DEPSED)\\/$(@F):/g\"' \
	-e '\"s/\ $(TMPCREATEDCINTSED)/\ /\"' \
	> $(localdep)/$(MAIN_DIRECTORY)_CreatedCint.d'
#	$(RM) $(localdep)/$(TMPCREATEDCINTCC).$(CppSuf)

$(LIBRARY)(%.o): %.F 
	$(FC)  $(FFLAGS) \
	$(addprefix $(INC_FLAG),$(CPPFLAGS)) \
	$(addprefix $(INC_FLAG),$(INCLUDES)) -c \
	$<  -o $(tempobj)/$%
	$(AR) r $@ $(tempobj)/$%
	@$(RM)  $(tempobj)/$% 

$(LIBRARY)(%.o): %.c  
	$(CC) $(CFLAGS)  $(CPPFLAGS) \
		$(INCLUDES) -c \
		$< -o $(tempobj)/$%
	$(AR) r $@ $(tempobj)/$%
	@$(RM) $(tempobj)/$%

$(LIBRARY)(%.o): %.$(CppSuf)  
	$(CXX) $(CXXFLAGS)  $(CPPFLAGS) \
		$(INCLUDES) -c \
		$< -o $(tempobj)/$%
	$(AR) r $@ $(tempobj)/$%
	@$(RM)  $(tempobj)/$%

%.o: %.F
	$(FC)  $(FFLAGS) \
	$(addprefix $(INC_FLAG), $(CPPFLAGS)) \
	$(addprefix $(INC_FLAG), $(INCLUDES)) -c $<

%.o: %.c
	$(CC) $(CFLAGS)  $(CPPFLAGS) $(INCLUDES) -c $<

%.o: %.$(CppSuf)
	$(CXX) $(CXXFLAGS)  $(CPPFLAGS) $(INCLUDES) -c $<

#-------------------------------------------------
#  Dependencies for each source file 
#-------------------------------------------------


ifndef NODEP

# libraries:
ifneq ($(strip $(DEPS)),)
    include $(DEPS)
endif

# created objects:
ifneq ($(strip $(CREATEDDEPS)),)
   include $(CREATEDDEPS)
endif

# cint
ifneq ($(strip $(CINTDEPS)),)
   include $(CINTDEPS)
endif

ifneq ($(strip $(CREATEDCINTDEPS)),)
   include $(CREATEDCINTDEPS)
endif

endif
