#
# Rules for Makefiles
#


#
# List of supported targets-
#
# SunOS		The Most tested
# HP-UX 	Working on it
# AIX	        AIX what else
# Linux		linux with gcc
# IRIX          sgi (working on it)
# SunOS4        SunOS 4.1.x run at CMU
#

# Set up the environment

ifndef OS_NAME
  export OS_NAME := $(shell $(CLAS_CMS)/uname_clas)
endif

ifndef TOP_DIR
  export TOP_DIR = /home/$(USER)
endif

PWD := $(shell pwd)


XINCLUDE_SunOS = -I/usr/openwin/include -I/usr/dt/include
XINCLUDE_HP-UX = -I/usr/include/Xm -I/usr/include/X11
XINCLUDE_AIX		:= $(XINCLUDE_HP-UX)
XINCLUDE_Linux		:= $(XINCLUDE_HP-UX)
XINCLUDE_LinuxRH4	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRH5	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRH6	:= $(XINCLUDE_Linux)
XINCLUDE_IRIX		:= $(XINCLUDE_HP-UX)
XINCLUDE_SunOS4		:= $(XINCLUDE_SunOS)
XINCLUDE:= $(XINCLUDE_$(OS_NAME))

#
# INCLUDE defines the location of all general purpose include files
#

INCLUDE_SunOS =	-I. -I../include -I$(CLAS_PACK)/include 
INCLUDE_HP-UX		:= $(INCLUDE_SunOS)
INCLUDE_AIX		:= $(INCLUDE_SunOS)
INCLUDE_Linux		:= $(INCLUDE_SunOS)
INCLUDE_LinuxRH4	:= $(INCLUDE_Linux)
INCLUDE_LinuxRH5	:= $(INCLUDE_Linux)
INCLUDE_LinuxRH6	:= $(INCLUDE_Linux)
INCLUDE_IRIX		:= $(INCLUDE_SunOS)
INCLUDE_SunOS4		:= $(INCLUDE_SunOS)

INCLUDE:= $(INCLUDE_$(OS_NAME))
 
ifndef INCLUDE
INCLUDE:=$(INCLUDE_SunOS)
endif


#
# XLIBS defines the location of the X11 libraries
#
XLIBS_SunOS = -lFm_c /usr/dt/lib/libXm.so libXt.so libX11.so libw.so
XLIBS_AIX = -lFm_c -lXm -lXt -lX11 -lPW
XLIBS_Linux = -lFm_c libXm.so.1 libXt.so libX11.so libICE.so libSM.so libXintl.so.6 libXext.so libXpm.so
XLIBS_LinuxRH4 = $(XLIBS_Linux)
XLIBS_LinuxRH5 = $(XLIBS_Linux)
XLIBS_LinuxRH6 = $(XLIBS_Linux)
XLIBS_IRIX = -lFm_c libXm.so libXt.so libXext.so libX11.so libPW.so
XLIBS_SunOS4 = -lXm -lXt -lX11
XLIBS:= $(XLIBS_$(OS_NAME))





#
# FINCLUDE defines the location of F77 include files
#    *and* any special f77 switches 
#

FINCLUDE_SunOS = -I. -I../include -I$(CLAS_PACK)/include 
FINCLUDE_HP-UX		:= $(INCLUDE_SunOS)
FINCLUDE_AIX		:= -WF,-I. -WF,-I../include -WF,-I$(CLAS_PACK)/include

FINCLUDE_Linux		:= $(INCLUDE_SunOS)
FINCLUDE_LinuxRH4	:= $(INCLUDE_Linux)
FINCLUDE_SunOS4		:= $(INCLUDE_SunOS)

FINCLUDE:= $(FINCLUDE_$(OS_NAME))

ifndef FINCLUDE
FINCLUDE:=$(FINCLUDE_SunOS)
endif

#
# Here: COPT- defines the flags used to build the %.oo objects (both C and f77)
#       CDBG- defines the flags used to build the %.o objects,
#			  note that for the optimised targets (such as irix5o)
#			  CDBG is explicitely set equal to COPT
#

ifdef DEBUG
ADD_DEBUG= _debug
endif

ifdef DBG
CDBG_default=		-g
CDBG_HP-UX=	-g
CDBG_Linux = -g
CDBG_LinuxRH4 := $(CDBG_Linux)
CDBG_IRIX = -g
ADD_DEBUG = _debug

CDBG:=$(CDBG_$(OS_NAME))

ifndef CDBG
CDBG:=$(CDBG_default)
endif
endif

#
# Various target-dependant C compiler flags (such as -n32, -mips2, etc...)
#


CFLAGS_SunOS:= -DSunOS         
CFLAGS_HP-UX:= -Ae -DHPUX
CFLAGS_IRIX:= -DIRIX
CFLAGS_AIX:= -DAIX		
CFLAGS_SunOS4:= -DSunOS
CFLAGS_Linux:= -DLinux
CFLAGS_LinuxRH4:= -DLinux
CFLAGS_LinuxRH5:= -DLinux
CFLAGS_LinuxRH6:= -DLinux


CFLAGS:= $(CFLAGS_$(OS_NAME))

#
# C++ stuff
#

CCFLAGS:= $(CCFLAGS_$(OS_NAME))

#
# Location of the Fortran run-time libraries
#

FLIB_SunOS=             -R/opt/SUNWspro/lib -L/opt/SUNWspro/lib -lV77 -lF77 -lM77 -L/opt/SUNWspro/SC4.0/lib -lsunmath
FLIB_IRIX= -L/usr/lib -lF77 -lI77 -lU77 -lisam
FLIB_IRIX64= -L/usr/lib -lF77 -lI77 -lU77 -lisam
FLIB_SunOS4= 
FLIB_Linux=  /usr/lib/libf2c.a  

ifeq "$(OS_NAME)" "Linux"
# trying to get the libf2c or libg2c library automatically.

  EGCS := $(shell g++ --version | awk -F- '{if($$1=="egcs")printf("egcs")}')
  FLIB_Linux := -l$(shell $(CLAS_CMS)/g77-fortran-lib)
  FFLAGS_Linux := -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME)

#  USE_EGCS  = $(shell g77 --version | awk -F- '{if($$1=="egcs")printf("egcs")}')
#  ifeq "$(USE_EGCS)" "egcs"
#     FLIB_Linux = $(shell ls /usr/lib/gcc-lib/i???-*-linux/egcs-*/libf2c.a)
#  else
#     FLIB_Linux=  /usr/lib/libf2c.a  
#  endif


endif

ifeq "$(OS_NAME)" "LinuxRH5"
# trying to get the libf2c or libg2c library automatically.

  EGCS := $(shell g++ --version | awk -F- '{if($$1=="egcs")printf("egcs")}')
  FLIB_LinuxRH5 := -l$(shell $(CLAS_CMS)/g77-fortran-lib)
  FFLAGS_LinuxRH5 := -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME)

#  USE_EGCS  = $(shell g77 --version | awk -F- '{if($$1=="egcs")printf("egcs")}')
#  ifeq "$(USE_EGCS)" "egcs"
#     FLIB_Linux = $(shell ls /usr/lib/gcc-lib/i???-*-linux/egcs-*/libf2c.a)
#  else
#     FLIB_Linux=  /usr/lib/libf2c.a  
#  endif


endif

ifeq "$(OS_NAME)" "LinuxRH6"
# trying to get the libf2c or libg2c library automatically.

  EGCS := $(shell g++ --version | awk -F- '{if($$1=="egcs")printf("egcs")}')
  FLIB_LinuxRH6 := -l$(shell $(CLAS_CMS)/g77-fortran-lib)
  FFLAGS_LinuxRH6 := -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME)

#  USE_EGCS  = $(shell g77 --version | awk -F- '{if($$1=="egcs")printf("egcs")}')
#  ifeq "$(USE_EGCS)" "egcs"
#     FLIB_Linux = $(shell ls /usr/lib/gcc-lib/i???-*-linux/egcs-*/libf2c.a)
#  else
#     FLIB_Linux=  /usr/lib/libf2c.a  
#  endif


endif

ifeq "$(OS_NAME)" "LinuxRH4"
# trying to get the libf2c or libg2c library automatically.

  EGCS := $(shell g++ --version | awk -F- '{if($$1=="egcs")printf("egcs")}')
  FLIB_LinuxRH4 := -l$(shell $(CLAS_CMS)/g77-fortran-lib)
  FFLAGS_LinuxRH4 := -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME)

#  USE_EGCS  = $(shell g77 --version | awk -F- '{if($$1=="egcs")printf("egcs")}')
#  ifeq "$(USE_EGCS)" "egcs"
#     FLIB_Linux = $(shell ls /usr/lib/gcc-lib/i???-*-linux/egcs-*/libf2c.a)
#  else
#     FLIB_Linux=  /usr/lib/libf2c.a  
#  endif


endif




FLIB:= $(FLIB_$(OS_NAME))

#
# Location of the CERN library is given by the CERNLIBDIR
# environment variable. If it is not set, the default
# is in $CLAS_PACK/lib.$OSNAME
#

CERNLIB=		-lpacklib -lkernlib -lmathlib

CERN_SunOS = 		-L/site/cernlib/sun4_solaris2/97a/lib
CERN_SunOS4 =           -L/usr/local/lib

CERN := $(CERN_$(SunOS))

ifndef CERNLIBDIR
CERNLIBDIR:=	$(CLAS_LIB)
endif

#
# Additional C libraries
#

CLIB_irix4 =		-lsun -lm -lc_s
CLIB_irix5 =
CLIB_irix6o =		-lfastm
CLIB_AIX =		-lm
CLIB_AIXo =		-lm
CLIB_AIXppc601o =	-lm
CLIB_AIXp2o =	-lm

CLIB:= $(CLIB_$(OS_NAME))

#
# ACPP: the C preprocessor to create dependacies list
#

ACPP_default:=		gcc -M
ACPP_HP-UX:=            gcc -M
ACPP_SunOS:=            gcc -M
ACPP_Linux:=		gcc -M
ACPP_LinuxRH4:=		gcc -M
ACPP_IRIX:=             cc -M
ACPP_SunOS4:=            gcc -M
ACPP_OSF1:=             gcc -M

ACPP:=$(ACPP_$(OS_NAME))

ifndef ACPP
ACPP=$(ACPP_default)
endif

CC_default:=		cc
CC_Linux:=		gcc
CC_LinuxRH4:=		gcc
CC_SunOS:=		cc
CC_HP-UX:= 	        cc
CC_IRIX:=               cc
CC_OSF1:=               gcc
CC_AIX:=		cc
CC_SunOS4:=		gcc

CC:=$(CC_$(OS_NAME))

ifeq "$(EGCS)" "egcs"
CC:= egcs
endif

ifndef CC
CC=$(CC_default)
endif

FC_default:=		f77 -e
FC_Linux:=		g77
FC_LinuxRH4:=		g77
FC_linuxo:=		g77
FC_IRIX:=               f77 
FC_OSF1:=               f77
FC_AIX:=                f77 -qfixed=132 -qextname

FC:=$(FC_$(OS_NAME))

ifndef FC
FC=$(FC_default)
endif

#
# LIBDIR: all the library files are created/updated there
#

ifndef LIBDIR
LIBDIR=$(CLAS_LIB)
endif

#
# VPATH: library search path
#

VPATH_default=		$(CLAS_LIB)

VPATH:= $(VPATH_$(OS_NAME))

ifndef VPATH
VPATH:=$(VPATH_default)
endif

# add LIBDIR and the CERN library path

#
# AR: the 'ar' command- to set group-writable permissions
#			on newly created libraries
#

ifeq ($(OS_NAME),SunOS4)
 AR= umask 2; /usr/5bin/ar
else
 AR= umask 2; ar
endif

.PHONY : all

help: 
	@echo "D.P. Weygand make scheme:"
	@echo " by default optimized code is produced"
	@echo " debug flag \"DBG = 1\" on command line"
	@echo " example: make target \"DBG=1\" "

#
# These are the main rules to build object files-
#
# %.o --- build "normal" object files (usually with -g)
# %.oo -- build "optimised" object files (always with -O)
#
#

%.oo: %.c
	$(ACPP) $< $(INCLUDE) > $*.oo.depend
	$(CC) -c $< $(COPT) $(CFLAGS) $(INCLUDE)
	mv $*.o $*.oo

%.oo: %.f
	$(FC) -c $< $(COPT) $(FINCLUDE)
	mv $*.o $*.oo


%.o: %.f
	$(FC) -c $< $(CDBG) $(FINCLUDE)

%.o: %.F
	$(FC) -c $< $(CDBG) $(FINCLUDE)

%.o: %.c
	echo Making $<
	$(ACPP) $< $(INCLUDE) $(XINCLUDE) > $@.depend
	$(CC) -c $< $(CDBG) $(CFLAGS) $(INCLUDE) $(XINCLUDE)

#
# Make everything precious- to avoid removal of source files after targets are built.
#

.PRECIOUS: %


debug:
	@echo CLAS_PACK\\t $(CLAS_PACK)	
	@echo CLAS_LIB\\t $(CLAS_LIB)
	@echo CERN_LIB\\t $(CERN_LIB)
	@echo XLIBS\\t\\t $(XLIBS)
	@echo LIBDIR\\t\\t $(LIBDIR)
	@echo OS_NAME\\t\\t $(OS_NAME)
	@echo VPATH\\t\\t $(VPATH)
	@echo CC\\t\\t $(CC)
	@echo CFLAGS\\t\\t $(CFLAGS)
	@echo CCFLAGS\\t\\t $(CCFLAGS)
	@echo INCLUDE\\t\\t $(INCLUDE)	
	@echo XINCLUDE\\t\\t $(XINCLUDE)
	@echo FINCLUDE\\t\\t $(FINCLUDE)
	@echo FC\\t\\t $(FC)
	@echo CDBG\\t\\t $(CDBG)


clean:
	rm -f *.o *.oo *.depend
	echo >dummy.depend


#
# end file
#







