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
# LinuxRH4	linux with gcc
# LinuxRH6	linux with egcs/gcc
# IRIX          sgi (working on it)
# SunOS4        SunOS 4.1.x run at CMU
#
# tests Linux for egcs g++ compiler: if true, uses egcs instead of gcc
#        does test of version of g77 (or egcs) to determine whether to use
#        libg2c.a or libf2c.a

# Set up the environment

ifdef USEGNU
ADD_GNU=/gnu
endif


VDIR= $(CLAS_PACK)/scripts/vdirs

ifndef OS_NAME
  export OS_NAME := $(shell $(CLAS_CMS)/uname_clas)
endif

ifndef TOP_DIR
  export TOP_DIR = /home/$(USER)
endif

ifndef LOCAL_INC
	export LOCAL_INC = $(TOP_DIR)/packages/inc_derived
endif

ifndef LOCAL_LIB
	export LOCAL_LIB = $(TOP_DIR)/lib/$(OS_NAME)$(ADD_GNU)
endif

ifndef LOCAL_BIN
	export LOCAL_BIN = $(TOP_DIR)/bin/$(OS_NAME)$(ADD_GNU)
endif

PWD := $(shell pwd)

OBJDIR= $(OS_NAME)$(ADD_DEBUG)$(ADD_GNU)


XINCLUDE_SunOS = -I/usr/openwin/include -I/usr/dt/include
XINCLUDE_HP-UX = -I/usr/include/Xm -I/usr/include/X11
XINCLUDE_AIX		:= $(XINCLUDE_HP-UX)
XINCLUDE_Darwin		= -I/sw/include -I/usr/include/X11 -I/usr/X11R6/include
XINCLUDE_Linux		= -I/usr/include/Xm -I/usr/include/X11 -I/usr/X11R6/include
XINCLUDE_LinuxRH5	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRH6	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRH7	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRH8	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRH9	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRHEL3	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRHEL4	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRHEL5	:= $(XINCLUDE_Linux)
XINCLUDE_Linux64RHEL3	:= $(XINCLUDE_Linux)
XINCLUDE_Linux64RHEL4	:= $(XINCLUDE_Linux)
XINCLUDE_Linux64RHEL5	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRHFC2	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRHFC3	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRHFC4	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRHFC7	:= $(XINCLUDE_Linux)
XINCLUDE_LinuxRHFC8	:= $(XINCLUDE_Linux)
XINCLUDE_Linux64RHFC2	:= $(XINCLUDE_Linux)
XINCLUDE_Linux64RHFC3	:= $(XINCLUDE_Linux)
XINCLUDE_Linux64RHFC4	:= $(XINCLUDE_Linux)
XINCLUDE_Linux64RHFC8	:= $(XINCLUDE_Linux)
XINCLUDE_IRIX		:= $(XINCLUDE_HP-UX)
XINCLUDE_SunOS4		:= $(XINCLUDE_SunOS)
XINCLUDE:= $(XINCLUDE_$(OS_NAME))


#
# LOCAL_INCLUDE
#
LOCAL_INCLUDE = -I. -I$(TOP_DIR)/packages/inc_derived -I$(TOP_DIR)/packages/include



#
# INCLUDE defines the location of all general purpose include files
#

INCLUDE_SunOS =	-I../include -I$(CLAS_PACK)/include -I$(CLAS_PACK)/inc_derived
INCLUDE_HP-UX		:= $(INCLUDE_SunOS)
INCLUDE_AIX		:= $(INCLUDE_SunOS) -I/usr/local/include
INCLUDE_Darwin		:= $(INCLUDE_SunOS)
INCLUDE_Linux		:= $(INCLUDE_SunOS)
INCLUDE_LinuxRH4	:= $(INCLUDE_Linux)
INCLUDE_LinuxRH5	:= $(INCLUDE_Linux)
INCLUDE_LinuxRH6	:= $(INCLUDE_Linux)
INCLUDE_LinuxRH7	:= $(INCLUDE_Linux)
INCLUDE_LinuxRH8	:= $(INCLUDE_Linux)
INCLUDE_LinuxRH9	:= $(INCLUDE_Linux)
INCLUDE_LinuxRHEL3	:= $(INCLUDE_Linux)
INCLUDE_LinuxRHEL4	:= $(INCLUDE_Linux)
INCLUDE_LinuxRHEL5	:= $(INCLUDE_Linux)
INCLUDE_Linux64RHEL3	:= $(INCLUDE_Linux)
INCLUDE_Linux64RHEL4	:= $(INCLUDE_Linux)
INCLUDE_Linux64RHEL5	:= $(INCLUDE_Linux)
INCLUDE_LinuxRHFC2	:= $(INCLUDE_Linux)
INCLUDE_LinuxRHFC3	:= $(INCLUDE_Linux)
INCLUDE_LinuxRHFC4	:= $(INCLUDE_Linux)
INCLUDE_LinuxRHFC7	:= $(INCLUDE_Linux)
INCLUDE_LinuxRHFC8  	:= $(INCLUDE_Linux)
INCLUDE_Linux64RHFC2	:= $(INCLUDE_Linux)
INCLUDE_Linux64RHFC3	:= $(INCLUDE_Linux)
INCLUDE_Linux64RHFC4	:= $(INCLUDE_Linux)
INCLUDE_IRIX		:= $(INCLUDE_SunOS)
INCLUDE_SunOS4		:= $(INCLUDE_SunOS)

INCLUDE:= $(LOCAL_INCLUDE) $(INCLUDE_$(OS_NAME))
 
ifndef INCLUDE
INCLUDE:=$(INCLUDE_SunOS)
endif


#
# XLIBS defines the location of the X11 libraries
#
XLIBS_SunOS = /usr/dt/lib/libXm.so /usr/openwin/lib/libXt.so /usr/openwin/lib/libX11.so /usr/lib/libw.so
XLIBS_AIX = -lFm_c -lXm -lXt -lX11 -lPW
XLIBS_Darwin = -L/usr/X11R6/lib  /usr/X11R6/lib/libXm.so /usr/X11R6/lib/libXt.so /usr/X11R6/lib/libX11.so /usr/X11R6/lib/libICE.so /usr/X11R6/lib/libSM.so  /usr/X11R6/lib/libXext.so /usr/X11R6/lib/libXpm.so /usr/X11R6/lib/libXp.so
XLIBS_Linux = -L/usr/X11R6/lib  /usr/X11R6/lib/libXm.so /usr/X11R6/lib/libXt.so /usr/X11R6/lib/libX11.so /usr/X11R6/lib/libICE.so /usr/X11R6/lib/libSM.so  /usr/X11R6/lib/libXext.so /usr/X11R6/lib/libXpm.so /usr/X11R6/lib/libXp.so
XLIBS_LinuxRH4 = $(XLIBS_Linux)
XLIBS_LinuxRH5 = $(XLIBS_Linux)
XLIBS_LinuxRH6 = $(XLIBS_Linux)
XLIBS_LinuxRH7 = $(XLIBS_Linux)
XLIBS_LinuxRH8 = $(XLIBS_Linux)
XLIBS_LinuxRHFC8  = $(XLIBS_Linux)
XLIBS_LinuxRH9 = $(XLIBS_Linux)
XLIBS_LinuxRHEL3 = $(XLIBS_Linux)
XLIBS_LinuxRHEL4 = $(XLIBS_Linux)
XLIBS_LinuxRHEL5 = $(XLIBS_Linux)
XLIBS_LinuxRHFC2 = $(XLIBS_Linux)
XLIBS_LinuxRHFC3 = $(XLIBS_Linux)
XLIBS_LinuxRHFC4 = $(XLIBS_Linux)
XLIBS_LinuxRHFC7 = $(XLIBS_Linux)


XLIBS_Linux64RHEL3 = -L/usr/X11R6/lib64  /usr/X11R6/lib64/libXm.so /usr/X11R6/lib64/libXt.so /usr/X11R6/lib64/libX11.so /usr/X11R6/lib64/libICE.so /usr/X11R6/lib64/libSM.so  /usr/X11R6/lib64/libXext.so /usr/X11R6/lib64/libXpm.so /usr/X11R6/lib64/libXp.so
XLIBS_Linux64RHEL4 = -L/usr/X11R6/lib64  /usr/X11R6/lib64/libXm.so /usr/X11R6/lib64/libXt.so /usr/X11R6/lib64/libX11.so /usr/X11R6/lib64/libICE.so /usr/X11R6/lib64/libSM.so  /usr/X11R6/lib64/libXext.so /usr/X11R6/lib64/libXpm.so /usr/X11R6/lib64/libXp.so
XLIBS_Linux64RHEL5 = -L/usr/X11R6/lib64  /usr/X11R6/lib64/libXm.so /usr/X11R6/lib64/libXt.so /usr/X11R6/lib64/libX11.so /usr/X11R6/lib64/libICE.so /usr/X11R6/lib64/libSM.so  /usr/X11R6/lib64/libXext.so /usr/X11R6/lib64/libXpm.so /usr/X11R6/lib64/libXp.so
XLIBS_Linux64RHFC2 = -L/usr/X11R6/lib64  /usr/X11R6/lib64/libXm.so /usr/X11R6/lib64/libXt.so /usr/X11R6/lib64/libX11.so /usr/X11R6/lib64/libICE.so /usr/X11R6/lib64/libSM.so  /usr/X11R6/lib64/libXext.so /usr/X11R6/lib64/libXpm.so /usr/X11R6/lib64/libXp.so
XLIBS_Linux64RHFC3 = -L/usr/X11R6/lib64  /usr/X11R6/lib64/libXm.so /usr/X11R6/lib64/libXt.so /usr/X11R6/lib64/libX11.so /usr/X11R6/lib64/libICE.so /usr/X11R6/lib64/libSM.so  /usr/X11R6/lib64/libXext.so /usr/X11R6/lib64/libXpm.so /usr/X11R6/lib64/libXp.so
XLIBS_Linux64RHFC4 = -L/usr/X11R6/lib64  /usr/X11R6/lib64/libXm.so /usr/X11R6/lib64/libXt.so /usr/X11R6/lib64/libX11.so /usr/X11R6/lib64/libICE.so /usr/X11R6/lib64/libSM.so  /usr/X11R6/lib64/libXext.so /usr/X11R6/lib64/libXpm.so /usr/X11R6/lib64/libXp.so


XLIBS_IRIX = -lFm_c libXm.so libXt.so libXext.so libX11.so libPW.so
XLIBS_SunOS4 = -lXm -lXt -lX11
XLIBS:= $(XLIBS_$(OS_NAME))


#
# FINCLUDE defines the location of F77 include files
#    *and* any special f77 switches 
#

FINCLUDE_SunOS = -I. -I../include -I$(CLAS_PACK)/include -I$(CLAS_PACK)/inc_derived
FINCLUDE_HP-UX		:= $(INCLUDE_SunOS)
FINCLUDE_AIX		:= -WF,-I. -WF,-I../include -WF,-I$(CLAS_PACK)/include -WF,-I$(CLAS_PACK)/inc_derived
FINCLUDE_Darwin		:= $(INCLUDE_SunOS)
FINCLUDE_Linux		:= $(INCLUDE_SunOS)
FINCLUDE_LinuxRH4	:= $(INCLUDE_Linux)
FINCLUDE_LinuxRH5	:= $(INCLUDE_Linux)
FINCLUDE_LinuxRH6	:= $(INCLUDE_Linux)
FINCLUDE_LinuxRH7	:= $(INCLUDE_Linux)
FINCLUDE_LinuxRH8	:= $(INCLUDE_Linux)
FINCLUDE_LinuxRH9	:= $(INCLUDE_Linux)
FINCLUDE_LinuxRHEL3	:= $(INCLUDE_Linux)
FINCLUDE_LinuxRHEL4	:= $(INCLUDE_Linux)
FINCLUDE_LinuxRHEL5	:= $(INCLUDE_Linux)
FINCLUDE_Linux64RHEL3	:= $(INCLUDE_Linux)
FINCLUDE_Linux64RHEL4	:= $(INCLUDE_Linux)
FINCLUDE_Linux64RHEL5	:= $(INCLUDE_Linux)
FINCLUDE_LinuxRHFC2	:= $(INCLUDE_Linux)
FINCLUDE_LinuxRHFC3	:= $(INCLUDE_Linux)
FINCLUDE_LinuxRHFC4	:= $(INCLUDE_Linux)
FINCLUDE_LinuxRHFC7	:= $(INCLUDE_Linux)
FINCLUDE_LinuxRHFC8	:= $(INCLUDE_Linux)
FINCLUDE_Linux64RHFC2	:= $(INCLUDE_Linux)
FINCLUDE_Linux64RHFC3	:= $(INCLUDE_Linux)
FINCLUDE_Linux64RHFC4	:= $(INCLUDE_Linux)
FINCLUDE_SunOS4		:= $(INCLUDE_SunOS)

ifdef USEGNU
FINCLUDE:= $(FINCLUDE_Linux)
else
FINCLUDE:= $(FINCLUDE_$(OS_NAME))
endif

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
DBG=1
endif

OPFLAG_default= -O
OPFLAG_SunOS = -O
OPFLAG_HP-UX=	-O2
OPFLAG_Darwin = -O
OPFLAG_Linux = -O
OPFLAG_LinuxRH4 := $(OPFLAG_Linux)
OPFLAG_LinuxRH5 := $(OPFLAG_Linux)
OPFLAG_LinuxRH6 := $(OPFLAG_Linux)
OPFLAG_LinuxRH7 := $(OPFLAG_Linux)
OPFLAG_LinuxRH8 := $(OPFLAG_Linux)
OPFLAG_LinuxRH9 := $(OPFLAG_Linux)
OPFLAG_LinuxRHEL3 := $(OPFLAG_Linux)
OPFLAG_LinuxRHEL4 := $(OPFLAG_Linux)
OPFLAG_LinuxRHEL5 := $(OPFLAG_Linux)
OPFLAG_Linux64RHEL3 = -O2
OPFLAG_Linux64RHEL4 = -O2
OPFLAG_Linux64RHEL5 = -O2
OPFLAG_LinuxRHFC2 := $(OPFLAG_Linux)
OPFLAG_LinuxRHFC3 := $(OPFLAG_Linux)
OPFLAG_LinuxRHFC4 := $(OPFLAG_Linux)
OPFLAG_LinuxRHFC7 := $(OPFLAG_Linux)
OPFLAG_LinuxRHFC8    := $(OPFLAG_Linux)
OPFLAG_Linux64RHFC2 = -O2
OPFLAG_Linux64RHFC3 = -O2
OPFLAG_Linux64RHFC4 = -O2
OPFLAG_IRIX = -O
OPFLAG:= $(OPFLAG_$(OS_NAME))



ifdef DBG
CDBG_default=		-g
CDBG_SunOS = -g
CDBG_HP-UX=	-g
CDBG_Darwin = -g
CDBG_Linux = -g
CDBG_LinuxRH4 := $(CDBG_Linux)
CDBG_LinuxRH5 := $(CDBG_Linux)
CDBG_LinuxRH6 := $(CDBG_Linux)
CDBG_LinuxRH7 := $(CDBG_Linux)
CDBG_LinuxRH8 := $(CDBG_Linux)
CDBG_LinuxRH9 := $(CDBG_Linux)
CDBG_LinuxRHEL3 := $(CDBG_Linux)
CDBG_LinuxRHEL4 := $(CDBG_Linux)
CDBG_LinuxRHEL5 := $(CDBG_Linux)
CDBG_Linux64RHEL3 := $(CDBG_Linux)
CDBG_Linux64RHEL4 := $(CDBG_Linux)
CDBG_Linux64RHEL5 := $(CDBG_Linux)
CDBG_LinuxRHFC2 := $(CDBG_Linux)
CDBG_LinuxRHFC3 := $(CDBG_Linux)
CDBG_LinuxRHFC4 := $(CDBG_Linux)
CDBG_LinuxRHFC7 := $(CDBG_Linux)
CDBG_LinuxRHFC8    := $(CDBG_Linux)
CDBG_Linux64RHFC2 := $(CDBG_Linux)
CDBG_Linux64RHFC3 := $(CDBG_Linux)
CDBG_Linux64RHFC4 := $(CDBG_Linux)
CDBG_IRIX = -g
ADD_DEBUG = _debug

CDBG:=$(CDBG_$(OS_NAME))



else
CDBG:= $(OPFLAG)
endif


# the NSL socket library
NSL_SunOS:= -lsocket        
NSL_HP-UX:= 
NSL_IRIX:= 
NSL_AIX:= 	
NSL_SunOS4:= 
NSL_Darwin:=
NSL_Linux:=
NSL_LinuxRH4:=
NSL_LinuxRH5:=
NSL_LinuxRH6:=
NSL_LinuxRH7:=
NSL_LinuxRH8:=
NSL_LinuxRH9:=
NSL_LinuxRHEL3:=
NSL_LinuxRHEL4:=
NSL_LinuxRHEL5:=
NSL_Linux64RHEL3:=
NSL_Linux64RHEL4:=
NSL_Linux64RHEL5:=
NSL_LinuxRHFC2:=
NSL_LinuxRHFC3:=
NSL_LinuxRHFC4:=
NSL_LinuxRHFC7:=
NSL_Linux64RHFC2:=
NSL_Linux64RHFC3:=
NSL_Linux64RHFC4:=

NSL:= $(CFLAGS_$(OS_NAME))


#
# Various target-dependant C compiler flags (such as -n32, -mips2, etc...)
#


CFLAGS_SunOS:= -DSunOS         
CFLAGS_HP-UX:= -DHPUX
CFLAGS_IRIX:= -DIRIX
CFLAGS_AIX:= -DAIX		
CFLAGS_SunOS4:= -DSunOS
CFLAGS_Darwin:= -DLinux -DDarwin
CFLAGS_Linux:= -DLinux
CFLAGS_LinuxRH4:= -DLinux
CFLAGS_LinuxRH5:= -DLinux -DLinuxRH5
CFLAGS_LinuxRH6:= -DLinux -DLinuxRH6
CFLAGS_LinuxRH7:= -DLinux -DLinuxRH7
CFLAGS_LinuxRH8:= -DLinux -DLinuxRH8
CFLAGS_LinuxRH9:= -DLinux -DLinuxRH9
CFLAGS_LinuxRHEL3:= -DLinux -DLinuxRHEL3
CFLAGS_LinuxRHEL4:= -DLinux -DLinuxRHEL4
CFLAGS_LinuxRHEL5:= -DLinux -DLinuxRHEL5
CFLAGS_Linux64RHEL3:= -DLinux -DLinuxRHEL3
CFLAGS_Linux64RHEL4:= -DLinux -DLinuxRHEL4
CFLAGS_Linux64RHEL5:= -DLinux -DLinuxRHEL5
CFLAGS_LinuxRHFC2:= -DLinux -DLinuxRHFC2
CFLAGS_LinuxRHFC3:= -DLinux -DLinuxRHFC3
CFLAGS_LinuxRHFC4:= -DLinux -DLinuxRHFC4
CFLAGS_LinuxRHFC7:= -DLinux -DLinuxRHFC7
CFLAGS_LinuxRHFC8   := -DLinux -DLinuxRHFC8
CFLAGS_Linux64RHFC2:= -DLinux -DLinuxRHFC2
CFLAGS_Linux64RHFC3:= -DLinux -DLinuxRHFC3
CFLAGS_Linux64RHFC4:= -DLinux -DLinuxRHFC4

ifdef USEGNU
CFLAGS:= $(CFLAGS) -DUSEGNU
endif


CFLAGS:= $(CFLAGS_$(OS_NAME))

ifdef USEGNU
CFLAGS:= $(CFLAGS) -DUSEGNU
endif

#
# C++ stuff
#

CCFLAGS:= $(CCFLAGS_$(OS_NAME))

#
# Location of the Fortran run-time libraries
#

FLIB_SunOS=             -R/opt/SUNWspro/lib -L/opt/SUNWspro/lib -lV77 -lF77 -lM77 -L/opt/SUNWspro/SC4.2/lib -lsunmath -lnsl
FLIB_IRIX= -L/usr/lib -lF77 -lI77 -lU77 -lisam
FLIB_IRIX64= -L/usr/lib -lF77 -lI77 -lU77 -lisam
FLIB_DARWIN= -Lg2c
FLIB_Linux = -lg2c
FLIB_LinuxRHFC8 = -lg2c
ifeq ($(SGI_ABI),-n32)
FLIB_IRIX= -lftn
FLIB_IRIX64= -lftn
FLIB_HP-UX= -L/usr/lib -lcl
endif
FLIB_SunOS4=
FLIB_NATIVE:= $(FLIB_$(OS_NAME))



ifeq "$(OS_NAME)" "Linux"
  EGCS=$(shell $(CLAS_CMS)/egcs_style)
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=$(shell $(CLAS_CMS)/g77-fortran-lib)
#  FLIB_Linux=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif

ifeq "$(OS_NAME)" "LinuxRH5"
  EGCS=$(shell $(CLAS_CMS)/egcs_style)
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=$(shell $(CLAS_CMS)/g77-fortran-lib)
#  FLIB_Linux=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif

FFLAGS_Darwin:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux -DDarwin 
FFLAGS_Linux:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) 
FFLAGS_LinuxRH5:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_LinuxRH6:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_LinuxRH7:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_LinuxRH8:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_LinuxRH9:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_LinuxRHEL3:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_LinuxRHEL4:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_LinuxRHEL5:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_Linux64RHEL3:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_Linux64RHEL4:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_Linux64RHEL5:= -fno-automatic  -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_LinuxRHFC2:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_LinuxRHFC3:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_LinuxRHFC4:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_LinuxRHFC7:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_LinuxRHFC8   := -fno-automatic -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_Linux64RHFC2:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_Linux64RHFC3:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux
FFLAGS_Linux64RHFC4:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux

ifeq "$(OS_NAME)" "LinuxRH6"
  EGCS=$(shell $(CLAS_CMS)/egcs_style)
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=$(shell $(CLAS_CMS)/g77-fortran-lib)
  FLIB_LinuxRH6=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif
ifeq "$(OS_NAME)" "LinuxRH7"
  EGCS=$(shell $(CLAS_CMS)/egcs_style)
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=$(shell $(CLAS_CMS)/g77-fortran-lib)
  FLIB_LinuxRH7=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif
ifeq "$(OS_NAME)" "LinuxRH8"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_LinuxRH8=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif
ifeq "$(OS_NAME)" "LinuxRH9"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_LinuxRH9=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif
ifeq "$(OS_NAME)" "LinuxRHEL3"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_LinuxRHEL3=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif
ifeq "$(OS_NAME)" "LinuxRHEL4"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_LinuxRHEL4=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif

ifeq "$(OS_NAME)" "LinuxRHEL5"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_LinuxRHEL5=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif

ifeq "$(OS_NAME)" "LinuxRHFC2"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_LinuxRHFC2=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif
ifeq "$(OS_NAME)" "LinuxRHFC3"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_LinuxRHFC3=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif
ifeq "$(OS_NAME)" "LinuxRHFC4"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_LinuxRHFC4=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif
ifeq "$(OS_NAME)" "LinuxRHFC7"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_LinuxRHFC7=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif
ifeq "$(OS_NAME)" "LinuxRHFC8"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_LinuxRHFC8=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif


ifeq "$(OS_NAME)" "Linux64RHEL3"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_Linux64RHEL3=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif
ifeq "$(OS_NAME)" "Linux64RHEL4"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_Linux64RHEL4=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif

ifeq "$(OS_NAME)" "Linux64RHEL5"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_Linux64RHEL5=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif

ifeq "$(OS_NAME)" "Linux64RHFC2"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_Linux64RHFC2=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif
ifeq "$(OS_NAME)" "Linux64RHFC3"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_Linux64RHFC3=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif
ifeq "$(OS_NAME)" "Linux64RHFC4"
 EGCS=$(shell $(CLAS_CMS)/egcs_style)	
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
  FLIBRARY=g2c
  FLIB_Linux64RHFC4=$(shell $(CLAS_CMS)/get_libraries g77) -l$(FLIBRARY)
endif
FFLAGS_Linux:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DLinux

ifeq "$(OS_NAME)" "LinuxRH4"
  EGCS := $(shell g++ --version | awk -F- '{if($$1=="egcs")printf("egcs")}')
  FLIB_LinuxRH4 := -l$(shell $(CLAS_CMS)/g77-fortran-lib)
  FFLAGS_LinuxRH4 := -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME)
endif

ifdef USEGNU
  EGCS := $(shell g++ --version | awk -F- '{if($$1=="egcs")printf("egcs")}')
  FLIB := -lg2c
  FFLAGS:= -fno-automatic -finit-local-zero -ffixed-line-length-none -fno-second-underscore -D$(OS_NAME) -DUSEGNU
else
  FLIB:= $(FLIB_$(OS_NAME))
  FFLAGS:= $(FFLAGS_$(OS_NAME))
endif

#
# Location of the CERN library is given by the CERNLIBDIR
# environment variable. If it is not set, the default
# is in $CLAS_PACK/lib.$OSNAME
#

CERNLIB= -L$(CERN_ROOT)/lib -lpacklib -lmathlib -lkernlib -lphtools

CERN_SunOS = 		-L/site/cernlib/sun4_solaris2/97a/lib
CERN_SunOS4 =           -L/usr/local/lib

CERN := $(CERN_$(OS_NAME))

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
CLIB_Darwin = 
CLIB_LinuxRH5 =		-lm -lc
CLIB_LinuxRH6 =		-lm -lc
CLIB_LinuxRH7 =		-lm -lc
CLIB_LinuxRH8 =		-lm -lc
CLIB_LinuxRH9 =		-lm -lc
CLIB_LinuxRHEL3 =	-lm -lc
CLIB_LinuxRHEL4 =	-lm -lc
CLIB_LinuxRHEL5 =	-lm -lc
CLIB_Linux64RHEL3 =	-lm -lc
CLIB_Linux64RHEL4 =	-lm -lc
CLIB_Linux64RHEL5 =	-lm -lc
CLIB_LinuxRHFC2 =	-lm -lc
CLIB_LinuxRHFC3 =	-lm -lc
CLIB_LinuxRHFC4 =	-lm -lc
CLIB_LinuxRHFC7 =	-lm -lc
CLIB_LinuxRHFC8    =	-lm -lc
CLIB_Linux64RHFC2 =	-lm -lc
CLIB_Linux64RHFC3 =	-lm -lc
CLIB_Linux64RHFC4 =	-lm -lc
CLIB:= $(CLIB_$(OS_NAME))

#
# ACPP: the C preprocessor to create dependacies list
#

ACPP_default:=		gcc -M
ACPP_HP-UX:=            gcc -M
ACPP_SunOS:=            gcc -M
ACPP_Darwin:=		gcc -M
ACPP_Linux:=		gcc -M
ACPP_IRIX:=             gcc -M
ACPP_SunOS4:=            gcc -M

ACPP:=$(ACPP_$(OS_NAME))

ifndef ACPP
ACPP=$(ACPP_default)
endif

#
# Don't do that. CC is often predefined by make.
#
# ifndef CC

CC_default:=		cc
CC_Darwin:=		gcc
CC_Linux:=		gcc
CC_LinuxRH4:=		gcc
CC_SunOS:=		cc
CC_HP-UX:= 	        cc -Ae
CC_IRIX:=               cc
CC_OSF1:=               gcc
CC_AIX:=		cc
CC_SunOS4:=		gcc

ifdef USEGNU
CC:= gcc
else
CC:=$(CC_$(OS_NAME))
endif

ifeq "$(EGCS)" "egcs"
CC:= egcs
endif

ifndef CC
CC=$(CC_default)
endif

# endif

C++_default:=		CC
C++_Darwin:=		g++
C++_Linux:=		g++
C++_LinuxRH4:=		g++
C++_LinuxRH5:=		g++
C++_LinuxRH6:=		g++
C++_LinuxRH7:=		g++
C++_LinuxRH8:=          g++
C++_LinuxRH9:=		g++
C++_LinuxRHEL3:=	g++
C++_LinuxRHEL4:=	g++
C++_LinuxRHEL5:=	g++
C++_Linux64RHEL3:=	g++
C++_Linux64RHEL4:=	g++
C++_Linux64RHEL5:=	g++
C++_LinuxRHFC2:=	g++
C++_LinuxRHFC3:=	g++
C++_LinuxRHFC4:=	g++
C++_LinuxRHFC7:=	g++
C++_LinuxRHFC8   :=	g++
C++_Linux64RHFC2:=	g++
C++_Linux64RHFC3:=	g++
C++_Linux64RHFC4:=	g++
C++_SunOS:=		CC
C++_HP-UX:= 	        	CC -Ae
C++_IRIX:=               		g++
C++_OSF1:=               	g++
C++_AIX:=		c++
C++_SunOS4:=		CC

ifdef USEGNU
C++:=g++
else
C++:=$(C++_$(OS_NAME))
endif

ifndef C++
C++=$(C++_default)
endif

FC_default:=		f77 -e
FC_Darwin:=		g77
FC_Linux:=		g77
FC_LinuxRH4:=		g77
FC_LinuxRH5:=		g77
FC_LinuxRH6:=		g77
FC_LinuxRH7:=		g77
FC_LinuxRH8:=           g77
FC_LinuxRH9:=           g77
FC_LinuxRHEL3:=         g77
FC_LinuxRHEL4:=         g77
FC_LinuxRHEL5:=         gfortran
FC_Linux64RHEL3:=         g77
FC_Linux64RHEL4:=         g77
FC_Linux64RHEL5:=         g77
FC_Linux64RHEL5:=         gfortran
FC_LinuxRHFC2:=         g77
FC_LinuxRHFC3:=         g77
FC_LinuxRHFC4:=         g77
FC_LinuxRHFC7:=         g77
FC_LinuxRHFC8  :=         gfortran
FC_Linux64RHFC2:=         g77
FC_Linux64RHFC3:=         g77
FC_Linux64RHFC4:=         g77
FC_linuxo:=		g77
FC_HP-UX:=              fort77 +es +ppu 
FC_IRIX:=               f77 
FC_OSF1:=               f77
FC_AIX:=                f77 -qfixed=132 -qextname

ifdef USEGNU
FC:= g77
else
FC:=$(FC_$(OS_NAME))
endif

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
# VPATH: search path
#

vpath %.o $(ODIR)


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

# ROOT
ifdef ROOTSYS
ifndef LD_LIBRARY_PATH
LD_LIBRARY_PATH = $(ROOTSYS)/lib
endif
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

$(OBJDIR)/%.oo: %.c
	$(VDIR) $(OBJDIR)
	$(ACPP) $< $(INCLUDE)  $(CFLAGS) > $*.oo.depend
	$(CC) -c -o $@ $< $(COPT) $(CFLAGS) $(INCLUDE)
	mv $*.o $(OBJDIR)/$*.oo

$(OBJDIR)/%.oo: %.f
	$(VDIR) $(OBJDIR)
	$(FC) -c $< $(COPT) $(FINCLUDE) $(FFLAGS)
	mv $*.o  $(OBJDIR)/$*.oo


$(OBJDIR)/%.o: %.f
	$(VDIR) $(OBJDIR)
	$(FC) -c $< $(CDBG) $(FINCLUDE) $(FFLAGS)
	mv $*.o  $(OBJDIR)/$*.o

$(OBJDIR)/%.o: %.F
	$(VDIR) $(OBJDIR)
	$(FC) -c $< $(CDBG) $(FFLAGS) $(FINCLUDE)
	mv $*.o  $(OBJDIR)/$*.o

$(OBJDIR)/%.o: %.c
	echo Making $@
	$(VDIR) $(OBJDIR)
	$(ACPP) $< $(INCLUDE) $(XINCLUDE)  $(CFLAGS) > $@.depend
	$(CC) -c -o $@ $< $(CDBG) $(CFLAGS) $(INCLUDE) $(XINCLUDE)

$(OBJDIR)/%.o: %.cc
	echo Making $@
	$(VDIR) $(OBJDIR)
	$(ACPP) $< $(INCLUDE) $(XINCLUDE) $(CFLAGS) $(CCFLAGS) > $@.depend
	$(C++) -c -o $@ $< $(CDBG) $(CFLAGS) $(CCFLAGS) $(INCLUDE) $(XINCLUDE)

$(OBJDIR)/%.o: %.C
	echo Making $@
	$(VDIR) $(OBJDIR)
	$(ACPP) $< $(INCLUDE) $(XINCLUDE) $(CFLAGS) $(CCFLAGS) > $@.depend
	$(C++) -c -o $@ $< $(CDBG) $(CFLAGS) $(CCFLAGS) $(INCLUDE) $(XINCLUDE)


#
# Make everything precious- to avoid removal of source files after targets are built.
#

.PRECIOUS: %


debug:
	@echo CLAS_PACK  $(CLAS_PACK)	
	@echo CLAS_LIB  $(CLAS_LIB)
	@echo CERN_LIB  $(CERN_LIB)
	@echo XLIBS   $(XLIBS)
	@echo LIBDIR   $(LIBDIR)
	@echo OS_NAME   $(OS_NAME)
	@echo VPATH   $(VPATH)
	@echo CC   $(CC)
	@echo CC_OS   $(CC_Darwin)
	@echo CC_Darwin $(CC_Darwin)
	@echo CFLAGS   $(CFLAGS)
	@echo CCFLAGS   $(CCFLAGS)
	@echo INCLUDE   $(INCLUDE)	
	@echo XINCLUDE   $(XINCLUDE)
	@echo FINCLUDE   $(FINCLUDE)
	@echo FC   $(FC)
	@echo CDBG   $(CDBG)
	@echo FLIB   $(FLIB)
	@echo FLIBRARY  $(FLIBRARY)
	@echo ROOTSYS  $(ROOTSYS)
	@echo LD_LIBRARY_PATH  $(LD_LIBRARY_PATH)
	@echo ROOTCFLAGS  $(ROOTCFLAGS)
	@echo ROOTLIBS  $(ROOTLIBS)
	@echo ROOTGLIBS  $(ROOTGLIBS)
clean:
	rm -f *.o *.oo *.depend $(OS_NAME)$(ADD_DEBUG)/*.o $(OS_NAME)$(ADD_DEBUG)/*.depend
	echo >dummy.depend


#
# end file
#







