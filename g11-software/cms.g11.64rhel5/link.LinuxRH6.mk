#
#  This Makefile snipet contains the machine DEPENDENT libraries needed
#  in the  link list .
#  These are needed to link:
#                 RECSIS      stored in RECSIS_MD_LIBS
#                 GSIM_BAT              GSIM_BAT_MD_LIBS
#                 GSIM_INT              GSIM_INT_MD_LIBS
#                 FDUMP                 FDUMP_MD_LIBS
#                 swish                 SWISH_MD_LIBS
#                 ced                   CED_MD_LIBS
#                 EventStore and associated exe's
#
#  The machine INDEPENDENT parts of the link list are found in
#  $(CLAS_CMS)/link.mk, which defines variables named 
#  PACKAGE_LIBS,  where PACKAGE = RECSIS .....
#
#  Thus the complete link list (for example for RECSIS) is obtained by:
#  $(LIBNAMES) = $(RECSIS_LIBS) $(RECSIS_MD_LIBS)
#
#  1/8/97 apf
#
#  1/18/97 apf  NOTE: I had trouble picking up shared libraries automatically
#                     so I added another variable RECSIS_SHARED_LIB and
#                     GSIM_BAT_SHARED_LIB.  These SHARED variables must
#                     contain the complete path and suffix for each library.
#                     I hope to fix this cludge soon.  If you happen to have
#                     static version of X11, Xt you can add them to the 
#                     GSIM_BAT_MD_LIB list and cross your fingers.
#  
#  1/21/97 apf  FIXED shared lib problem
#
#  7 July 97 gpg Included libraries for online version of RECSIS on
#                on the sun and dummy libraries for other platforms.
#
#
#  5-12-08  Test for the VERSION of linux. If the kernel is
#           2.0.33 or higher, assume this is glibc based system, and
#           include different libraries: -lcrypt
#           use `uname -r` to find the version of the linux.
#
#  9-3-98   Test the g77 for the version. If it is the "egcs" version of
#           g77, we need the egcs version of the libf2c.a !
#
#  temporary fix as of 8-12-99: on the CUE ifarmln machines I always
#           get undefined symbols in the link lists (xargc, f__xargc),
#           it can be solved via adding libg2c (which is not on the path
#           for these machines).
#

ifeq "$(OS_NAME)" "Linux"
  EGCS=$(shell $(CLAS_CMS)/egcs_style)
  ifeq "$(EGCS)" "egcs"
     CFLAGS+=-DEGCS
  endif
endif

RECSIS_SHARED_LIBS += -lnsl -ldl

# /lib/libdl.so.1
RECSIS_MD_LIBS = fputil$(ADD_DEBUG) bos$(ADD_DEBUG) fpack$(ADD_DEBUG)
ifndef MAP
  RECLIBS3 += z
endif

#
# MAKE can't do a comparison, so we do this in awk, note the $$ to fool 
# the shell into allowing for a $3 in the string.
#

USE_GLIBC = $(shell $(CLAS_CMS)/glibc_style)
USE_EGCS  = $(shell $(CLAS_CMS)/egcs_style)

#
ifdef ONLINE
    ONLINE_MESSAGE = '\n \
The ONLINE=yes option only works on the JLAB CLON \n \
cluster Suns because of the need for proprietary  \n \
routines. Your version of recsis will NOT be able \n \
to read the DD ring and will not have the "ONLINE"\n \
suffix.\n '
else
    ONLINE_MESSAGE = 
endif

GSIM_INT_MD_LIBS = lapack3 blas nsl patches$(ADD_DEBUG)

ifdef USE_STATIC_LIBS
   GSIM_INT_SHARED_LIBS	+= -static
   GSIM_BAT_SHARED_LIBS += -static
   PB_BAT_SHARED_LIBS   += -static
   CED_SHARED_LIBS      += -static
endif

ifdef NO_MOTIF
   GSIM_INT_SHARED_LIBS += -lXp -lXext -lX11 
else
   GSIM_INT_SHARED_LIBS += -lXm -lXt -lSM -lICE -lX11 -lXext -lXaw -lXp 
endif

ifeq "$(USE_GLIBC)" "GLIBC"
  GSIM_INT_SHARED_LIBS += -lcrypt
else
  ifeq ($(LINUX_VERSION),5)
    GSIM_INT_SHARED_LIBS += -lcrypt
  endif
endif

GSIM_INT_SHARED_LIBS += -ldl

GSIM_BAT_MD_LIBS = nsl patches$(ADD_DEBUG)
GSIM_BAT_SHARED_LIBS += -lX11

EVENTSTORE_MD_LIBS =

SWISH_MD_LIBS = m
SWISH_SHARED_LIBS += /lib/libdl.so.1

FDUMP_MD_LIBS = m dl

CED_MD_LIBS = patches$(ADD_DEBUG)
CED_SHARED_LIBS += -lXext -lXaw -lXp

PB_INT_MD_LIBS = patches$(ADD_DEBUG)
PB_INT_SHARED_LIBS = -lXm -lX11 -lXt

PB_BAT_MD_LIBS = patches$(ADD_DEBUG)
PB_BAT_SHARED_LIBS += -lX11 -lXt

BOSIO_MD_LIBS +=

#
# NO libg++.so the compiler should be able to find it if it needs it. Besides,
# libg++ has been depreciated, one should only need libstd++.so.
#
ROOT_MD_LIBS =  m dl
ROOT_SHARED_LIBS = -lXpm -lX11 -lm -ldl
#
