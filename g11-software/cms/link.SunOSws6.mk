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
#                     Or simply a -l preceding it
#                     I hope to fix this cludge soon.  If you happen to have
#                     static version of X11, Xt you can add them to the 
#                     RECSIS_MD_LIB list and cross your fingers.
#
#  1/21/97 apf FIX shared lib problem...no longer need XXX_SHARED_LIB
#              variable
#  
#
#  7/7/97 gpg Included libraries for online version of RECSIS on the
#             Sun and dummy libraries for other platforms.
#
#  11/2/98 ea Included libraries for ROOT on Sun
#
#  1/15/99 gpg added libraries for web-based time histories for online RECSIS.
#  
#  6/8/99 gpg added libraries for new event transfer (et) software.     
#
#  2/8/00 more modifications to keep up with changes to et. 

ifdef ONLINE
    LIB_PATH += $(CLON_LIB) $(CODA)/SunOS/lib
    LIB_PATH += /apps/gcc/gcc-2.8.1/lib/gcc-lib/sparc-sun-solaris2.6/2.8.1
    RECSIS_MD_LIBS =  ipc et_ et bosio clasutil util msql socket util
    RECSIS_MD_LIBS += nsl kernlib  mysqlclient m nsl socket gcc
    RECSIS_SHARED_LIBS += -lposix4 -lC -lresolv
else
#    RECSIS_MD_LIBS =  fputil$(ADD_DEBUG) bos$(ADD_DEBUG) fpack$(ADD_DEBUG) packlib nsl socket
    RECSIS_MD_LIBS =  packlib nsl socket
    RECSIS_MD_LIBS +=  online_dummy
endif

RECSIS_SHARED_LIBS += -ldl 

ONLINE_MESSAGE = ""

GSIM_INT_MD_LIBS = Xt X11 w intl c m nsl socket
GSIM_INT_SHARED_LIBS = -lXm -lgen -ldl

GSIM_BAT_MD_LIBS = Xt X11 m nsl socket m 

EVENTSTORE_MD_LIBS = nsl socket m

SWISH_MD_LIBS += nsl socket m

FDUMP_MD_LIBS = nsl socket

CED_MD_LIBS= nsl socket

PB_INT_MD_LIBS = Xt X11 w intl c m nsl socket
PB_INT_SHARED_LIBS = -lXm -lgen -ldl 

PB_BAT_MD_LIBS = Xt X11 m nsl socket m
PB_BAT_SHARED_LIBS = 

BOSIO_MD_LIBS += nsl socket

ROOT_MD_LIBS = Xt X11 m c socket sunmath nsl C
ROOT_SHARED_LIBS = -lgen -ldl -lXm
FORTRAN_SHLIBS = -lF77 -lM77
