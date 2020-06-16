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
#                     RECSIS_MD_LIB list and cross your fingers.
#  
#  7 July 97 gpg Included libraries for online version of RECSIS on
#                on the sun and dummy libraries for other platforms.
#                also added message warning users that try to build
#                the online recsis on an unsupported platform.
#

RECSIS_MD_LIBS = fputil$(ADD_DEBUG) bos$(ADD_DEBUG) fpack$(ADD_DEBUG)
RECSIS_SHARED_LIBS += -lm

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

GSIM_INT_MD_LIBS = Xm Xt X11 m

GSIM_BAT_MD_LIBS = Xt X11 m 

EVENTSTORE_MD_LIBS =

SWISH_MD_LIBS = m

FDUMP_MD_LIBS =

CED_MD_LIBS =

PB_INT_MD_LIBS = Xm Xt X11 m

PB_BAT_MD_LIBS = Xt X11 m 

BOSIO_MD_LIBS += m

ROOT_MD_LIBS = X11 m dld Xpm X11 

