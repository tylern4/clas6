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
#                     static vers ion of X11, Xt you can add them to the 
#                     GSIM_BAT_MD_LIB list and cross your fingers.
#  
# This version is made for Red Hat Enterprise Level 5, which means
# you are using gfortran. Note that you will need to get a gfortran
# compiled version of CERN_LIB. Best to use the CERNLIB 2006/slc4_ia32_gcc4
# version on i386 and elf_i386 systems (all 32 bit Linuxes) and
# the 2006/slc4_amd64_gcc4 version on AMD64 (x86_64) systems.
#
# As of 2007/10/23 this is still in need of testing (i.e. do we get
# the correct results!)
#

TCL_VERSION = 8.4

ifdef GFORTRAN
  LIBNAMES += gfortran
else
  LIBNAMES += g2c
endif

RECSIS_SHARED_LIBS += -ltcl$(TCL_VERSION) -lnsl -ldl -lstdc++ 

RECSIS_MD_LIBS = fputil$(ADD_DEBUG) bos$(ADD_DEBUG) fpack$(ADD_DEBUG) tcl$(TCL_VERSION) patches$(ADD_DEBUG) 
ifndef MAP
  RECLIBS3 += z
endif

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

ifdef USE_STATIC_LIBS
   GSIM_INT_SHARED_LIBS	+= -static
   GSIM_BAT_SHARED_LIBS += -static
   PB_BAT_SHARED_LIBS   += -static
   CED_SHARED_LIBS      += -static
endif

GSIM_INT_LIBS0   = gsim 
GSIM_INT_LIBS0  += cc dc sc tag c_bos_io $(CAL_LIB)
GSIM_INT_LIBS0  += recutl  clasutil fputil
GSIM_INT_LIBS0  += c_bos_io bos fpack online_dummy
GSIM_INT_LIBS  = $(addsuffix $(ADD_DEBUG), $(GSIM_INT_LIBS0))
GSIM_INT_LIBS  += geant321 pawlib 
GSIM_INT_LIBS  += graflib grafX11 packlib phtools
GSIM_INT_LIBS  += mathlib kernlib lapack3 blas

ifndef MAP
  GSIM_INT_LIBS  += mysqlclient
  GSIM_INT_SHARED_LIBS += -ltcl$(TCL_VERSION)
endif

GSIM_INT_MD_LIBS = patches$(ADD_DEBUG) nsl 

ifndef MAP
   GSIM_INT_MD_LIBS += z
endif


ifdef NO_MOTIF
   GSIM_INT_SHARED_LIBS += -lXp -lXext -lX11 
else
   GSIM_INT_SHARED_LIBS += -lXm -lXt -lSM -lICE -lX11 -lXext -lXaw -lXp 
endif

GSIM_INT_SHARED_LIBS += -lnsl -lcrypt -ldl -lblas 

GSIM_BAT_MD_LIBS = nsl patches$(ADD_DEBUG)
GSIM_BAT_SHARED_LIBS += -lX11 -lblas /usr/lib/gcc/i386-redhat-linux/4.1.1/libgfortran.a

EVENTSTORE_MD_LIBS =

SWISH_MD_LIBS = m
SWISH_SHARED_LIBS += /lib/libdl.so.2

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
