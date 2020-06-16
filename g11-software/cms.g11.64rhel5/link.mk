#
#  This Makefile snipet contains the machine independent link list 
#  needed to link:
#                 RECSIS           $RECSIS_LIBS
#                 GSIM_BAT
#                 GSIM_INT
#                 FDUMP
#                 swish
#                 ced
#                 EventStore and associated exe's
#
#  The machine dependent parts of the link list are found in
#  $(CLAS_CMS)/link.$(OS_NAME).mk, which defines variables named 
#  PACKAGE_MD_LIBS,  where PACKAGE = RECSIS .....
#
#  Thus the complete link list (for example for RECSIS) is obtained by:
#  $(LIBNAMES) = $(RECSIS_LIBS) $(RECSIS_MD_LIBS)
#
#  1/8/97 apf
#
# removed 'online_dummy' from RECSIS_LIBS2 and put it in
# link.OS_NAME.mk to implement the online version of recsis
#                            gpg 7/10/97
#

# If MAP is defined use the Map rather than the CalDB
ifndef MAP
  CAL_LIB = caldbMap
  RECLIBS3 = mysqlclient
  LIB_PATH += $(MYSQL_LIB_PATH)
else
  CAL_LIB = mapmanager
endif

#  RECSIS libraries
RECLIBS1  = recsis
RECSIS_LIBS1 = $(addsuffix $(ADD_DEBUG), $(RECLIBS1))
ifdef ONLINE
  RECLIBS2  = pid
else
  RECLIBS2  = user pid
endif
RECLIBS2 += sc ec lac tag st trk cc dc seb icf recutl
RECLIBS2 += vertex epics c_sql
RECLIBS2 += c_bos_io $(CAL_LIB) clasutil
ifndef ONLINE
RECLIBS2 += scat online_dummy
else
RECLIBS2 += offline_dummy
endif
RECSIS_LIBS2 = $(addsuffix $(ADD_DEBUG), $(RECLIBS2))
RECLIBS3 += mathlib packlib
RECSIS_LIBS3 = $(RECLIBS3)
RECSIS_LIBS = $(RECSIS_LIBS1) $(RECSIS_LIBS2) $(RECSIS_LIBS3)
RECSIS_SHARED_LIBS += -ltcl$(TCL_VERSION)

#  GSIM libraries

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

# Duplicate the libraries which should always be the same between 
# 'int' and 'bat' version.

GSIM_BAT_LIBS  = $(addsuffix $(ADD_DEBUG), $(GSIM_INT_LIBS0))
GSIM_BAT_LIBS  += geant321 pawlib 
GSIM_BAT_LIBS  += graflib grafX11
GSIM_BAT_LIBS  += mathlib kernlib packlib

ifndef MAP
  GSIM_BAT_LIBS  += mysqlclient
  GSIM_BAT_SHARED_LIBS += -ltcl$(TCL_VERSION)
endif

# CELEG libraries
LEPTO=$(shell ls -1 $(CERN)/$(CERN_LEVEL)/lib/liblepto*.a)
LEPTO_LIB=$(subst lib,,$(basename $(notdir $(LEPTO))))

CELEG_LIBS0  = celeg
CELEG_LIBS0  += recutl  clasutil fputil
CELEG_LIBS0  += c_bos_io bos fpack online_dummy
CELEG_LIBS   = $(addsuffix $(ADD_DEBUG), $(CELEG_LIBS0))
CELEG_LIBS   += $(LEPTO_LIB) jetset74
CELEG_LIBS   += mathlib kernlib packlib phtools

# EventStore Libraries

EVENTSTORE_LIBS = scat$(ADD_DEBUG)

# scat enabled WISH shell

SWISH_LIBS = scat$(ADD_DEBUG) Xt X11
SWISH_SHARED_LIBS += -ltk -ltcl 

#  FDUMP libraries

FDUMP_LIBS  = fputil$(ADD_DEBUG) bos$(ADD_DEBUG) clasutil$(ADD_DEBUG) 
FDUMP_LIBS += fpack$(ADD_DEBUG) c_bos_io$(ADD_DEBUG)
FDUMP_LIBS += mathlib kernlib packlib

#  CED libraries

CED_LIBS = ced$(ADD_DEBUG) Hv$(ADD_DEBUG) dc$(ADD_DEBUG) recutl$(ADD_DEBUG) c_bos_io$(ADD_DEBUG) mapmanager$(ADD_DEBUG)
CED_LIBS += fputil$(ADD_DEBUG) bos$(ADD_DEBUG) fpack$(ADD_DEBUG) c_bos_io$(ADD_DEBUG)  clasutil$(ADD_DEBUG) packlib
CED_SHARED_LIBS = -lXm -lXt -lX11 -lm

# Beam Line libraries

PB_LIBS = photon_beam pb_main pb_pc pb_ps pb_tac pb_tagger pb_target pb_line pb_utils
PHOTON_BEAM_LIBS = $(addsuffix $(ADD_DEBUG), $(PB_LIBS))

PB_INT_LIBS  = $(PHOTON_BEAM_LIBS)
PB_INT_LIBS  += geant321 pawlib 
PB_INT_LIBS  += graflib grafX11 packlib phtools
PB_INT_LIBS  += mathlib kernlib packlib

PB_BAT_LIBS  = $(PHOTON_BEAM_LIBS)
PB_BAT_LIBS  += geant321 pawlib 
PB_BAT_LIBS  += graflib grafX11
PB_BAT_LIBS  += mathlib kernlib packlib


# moved these to link.OSNAME.mk as they are somewhat machine dependent
#PB_BAT_LIBS  += Xt X11

BOSIO_LIBS += bosio$(ADD_DEBUG)

#  ROOT platform independent SHARED libraries
ifdef ROOT 
 ifdef ROOTSYS

# starting version 2.22 
   ROOT_SH_LIBS := $(shell ${ROOTSYS}/bin/root-config --libs)
   ROOT_SH_GLIBS := $(shell ${ROOTSYS}/bin/root-config --glibs)

# older versions
   ifeq ($(strip $(ROOT_SH_LIBS)),)
    ROOT_SH_LIBS = -lNew -lBase -lCint -lClib -lCont -lFunc -lGraf
    ROOT_SH_LIBS += -lGraf3d -lHist -lHtml -lMatrix -lMeta -lMinuit -lNet -lPostscript -lPhysics
    ROOT_SH_LIBS += -lProof -lRint -lTree -lUnix -lZip
    ROOT_SH_LIBS += -lGpad -lGui -lGX11 -lX3d -lXpm
  #ROOT_SH_LIBS += -lEG -lEGPythia -lEGVenus
   endif
 endif
endif

