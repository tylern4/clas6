#
# Place these enviroment variable definitions in your .cshrc
# NOTE: to toggle between PRODUCTION and DEVELOPMENT you need to
#       redefine CLAS_LEVEL to your choice
#
setenv OSCLAS "`/usr/local/clas/BUILDS/LATEST/packages/cms/uname_clas`"
setenv POSIX_SHELL /usr/bin/sh

# Get LD_LIBRARY_PATH before it gets modified 
if (!($?LD_LIBRARY_PATH_0)) then
 if (($?LD_LIBRARY_PATH)) then
    setenv LD_LIBRARY_PATH_0 $LD_LIBRARY_PATH
 else
    setenv LD_LIBRARY_PATH_0 :
# simplest trick I though of, I guess this is not harm
 endif
endif 

if (!($?TOP_DIR)) then
  echo Warning: TOP_DIR is not defined yet, LD_LIBRARY_PATH won\'t point to your local shared libraries.
  echo -------- You might want to define TOP_DIR BEFORE you source this file.
  setenv TOP_DIR :
endif

# change the username to your username on JLABS1

setenv CVSROOT pricej@cvs.jlab.org:/group/clas/clas_cvs
if ($OSCLAS == "SunOS") then
  setenv CVS_RSH /usr/local/bin/ssh
else
  setenv CVS_RSH /usr/bin/ssh
endif

# define the top of the "clas" tree on your local box

setenv CLAS_LOCATION /usr/local/clas


#
setenv CLAS_ROOT  $CLAS_LOCATION
setenv BUILDS     $CLAS_ROOT/BUILDS
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT

# Need to define where the HV package is kept:

setenv HV_LOCATION  $CLAS_ROOT/Hv/Hv

#  the user should be able to toggle this to development or production

alias  use_late 'setenv CLAS_LEVEL   LATEST; \
		 setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
		 setenv CLAS_SLIB    $BUILDS/$CLAS_LEVEL/slib/$OSCLAS; \
		 setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
		 setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
		 setenv CLAS_CMS     $CLAS_PACK/cms; \
		 setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
        setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH_0}:${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}; \
		 setenv RECSIS       $CLAS_PACK; \
		 echo "CLAS enviroment now set for LATEST" '

alias  use_dev         'setenv CLAS_LEVEL   DEVELOPMENT; \
			setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
			setenv CLAS_SLIB     $BUILDS/$CLAS_LEVEL/slib/$OSCLAS; \
			setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
			setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
			setenv CLAS_CMS     $CLAS_PACK/cms; \
			setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
        setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH_0}:${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}; \
			setenv RECSIS       $CLAS_PACK; \
			echo "CLAS enviroment now set for DEVELOPMENT" '

alias  use_prod \
     'setenv CLAS_LEVEL   PRODUCTION; \
      setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
      setenv CLAS_SLIB    $BUILDS/$CLAS_LEVEL/slib/$OSCLAS; \
      setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
      setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
      setenv CLAS_CMS     $CLAS_PACK/cms; \
      setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
      setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH_0}:${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}; \
      setenv RECSIS       $CLAS_PACK; \
      echo "CLAS enviroment now set for PRODUCTION" '

use_prod

# you should define these appropriately for the PARMS or DATA area

setenv CLAS_DATA  /home/price/DATA
setenv CLAS_PARMS $CLAS_ROOT/PARMS

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_PARMS

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB

# use 2001 version of CERN software
setenv CERN /usr/local/cern
setenv CERN_LEVEL 2001
setenv CERN_ROOT $CERN/$CERN_LEVEL
setenv CERN_LIB $CERN_ROOT/lib
setenv CERN_BIN $CERN_ROOT/bin
setenv ROOTSYS /usr/local/cern/root
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${ROOTSYS}/lib

# some useful cvs aliases
setenv MYSQL_INCLUDE_PATH /usr/include/mysql
setenv MYSQL_LIB_PATH /usr/lib/mysql

setenv PATH ${PATH}:${CERN_BIN}:${CLAS_BIN}:${CLAS_SCRIPTS}:${ROOTSYS}/bin
