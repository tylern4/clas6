#
# Place these enviroment variable definitions in your .cshrc
# NOTE: to toggle between PRODUCTION and DEVELOPMENT you need to
#       redefine CLAS_LEVEL to your choice
#
setenv OSCLAS "`uname`"
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

if($USER == 'brooksw' )then
	setenv CVSROOT    :pserver:brooksw@jlabs1.cebaf.gov:/group/clas/clas_cvs
else
	setenv CVSROOT    :pserver:$USER@jlabs1.cebaf.gov:/group/clas/clas_cvs
endif

# define the top of the "clas" tree on your local box

setenv CLAS_LOCATION /home/brooksw

# Need to define where the HV package is kept:

setenv HV_LOCATION  $CLAS_ROOT/Hv/Hv

#
setenv CLAS_ROOT  $CLAS_LOCATION
setenv BUILDS     $CLAS_ROOT
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT
setenv TOP_DIR    $CLAS_DEVEL

#  the user should be able to toggle this to development or production

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

alias  use_prod        'setenv CLAS_LEVEL   PRODCUTION; \
			setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
			setenv CLAS_SLIB     $BUILDS/$CLAS_LEVEL/slib/$OSCLAS; \
			setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
			setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
			setenv CLAS_CMS     $CLAS_PACK/cms; \
			setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
     setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH_0}:${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}; \
			setenv RECSIS       $CLAS_PACK; \
			echo "CLAS enviroment now set for PRODUCTION" '

use_dev

# you should define these appropriately for the PARMS or DATA area

setenv CLAS_DATA  /scratch
setenv CLAS_PARMS $CLAS_ROOT/parms

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_PARMS

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB

# Set the TOP dir to be more convenient at UNH:

setenv TOP_DIR  $CLAS_ROOT


