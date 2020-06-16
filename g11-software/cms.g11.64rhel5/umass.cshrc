#
# Place these enviroment variable definitions in your .cshrc
# NOTE: to toggle between PRODUCTION and DEVELOPMENT you need to
#       redefine CLAS_LEVEL to your choice
#
setenv OSCLAS "`uname`"
setenv POSIX_SHELL /usr/bin/sh

# change the username to your username on JLABS1

if($USER == 'miskimen' )then
	setenv CVSROOT    :pserver:miskimen@jlabs1.cebaf.gov:/apps/clas/u1/clas_cvs
else
	setenv CVSROOT    :pserver:miskimen@jlabs1.cebaf.gov:/apps/clas/u1/clas_cvs
endif

# define the top of the "clas" tree on your local box

setenv CLAS_LOCATION /export/home/CLAS/

setenv CLAS_ROOT  $CLAS_LOCATION
setenv BUILDS     $CLAS_ROOT
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT
setenv TOP_DIR    $CLAS_DEVEL

#  the user should be able to toggle this to development or production

alias  use_dev         'setenv CLAS_LEVEL   DEVELOPMENT; \
			setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
			setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
			setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
			setenv CLAS_CMS     $CLAS_PACK/cms; \
			setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
			setenv RECSIS       $CLAS_PACK; \
			echo "CLAS enviroment now set for DEVELOPMENT" '

alias  use_prod        'setenv CLAS_LEVEL   PRODCUTION; \
			setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
			setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
			setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
			setenv CLAS_CMS     $CLAS_PACK/cms; \
			setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
			setenv RECSIS       $CLAS_PACK; \
			echo "CLAS enviroment now set for PRODUCTION" '

use_dev

# you should define these appropriately for the PARMS or DATA area

setenv CLAS_DATA  $CLAS_ROOT/data
setenv CLAS_PARMS $CLAS_ROOT/parms

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_PARMS

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB

# Set the TOP dir to be more convenient at UNH:

setenv TOP_DIR  $CLAS_ROOT

