#
# Place these enviroment variable definitions in your .cshrc
# NOTE: to toggle between PRODUCTION and DEVELOPMENT you need to
#       redefine CLAS_LEVEL to your choice
#
setenv OSCLAS "`uname`"
setenv POSIX_SHELL /usr/bin/sh

# change the username to your username on JLABS1

setenv CVSROOT    :pserver:$USER@jlabs1.cebaf.gov:/apps/clas/u1/clas_cvs

# define the top of the "clas" tree on your local box

setenv CLAS_LOCATION /claspc1/home/claslib

setenv CLAS_ROOT  $CLAS_LOCATION
setenv BUILDS     $CLAS_ROOT
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT

#  the user should be able to toggle this to development or production

alias  use_dev         'setenv CLAS_LEVEL   DEVELOPMENT; \
			setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
			setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
			setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
			setenv CLAS_CMS     $CLAS_PACK/cms; \
			setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
			setenv RECSIS       $CLAS_PACK; '

alias  use_prod        'setenv CLAS_LEVEL   PRODUCTION; \
			setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
			setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
			setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
			setenv CLAS_CMS     $CLAS_PACK/cms; \
			setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
			setenv RECSIS       $CLAS_PACK; '


use_dev

# you should define these appropriately for the PARMS or DATA area

setenv CLAS_DATA  $CLAS_LOCATION/DATA
setenv CLAS_PARMS $CLAS_LOCATION/PARMS

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_PARMS

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB

# setup CERN STUFF

setenv CERN /claspc1/home/claslib/CERN

alias cern_96a 'setenv CERN_LEVEL 96a; \
		setenv CERN_ROOT $CERN/$CERN_LEVEL; \
		setenv CERN_LIB $CERN/$CERN_LEVEL/lib; \
		setenv CERN_BIN $CERN/$CERN_LEVEL/bin'

alias cern_97a 'setenv CERN_LEVEL 97a; \
		setenv CERN_ROOT $CERN/$CERN_LEVEL; \
		setenv CERN_LIB $CERN/$CERN_LEVEL/lib; \
		setenv CERN_BIN $CERN/$CERN_LEVEL/bin'

cern_96a

# end of claspc1.cshrc


# Need to define where the HV package is kept:

setenv HV_LOCATION  /claspc3/home/heddle/Hv

