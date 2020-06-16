#
# Place these enviroment variable definitions in your .cshrc
# NOTE: to toggle between PRODUCTION and DEVELOPMENT you need to
#       redefine CLAS_LEVEL to your choice
#
setenv OSCLAS "`uname`"
setenv OS_NAME $OSCLAS
setenv POSIX_SHELL /bin/sh

 
# define the top of the "clas" tree on your local box

setenv CLAS_LOCATION /home/clas

setenv CLAS_ROOT  $CLAS_LOCATION
setenv BUILDS     $CLAS_ROOT/builds
setenv CLAS_PROD  $BUILDS/pro
setenv CLAS_DEVEL $BUILDS/dev

#  the user should be able to toggle this to development or production

alias  use_dev         'setenv CLAS_LEVEL   dev; \
			setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
			setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
			setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
			setenv CLAS_CMS     $CLAS_PACK/cms; \
			setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
			setenv RECSIS       $CLAS_PACK; '

alias  use_prod        'setenv CLAS_LEVEL   pro; \
			setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
			setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
			setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
			setenv CLAS_CMS     $CLAS_PACK/cms; \
			setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
			setenv RECSIS       $CLAS_PACK; '


use_prod

# you should define these appropriately for the PARMS or DATA area

setenv CLAS_DATA  /home2/data
setenv CLAS_PARMS $CLAS_LOCATION/parms

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_PARMS

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB

# setup CERN STUFF

setenv CERN /home/cern

alias cern_96a 'setenv CERN_LEVEL 96a; \
		setenv CERN_ROOT $CERN/$CERN_LEVEL; \
		setenv CERN_LIB $CERN/$CERN_LEVEL/lib; \
		setenv CERN_BIN $CERN/$CERN_LEVEL/bin'

alias cern_97a 'setenv CERN_LEVEL 97a; \
		setenv CERN_ROOT $CERN/$CERN_LEVEL; \
		setenv CERN_LIB $CERN/$CERN_LEVEL/lib; \
		setenv CERN_BIN $CERN/$CERN_LEVEL/bin'

cern_97a


# Need to define where the HV package is kept:

setenv HV_LOCATION  /home/clas/Hv

# setup  X locations for solomon.physics.sc.edu

setenv 	X_LIB  "/usr/X11/lib /usr/dt/lib"
setenv  X_INC  "/usr/X11R6/include /usr/dt/include"



