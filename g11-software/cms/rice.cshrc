#
# Place these enviroment variable definitions in your .cshrc
# NOTE: to toggle between PRODUCTION and DEVELOPMENT you need to
#       redefine CLAS_LEVEL to your choice
#
setenv POSIX_SHELL /usr/bin/sh
setenv CLAS_LOCATION /scratch/mutchler
#
# This is a half-way cludge to get the OSCLAS correct:
# uname_clas is a bourne shell script that replaces uname.
#

setenv OSCLAS "`uname`"

setenv CLAS_ROOT  $CLAS_LOCATION
setenv CVSROOT    :pserver:${USER}@jlabs1.cebaf.gov:/group/clas/clas_cvs
setenv BUILDS     $CLAS_ROOT
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT
setenv CLAS_BUILD $CLAS_LOCATION

setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS
setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS
setenv CLAS_PACK    $CLAS_BUILD/packages
setenv CLAS_CMS     $CLAS_PACK/cms
setenv CLAS_SCRIPTS $CLAS_PACK/scripts
setenv RECSIS       $CLAS_PACK

#  the user should be able to toggle this to development or production

alias  use_dev   source $CLAS_ROOT/builds/DEVELOPMENT/packages/cms/cmu.cshrc DEVELOPMENT
alias  use_prod  source $CLAS_ROOT/builds/DEVELOPMENT/packages/cms/cmu.cshrc PRODUCTION
alias  use_daily source $CLAS_ROOT/builds/DEVELOPMENT/packages/cms/cmu.cshrc LATEST
alias  setup_clas source $CLAS_ROOT/builds/DEVELOPMENT/packages/cms/cmu.cshrc


setenv CLAS_DATA  $CLAS_LOCATION/data
setenv CLAS_PARMS $CLAS_ROOT/parms

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_PARMS

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB

# HV package information needed

setenv HV_LOCATION $CLAS_PACK/Hv
