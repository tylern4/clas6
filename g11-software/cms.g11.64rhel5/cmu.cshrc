#
# Place these enviroment variable definitions in your .cshrc
# NOTE: to toggle between PRODUCTION and DEVELOPMENT you need to
#       redefine CLAS_LEVEL to your choice
#
setenv POSIX_SHELL /usr/bin/sh
setenv CLAS_LOCATION /home/clas

setenv CLAS_ROOT  $CLAS_LOCATION
setenv CVSROOT    :pserver:${USER}@jlabs1.jlab.org:/group/clas/clas_cvs
setenv BUILDS     $CLAS_ROOT/builds
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT

# going to match the jlab model a little closer
if ($#argv == 0) then
  if ($?CLAS_BUILD) then
    #echo '$CLAS_BUILD' already defined: using existing definition of CLAS_BUILD
  else
    #echo '$CLAS_BUILD' not defined: using the default
    setenv CLAS_BUILD $CLAS_ROOT/builds/DEVELOPMENT
  endif
else 
  setenv CLAS_BUILD $CLAS_ROOT/builds/$argv[1]
endif

setenv CLAS_PACK    $CLAS_BUILD/packages
setenv CLAS_CMS     $CLAS_PACK/cms
setenv CLAS_SCRIPTS $CLAS_PACK/scripts
#
# This is a half-way cludge to get the OSCLAS correct:
# uname_clas is a bourne shell script that replaces uname.
#

setenv OSCLAS "`${CLAS_CMS}/uname_clas`"

setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS
setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS
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
