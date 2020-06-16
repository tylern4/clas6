#
# Place these enviroment variable definitions in your .cshrc
# NOTE: to toggle between PRODUCTION and DEVELOPMENT you need to
#       redefine CLAS_LEVEL to your choice
#
#setenv OSCLAS "`uname`"
setenv POSIX_SHELL /usr/bin/sh

if ("`uname`" == "Linux") then
	setenv CLAS_LOCATION /home
else if ("`uname`" == "SunOS") then
	setenv CLAS_LOCATION /usr/users
endif

if (!($?CLAS_ROOT)) setenv CLAS_ROOT  $CLAS_LOCATION/clas
#setenv CVSROOT    :pserver:$USER@jlabs1.cebaf.gov:/group/clas/clas_cvs
setenv BUILDS     $CLAS_ROOT/builds
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT

if (!($?TOP_DIR)) then
	if ("`uname`" == "Linux") then
		setenv TOP_DIR /home/$USER
	else if ("`uname`" == "SunOS") then
		setenv TOP_DIR /usr/users/$USER
	endif
endif

#  the user should be able to toggle this to development or production
#
# development or production or private?
#
if ($#argv == 0) then
  if ($?CLAS_BUILD) then
    #echo '$CLAS_BUILD' already defined: using existing definition of CLAS_BUILD
  else
    #echo '$CLAS_BUILD' not defined: using the default
    setenv CLAS_BUILD $CLAS_ROOT/builds/PRODUCTION
  endif
else 
  setenv CLAS_BUILD $CLAS_ROOT/builds/$argv[1]
endif
setenv CLAS_PACK    $CLAS_BUILD/packages
setenv CLAS_CMS     $CLAS_PACK/cms
setenv CLAS_SCRIPTS $CLAS_PACK/scripts
setenv OSCLAS       "`${CLAS_CMS}/uname_clas`"
setenv OS_NAME      `$CLAS_CMS/uname_clas`
setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS
setenv CLAS_SLIB    $CLAS_BUILD/slib/$OSCLAS
setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS
setenv RECSIS       $CLAS_PACK
if ($?LD_LIBRARY_PATH) then
  setenv LD_LIBRARY_PATH ${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}:${LD_LIBRARY_PATH}
else
  setenv LD_LIBRARY_PATH ${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}
endif

alias use_dev  source $CLAS_LOCATION/clas/builds/DEVELOPMENT/packages/cms/rich.cshrc DEVELOPMENT
alias use_prod source $CLAS_LOCATION/clas/builds/PRODUCTION/packages/cms/rich.cshrc PRODUCTION
alias use_daily source $CLAS_LOCATION/clas/builds/LATEST/packages/cms/rich.cshrc LATEST

alias use_test source $CLAS_LOCATION/clas/builds/TEST/packages/cms/rich.cshrc TEST

alias setup_clas source $CLAS_LOCATION/clas/builds/PRODUCTION/packages/cms/rich.cshrc

setenv CLAS_PARMS $CLAS_ROOT/parms
setenv CLAS_TOOLS $CLAS_ROOT/tools

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_ROOT/clsrc/recsis/runtime

# setup mysql environment

setenv MYSQL_INCLUDE_PATH /usr/include/mysql
setenv MYSQL_LIB_PATH /usr/lib/mysql
setenv CLAS_CALDB_HOST psc1.richmond.edu

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB

# Need to define where the HV package is kept:

setenv HV_LOCATION  $CLAS_PACK/Hv

#done with jlab.cshrc






