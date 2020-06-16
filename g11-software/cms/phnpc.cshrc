#!/bin/csh -f
# Setup for CLAS analysis at Saclay on Linux PC's
# 01/08/02 - FJS
# Adapted for current RH7.2 setup at Saclay

echo CLAS Analysis setup at Saclay

setenv OSCLAS "`uname`"
setenv OS_NAME "`uname`"
setenv POSIX_SHELL /bin/sh

# define the top of the "clas" tree on the CCPN

setenv CLAS_LOCATION /local/home/librarian

# A few more environment variables

setenv CLAS_ROOT   $CLAS_LOCATION
setenv BUILDS      $CLAS_ROOT
setenv CLAS_PROD   $BUILDS/PRODUCTION
setenv CLAS_LATEST $CLAS_LOCATION/LATEST 
setenv CLAS_DEVEL  $BUILDS/DEVELOPMENT

# Use latest by default

if (!($?CLAS_BUILD)) then
 echo CLAS_BUILD not set, setting it to LATEST
 setenv CLAS_BUILD $CLAS_LOCATION/LATEST
else
 echo "CLAS_BUILD set to" $CLAS_BUILD
endif

# Yet a few more environment variables

setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS
setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS
setenv CLAS_PACK    $CLAS_BUILD/packages
setenv CLAS_CMS     /local/home/librarian/LATEST/packages/cms
setenv CLAS_SCRIPTS $CLAS_PACK/scripts
setenv RECSIS       $CLAS_PACK

if (-f ${CLAS_CMS}/set_top_dir.csh) then
  alias settop 'source $CLAS_CMS/set_top_dir.csh \!:*'
endif
if (-f ${CLAS_CMS}/set_cms.csh) then
  alias setcms 'source $CLAS_CMS/set_cms.csh \!:*'
endif

if (!($?TOP_DIR)) then
  if (-f ${CLAS_CMS}/set_top_dir.csh) then
    source $CLAS_CMS/set_top_dir.csh $HOME
  else
    setenv TOP_DIR $HOME
    setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${TOP_DIR}/slib/${OSCLAS}
  endif
endif

if (-f ${CLAS_CMS}/set_slib.csh) then
  source $CLAS_CMS/set_slib.csh $CLAS_BUILD/slib/$OSCLAS
else
  setenv CLAS_SLIB $CLAS_BUILD/slib/$OSCLAS
  if ($?LD_LIBRARY_PATH) then
    setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${CLAS_SLIB}
  else
    setenv LD_LIBRARY_PATH $CLAS_SLIB
  endif
endif

# you should define these appropriately for the PARMS or DATA area

setenv CLAS_DATA  $CLAS_LOCATION/data
setenv CLAS_PARMS /dsm/sphn/clas/librarian/parms

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_PARMS

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB
setenv TCL_INC /usr/include
setenv TCL_LIB /usr/lib

# Set the MYSQL variables for use with caldb...

setenv MYSQL_INCLUDE_PATH /usr/include/mysql
setenv MYSQL_LIB_PATH /usr/lib/mysql
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${MYSQL_LIB_PATH}

# ROOT env

source $CLAS_LOCATION/scripts/set_rootenv.csh pro

# By default, $TOP_DIR=$HOME, you HAVE to change that in .cshrc or something

settop $HOME


