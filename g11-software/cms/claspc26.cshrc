#
# $Id: claspc26.cshrc,v 1.2 2002/03/19 21:51:28 hovanes Exp $
#
# Place these environment variable definitions in your .cshrc or .login

setenv POSIX_SHELL /usr/bin/sh
setenv CLAS_LOCATION /group

if (!($?CLAS_ROOT)) setenv CLAS_ROOT  $CLAS_LOCATION/clas
setenv CVSROOT    $CLAS_ROOT/clas_cvs
setenv BUILDS     $CLAS_ROOT/builds
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT

if (!($?TOP_DIR)) then
  setenv TOP_DIR /home/$USER
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
#setenv OSCLAS       "LinuxRH6"
setenv OS_NAME      `$CLAS_CMS/uname_clas`
#setenv OS_NAME      "LinuxRH6"
setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS
setenv CLAS_SLIB    $CLAS_BUILD/slib/$OSCLAS
setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS
setenv RECSIS       $CLAS_PACK
if ($?LD_LIBRARY_PATH) then
  setenv LD_LIBRARY_PATH ${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}:${LD_LIBRARY_PATH}
else
  setenv LD_LIBRARY_PATH ${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}
endif

alias use_dev  source /home/hovanes/packages/cms/claspc26.cshrc DEVELOPMENT
alias use_prod source /home/hovanes/packages/cms/claspc26.cshrc PRODUCTION
alias use_latest source /home/hovanes/packages/cms/claspc26.cshrc LATEST

alias setup_clas source /group/clas/builds/PRODUCTION/packages/cms/claspc26.cshrc

setenv CLAS_PARMS $CLAS_ROOT/parms
setenv CLAS_TOOLS $CLAS_ROOT/tools

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_ROOT/clsrc/recsis/runtime

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB

# Need to define where the HV package is kept:

setenv HV_LOCATION  $CLAS_PACK/Hv

# Identify location of MySQL files

setenv MYSQL_INCLUDE_PATH /group/clas/mysql/$OS_NAME/include
setenv MYSQL_LIB_PATH /group/clas/mysql/$OS_NAME/lib

# done with claspc26.cshrc
