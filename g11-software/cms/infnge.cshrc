#
# $Id: infnge.cshrc,v 1.1 2001/07/09 08:03:00 vvsap Exp $
#
# Place these environment variable definitions in your .cshrc or .login

setenv POSIX_SHELL /usr/bin/sh
#setenv CLAS_LOCATION /usr/local
setenv CLAS_LOCATION /scratch

#if (!($?CLAS_ROOT)) setenv CLAS_ROOT  $CLAS_LOCATION/clas
 
setenv CLAS_ROOT  $CLAS_LOCATION/clas
 
# change the username to your username on JLABS1
 
if($USER == 'batta' )then
    setenv CVSROOT    :pserver:battagli@jlabs1.cebaf.gov:/group/clas/clas_cvs
else
    setenv CVSROOT    :pserver:$USER@jlabs1.cebaf.gov:/group/clas/clas_cvs
endif

# setenv CVSROOT    $CLAS_ROOT/clas_cvs
setenv BUILDS     $CLAS_ROOT
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT

if (!($?TOP_DIR)) then
  setenv TOP_DIR /home/users/$USER
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
    setenv CLAS_BUILD $CLAS_ROOT/PRODUCTION
  endif
else 
  setenv CLAS_BUILD $CLAS_ROOT/$argv[1]
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

alias use_dev  source $CLAS_BUILD/DEVELOPMENT/packages/cms/infnge.cshrc DEVELOPMENT
alias use_prod source $CLAS_BUILD/PRODUCTION/packages/cms/infnge.cshrc PRODUCTION
alias use_latest source $CLAS_BUILD/LATEST/packages/cms/infnge.cshrc LATEST

alias setup_clas source $CLAS_BUILD/PRODUCTION/packages/cms/jlab.cshrc

setenv CLAS_PARMS $CLAS_ROOT/parms
setenv CLAS_TOOLS $CLAS_ROOT/tools

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_ROOT/parms

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB

# Need to define where the HV package is kept:

setenv HV_LOCATION  $CLAS_PACK/Hv

# Identify location of MySQL files

setenv MYSQL_INCLUDE_PATH $CLAS_ROOT/mysql/$OS_NAME/include
setenv MYSQL_LIB_PATH $CLAS_ROOT/mysql/$OS_NAME/lib

# done with infnge.cshrc
