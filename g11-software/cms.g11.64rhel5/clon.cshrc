#
# $Id: clon.cshrc,v 1.6 2001/10/30 23:17:37 boiarino Exp $
#
# Place these environment variable definitions in your .cshrc or .login

setenv POSIX_SHELL   /usr/bin/sh
setenv CLAS_LOCATION /group
setenv CVSROOT       $CLAS_LOCATION/clas/clas_cvs
setenv CLAS_WWW      $CLAS_LOCATION/www/html
setenv CLAS_FTP      /site/ftp/pub/clas
setenv CLAS_DOC      /site/ftp/pub/clas/doc

if (!($?CLAS_ROOT)) setenv CLAS_ROOT  $CLAS_LOCATION/clas
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

alias use_dev  source /group/clas/builds/DEVELOPMENT/packages/cms/jlab.cshrc DEVELOPMENT
alias use_prod source /group/clas/builds/PRODUCTION/packages/cms/jlab.cshrc PRODUCTION
alias use_latest source /group/clas/builds/LATEST/packages/cms/jlab.cshrc LATEST
alias setup_clas source /group/clas/builds/PRODUCTION/packages/cms/jlab.cshrc

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

setenv MYSQL_INCLUDE_PATH /usr/local/clas/offline/mysql/include/mysql
setenv MYSQL_LIB_PATH /usr/local/clas/offline/mysql/lib/mysql

# done with jlab.cshrc







