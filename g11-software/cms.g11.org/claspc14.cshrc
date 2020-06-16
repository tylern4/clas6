# $Id: claspc14.cshrc,v 1.4 2003/05/13 19:22:31 marki Exp $
#
setenv CLAS_ROOT  /home/claslib
setenv CVSROOT    /group/clas/clas_cvs
setenv BUILDS     $CLAS_ROOT/builds

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
setenv OS_NAME      `$CLAS_CMS/uname_clas`
setenv CLAS_LIB     $CLAS_BUILD/lib/$OS_NAME
setenv CLAS_BIN     $CLAS_BUILD/bin/$OS_NAME
setenv CLAS_SCRIPTS $CLAS_PACK/scripts
setenv RECSIS       $CLAS_PACK

alias setup_clas source $BUILDS/PRODUCTION/packages/cms/claspc14.cshrc
alias use_dev setup_clas DEVELOPMENT
alias use_prod setup_clas PRODUCTION
alias use_latest setup_clas LATEST

setenv CLAS_PARMS $CLAS_ROOT/parms
setenv CLAS_TOOLS $CLAS_ROOT/tools

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_ROOT/recsis/runtime

# Need to define where the HV package is kept:

setenv HV_LOCATION  $CLAS_PACK/Hv

# Identify location of MySQL files

setenv MYSQL_INCLUDE_PATH /usr/include/mysql
setenv MYSQL_LIB_PATH /usr/lib/mysql

# set up CERN directories

setenv CERN /usr/local/cern
setenv CERN_LEVEL 2002
setenv CERN_ROOT $CERN/$CERN_LEVEL

# done with claspc14.cshrc
