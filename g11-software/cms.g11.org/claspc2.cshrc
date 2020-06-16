# $Id: claspc2.cshrc,v 1.15 2001/05/22 17:10:52 marki Exp $
#
if (!($?CLAS_ROOT)) setenv CLAS_ROOT /home/claslib
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

alias use_dev  source /group/clas/builds/DEVELOPMENT/packages/cms/claspc2.cshrc DEVELOPMENT
alias use_prod source /group/clas/builds/PRODUCTION/packages/cms/claspc2.cshrc PRODUCTION
alias use_daily source /group/clas/builds/LATEST/packages/cms/claspc2.cshrc LATEST
alias use_test source /group/clas/builds/TEST/packages/cms/claspc2.cshrc TEST
alias setup_clas source /group/clas/builds/PRODUCTION/packages/cms/claspc2.cshrc

setenv CLAS_PARMS $CLAS_ROOT/parms
setenv CLAS_TOOLS $CLAS_ROOT/tools

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME /home/claslib/recsis/runtime

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB

# Need to define where the HV package is kept:

setenv HV_LOCATION  $CLAS_PACK/Hv

# Identify location of MySQL files

setenv MYSQL_INCLUDE_PATH /group/clas/mysql/$OS_NAME/include
setenv MYSQL_LIB_PATH /group/clas/mysql/$OS_NAME/lib

# done with claspc2.cshrc
