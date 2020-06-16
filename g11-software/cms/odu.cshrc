setenv OSCLAS "`uname`"
setenv POSIX_SHELL /usr/bin/sh

# change the username to your username on JLABS1

# define the top of the "clas" tree on your local box

if (!($?CLAS_ROOT)) setenv CLAS_ROOT /clascvsb
setenv CVSROOT    $CLAS_ROOT/clas_cvs
setenv BUILDS     $CLAS_ROOT/builds
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT

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

alias setup_clas source $CLAS_CMS/odu.cshrc

setenv CLAS_PARMS $CLAS_ROOT/parms

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME you_need_to_set_RECSIS_RUNTIME_to_run_RECSIS
