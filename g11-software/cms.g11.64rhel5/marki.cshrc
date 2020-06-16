setenv POSIX_SHELL /usr/bin/sh
setenv CLAS_LOCATION /usr/local

if (!($?CLAS_ROOT)) setenv CLAS_ROOT  $CLAS_LOCATION/clas
setenv BUILDS     $CLAS_ROOT/builds
setenv CLAS_PROD  $BUILDS/prod
setenv CLAS_DEVEL $BUILDS/devel

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
    setenv CLAS_BUILD $CLAS_ROOT/builds/prod
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

alias setup_clas source /group/clas/builds/PRODUCTION/packages/cms/jlab.cshrc

setenv CLAS_PARMS $CLAS_ROOT/parms

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_ROOT/clsrc/recsis/runtime

setenv MYSQL_INCLUDE_PATH /usr/include/mysql
setenv MYSQL_LIB_PATH /usr/lib/mysql

# protect user against old TCL enviroment variables

setenv TCL_INC /usr/include
setenv TCL_LIB /usr/lib

# Need to define where the HV package is kept:

setenv HV_LOCATION  $CLAS_PACK/Hv

setenv CERN /apps/cernlib/i386_rhel3
setenv CERN_LEVEL 2003
setenv CERN_ROOT $CERN/$CERN_LEVEL

setenv ROOTSYS /apps/root/3.05-07-gcc3.2.3/root
set CLAS_LD_LIBRARY_PATH = ${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}:${ROOTSYS}/lib:${MYSQL_LIB_PATH}:${TCL_LIB}
if ($?LD_LIBRARY_PATH) then
  setenv LD_LIBRARY_PATH ${CLAS_LD_LIBRARY_PATH}:${LD_LIBRARY_PATH}
else
  setenv LD_LIBRARY_PATH ${CLAS_LD_LIBRARY_PATH}
endif

# end of marki.cshrc
