# Setup for CLAS analysis at IN2P3/CCPN
# 01/15/01 - FJS

echo CLAS Analysis setup

setenv ROOTSYS /afs/in2p3.fr/group/clas/librarian/root/pro
setenv OSCLAS "`uname`"
setenv OS_NAME "`uname`"
setenv POSIX_SHELL /usr/bin/sh

# Get LD_LIBRARY_PATH before it gets modified 
if (!($?LD_LIBRARY_PATH_0)) then
 if (($?LD_LIBRARY_PATH)) then
    setenv LD_LIBRARY_PATH_0 $LD_LIBRARY_PATH
 else
    setenv LD_LIBRARY_PATH_0 :
# simplest trick I though of, I guess this is not harm
 endif
endif 

# define the top of the "clas" tree on the CCPN

setenv CLAS_LOCATION /afs/in2p3.fr/group/clas/librarian

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
setenv CLAS_CMS     /afs/in2p3.fr/group/clas/librarian/LATEST/packages/cms
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
setenv CLAS_PARMS $CLAS_ROOT/parms

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_PARMS

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB
setenv TCL_INC /opt/gnu/include
setenv TCL_LIB "/usr/lib /opt/gnu/lib"

# Set the MYSQL variables for use with caldb...

setenv MYSQL_INCLUDE_PATH /usr/local/include/mysql
setenv MYSQL_LIB_PATH /usr/local/lib/mysql
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${MYSQL_LIB_PATH}

# Setup root
if ($?LD_LIBRARY_PATH) then
setenv LD_LIBRARY_PATH ${ROOTSYS}/lib:${LD_LIBRARY_PATH}
else
setenv LD_LIBRARY_PATH ${ROOTSYS}/lib
endif

# Get a better PATH
setenv PATH ${PATH}:${ROOTSYS}/bin:${CLAS_BIN}

# By default, $TOP_DIR=$HOME, you HAVE to change that

settop $HOME


