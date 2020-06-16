# CLAS setup file - csh version
#

setenv POSIX_SHELL /usr/bin/sh
setenv CLAS_LOCATION /usr/local/clas
setenv CLAS_ROOT   $CLAS_LOCATION
setenv CLAS_BUILDS $CLAS_ROOT/BUILDS
setenv CVSROOT pricej@cvs.jlab.org:/group/clas/clas_cvs
setenv CVS_RSH `which ssh`

setenv CLAS_PARMS $CLAS_ROOT/PARMS
setenv HV_LOCATION  $CLAS_BUILDS/packages/Hv
setenv CLAS_PACK    $CLAS_BUILDS/PRODUCTION/packages
setenv CLAS_CMS     $CLAS_PACK/cms
setenv OSCLAS "`$CLAS_CMS/uname_clas`"
setenv CLAS_LIB     $CLAS_BUILDS/PRODUCTION/lib/$OSCLAS
setenv CLAS_SLIB    $CLAS_BUILDS/PRODUCTION/slib/$OSCLAS
setenv CLAS_BIN     $CLAS_BUILDS/PRODUCTION/bin/$OSCLAS
setenv CLAS_SCRIPTS $CLAS_PACK/scripts
setenv RECSIS       $CLAS_PACK
setenv RECSIS_RUNTIME $CLAS_PARMS

# Get LD_LIBRARY_PATH before it gets modified 
if (!($?LD_LIBRARY_PATH_0)) then
 if (($?LD_LIBRARY_PATH)) then
    setenv LD_LIBRARY_PATH_0 $LD_LIBRARY_PATH
 else
    setenv LD_LIBRARY_PATH_0 :
# simplest trick I though of, I guess this is not harm
 endif
endif 

if (!($?TOP_DIR)) then
  echo Warning: TOP_DIR is not defined yet, LD_LIBRARY_PATH won\'t point to your local shared libraries.
  echo -------- You might want to define TOP_DIR BEFORE you source this file.
  setenv TOP_DIR :
endif


# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB

# use 2001 version of CERN software
setenv CERN /usr/local/cern
setenv CERN_LEVEL 2003
setenv CERN_ROOT $CERN/$CERN_LEVEL
setenv CERN_LIB $CERN_ROOT/lib
setenv CERN_BIN $CERN_ROOT/bin
setenv ROOTSYS /usr/local/cern/root
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${ROOTSYS}/lib

# some useful cvs aliases
setenv MYSQL_INCLUDE_PATH /usr/include/mysql
setenv MYSQL_LIB_PATH /usr/lib/mysql

setenv PATH ${PATH}:${CERN_BIN}:${CLAS_BIN}:${CLAS_SCRIPTS}:${ROOTSYS}/bin

# some useful cvs aliases
setenv MYSQL_INCLUDE_PATH /usr/include/mysql
setenv MYSQL_LIB_PATH /usr/lib/mysql

