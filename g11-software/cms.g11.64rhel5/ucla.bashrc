# CLAS setup file - bash version
#

export POSIX_SHELL=/usr/bin/sh
export CLAS_LOCATION=/usr/local/clas
export CLAS_ROOT=$CLAS_LOCATION
export CLAS_BUILDS=$CLAS_ROOT/builds
export CVSROOT=pricej@cvs.jlab.org:/group/clas/clas_cvs
export CVS_RSH=`which ssh`

export CLAS_PARMS=$CLAS_ROOT/parms
export HV_LOCATION=$CLAS_BUILDS/packages/Hv
export CLAS_PACK=$CLAS_BUILDS/PRODUCTION/packages
export CLAS_CMS=$CLAS_PACK/cms
export OSCLAS="`$CLAS_CMS/uname_clas`"
export CLAS_LIB=$CLAS_BUILDS/lib/$OSCLAS
export CLAS_SLIB=$CLAS_BUILDS/slib/$OSCLAS
export CLAS_BIN=$CLAS_BUILDS/bin/$OSCLAS
export CLAS_SCRIPTS=$CLAS_PACK/scripts
export RECSIS=$CLAS_PACK
export RECSIS_RUNTIME=$CLAS_PARMS

# protect user against old TCL enviroment variables
export TCL_INC
export TCL_LIB

export CERN=/usr/local/cern
export CERN_LEVEL=2003
export CERN_ROOT=$CERN/$CERN_LEVEL
export CERN_LIB=$CERN_ROOT/lib
export CERN_BIN=$CERN_ROOT/bin
export ROOTSYS=/usr/local/cern/root
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}:${ROOTSYS}/lib

export PATH=${PATH}:${CERN_BIN}:${CLAS_BIN}:${CLAS_SCRIPTS}:${ROOTSYS}/bin

# some useful cvs aliases
export MYSQL_INCLUDE_PATH=/usr/include/mysql
export MYSQL_LIB_PATH=/usr/lib/mysql

