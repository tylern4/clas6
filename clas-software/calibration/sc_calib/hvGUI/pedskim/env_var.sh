#!/bin/csh -f
#use this file for performing the TOF HV calibration

setenv RUN_USER  carman
setenv TOP_DIR   /work/clas/disk1/carman/tof_cosmic/

setenv CSQL_DBHOST clasdb
setenv CSQL_USER   offline_eg3                    # database user name
setenv CSQL_DB     eg3_offline                    # database name
setenv CSQL_DDL    ${TOP_DIR}/clas/packages/bankdefs/csql.ddl  # ddl file name
setenv CSQL_CALIB  ${TOP_DIR}/clas/packages/bankdefs/calb.ddl  # calib. constants DDL

setenv CLAS_CALDB_RUNINDEX calib_user.RunIndexEG3a

setenv MYSQL_INCLUDE_PATH /apps/mysql/include/mysql
setenv MYSQL_LIB_PATH /apps/mysql/lib/mysql

setenv ROOTSYS /u/apps/root/PRO/root
setenv ROOTLIB $ROOTSYS/lib
setenv CLAS_PARMS /work/clas/production/claseg3/PARMS
setenv CLAS_ROOT $TOP_DIR/clas
#
# Discover what we are running on and set the bin path properly.
#
set UNAME=`uname`
if ( ${UNAME} == "Linux" ) then
    setenv OS_VERSION `awk '/release/{if(loc=match($0,/Enterprise Linux /)){printf 
      "EL"};print substr($0,match($0,/[123456789]/),1)}' /etc/redhat-release`
    setenv OS_NAME "LinuxRH${OS_VERSION}"
else
  setenv OS_NAME ${UNAME}
endif

setenv CERN /apps/cernlib/i386_rhel3
setenv CERN_LEVEL 2003
setenv CERN_ROOT $CERN/$CERN_LEVEL
setenv CERN_LIB /apps/cernlib/i386_redhat72_gcc3/2001/lib

setenv CLAS_LIB $CLAS_ROOT/lib/${OS_NAME}
setenv CLAS_SLIB $CLAS_ROOT/slib/${OS_NAME}
setenv CLAS_PACK $CLAS_ROOT/packages
setenv CLAS_CMS $CLAS_PACK/cms
setenv CLAS_SCRIPTS $CLAS_PACK/scripts
setenv RECSIS $CLAS_ROOT/packages
setenv USER_BIN ${CLAS_PACK}/bin/${OS_NAME}

setenv CVS_RSH ssh
setenv CVSROOT pmatt@cvs.jlab.org:/group/clas/clas_cvs

set path = (${ROOTSYS}/bin /apps/bin /u/home/${RUN_USER}/bin/${OS_NAME}  
           /u/home/${RUN_USER}/bin/scripts $path)
setenv LD_LIBRARY_PATH ${CERN_LIB}:${CLAS_SLIB}:${CLAS_LIB}:${ROOTLIB}:${MYSQL_LIB_PATH}
