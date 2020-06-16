export ROOTSYS=/usr/local/root
export MYSQLINC=/usr/include/mysql
export MYSQLLIB=/usr/lib64/mysql
export CLAS_PARMS=/group/clas/parms
export CLAS6=/usr/local/g11
export OS_NAME=LinuxDocker
export PATH=$CLAS6/bin/$OS_NAME:$PATH
export CERN=/usr/local/cernlib/x86_64_rhel6
export CERN_LEVEL=2005
export CERN_ROOT=$CERN/$CERN_LEVEL
export CVSCOSRC=$CERN/$CERN_LEVEL/src
export PATH=$CERN/$CERN_LEVEL/src:$PATH
export CERN_LIB=$CERN_ROOT/lib
export CERNLIB=$CERN_ROOT/lib
export CERN_BIN=$CERN_ROOT/bin
export LD_LIBRARY_PATH=$ROOTSYS/lib/root:$ROOTSYS/lib:$CLAS6/lib/$OS_NAME
export CLAS_CALDB_RUNINDEX="calib_user.RunIndexg11a"

#source $ROOTSYS/bin/thisroot.sh

