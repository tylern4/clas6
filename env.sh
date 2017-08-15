export ROOTSYS=/usr/local/root
export MYSQLINC=/usr/include/mysql
export MYSQLLIB=/usr/lib64/mysql
export CLAS_PARMS=/clas/parms
export PATH=/clas-software/build/bin:$PATH
export CERN=/usr/local/cernlib/x86_64_rhel6
export CERN_LEVEL=2005
export CERN_ROOT=$CERN/$CERN_LEVEL
export CVSCOSRC=$CERN/$CERN_LEVEL/src
export PATH=$CERN/$CERN_LEVEL/src:$PATH
export CERN_LIB=$CERN_ROOT/lib
export CERN_BIN=$CERN_ROOT/bin
export CLAS_TOOL=/clas-software/analysis/ClasTool
export PATH=$PATH:$CLAS_TOOL/bin/Linux
export LD_LIBRARY_PATH=$ROOTSYS/lib:$CLAS_TOOL/slib/Linux

source $ROOTSYS/bin/thisroot.sh

#export CLAS_CALDB_HOST="127.0.0.1"
export CLAS_CALDB_HOST=$CLASDB_PORT_3306_TCP_ADDR
export CLAS_CALDB_PASS=""
export CLAS_CALDB_USER="root"
