export MYSQLINC=/usr/include/mysql
export MYSQLLIB=/usr/lib64/mysql
export CLAS_PARMS=/clas/parms
export RECSIS_RUNTIME=$CLAS_PARMS/recsis/runtime
export PATH=/clas_software/build/bin:$PATH

export PS1="[docker:\w]$ "

source $ROOTSYS/bin/thisroot.sh

export CLAS_CALDB_HOST=$CLASDB_PORT_3306_TCP_ADDR
export CLAS_CALDB_PASS=""
export CLAS_CALDB_USER="root"
