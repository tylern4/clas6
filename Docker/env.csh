setenv ROOTSYS /usr/local/root
setenv MYSQLINC /usr/include/mysql
setenv MYSQLLIB /usr/lib64/mysql
setenv CLAS_PARMS /group/clas/parms
setenv PATH /usr/local/clas-software/build/bin:$PATH
setenv CERN /usr/local/cernlib/x86_64_rhel6
setenv CERN_LEVEL 2005
setenv CERN_ROOT $CERN/$CERN_LEVEL
setenv CVSCOSRC $CERN/$CERN_LEVEL/src
setenv PATH $CERN/$CERN_LEVEL/src:$PATH
setenv CERN_LIB $CERN_ROOT/lib
setenv CERN_BIN $CERN_ROOT/bin
setenv CLAS_TOOL /usr/local/clas-software/analysis/ClasTool
setenv PATH $PATH:$CLAS_TOOL/bin/Linux
setenv LD_LIBRARY_PATH $ROOTSYS/lib:$CLAS_TOOL/slib/Linux

source $ROOTSYS/bin/thisroot.csh

