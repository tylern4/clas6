#
# $Id: jlab.cshrc,v 1.54 2004/03/18 19:05:20 claslib Exp $
#
# Place these environment variable definitions in your .cshrc or .login

setenv POSIX_SHELL /usr/bin/sh
setenv CLAS_LOCATION /group

if (!($?CLAS_ROOT)) setenv CLAS_ROOT  $CLAS_LOCATION/clas
setenv CVSROOT    $CLAS_ROOT/clas_cvs
setenv BUILDS     $CLAS_ROOT/builds
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT

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

alias use_dev  source /group/clas/builds/DEVELOPMENT/packages/cms/jlab.cshrc DEVELOPMENT
alias use_prod source /group/clas/builds/PRODUCTION/packages/cms/jlab.cshrc PRODUCTION
alias use_latest source /group/clas/builds/LATEST/packages/cms/jlab.cshrc LATEST

alias setup_clas source /group/clas/builds/PRODUCTION/packages/cms/jlab.cshrc

setenv CLAS_PARMS $CLAS_ROOT/parms
setenv CLAS_TOOLS $CLAS_ROOT/tools

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_ROOT/clsrc/recsis/runtime

# protect user against old TCL enviroment variables

setenv TCL_INC /apps/tcl/include
setenv TCL_LIB /apps/tcl/lib
setenv TCL_VERSION 8.3

# Need to define where the HV package is kept:

setenv HV_LOCATION  $CLAS_PACK/Hv

# Identify location of MySQL files

setenv MYSQL_INCLUDE_PATH /apps/mysql/include/mysql
setenv MYSQL_LIB_PATH /apps/mysql/lib/mysql

if (`uname` == 'SunOS') then 
    use root/3.03-06
    setenv CERN /apps/cernlib
    setenv CERN_LEVEL 2001
else if ($OS_NAME == 'LinuxRH7') then
    use gcc/3.0.4
    use root/3.03-06-gcc3.0.4
    setenv ROOTSYS /apps/root/3.03-06-gcc3.0.4/root
    setenv CERN /apps/cernlib/i386_redhat72_gcc3
    setenv CERN_LEVEL 2001
else if ($OS_NAME == 'LinuxRHEL3') then
    use gcc
    use root
    setenv ROOTSYS /apps/root/PRO/root
    setenv CERN /apps/cernlib/i386_rhel3
    setenv CERN_LEVEL 2003
else if (`uname` == 'Linux') then
    use gcc
    use root
    use cernlib
endif
setenv CERN_ROOT $CERN/$CERN_LEVEL

set CLAS_LD_LIBRARY_PATH = ${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}:${ROOTSYS}/lib:${MYSQL_LIB_PATH}:${TCL_LIB}
if ($?LD_LIBRARY_PATH) then
  setenv LD_LIBRARY_PATH ${CLAS_LD_LIBRARY_PATH}:${LD_LIBRARY_PATH}
else
  setenv LD_LIBRARY_PATH ${CLAS_LD_LIBRARY_PATH}
endif

# define location of CVS repository

setenv CVSROOT /group/clas/clas_cvs
setenv CVSEDITOR emacs

# done with jlab.cshrc
