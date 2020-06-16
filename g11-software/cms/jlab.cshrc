#
# $Id: jlab.cshrc,v 1.58 2008/09/19 00:13:21 turonski Exp $
#
# Place these environment variable definitions in your .cshrc or .login

setenv POSIX_SHELL /usr/bin/sh
setenv CLAS_LOCATION /group

if (!($?CLAS_ROOT)) setenv CLAS_ROOT  $CLAS_LOCATION/clas
setenv CVSROOT    $CLAS_ROOT/clas_cvs
setenv BUILDS     $CLAS_ROOT/builds
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT
setenv CLAS_PREV  $BUILDS/PREVIOUS

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

#setenv CLAS_BUILD   /group/clas/builds/release-f8-1

setenv CLAS_PACK    $CLAS_BUILD/packages
setenv CLAS_CMS     $CLAS_PACK/cms
setenv CLAS_SCRIPTS $CLAS_PACK/scripts
setenv OSCLAS       "`${CLAS_CMS}/uname_clas`"
setenv OS_NAME      `$CLAS_CMS/uname_clas`
setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS
setenv CLAS_SLIB    $CLAS_BUILD/slib/$OSCLAS
setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS
setenv RECSIS       $CLAS_PACK
setenv CLASTOOL     $CLAS_PACK/ClasTool

alias use_dev  source /group/clas/builds/DEVELOPMENT/packages/cms/jlab.cshrc DEVELOPMENT
alias use_prod source /group/clas/builds/PRODUCTION/packages/cms/jlab.cshrc PRODUCTION
alias use_latest source /group/clas/builds/LATEST/packages/cms/jlab.cshrc LATEST
alias use_previous source /group/clas/builds/PREVIOUS/packages/cms/jlab.cshrc PREVIOUS

alias setup_clas source /group/clas/builds/PRODUCTION/packages/cms/jlab.cshrc

setenv CLAS_PARMS $CLAS_ROOT/parms
setenv CLAS_TOOLS $CLAS_ROOT/tools

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_ROOT/clsrc/recsis/runtime

# protect user against old TCL enviroment variables

if (( $OS_NAME == LinuxRHEL5) || ( $OS_NAME == LinuxRHFC8)) then
    setenv TCL_VERSION   8.4
    setenv TK_LIBRARY    /usr/lib/tk${TCL_VERSION}
    setenv TCL_LIBRARY   /usr/lib/tcl${TCL_VERSION}
    setenv TCLINSTALLDIR /usr/share/tcl${TCL_VERSION}
    setenv TCL_INC       /usr/include
    setenv TCL_LIB       /usr/lib
else
    setenv TCL_VERSION 8.3
    setenv TCL_INC /apps/tcl/include
    setenv TCL_LIB /apps/tcl/lib
endif

# Need to define where the HV package is kept:

setenv HV_LOCATION  $CLAS_PACK/Hv

# Setup FORTRAN compiler

if (( $OS_NAME == LinuxRHEL5) || ( $OS_NAME == LinuxRHFC8)) then
    setenv GFORTRAN 1
endif

# Identify location of MySQL files

# setup mysql environment
if (( $OS_NAME == LinuxRHEL5) || ( $OS_NAME == LinuxRHFC8)) then
    setenv MYSQL_INCLUDE_PATH /usr/include/mysql
    setenv MYSQL_LIB_PATH     /usr/lib/mysql
    setenv MYSQL              /usr/share/mysql
else
    setenv MYSQL_INCLUDE_PATH /apps/mysql/include
    setenv MYSQL_LIB_PATH     /apps/mysql/lib
endif

# Set up CERNLIB

if (`uname` == 'SunOS') then 
    use root
    setenv CERN /apps/cernlib
    setenv CERN_LEVEL 2001
else if ( $OS_NAME == LinuxRHEL3) then
    use gcc
    use root
    setenv CERN /apps/cernlib/i386_rhel3
    setenv CERN_LEVEL 2004
else if ( $OS_NAME == LinuxRHEL4) then		    
    use gcc
    use root
    setenv CERN /apps/cernlib/i386_rhel4
    setenv CERN_LEVEL 2005
else if ( $OS_NAME == LinuxRHEL5) then		    
    setenv CERN /apps/cernlib/i386_rhel5
    setenv CERN_LEVEL 2005
else if ( $OS_NAME == LinuxRHFC8) then		    
    setenv CERN /apps/cernlib/i386_fc8
    setenv CERN_LEVEL 2005
#    setenv CERN /usr/lib/cernlib
#    setenv CERN_LEVEL 2006
else
    setenv CERN /usr/lib/cernlib
    setenv CERN_LEVEL 2006
endif
setenv ROOTSYS /apps/root/PRO/root
setenv ROOTLIB $ROOTSYS/lib
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
