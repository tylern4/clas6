#
# Place these enviroment variable definitions in your .cshrc
# NOTE: to toggle between PRODUCTION and DEVELOPMENT you need to
#       redefine CLAS_LEVEL to your choice
#
setenv OSCLAS "`uname`"
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

if (!($?TOP_DIR)) then
  echo Warning: TOP_DIR is not defined yet, LD_LIBRARY_PATH won\'t point to your local shared libraries.
  echo -------- You might want to define TOP_DIR BEFORE you source this file.
  setenv TOP_DIR :
endif

# change the username to your username on JLABS1

if($USER == 'hadjida' )then
	setenv CVSROOT    :pserver:cynthia@jlabs1.cebaf.gov:/group/clas/clas_cvs
else
	setenv CVSROOT    :pserver:$USER@jlabs1.cebaf.gov:/group/clas/clas_cvs
endif

# define the top of the "clas" tree on your local box

setenv CLAS_LOCATION /import/projet2/phase/meson/clas/

#
setenv CLAS_ROOT  $CLAS_LOCATION
setenv BUILDS     $CLAS_ROOT
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT
#setenv TOP_DIR    $CLAS_DEVEL
setenv CERN_ROOT  $CERN/cern

# Need to define where the HV package is kept:

setenv HV_LOCATION  $CLAS_ROOT/Hv/Hv

#  the user should be able to toggle this to development or production

alias  use_dev         'setenv CLAS_LEVEL   DEVELOPMENT; \
			setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
			setenv CLAS_SLIB     $BUILDS/$CLAS_LEVEL/slib/$OSCLAS; \
			setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
			setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
			setenv CLAS_CMS     $CLAS_PACK/cms; \
			setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
			setenv TOP_DIR $BUILDS/$CLAS_LEVEL; \
        setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH_0}:${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}; \
			setenv RECSIS       $CLAS_PACK; \
			setenv CLAS_BUILD /home/hadjida/clas/$CLAS_LEVEL; \
			echo "CLAS enviroment now set for DEVELOPMENT" '

alias  use_prod        'setenv CLAS_LEVEL   PRODUCTION; \
			setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
			setenv CLAS_SLIB     $BUILDS/$CLAS_LEVEL/slib/$OSCLAS; \
			setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
			setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
			setenv CLAS_CMS     $CLAS_PACK/cms; \
			setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
			setenv TOP_DIR $BUILDS/$CLAS_LEVEL; \
     setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH_0}:${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}:${CERN}; \
			setenv RECSIS       $CLAS_PACK; \
			setenv CLAS_BUILD /home/hadjida/clas/$CLAS_LEVEL; \
			echo "CLAS enviroment now set for PRODUCTION" '

use_dev



# you should define these appropriately for the PARMS or DATA area

setenv CLAS_DATA  /work/clas
setenv CLAS_PARMS $CLAS_ROOT/parms

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_PARMS

# protect user against old TCL enviroment variables

setenv TCL_INC /import/tcl/tcl/include
setenv TCL_LIB /import/tcl/tcl/lib

# Set the TOP dir to be more convenient at UNH:

#setenv TOP_DIR  $CLAS_ROOT

#use the old version of Sunwspro (version 4.2) 

use -o sunwspro

#problem with gmake of gnu often use in the Makefile as make

set path=( /import/gnu/gnu/bin $path)

#access to perl command

#cd $CLAS_PACK/scripts
#clasrename.sh


