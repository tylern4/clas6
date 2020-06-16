#
# Place these enviroment variable definitions in your .cshrc
# NOTE: to toggle between PRODUCTION and DEVELOPMENT you need to
#       redefine CLAS_LEVEL to your choice
#
# Added shared libraris environement variables CLAS/SLIB & LD_LIBRARY_PATH
#

setenv OSCLAS "`uname`"
setenv POSIX_SHELL /usr/bin/sh
setenv CLAS_LOCATION /group

setenv CLAS_ROOT  $CLAS_LOCATION/clas
setenv CVSROOT    $CLAS_ROOT/clas_cvs
setenv BUILDS     $CLAS_ROOT/builds
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT

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

#  the user should be able to toggle this to development or production

alias  use_dev         'setenv CLAS_LEVEL   DEVELOPMENT; \
			setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
			setenv CLAS_SLIB     $BUILDS/$CLAS_LEVEL/slib/$OSCLAS; \
			setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
			setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
			setenv CLAS_CMS     $CLAS_PACK/cms; \
			setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
       setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH_0}:${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}; \
			setenv RECSIS       $CLAS_PACK'

alias  use_prod        'setenv CLAS_LEVEL   PRODUCTION; \
			setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
			setenv CLAS_SLIB     $BUILDS/$CLAS_LEVEL/slib/$OSCLAS; \
			setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
			setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
			setenv CLAS_CMS     $CLAS_PACK/cms; \
			setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
        setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH_0}:${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}; \
			setenv RECSIS       $CLAS_PACK'

alias  use_late        'setenv CLAS_LEVEL   LATEST; \
			setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; \
			setenv CLAS_SLIB     $BUILDS/$CLAS_LEVEL/slib/$OSCLAS; \
			setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; \
			setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; \
			setenv CLAS_CMS     $CLAS_PACK/cms; \
			setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
        setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH_0}:${TOP_DIR}/slib/${OSCLAS}:${CLAS_SLIB}; \
			setenv RECSIS       $CLAS_PACK'

use_dev

setenv CLAS_DATA  /work/clas
setenv CLAS_PARMS $CLAS_ROOT/parms

setenv CLAS_WWW   /site/www/html/clas
setenv CLAS_FTP   /site/ftp/pub/clas
setenv CLAS_DOC   /site/ftp/pub/clas/doc

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_ROOT/clsrc/recsis/runtime

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB


