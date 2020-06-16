#
# Place these enviroment variable definitions in your .cshrc
# NOTE: to toggle between PRODUCTION and DEVELOPMENT you need to
#       redefine CLAS_LEVEL to your choice
#
echo sourcing saclaypc1.cshrc

setenv OSCLAS "`uname`"
setenv POSIX_SHELL /usr/bin/sh

# change the username to your username on JLABS1

setenv CVSROOT    :pserver:$USER@jlabs1.cebaf.gov:/group/clas/clas_cvs

# define the top of the "clas" tree on your local box

setenv CLAS_LOCATION /group
if (!($?CLAS_ROOT)) setenv CLAS_ROOT  $CLAS_LOCATION/clas
setenv BUILDS     $CLAS_ROOT/builds
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT

# THE USER should be able to toggle this to development or production
#
# development or production or private?
#
if ($#argv == 0) then
  if ($?CLAS_BUILD) then
    echo '$CLAS_BUILD' already defined: using existing definition of CLAS_BUILD
  else
     echo '$CLAS_BUILD' not defined: using PRODUCTION
    setenv CLAS_BUILD $CLAS_PROD
  endif
else 
  echo using $argv[1]
  setenv CLAS_BUILD $BUILDS/$argv[1]
endif
setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS
setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS
setenv CLAS_PACK    $CLAS_BUILD/packages
setenv CLAS_CMS     $CLAS_PACK/cms
setenv CLAS_SCRIPTS $CLAS_PACK/scripts
setenv RECSIS       $CLAS_PACK

if ( -e /group/clas/builds/DEVELOPMENT/packages/cms/saclaypc1.cshrc ) then 
alias use_dev  source /group/clas/builds/DEVELOPMENT/packages/cms/saclaypc1.cshrc DEVELOPMENT
else
alias  use_dev         'setenv CLAS_LEVEL   DEVELOPMENT; \
                        setenv CLAS_BUILD   $CLAS_DEVEL; \
                        setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS; \
                        setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS; \
                        setenv CLAS_PACK    $CLAS_BUILD/packages; \
                        setenv CLAS_CMS     $CLAS_PACK/cms; \
                        setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
                        setenv RECSIS       $CLAS_PACK; '

endif

if ( -e /group/clas/builds/PRODUCTION/packages/cms/saclaypc1.cshrc ) then 
alias use_prod source /group/clas/builds/PRODUCTION/packages/cms/saclaypc1.cshrc PRODUCTION
else
alias use_prod         'setenv CLAS_LEVEL   PRODUCTION; \
                        setenv CLAS_BUILD   $CLAS_PROD; \
                        setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS; \
                        setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS; \
                        setenv CLAS_PACK    $CLAS_BUILD/packages; \
                        setenv CLAS_CMS     $CLAS_PACK/cms; \
                        setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
                        setenv RECSIS       $CLAS_PACK; '
endif

if ( -e /group/clas/builds/LATEST/packages/cms/saclaypc1.cshrc ) then 
alias use_daily source /group/clas/builds/LATEST/packages/cms/saclaypc1.cshrc LATEST
else
alias use_daily        'setenv CLAS_LEVEL   LATEST; \
                        setenv CLAS_BUILD   $BUILDS/$CLAS_LEVEL; \
                        setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS; \
                        setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS; \
                        setenv CLAS_PACK    $CLAS_BUILD/packages; \
                        setenv CLAS_CMS     $CLAS_PACK/cms; \
                        setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
                        setenv RECSIS       $CLAS_PACK; '
endif

if ( -e /group/clas/builds/TEST/packages/cms/saclaypc1.cshrc ) then 
alias use_test source /group/clas/builds/TEST/packages/cms/saclaypc1.cshrc TEST
else
alias  use_test        'setenv CLAS_LEVEL   TEST; \
                        setenv CLAS_BUILD   $BUILDS/$CLAS_LEVEL; \
                        setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS; \
                        setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS; \
                        setenv CLAS_PACK    $CLAS_BUILD/packages; \
                        setenv CLAS_CMS     $CLAS_PACK/cms; \
                        setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
                        setenv RECSIS       $CLAS_PACK; '
endif

if ( -e /group/clas/builds/PRODUCTION/packages/cms/saclaypc1.cshrc ) then 
alias setup_clas source /group/clas/builds/PRODUCTION/packages/cms/saclaypc1.cshrc
else
alias setup_clas       'setenv CLAS_LEVEL   PRODUCTION; \
                        setenv CLAS_BUILD   $BUILDS/$CLAS_LEVEL; \
                        setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS; \
                        setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS; \
                        setenv CLAS_PACK    $CLAS_BUILD/packages; \
                        setenv CLAS_CMS     $CLAS_PACK/cms; \
                        setenv CLAS_SCRIPTS $CLAS_PACK/scripts; \
                        setenv RECSIS       $CLAS_PACK; '
endif

# you should define these appropriately for the PARMS or DATA area

setenv CLAS_DATA  $CLAS_LOCATION/DATA
setenv CLAS_PARMS $CLAS_ROOT/parms

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

setenv RECSIS_RUNTIME $CLAS_PARMS

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB

# setup CERN STUFF

setenv CERN /usr/share/cernlib/pc_linux

alias cern_96a 'setenv CERN_LEVEL 96a; \
		setenv CERN_ROOT $CERN/$CERN_LEVEL; \
		setenv CERN_LIB $CERN/$CERN_LEVEL/lib; \
		setenv CERN_BIN $CERN/$CERN_LEVEL/bin'

alias cern_97a 'setenv CERN_LEVEL 97a; \
		setenv CERN_ROOT $CERN/$CERN_LEVEL; \
		setenv CERN_LIB $CERN/$CERN_LEVEL/lib; \
		setenv CERN_BIN $CERN/$CERN_LEVEL/bin'

cern_97a

# end of claspc1.cshrc


# Need to define where the HV package is kept:

setenv HV_LOCATION $CLAS_PACK/Hv

echo end of saclaypc1.cshrc