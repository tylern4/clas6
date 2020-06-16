#
# OFFSITE.cshrc template.  OFFSITE librarians should edit this file so
# that it reflects the local installation of CLAS software.  Then
# give it a unique name, ie pitt.cshrc, odu.cshrc, then check it back
# into the CVS repository so you can version control it in the future.
# (that is do:  cvs add pitt.cshrc; 
#               cvs commit -m ".cshrc file for pitt" pitt.cshrc   )
#
#  GOOD LUCK!!!!!!
#

#setenv OSCLAS "`uname`"
export OSCLAS="`uname`"

#setenv POSIX_SHELL /usr/bin/sh
export POSIX_SHELL=/usr/bin/sh

# take the following line and place in YOUR .cshrc so you can get access
# to the clas repository

# setenv CVSROOT    :pserver:USERNAME@jlabs1.cebaf.gov:/apps/clas/u1/clas_cvs

# define the top of the "clas" tree on your local box

#setenv CLAS_LOCATION /gluon3/clas
export CLAS_LOCATION=/npp/src/clas

# you should define these appropriately for the PARMS or DATA area

#setenv CLAS_DATA  $CLAS_LOCATION/data
export CLAS_DATA=$CLAS_LOCATION/data

#setenv CLAS_PARMS $CLAS_LOCATION/parms
export CLAS_PARMS=$CLAS_LOCATION/parms

# the rest of the file should not need editting if the directory structure
# is similar to that found at jlab

#setenv CLAS_ROOT  $CLAS_LOCATION
export CLAS_ROOT=$CLAS_LOCATION

#setenv BUILDS     $CLAS_ROOT/builds
export BUILDS=$CLAS_ROOT/builds

#setenv CLAS_PROD  $BUILDS/PRODUCTION
export CLAS_PROD=$BUILDS/PRODUCTION

#setenv CLAS_DEVEL $BUILDS/DEVELOPMENT
export CLAS_DEVEL=$BUILDS/DEVELOPMENT

#setenv TOP_DIR    $CLAS_DEVEL
export TOP_DIR=$CLAS_DEVEL

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS

#setenv RECSIS_RUNTIME $CLAS_PARMS
export RECSIS_RUNTIME=$CLAS_PARMS

#  the user should be able to toggle this to development or production

#alias  use_dev         'setenv CLAS_LEVEL   DEVELOPMENT; setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; setenv CLAS_CMS     $CLAS_PACK/cms; setenv CLAS_SCRIPTS $CLAS_PACK/scripts; setenv RECSIS       $CLAS_PACK'
alias use_dev='export CLAS_LEVEL=DEVELOPMENT; export CLAS_LIB=$BUILDS/$CLAS_LEVEL/lib/$OSCLAS; export CLAS_BIN=$BUILDS/$CLAS_LEVEL/bin/$OSCLAS; export CLAS_PACK=$BUILDS/$CLAS_LEVEL/packages; export CLAS_CMS=$CLAS_PACK/cms; export CLAS_SCRIPTS=$CLAS_PACK/scripts; export RECSIS=$CLAS_PACK'

#alias  use_prod        'setenv CLAS_LEVEL   PRODCUTION; setenv CLAS_LIB     $BUILDS/$CLAS_LEVEL/lib/$OSCLAS; setenv CLAS_BIN     $BUILDS/$CLAS_LEVEL/bin/$OSCLAS; setenv CLAS_PACK    $BUILDS/$CLAS_LEVEL/packages; setenv CLAS_CMS     $CLAS_PACK/cms; setenv CLAS_SCRIPTS $CLAS_PACK/scripts; setenv RECSIS       $CLAS_PACK'
alias use_prod='export CLAS_LEVEL=PRODCUTION; export CLAS_LIB=$BUILDS/$CLAS_LEVEL/lib/$OSCLAS; export CLAS_BIN=$BUILDS/$CLAS_LEVEL/bin/$OSCLAS; export CLAS_PACK=$BUILDS/$CLAS_LEVEL/packages; export CLAS_CMS=$CLAS_PACK/cms; export CLAS_SCRIPTS=$CLAS_PACK/scripts; export RECSIS=$CLAS_PACK'

use_dev

#currently only suports AIX
export CERN=/dept/phys/photon4/cern/AIX/pro
export CERN_ROOT=${CERN}
export CERN_LIB=${CERN_ROOT}/lib
export CERN_BIN=${CERN_ROOT}/bin
source $CLAS_ROOT/Hv/Hv.bashrc
export HV_LOCATION=$HV_ROOT

# end of offsite.cshrc template


