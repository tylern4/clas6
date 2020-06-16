#
# Place these enviroment variable definitions in your .cshrc
# NOTE: to toggle between PRODUCTION and DEVELOPMENT you need to
#       redefine CLAS_LEVEL to your choice
#
setenv OSCLAS "`uname`"
setenv POSIX_SHELL /usr/bin/sh

########################################################
# change this to point to your CDROM mount point
########################################################

setenv CDROM /claspc2/cdrom

setenv CLAS_LOCATION $CDROM

setenv CLAS_ROOT  $CLAS_LOCATION
setenv CVSROOT    :pserver:$USER@jlabs1.cebaf.gov:/group/clas/clas_cvs
setenv BUILDS     $CLAS_ROOT

setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT

#  the user should be able to toggle this to development or production
#
# development or production or private?
#

#################################################
# this will have to change with each release....
#################################################

setenv CLAS_BUILD $CLAS_ROOT/release-1-18

setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS
setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS
setenv CLAS_PACK    $CLAS_BUILD/packages
setenv CLAS_CMS     $CLAS_PACK/cms
setenv CLAS_SCRIPTS $CLAS_PACK/scripts
setenv RECSIS       $CLAS_PACK


setenv CLAS_PARMS $CLAS_ROOT/parms

# the recsis job counter

setenv RECSIS_RUNTIME $HOME/.recsis

# make the directory .recsis and file recseq.ini if they do not
# exist

if (!(-e $RECSIS_RUNTIME)) then
  mkdir $HOME/.recsis
endif
if (!(-e $RECSIS_RUNTIME/recseq.ini)) then
  echo 0001 > $RECSIS_RUNTIME/recseq.ini
endif

# Need to define where the HV package is kept:

setenv HV_LOCATION  $CLAS_PACK/Hv


#done with jlab.cshrc






