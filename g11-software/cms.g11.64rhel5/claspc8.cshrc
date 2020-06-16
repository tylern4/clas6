#
# Place these enviroment variable definitions in your .cshrc
# NOTE: to toggle between PRODUCTION and DEVELOPMENT you need to
#       redefine CLAS_LEVEL to your choice
#
setenv OSCLAS "`uname`"
setenv POSIX_SHELL /usr/bin/sh
setenv CLAS_LOCATION /group

if (!($?CLAS_ROOT)) setenv CLAS_ROOT  $CLAS_LOCATION/clas
setenv CVSROOT    :pserver:$USER@jlabs1.cebaf.gov:/group/clas/clas_cvs
setenv BUILDS     $CLAS_ROOT/builds
setenv CLAS_PROD  $BUILDS/PRODUCTION
setenv CLAS_DEVEL $BUILDS/DEVELOPMENT

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
setenv CLAS_LIB     $CLAS_BUILD/lib/$OSCLAS
setenv CLAS_BIN     $CLAS_BUILD/bin/$OSCLAS
setenv CLAS_PACK    $CLAS_BUILD/packages
setenv CLAS_CMS     $CLAS_PACK/cms
setenv CLAS_SCRIPTS $CLAS_PACK/scripts

alias use_dev  source /group/clas/builds/DEVELOPMENT/packages/cms/claspc8.cshrc DEVELOPMENT
alias use_prod source /group/clas/builds/PRODUCTION/packages/cms/claspc8.cshrc PRODUCTION
alias use_daily source /group/clas/builds/LATEST/packages/cms/claspc8.cshrc LATEST
alias use_prod-1-2 source /group/clas/builds/PROD-1-2/packages/cms/claspc8.cshrc PROD-1-2
alias use_prod_dec97 source /group/clas/builds/PROD_DEC97/packages/cms/claspc8.cshrc PROD_DEC97
alias setup_clas source /group/clas/builds/PRODUCTION/packages/cms/claspc8.cshrc


setenv CLAS_PARMS $CLAS_ROOT/parms

# protect user against old TCL enviroment variables

unsetenv TCL_INC
unsetenv TCL_LIB

# remote build aliases

alias cmake    "make_hp \!* &; make_sun \!* &; make_aix \!* &; make_linux \!* &"
alias make_all "make_hp \!* &; make_sun \!* &; make_aix \!* &; make_linux \!* &"

if ("`uname`" == "HP-UX") then

 alias make_hp  'remsh jlabh2 -n "cd $PWD; make \!* >&\! hp.log"'
 alias make_sun 'remsh jlabs2 -n "cd $PWD; make \!* >&\! sun.log"'
 alias make_aix 'remsh jlaba2 -n "cd $PWD; make \!* >&\! aix.log"'
 alias make_linux 'remsh ifarml2 -n "cd $PWD; make \!* NO_MOTIF=yes >&\! linux.log"'

else if ("`uname`" == "AIX") then

 alias make_hp  'rsh jlabh2 -n "cd $PWD; make \!* >&\! hp.log"'
 alias make_sun 'rsh jlabs2 -n "cd $PWD; make \!* >&\! sun.log"'
 alias make_aix 'rsh jlaba2 -n "cd $PWD; make \!* >&\! aix.log"'
 alias make_linux 'rsh ifarml2 -n "cd $PWD; make \!* NO_MOTIF=yes >&\! linux.log"'

else 

 alias make_hp  'rsh -n jlabh2 "cd $PWD; make \!* >&\! hp.log"'
 alias make_sun 'rsh -n jlabs2 "cd $PWD; make \!* >&\! sun.log"'
 alias make_aix 'rsh -n jlaba2 "cd $PWD; make \!* >&\! aix.log"'
 alias make_linux 'rsh -n ifarml2 "cd $PWD; make \!* NO_MOTIF=yes >&\! linux.log"'

endif

#done with claspc8.cshrc






