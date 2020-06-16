# sets up clas code environment for timbale
#

# main
setenv OSCLAS "`uname`"
setenv POSIX_SHELL /usr/bin/sh
setenv CLAS_LOCATION /group/clas            # where the CUE site disk is mounted on maraca
setenv CLAS_ROOT $CLAS_LOCATION
setenv BUILDS $CLAS_ROOT/builds
setenv CVSROOT :pserver:richardt@jlabs1.jlab.org:/group/clas/clas_cvs
setenv EDITOR emacs

if($# == 0) then
  echo "Valid options are:"
  echo "l  --  LATEST"
  echo "1  --  prod-1-1"
  echo "2  --  prod-1-2"
  echo "3  --  prod-1-3"
else 
  switch ($1) 
    case 'l'
        setenv CLAS_BUILD $CLAS_ROOT/builds/LATEST
 	setenv CLAS_PARMS $CLAS_ROOT/parms
	echo "LATEST libraries"
	breaksw
    case '1'
	 setenv CLAS_BUILD /group/clasdev/clas/src/prod-1-1
	 setenv CLAS_PARMS /group/clasdev/clas/src/prod-1-1/parms
	 echo "prod-1-1 libraries"
	breaksw
    case '2'
	 setenv CLAS_BUILD /group/clasdev/clas/src/prod-1-2
	 setenv CLAS_PARMS /group/clasdev/clas/src/prod-1-2/parms
	 echo "prod-1-2 libraries"
	breaksw
    case '3'
	 setenv CLAS_BUILD /group/clasdev/clas/src/prod-1-3
	 setenv CLAS_PARMS /group/clasdev/clas/src/prod-1-3/parms
	 echo "prod-1-3 libraries"
	breaksw
    default:
       echo "I don't understand that."
       breaksw
  endsw
endif

setenv CLAS_LIB $CLAS_BUILD/lib/$OSCLAS
setenv CLAS_BIN $CLAS_BUILD/bin/$OSCLAS
setenv CLAS_PACK $CLAS_BUILD/packages
setenv CLAS_CMS $CLAS_PACK/cms
setenv CLAS_SCRIPTS $CLAS_PACK/scripts
setenv RECSIS $CLAS_PACK

# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS
setenv RECSIS_RUNTIME $CLAS_ROOT/clsrc/recsis/runtime

# protect user against old TCL enviroment variables
unsetenv TCL_INC
unsetenv TCL_LIB

# Need to define where the HV package is kept:
setenv HV_LOCATION /mnt/clas/u1/Hv/Hv

setenv CERN /cern
setenv CERN_LEVEL 96a
setenv CERN_ROOT $CERN/$CERN_LEVEL
setenv CERN_LIB $CERN/$CERN_LEVEL/lib
setenv CERN_BIN $CERN/$CERN_LEVEL/bin

setenv PATH "$PATH/:$CERN_BIN/:$CLAS_BIN/:$CLAS_SCRIPTS"

# some useful cvs aliases
alias trep 'setenv CVSROOT ~/.cvsroot'            # local  repository 
alias mrep 'setenv CVSROOT :pserver:thompson@maraca:/home/thompson/.cvsroot'
alias jrep 'setenv CVSROOT :pserver:richardt@jlabs1.jlab.org:/group/clas/clas_cvs' 
                                                  # remote CLAS repository
alias srep 'setenv CVSROOT :pserver:richardt@jlabs1.jlab.org:/home/richardt/coderepos'
                                                  # remote personal repository
alias cs 'cvs status | grep atus'                 # stands for Code Status 
alias ud 'cvs update'
alias rl 'cvs release -d'
alias cm 'cvs commit'





