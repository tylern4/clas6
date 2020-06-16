# sets up clas code environment for maraca
#

# main
export OSCLAS="`uname`"
export POSIX_SHELL=/usr/bin/sh
export CLAS_LOCATION=/group/clas            # where the CUE site disk is mounted on maraca
export CLAS_ROOT=$CLAS_LOCATION
export BUILDS=$CLAS_ROOT/builds
export CVSROOT=:pserver:richardt@jlabs1.jlab.org:/group/clas/clas_cvs
export EDITOR=emacs

if test $# -eq 0
then
  PrintUsage ""
else 
  case $1 in
    -d)
	export CLAS_BUILD=$CLAS_ROOT/builds/DEVELOPMENT
	export CLAS_PARMS=$CLAS_ROOT/parms
	echo "boring: DEVELOPMENT libraries";;
    -l)
        export CLAS_BUILD=$CLAS_ROOT/builds/LATEST
 	export CLAS_PARMS=$CLAS_ROOT/parms
	echo "bleeding edge: LATEST libraries";;
    -p1)
	 export CLAS_BUILD=/group/clasdev/clas/src/prod-1-1
	 export CLAS_PARMS=/group/clasdev/clas/src/prod-1-1/parms
	 echo "prod-1-1 libraries and calibration files";;
    -p2)
	 export CLAS_BUILD=/group/clasdev/clas/src/prod-1-2
	 export CLAS_PARMS=/group/clasdev/clas/src/prod-1-2/parms
	 echo "prod-1-2 libraries and calibration files";;
    -p3)
	 export CLAS_BUILD=/group/clasdev/clas/src/prod-1-3
	 export CLAS_PARMS=/group/clasdev/clas/src/prod-1-3/parms
	 echo "prod-1-3 libraries and calibration files";;
    -p4) export CLAS_BUILD=/group/clasdev/clas/src/prod-1-4
	 export CLAS_PARMS=/group/clasdev/clas/src/prod-1-4/parms
	 echo "prod-1-4 libraries andcalibration files ";;
    -p5) export CLAS_BUILD=/group/clasdev/clas/src/prod-1-5
	 export CLAS_PARMS=/group/clasdev/clas/src/prod-1-5/parms
	 echo "prod-1-5 libraries andcalibration files ";;
    *)	
      PrintUsage;;
  esac
fi

export CLAS_LIB=$CLAS_BUILD/lib/$OSCLAS
export CLAS_BIN=$CLAS_BUILD/bin/$OSCLAS
export CLAS_PACK=$CLAS_BUILD/packages
export CLAS_CMS=$CLAS_PACK/cms
export CLAS_SCRIPTS=$CLAS_PACK/scripts
export RECSIS=$CLAS_PACK

alias use_dev='.  $CLAS_BUILD/packages/cms/maraca.bashrc -d'
alias use_prod='. $CLAS_BUILD/packages/cms/maraca.bashrc -p3'
alias use_late='. $CLAS_BUILD/packages/cms/maraca.bashrc -l'


# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS
export RECSIS_RUNTIME=$CLAS_ROOT/clsrc/recsis/runtime

# protect user against old TCL enviroment variables
export TCL_INC
export TCL_LIB

# Need to define where the HV package is kept:
export HV_LOCATION=/mnt/clas/u1/Hv/Hv

export PATH=$PATH:$CLAS_PACK/scripts

export CERN=/cern
export CERN_LEVEL=96a
export CERN_ROOT=$CERN/$CERN_LEVEL
export CERN_LIB=$CERN/$CERN_LEVEL/lib
export CERN_BIN=$CERN/$CERN_LEVEL/bin

# some useful cvs aliases
alias lrep='export CVSROOT=~/.cvsroot'            # local  repository 
alias rrep='export CVSROOT=:pserver:richardt@jlabs1.jlab.org:/group/clas/clas_cvs' 
                                                  # remote CLAS repository
alias srep='export CVSROOT=:pserver:richardt@jlabs1.jlab.org:/home/richardt/coderepos'
                                                  # remote personal repository
alias cs='cvs status | grep atus'                 # stands for Code Status 
alias ud='cvs update'
alias rl='cvs release -d'
alias cm='cvs commit'

#------------------------------------------------------------------------------------------------#
PrintUsage() {
  echo 
  echo "I don't understand " $1
  echo "valid swithces:"
  echo "    -d  : use DEVELOPMENT libraries"
  echo "    -p1 : use prod-1-1    libraries"
  echo "    -p2 : use prod-1-2    libraries"
  echo "    -p3 : use prod-1-3    libraries"
  echo "    -l  : use LATEST      libraries"
}
#------------------------------------------------------------------------------------------------#




