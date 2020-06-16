# sets up clas code environment for maraca
#

# main
export OSCLAS="`uname`"
export POSIX_SHELL=/usr/bin/sh
export CLAS_LOCATION=/apps1/clas
export CLAS_ROOT=$CLAS_LOCATION
export BUILDS=$CLAS_ROOT/builds
export CVSROOT=pricej@jlabs1.jlab.org:/group/clas/clas_cvs
export CVS_RSH=/usr/bin/ssh
export EDITOR=emacs
export NO_MOTIF=true

if test $# -eq 0
then
  PrintUsage ""
else 
  case $1 in
    -p)
	export CLAS_BUILD=$BUILDS/PRODUCTION
	export CLAS_PARMS=$CLAS_ROOT/parms
        export HV_LOCATION=$CLAS_BUILD/packages/Hv
	echo "boring: PRODUCTION libraries";;
    -d)
	export CLAS_BUILD=$BUILDS/DEVELOPMENT
	export CLAS_PARMS=$CLAS_ROOT/parms
        export HV_LOCATION=$CLAS_BUILD/packages/Hv
	echo "boring: DEVELOPMENT libraries";;
    -l)
        export CLAS_BUILD=$BUILDS/LATEST
 	export CLAS_PARMS=$CLAS_ROOT/parms
        export HV_LOCATION=$CLAS_BUILD/packages/Hv
	echo "bleeding edge: LATEST libraries";;
    -p1)
	 export CLAS_BUILD=/group/clasdev/clas/src/prod-1-1
	 export CLAS_PARMS=/group/clasdev/clas/src/prod-1-1/parms
	 echo "prod-1-1 libraries rock.";;
    -p2)
	 export CLAS_BUILD=/group/clasdev/clas/src/prod-1-2
	 export CLAS_PARMS=/group/clasdev/clas/src/prod-1-2/parms
	 echo "prod-1-2 libraries suck.";;
    -p3)
	 export CLAS_BUILD=/group/clasdev/clas/src/prod-1-3
	 export CLAS_PARMS=/group/clasdev/clas/src/prod-1-3/parms
	 echo "prod-1-3 libraries ... who knows?";;
    -p4)
	 export CLAS_BUILD=/group/clasdev/clas/src/prod-1-4
	 export CLAS_PARMS=/group/clasdev/clas/src/prod-1-4/parms
	 echo "prod-1-4 libraries";;

   *)
       PrintUsage $1;;
  esac
fi

export CLAS_LIB=$CLAS_BUILD/lib/$OSCLAS
export CLAS_BIN=$CLAS_BUILD/bin/$OSCLAS
export CLAS_PACK=$CLAS_BUILD/packages
export CLAS_CMS=$CLAS_PACK/cms
export CLAS_SCRIPTS=$CLAS_PACK/scripts
export RECSIS=$CLAS_PACK

alias use_dev='.  $CLAS_BUILD/packages/cms/latech.bashrc -d'
alias use_prod='. $CLAS_BUILD/packages/cms/latech.bashrc -p3'
alias use_late='. $CLAS_BUILD/packages/cms/latech.bashrc -l'


# the files in RECSIS_RUNTIME should be moved to CLAS_PARMS
export RECSIS_RUNTIME=$CLAS_ROOT/clsrc/recsis/runtime

# protect user against old TCL enviroment variables
export TCL_INC
export TCL_LIB

export PATH=$PATH:$CLAS_PACK/scripts

export CERN=/apps1/cern/Linux
export CERN_LEVEL=98
export CERN_ROOT=$CERN/$CERN_LEVEL
export CERN_LIB=$CERN/$CERN_LEVEL/lib
export CERN_BIN=$CERN/$CERN_LEVEL/bin

# some useful cvs aliases

#------------------------------------------------------------------------------------------------#
PrintUsage() {
  echo 
  echo "I don't understand " $1
  echo "valid switches:"
  echo "    -d  : use DEVELOPMENT libraries"
  echo "    -p1 : use prod-1-1    libraries"
  echo "    -p2 : use prod-1-2    libraries"
  echo "    -p3 : use prod-1-3    libraries"
  echo "    -l  : use LATEST      libraries"
}
#------------------------------------------------------------------------------------------------#




