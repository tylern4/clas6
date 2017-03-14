#!/bin/sh
#
# Runs the build error finder for specific log files.
#
# The "-d" option will choose debug versions of the log files.
#
# $Id: build_log_errors.sh,v 1.16 2005/03/02 19:48:44 claslib Exp $
###############################################################
rm -f build_log_errors.log
if [ "$1" = -d ]
    then
    suffix=_debug
else
    suffix="";
fi
$CLAS_SCRIPTS/build_log_errors.perl LinuxRHEL3 make${suffix}_ifarml1.jlab.org.log $?
$CLAS_SCRIPTS/build_log_errors.perl SunOS make${suffix}_ifarms1.log $?
