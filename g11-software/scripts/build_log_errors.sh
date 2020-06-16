#!/bin/sh
#
# Runs the build error finder for specific log files.
#
# The "-d" option will choose debug versions of the log files.
#
# $Id: build_log_errors.sh,v 1.15 2004/05/06 18:59:14 claslib Exp $
###############################################################
rm -f build_log_errors.log
if [ "$1" = -d ]
    then
    suffix=_debug
fi
$CLAS_SCRIPTS/build_log_errors.perl LinuxRH7 make_ifarml1$suffix.log $?
$CLAS_SCRIPTS/build_log_errors.perl LinuxRHEL3 make_ifarml3$suffix.log $?
$CLAS_SCRIPTS/build_log_errors.perl SunOS make_ifarms2$suffix.log $?
