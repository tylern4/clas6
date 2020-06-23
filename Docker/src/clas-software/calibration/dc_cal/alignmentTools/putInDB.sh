#!/bin/tcsh
############################
#
# putInDB.sh
# 
# This is a shell script which calls
# the perl script which writes in the
# DC constants to the database.
#
# Note that it writes to your default
# RUNINDEX environment variable.
#
# The default is also to write in the 
# constants for the entire period
# of JLab running, so you may want to 
# set these each time you run, or edit
# this script to use some period with
# which you are comfortable.
#
# Matt Bellis
# 12/14/05
#
############################

#
# Print usage
# 
if($# < 2) then
  echo
  echo "Usage: putInDBcommand.sh <file of dc offsets> <pass num> [srmin=1] [srmax=99999]"
  echo
  exit(-1)
endif

set $srMin=1
set $srMax=99999

if($3 != "") then
  set $srMin = $3
endif

if($4 != "") then
  set $srMax = $4
endif

caldb_write_dcgeom.pl f=$1 c=frompass$2 srmin=$srMin srmax=$srMax it=$RUNINDEX


