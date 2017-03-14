#!/bin/tcsh
############################
#
# printDCAlignmentOffsets.sh
# 
# This is a shell script which calls
# the perl script which reads the
# DC constants from the database.
#
# Note that it reads from your default
# RUNINDEX environment variable.
#
# You need to pass in a run number
# for the offsets.
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
  echo "Usage: printDCAlignmentOffsets.sh <run number>"
  echo
  exit(-1)
endif

caldb_print_dcgeom.pl r=$1 it=$RUNINDEX hostname=clasdb
