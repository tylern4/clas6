#!/bin/tcsh
############################
#
# splitUpYourBosFile.sh
# 
# This is a shell script which will 
# take your main bosfile and split it
# into multiple smaller ones for ease
# and speed in cooking. 
#
# The files will be no more than 
# 25000 events.
#
# Matt Bellis
# 12/14/05
#
############################

#
# Test to see if some environment variables
# are set
#
set test = `printenv | grep DCALIGN`
if( $test == "" ) then 
    echo
    echo Environment variable DCALIGN is not defined
    echo
    exit(-1)
endif

#
# Write out the usage
#
if($# < 1) then
  echo
	echo "Wrong number of arguments"
  echo
  echo "Usage: splitUpYourBosFile.sh <original bos file> [max number of events in small files=25000]"
  echo
  exit(-1)
endif

#
# Make whatever directories are needed
#
if(! -e $DCALIGN/splitBosFiles)then
  echo
  echo "Making directory " $DCALIGN/splitBosFiles
  echo
  mkdir $DCALIGN/splitBosFiles
endif

set file = $1

set max = 25000
if( $2 != "" ) then 
    set max = $2
endif

cd $DCALIGN/splitBosFiles
splitbos -n $max -o alignfile $file

ls -l $DCALIGN/splitBosFiles/

