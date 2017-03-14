#!/bin/tcsh 
############################
#
# alignTheDriftChambers.sh
# 
# This is a shell script which calls
# the aligndc program which loops over
# the bos files cooked with user_align,
# and tries to figure out the optimum
# alignment offsets.
#
# The new offsets are written in a text
# file in the pass directory with a naming
# scheme corresponding to what inputs 
# were used. 
#
# Matt Bellis
# 12/14/05
#
############################

#
# Print usage
# 
if($# < 2) then

if($# < 1) then
  echo
  echo "Usage: alignTheDriftChambers.sh <pass> [whichalign=123] [sectors=123456] [region=3] [sigma=1000] [chi2 cut=-1]"
  echo
  exit (-1)
endif

#
# Set all the local varibles if they are
# passed in on the command line.
#
set pass = "pass"$1
set origalignfile = $DCALIGN/$pass/offsetsUsedInCookingTheseFiles.txt

set whichalign = 123
if($2 != "") then
  set whichalign = $2
endif

set sectors = 123456
if($3 != "") then
  set sectors = $3
endif

set reg = 3
if($4 != "") then
  set reg = $4
endif

set sigma = 1000
if($5 != "") then
  set sigma = $5
endif

set minChi2 = -1
if($6 != "") then
  set minChi2 = $6
endif

set infile = $DCALIGN/$pass/alignfile.out
set outfile = $DCALIGN/$pass/aligndc.sec$sectors.coord$whichalign.reg$reg.sigma$sigma.minChi2$minChi2.output 

echo aligndc -o$outfile -X$minChi2 -s$sectors -n150000 -icurrent -d$whichalign -l3 -rf$reg -w$reg$sigma -i$origalignfile $infile 
     aligndc -o$outfile -X$minChi2 -s$sectors -n150000 -icurrent -d$whichalign -l3 -rf$reg -w$reg$sigma -i$origalignfile $infile 

