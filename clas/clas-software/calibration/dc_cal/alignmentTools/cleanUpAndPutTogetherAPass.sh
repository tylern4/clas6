#!/bin/tcsh
############################
#
# cleanUpAndPutTogetherAPass.sh
# 
# This is a shell script which is run
# after a pass has finished cooking with
# user_align. 
#
# The script converts the hbook files to 
# root and concatenates the bos files 
# into one file for use in aligndc.
#
# Matt Bellis
# 12/14/05
#
############################

#
# Print usage
# 
if($# < 1) then
  echo
  echo "Usage: cleanUpAndPutTogetherAPass.sh <pass num>"
  echo
  exit(-1)
endif

set pass = pass$1

cd $DCALIGN/$pass

#
# Convert hbook to root files
#
foreach file(alignfile*hbook)
  echo $file
  h2root $file
  rm $file
end

#
# Concatenate the bos files with a file size limit
# of 2GB.
#
catbos -oalignfile.out -m2000000 alignfile.*.out 
if(-e alignfile.out.00) then
  mv alignfile.out.00 alignfile.out
  rm alignfile.out.*
end

#
# Clean up the excess files
#
rm *.hbook
rm alignfile*.out
