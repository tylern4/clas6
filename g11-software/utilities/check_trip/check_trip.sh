#!/bin/csh -f 


echo "********************************************"

set file = (`ls clas_0*.trip`)
echo "checking trip file $file";

cp $file trip.dat
check_trip_Linux

set bad_trip = (`filetest -e trip.new.dat`)
echo $bad_trip
if ($bad_trip == 1) then
   cp trip.new.dat $file
   echo original trip file substituted with corrected one
else
   echo trip file is good, keeping original
endif
echo "check of trip file completed"

echo "********************************************"

