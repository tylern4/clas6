#!/bin/sh
#
# script to write Pedestals to CalDB
#
#
runno=$1
runmin=$2
runmax=$3

awk '{print $3}' PED.dat > ped_left.dat
awk '{print $5}' PED.dat > ped_right.dat
awk '{print $4}' PED.dat > ped_left_err.dat
awk '{print $6}' PED.dat > ped_right_err.dat
#
#
/group/clas/tools/caldb/caldb_write_and_link.pl s=SC_CALIBRATIONS ss=pedestals i=left  min=${runmin} max=${runmax} srmin=${runno} ci="pedestals" f=ped_left.dat  it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=SC_CALIBRATIONS ss=pedestals i=right min=${runmin} max=${runmax} srmin=${runno} ci="pedestals" f=ped_right.dat it="$CLAS_CALDB_RUNINDEX"
###
/group/clas/tools/caldb/caldb_write_and_link.pl s=SC_CALIBRATIONS ss=pedu i=left  min=${runmin} max=${runmax} srmin=${runno} ci="pedestals" f=ped_left_err.dat  it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=SC_CALIBRATIONS ss=pedu i=right min=${runmin} max=${runmax} srmin=${runno} ci="pedestals" f=ped_right_err.dat it="$CLAS_CALDB_RUNINDEX"

#
# exit

