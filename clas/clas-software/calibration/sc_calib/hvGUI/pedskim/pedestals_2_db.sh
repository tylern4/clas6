#!/bin/sh
runno=$1
runmin=$2
runmax=$3

/group/clas/tools/caldb/caldb_write_and_link.pl s=SC_CALIBRATIONS_V2  ss=pedestals  i=left  \
 min=${runmin} max=${runmax} srmin=${runno} ci="newest pedestals" f=PED_LEFT2DB.dat it="calib.RunIndex"

/group/clas/tools/caldb/caldb_write_and_link.pl s=SC_CALIBRATIONS_V2  ss=pedestals  i=right  \
 min=${runmin} max=${runmax} srmin=${runno} ci="newest pedestals" f=PED_RIGHT2DB.dat it="calib.RunIndex"
