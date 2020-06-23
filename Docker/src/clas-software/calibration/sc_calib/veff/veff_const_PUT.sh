#!/bin/sh
#
# script to extract and commit veffective and Y-offsets constants to the CALDB
#
runno=$1
runmin=$2
runmax=$3

awk '{print $3}' min_parm > veff.dat 
awk '{print $5}' min_parm > veffu.dat
awk '{print $2}' min_parm > yoffset.dat

if(runno < 55357) then
  system = SC_CALIBRATIONS
else
  system = SC_CALIBRATIONS_V2
fi

#
#
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=veff     i=left  min=${runmin} max=${runmax} srmin=${runno} ci="veff"       f=veff.dat    it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=veff     i=right min=${runmin} max=${runmax} srmin=${runno} ci="veff"       f=veff.dat    it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=veffu    i=left  min=${runmin} max=${runmax} srmin=${runno} ci="veff_error" f=veffu.dat   it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=veffu    i=right min=${runmin} max=${runmax} srmin=${runno} ci="veff_error" f=veffu.dat   it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=Yoffset  i=value min=${runmin} max=${runmax} srmin=${runno} ci="Y offset"   f=yoffset.dat it="$CLAS_CALDB_RUNINDEX"
#
#
# exit
#
