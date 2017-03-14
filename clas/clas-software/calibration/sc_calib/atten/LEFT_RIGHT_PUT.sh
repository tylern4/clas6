#! /bin/sh 
# PUT LEFT-RIGHT into CALDB
#
#

runno=$1
runmin=$2
runmax=$3
file=$4

if(test ${runno} -lt 55357) then
  /group/clas/tools/caldb/caldb_write_and_link.pl s=SC_CALIBRATIONS ss=delta_T i=left_right  min=${runmin} max=${runmax} srmin=${runno} ci="left-right" f=${file} it="$CLAS_CALDB_RUNINDEX"
else
  /group/clas/tools/caldb/caldb_write_and_link.pl s=SC_CALIBRATIONS_V2 ss=delta_T i=left_right  min=${runmin} max=${runmax} srmin=${runno} ci="left-right" f=${file} it="$CLAS_CALDB_RUNINDEX"
fi
