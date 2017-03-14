#!/bin/sh
#
# script to put TIMEWALK constants into calDB
#
#
runno=$1
runmin=$2
runmax=$3

echo " THE RUN NUMBER OF THE DATA IS $runno"
echo " THE MINUMUM RUN NUMBER IS $runmin"
echo " THE MAXIMUM RUN NUMBER IS $runmax"

if(test ${runno} -lt 55357) then
  system = SC_CALIBRATIONS
else
  system = SC_CALIBRATIONS_V2
fi

/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=WALK_A0 i=left min=${runmin} max=${runmax} srmin=${runno} ci="timewalk constants" f=WALK_A0_left.dat   it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=SC_CALIBRATIONS ss=WALK1   i=left min=${runmin} max=${runmax} srmin=${runno} ci="timewalk constants" f=WALK1_left.dat     it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=WALK2   i=left min=${runmin} max=${runmax} srmin=${runno} ci="timewalk constants" f=WALK2_left.dat     it="$CLAS_CALDB_RUNINDEX"
#
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=WALK0u i=left   min=${runmin} max=${runmax} srmin=${runno} ci="timewalk constants" f=WALK0uleft.dat    it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=WALK1u i=left   min=${runmin} max=${runmax} srmin=${runno} ci="timewalk constants" f=WALK1uleft.dat    it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=WALK2u i=left   min=${runmin} max=${runmax} srmin=${runno} ci="timewalk constants" f=WALK2uleft.dat    it="$CLAS_CALDB_RUNINDEX"
#
#
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=WALK_A0 i=right min=${runmin} max=${runmax} srmin=${runno} ci="timewalk constants" f=WALK_A0_right.dat  it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=WALK1   i=right min=${runmin} max=${runmax} srmin=${runno} ci="timewalk constants" f=WALK1_right.dat    it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=WALK2   i=right min=${runmin} max=${runmax} srmin=${runno} ci="timewalk constants" f=WALK2_right.dat    it="$CLAS_CALDB_RUNINDEX"
#
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=WALK0u i=right   min=${runmin} max=${runmax} srmin=${runno} ci="timewalk constants" f=WALK0uright.dat   it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=WALK1u i=right   min=${runmin} max=${runmax} srmin=${runno} ci="timewalk constants" f=WALK1uright.dat   it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=WALK2u i=right   min=${runmin} max=${runmax} srmin=${runno} ci="timewalk constants" f=WALK2uright.dat   it="$CLAS_CALDB_RUNINDEX"
#
# exit
#
