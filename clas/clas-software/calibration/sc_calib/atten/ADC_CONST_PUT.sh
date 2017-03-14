#!/bin/sh
#
# script to commit ADC constants and attenutation constants to the CALDB
#
runno=$1
runmin=$2
runmax=$3
#
#


if(runno < 55357) then
  system = SC_CALIBRATIONS
else
  system = SC_CALIBRATIONS_V2
fi
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=NMIP_ADC     i=left  min=${runmin} max=${runmax} srmin=${runno} ci="gmean"       f=NMIP_ADC_LEFT.dat   it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=NMIP_ADC     i=right min=${runmin} max=${runmax} srmin=${runno} ci="gmean"       f=NMIP_ADC_RIGHT.dat  it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=NMIP_ADCu    i=left  min=${runmin} max=${runmax} srmin=${runno} ci="gmean_error" f=NMIP_ADCu_LEFT.dat  it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=NMIP_ADCu    i=right min=${runmin} max=${runmax} srmin=${runno} ci="gmean_error" f=NMIP_ADCu_RIGHT.dat it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=atten_length i=left  min=${runmin} max=${runmax} srmin=${runno} ci="attenuation" f=atten_length.dat    it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=atten_length i=right min=${runmin} max=${runmax} srmin=${runno} ci="attenuation" f=atten_length.dat    it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=atten_u      i=left  min=${runmin} max=${runmax} srmin=${runno} ci="attenuation" f=atten_u.dat         it="$CLAS_CALDB_RUNINDEX"
/group/clas/tools/caldb/caldb_write_and_link.pl s=${system} ss=atten_u      i=right min=${runmin} max=${runmax} srmin=${runno} ci="attenuation" f=atten_u.dat         it="$CLAS_CALDB_RUNINDEX"

#
#
# exit
#
