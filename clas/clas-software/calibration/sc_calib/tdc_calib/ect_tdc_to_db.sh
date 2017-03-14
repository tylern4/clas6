#!/bin/sh
runno=$1
runmin=$2
runmax=$3
runindex=$4

/group/clas/tools/caldb/caldb_write_and_link.pl s=EC_CALIB  ss=EC_Tch  i=InnerU  \
 min=${runmin} max=${runmax} srmin=${runno} ci="tdc_calib ect tdcs" f=ECT_TDC_Inner_U.dat it=${runindex}

/group/clas/tools/caldb/caldb_write_and_link.pl s=EC_CALIB  ss=EC_Tch  i=InnerV  \
 min=${runmin} max=${runmax} srmin=${runno} ci="tdc_calib ect tdcs" f=ECT_TDC_Inner_V.dat it=${runindex}

/group/clas/tools/caldb/caldb_write_and_link.pl s=EC_CALIB  ss=EC_Tch  i=InnerW  \
 min=${runmin} max=${runmax} srmin=${runno} ci="tdc_calib ect tdcs" f=ECT_TDC_Inner_W.dat it=${runindex}

/group/clas/tools/caldb/caldb_write_and_link.pl s=EC_CALIB  ss=EC_Tch  i=OuterU  \
 min=${runmin} max=${runmax} srmin=${runno} ci="tdc_calib ect tdcs" f=ECT_TDC_Outer_U.dat it=${runindex}

/group/clas/tools/caldb/caldb_write_and_link.pl s=EC_CALIB  ss=EC_Tch  i=OuterV  \
 min=${runmin} max=${runmax} srmin=${runno} ci="tdc_calib ect tdcs" f=ECT_TDC_Outer_V.dat it=${runindex}

/group/clas/tools/caldb/caldb_write_and_link.pl s=EC_CALIB  ss=EC_Tch  i=OuterW  \
 min=${runmin} max=${runmax} srmin=${runno} ci="tdc_calib ect tdcs" f=ECT_TDC_Outer_W.dat it=${runindex}
