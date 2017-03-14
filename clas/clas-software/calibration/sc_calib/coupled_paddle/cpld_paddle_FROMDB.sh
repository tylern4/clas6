#!/bin/sh
#! GET COUPLED PADDLE CONSTANTS FROM calDB AND PUT THEM INTO A FILE constants_in_DB.dat
#! MUST BE EDITED SO THAT IT CAN BE READ IN BY p2p_CAL.C 
runno=$1


/group/clas/tools/caldb/caldb_show_constants_run.pl s=SC_CALIBRATIONS ss=delta_T i=coupled_paddle1 \
 run=${runno} it="calib_user.RunIndexg8b" > coupledpaddle1_INDB.dat

/group/clas/tools/caldb/caldb_show_constants_run.pl s=SC_CALIBRATIONS ss=delta_T i=coupled_paddle2 \
 run=${runno} it="calib_user.RunIndexg8b" > coupledpaddle2_INDB.dat
