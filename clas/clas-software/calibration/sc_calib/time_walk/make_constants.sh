#! /bin/tcsh -f

#cat sec1left.dat sec2left.dat sec3left.dat sec4left.dat sec5left.dat  sec6left.dat > min_parm_left.dat
#cat sec1right.dat sec2right.dat sec3right.dat sec4right.dat sec5right.dat  sec6right.dat > min_parm_right.dat 

awk '{print $3}' min_parm_left.dat > WALK1_left.dat 
awk '{print $4}' min_parm_left.dat > WALK2_left.dat
awk '{print $5}' min_parm_left.dat > WALK_A0_left.dat

awk '{print $7}' min_parm_left.dat > WALK1uleft.dat
awk '{print $8}' min_parm_left.dat > WALK2uleft.dat
awk '{print $9}' min_parm_left.dat > WALK0uleft.dat

awk '{print $3}' min_parm_right.dat > WALK1_right.dat
awk '{print $4}' min_parm_right.dat > WALK2_right.dat
awk '{print $5}' min_parm_right.dat > WALK_A0_right.dat

awk '{print $7}' min_parm_right.dat > WALK1uright.dat
awk '{print $8}' min_parm_right.dat > WALK2uright.dat
awk '{print $9}' min_parm_right.dat > WALK0uright.dat
