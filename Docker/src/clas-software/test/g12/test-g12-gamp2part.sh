#!/bin/bash

export CLAS_PARMS=/group/clas/parms
export CLAS_CALDB_RUNINDEX=calib_user.RunIndexg12

gamp2part -r56855 -o3pi.part -T -S-0.321,0.378,-0.254,0.407 -z-110,-70 genr8.gamp
exit $?
