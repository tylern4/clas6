#!/bin/bash

export CLAS_PARMS=/group/clas/parms
export CLAS_CALDB_RUNINDEX=calib_user.RunIndexg12

gsim_bat -ffread g12.ffread -kine 1 -mcin 3pi.part -bosout 3pi.gsim -trig 2000000
exit $?
