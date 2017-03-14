#!/bin/sh

Vmin=99998
Vmax=99999
Vsrmin=99998
Vrunindex=${CLAS_CALDB_RUNINDEX}
Vfile=../gamecock/constants.dat.trunc

if [ ${Vmin} -gt 55356 ]; then
  Vsystem=SC_CALIBRATIONS_V2
else
  Vsystem=SC_CALIBRATIONS
fi

/group/clas/tools/caldb/caldb_write_and_link.pl s=${Vsystem}  ss=delta_T  i=paddle2paddle  min=${Vmin} max=${Vmax} srmin=${Vsrmin} \
  ci="paddle2paddle delay calibration" f=${Vfile} it=${Vrunindex}
