      SUBROUTINE CLASDD_GET
c
c
c  Documentation for subroutine GET_ET_EVENT
c
c  Purpose: This is a dummy routine to replace the ET software
c  -------- used for the online data acquisition at JLAB. The
c           online ET software is written solely for Sun
c           platforms so it is not included in the standard
c           distribution of RECSIS.
c
c
c  Author: GILFOYLE   Created:  Mon Jun 16 10:51:58 EDT 1999
c  -------
c
      IMPLICIT NONE
c
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='GET_ET_EVNT')
      PARAMETER (CRAUTH='GILFOYLE')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
c_end_var
c
c  executable code for routine GET_ET_EVNT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='You have called a dummy CLAS ET routine. It appears you'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='are trying to use the online event transfer (ET) without'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='compiling RECSIS with the "ONLINE=yes" option. If you '
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='really do not want to use ET, then set the tcl variable'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='READ_ET to zero in your inititalization file.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




