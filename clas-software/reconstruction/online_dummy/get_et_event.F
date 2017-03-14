      SUBROUTINE GET_ET_EVENT(iw, timeout)
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
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author: GILFOYLE   Created:  Mon Jun 16 10:51:58 EDT 1999
c  -------
c
c  Major revisions:
c  ----------------
c     
c
      IMPLICIT NONE
      SAVE
c
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
c#include "bcs.inc"
c
c  input/output variables:
c  -----------------------
c
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='GET_ET_EVENT')
      PARAMETER (CRAUTH='GILFOYLE')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, iw, timeout
c_end_var
c
c  executable code for routine GET_ET_EVENT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='You have called a dummy CLAS ET routine. It appears you'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='are trying to use the online event transfer (ET) without compiling'
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='RECSIS with the "ONLINE=yes" option. If you really do '
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='not want to use ET, then set the tcl variable READ_ET '
        CALL RECMES(CRNAME,'I',CRMESS)
        CRMESS='to zero in your inititalization file.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




