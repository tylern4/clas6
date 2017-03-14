CDECK  ID>, FBTST6.
      SUBROUTINE FBTST6
c
c_begin_doc
c  RCS ID string
c  $Id: fbtst6.F,v 1.1.1.1 1997/03/28 20:55:43 marki Exp $
c
c  Purpose:
c  --------
c        RESTORE BOS banks from FPACK file
c
c  Author:
c  -------
c     Volker Blobel          xxx Apr 15 15:00:00 EDT 1994
c
c  Major revisions:
c  ----------------
c     Serguei Boiarinov      Thu Oct 12 11:52:54 EDT 1995
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "bcscom.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information:
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/
     1'$Id: fbtst6.F,v 1.1.1.1 1997/03/28 20:55:43 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FBTST6.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER IERR
c_end_var
c----6----------------------------------------------------------------72
C-- open FPACK file for reading
      CALL FPARM('OPEN BOSINPUT UNIT=2 FILE="FPACK4.DAT" READ
     & RECL=32760 STATUS=OLD')
      CALL FPARM('END')
C-- for following test
   10 CONTINUE
C-- read all banks specified in the list E
      CALL FRBOS(IW,2,'E',IERR)
      PRINT *,' tst6: after FRBOS IERR=',IERR
      IF(IERR.EQ.-1) THEN
        PRINT *,' END OF FILE in tst6'
        GOTO 90
      ENDIF
      IF(IERR.GT.0) THEN
        PRINT *,' tst6: IERR=',IERR
        GOTO 90
      ENDIF
C-- print all banks named in the list 'E'
      WRITE(6,*) ' Test 6:'
      CALL BPRNT(IW,'E')
C-- drop event banks and make garbage collection
      CALL BDROP(IW,'E')
      CALL BGARB(IW)
      GOTO 10
C--
   90 CONTINUE
C
      print *,'=================== end of tst6 ====================='
      RETURN
      END
