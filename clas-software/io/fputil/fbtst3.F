CDECK  ID>, FBTST3.
      SUBROUTINE FBTST3
c
c_begin_doc
c  RCS ID string
c  $Id: fbtst3.F,v 1.1.1.1 1997/03/28 20:55:42 marki Exp $
c
c  Purpose:
c  --------
c        FSEQR test
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
     1'$Id: fbtst3.F,v 1.1.1.1 1997/03/28 20:55:42 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FBTST3.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER IRET
c_end_var
c----6----------------------------------------------------------------72
C-- open FPACK files for reading and writing
      CALL FPARM('OPEN BOSINPUT UNIT=2 FILE="FPACK1.DAT" READ
     & RECL=32760 STATUS=OLD')
      CALL FPARM('OPEN BOSOUTPUT UNIT=3 FILE="FPACK3.DAT" WRITE
     & RECL=32760 STATUS=NEW')
C-- read all banks specified in the list E
   10 CONTINUE
      CALL FSEQR('BOSINPUT',IRET)
      IF(IRET.LT.0) THEN
        print *,' TST3: end of file'
        GOTO 100
      ENDIF
C-- print all banks named in the list 'E'
      CALL BPRNT(IW,'E')
C-- force output
      CALL FSEQW('BOSOUTPUT')
C-- goto next event
      GOTO 10
C-- exit
  100 CONTINUE
      CALL FPARM('CLOSE')
C
      print *,'=================== end of tst3 ====================='
      RETURN
      END
