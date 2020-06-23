CDECK  ID>, BOSBL.
      SUBROUTINE BOSBL(JW)
c
c_begin_doc
c  RCS ID string
c  $Id: bosbl.F,v 1.1.1.1 1997/03/28 20:49:32 marki Exp $
c
c  Purpose:
c  --------
c     PRINT IO-STATISTIC ON BANKS
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
#include "bcs.inc"
#include "bossys.inc"
#include "param.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER JW(*)
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
     1'$Id: bosbl.F,v 1.1.1.1 1997/03/28 20:49:32 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BOSBL.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER I,ID,NN
      CHARACTER*4 CHAINT
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(LUP).LE.0) GOTO 100
      ID=JW(IOS)
      IF(ID.EQ.0) GOTO 100
      NN=MIN0(IW(IDNAM),JW(ID)/2)
      WRITE(JW(LUP),1001)
      WRITE(JW(LUP),1002) (CHAINT(IW(IDNAM+I)),JW(ID+2*I-1),JW(ID+2*I),
     +      I=1,NN)
  100 RETURN
 1001 FORMAT('0  ',5('NAME .....IN ...OUT',6X)/)
 1002 FORMAT(5(3X,A4,I8,I7,3X))
      END
