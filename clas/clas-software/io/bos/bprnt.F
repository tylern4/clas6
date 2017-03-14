CDECK  ID>, BPRNT.
      SUBROUTINE BPRNT(JW,LIST)
c
c_begin_doc
c  RCS ID string
c  $Id: bprnt.F,v 1.1.1.1 1997/03/28 20:49:34 marki Exp $
c
c  Purpose:
c  --------
c         some prints
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
      CHARACTER*(*) LIST
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
     1'$Id: bprnt.F,v 1.1.1.1 1997/03/28 20:49:34 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BPRNT.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER JW1,LENLS,JLS,NLS,ILS,NAMA,NWNAM,IND,INTCHA,NAMIND
      CHARACTER*4   NAME
c_end_var
c----6----------------------------------------------------------------72
C-----------------------------------------------------------------------
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'BPRNT')
      JW1=JW(1)
      JW(1)=0
      JW(2)=0
      IF(JW(LUP).EQ.0.OR.JW(LUB).EQ.0) GOTO 100
C------ KEEP, LISTNN ------------------------------------------------
C     STATEMENTS INSERTED IN SUBPROGRAM
C     RETURNS NAMI = NAME-INDEX FOR NEXT NAME IN LIST
C                    OR ZERO
C
      LENLS=LEN(LIST)
      IF(LENLS.EQ.0) THEN
         JLS=0
      ELSE IF(LENLS.EQ.1) THEN
         JLS=INDEX('CERST',LIST)
         IF(JLS.NE.0) THEN
            JLS=JLS+ILT
            NLS=JW(JW(JLS))
         END IF
      ELSE
         NLS=LENLS/4
         JLS=-1
      END IF
      ILS=0
    3 IF(JLS.EQ.0) THEN
         NAMI=0
      ELSE IF(JLS.LT.0) THEN
         IF(ILS.LT.NLS) THEN
            ILS=ILS+1
            NAME=LIST(4*ILS-3:4*ILS)
            NAMA=INTCHA(NAME)
C------ KEEP, NAMEN -------------------------------------------------
C     STATEMENTS INSERTED IN SUBPROGRAM
C     RETURNS NAMI = NAME-INDEX FOR INTEGER NAMA
C                    (ALWAYS NONZERO)
      NWNAM=IW(IDNAM)
      IW(IDNAM)=NAMA
      NAMI=IW(IDNAM-2)+MOD(IABS(IW(IDNAM)),NPRIM)-3
    1 NAMI=IW(IDPTR+NAMI)
      IF(IW(IDNAM+NAMI).NE.IW(IDNAM)) GOTO 1
      IW(IDNAM)=NWNAM
      NAMI=NAMI+NSYST
      IF(NAMI.EQ.NSYST) NAMI=NAMIND(NAME)
C--------------------------------------------------------------------
 
         ELSE
            JLS=0
            NAMI=0
         END IF
      ELSE
         IF(ILS.LT.NLS) THEN
            ILS=ILS+1
            NAMI=JW(JW(JLS)+ILS)
         ELSE
            JLS=0
            NAMI=0
         END IF
      END IF
C--------------------------------------------------------------------
 
      IF(NAMI.LE.0) GOTO 100
      IND=NAMI+1
   10 IND=JW(IND-1)
      IF(IND.EQ.0) GOTO 3
      CALL WPRNT(JW,IND)
      IF(JW1.NE.0) GOTO 3
      GOTO 10
C
  100 RETURN
      END
