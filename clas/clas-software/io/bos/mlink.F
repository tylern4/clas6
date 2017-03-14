CDECK  ID>, MLINK.
      INTEGER FUNCTION MLINK(JW,NAME,NR)
c
c_begin_doc
c  RCS ID string
c  $Id: mlink.F,v 1.1.1.1 1997/03/28 20:49:37 marki Exp $
c
c  Purpose:
c  --------
c
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
      INTEGER JW(*),NR
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
     1'$Id: mlink.F,v 1.1.1.1 1997/03/28 20:49:37 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='MLINK.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER MLINC,MLINX,NWNAM,JND,NLPL,NB,IND,KND,IL,IH,I,NAMIN,NAMAA
      INTEGER NAMA
      CHARACTER*4 NAME
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'MLINK')
      CALL UCTOH(NAME,NAMA,4,4)
   10 CONTINUE
C------ KEEP, NAMEZ -------------------------------------------------
C     STATEMENTS INSERTED IN SUBPROGRAM
C     RETURNS NAMI = NAME-INDEX FOR INTEGER NAMA
C                  = 0 IF NAME NOT YET USED
      NWNAM=IW(IDNAM)
      IW(IDNAM)=NAMA
      NAMI=IW(IDNAM-2)+MOD(IABS(IW(IDNAM)),NPRIM)-3
    1 NAMI=IW(IDPTR+NAMI)
      IF(IW(IDNAM+NAMI).NE.IW(IDNAM)) GOTO 1
      IW(IDNAM)=NWNAM
      IF(NAMI.NE.0) NAMI=NAMI+NSYST
C--------------------------------------------------------------------
 
   20 JW(1)=0
      JW(2)=0
      INDI=0
      IF(NAMI.EQ.0) GOTO 90
      JND=JW(IDL)+NAMI-NSYST
      IF(JW(JND).NE.0) GOTO 40
      GOTO 30
C
C     SWITCH OFF BINARY SEARCH
C
   25 CALL WDROP(JW,JW(JND))
   30 CONTINUE
C
C     USE SEQUENTIAL SEARCH
C
C------ KEEP, INDEX -------------------------------------------------
C     STATEMENTS INSERTED IN SUBPROGRAM
C     RETURNS INDI = INDEX OF BANK WITH NAME-INDEX NAMI AND NUMBER NR
C                    OR ZERO
C             INDJ = PREVIOUS INDEX (JW(INDJ-1) CONTAINS INDI)
C
      INDI=NAMI+1
      NLPL=0
    2 INDJ=INDI
      INDI=JW(INDJ-1)
      IF(INDI.NE.0) THEN
         NLPL=NLPL+1
         IF(JW(INDI-2).LT.NR) GOTO 2
         IF(JW(INDI-2).GT.NR) INDI=0
      END IF
C--------------------------------------------------------------------
 
      IF(NLPL.GT.NLPLM) CALL WBANK(JW,JW(JND),0,1,*90)
      GOTO 90
C
C     USE BINARY SEARCH
C
   40 IF(JW(JW(JND)).NE.0) GOTO 70
C
C     COUNT NUMBER OF BANKS
C
      NB=0
      IND=NAMI+1
   50 IND=JW(IND-1)
      IF(IND.NE.0) THEN
         NB=NB+1
         GOTO 50
      END IF
      IF(NB.LT.NLPLM) GOTO 25
      IF(JW(JW(JND)-2)-NHW.LT.2*NB) THEN
         CALL WBANK(JW,JW(JND),2*NB+10,1,*25)
      END IF
      JW(JW(JND))=2*NB
C
C     FILL NUMBER/INDEX PAIRS INTO BANK
C
      KND=JW(JND)
      IND=NAMI+1
   60 IND=JW(IND-1)
      IF(IND.NE.0) THEN
         JW(KND+1)=JW(IND-2)
         JW(KND+2)=IND
         KND=KND+2
         GOTO 60
      END IF
C
C     FIND NUMBER BY BINARY SEARCH IN LINK WORK BANK
C
   70 JND=JW(JND)
      INDI=0
      IL=JND+1
      IH=JND+JW(JND)-1
      I=IH
      IF(JW(IL).LE.NR.AND.JW(IH).GE.NR) THEN
      IF(JW(IL).EQ.NR) THEN
         INDI=JW(IL+1)
         GOTO 90
      END IF
   80    IF(JW(I).LT.NR) THEN
            IL=I
      ELSE
            IH=I
         END IF
         I=IL+((IH-IL)/4)*2
      IF(I.NE.IL) GOTO 80
      IF(JW(I).EQ.NR) INDI=JW(I+1)
      IF(JW(IH).EQ.NR) INDI=JW(IH+1)
      END IF
   90 IND=INDI
      JW(1)=0
      JW(2)=0
      GOTO 100
C
C     ENTRY WITH NAME-INDEX INSTEAD OF CHAR NAME
C
      ENTRY MLINC(JW,NAMIN,NR)
      NAMI=NAMIN
      GOTO 20
C
C     ENTRY WITH INTEGER NAME INSTEAD OF CHAR NAME
C
      ENTRY MLINX(JW,NAMAA,NR)
      NAMA=NAMAA
      GOTO 10
C exit
  100 MLINK=IND
      RETURN
      END
