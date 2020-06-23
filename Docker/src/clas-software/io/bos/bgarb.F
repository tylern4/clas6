CDECK  ID>, BGARB.
      SUBROUTINE BGARB(JW)
c
c_begin_doc
c  RCS ID string
c  $Id: bgarb.F,v 1.1.1.1 1997/03/28 20:49:30 marki Exp $
c
c  Purpose:
c  --------
c        Garbage collection for named banks
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
     1'$Id: bgarb.F,v 1.1.1.1 1997/03/28 20:49:30 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BDROP.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NUS,IH,NGAP,NCOP,NWRDS,IND,ICOP,NWRD,NAMA,NWNAM,JND,NR
      INTEGER NLPL,IND3,IA,IB
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'BGARB')
      JW(1)=0
      JW(2)=0
*     reduce cache space for BCS array
      IF(JW(11).EQ.1) CALL BDCACH(0)
      IF(JW(NDN).EQ.0) GOTO 100
C
C     STATISTIC ON GARBAGE COLLECTION
C
      JW(NGN)=JW(NGN)+1
      NUS=JW(NJW)-JW(IWK)+JW(IGP)-JW(NDW)
      IH=10.0*FLOAT(NUS)/FLOAT(JW(NJW))+1.0
      IH=MIN(10,MAX(1,IH))
      JW(NFH+IH)=JW(NFH+IH)+1
C
      JW(LCH)=JW(LCH)+1
      NGAP=0
      NCOP=0
      INDJ=0
      NWRDS=0
      IND =JW(IGN)
C
   10 CONTINUE
      IF(IND.LT.JW(IGP)) THEN
         IF(JW(IND).LT.0) THEN
C
C           BANK IS DROPPED
C
            IF(NCOP.GT.0) THEN
               CALL UCOPY2(JW(ICOP),JW(ICOP-NGAP),NCOP)
               NCOP=0
               INDJ=0
            END IF
            NWRD=-JW(IND)
            NGAP=NGAP+NWRD
            IND =IND +NWRD
         ELSE
C
C           BANK IS NOT DROPPED
C
            NWRD=JW(IND)+NHW
            IF(NGAP.NE.0) THEN
C
C              UPDATE INDEX
C
               IF(INDJ.EQ.0.OR.IND.NE.JW(INDJ-1)) THEN
                  IF(NCOP.NE.0) THEN
                     CALL UCOPY2(JW(ICOP),JW(ICOP-NGAP),NCOP)
                     NCOP=0
                  END IF
                  NAMA=JW(IND-3)
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
 
                  IF(NAMI.EQ.0) GOTO 90
C
C                 RESET LINK BANKS
C
                  JND=JW(IDL)+NAMI-NSYST
                  IF(JW(JND).NE.0) JW(JW(JND))=0
                  NR=JW(IND-2)
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
 
                  IF(INDI.EQ.0) GOTO 90
                  IND=INDI
               END IF
               JW(INDJ-1)=JW(INDJ-1)-NGAP
               INDJ=IND
               IF(NCOP.EQ.0) ICOP=IND-(NHW-1)
               NCOP=NCOP+NWRD
            END IF
            IND=IND+NWRD
         END IF
         NWRDS=NWRD
         GOTO 10
      END IF
C
      IF(NCOP.NE.0) CALL UCOPY2(JW(ICOP),JW(ICOP-NGAP),NCOP)
      JW(NDN)=JW(NDN)-NGAP
      JW(IGP)=JW(IGP)-NGAP
      JW(IGN)=JW(NJW)
      GOTO 100
C
C     ERROR
C
   90 IND3=IND-3
      IF(JW(LUP).GT.0) THEN
         WRITE(JW(LUP),1001) IND3
         CALL BBUFM(JW,IND3,IND3,JW(LUP))
         WRITE(JW(LUP),1002)
      END IF
      IA=MAX(1,10*((IND3-NWRDS)/10)-99)
      IB=10*(IND3/10)+100
      IF(JW(LUP).NE.0) THEN
      CALL BBUFM(JW,IA,IB,JW(LUP))
         IF(NAMI.NE.0) THEN
            WRITE(JW(LUP),1003) NAMI,INDJ
            IA=MAX(1,10*(INDJ/10)-99)
            IB=10*(INDJ/10)+100
            CALL BBUFM(JW,IA,IB,JW(LUP))
         END IF
      END IF
      CALL BOSDP(JW)
      CALL BBUFM(JW,1,2000,6)
      CALL BABEND('BGARB')
  100 RETURN
 1001 FORMAT('0BOS++BGARB+  ERROR AT INDEX',I8,' - STOP'/
     1       14X,'CONTENT OF THIS WORD IS PRINTED BELOW AND SHOULD ',
     2       'BE A NAME')
 1002 FORMAT('0',13X,'REGION AROUND THIS INDEX IS PRINTED BELOW'/)
 1003 FORMAT('0',13X,'NAME-INDEX IS',I3,'  LAST INDEX IS',I10)
      END
