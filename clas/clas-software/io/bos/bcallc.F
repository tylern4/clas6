CDECK  ID>, BCALLC.
      SUBROUTINE BCALLC(JW,NAMER,NR)
c
c_begin_doc
c  RCS ID string
c  $Id: bcallc.F,v 1.1.1.1 1997/03/28 20:49:29 marki Exp $
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
#include "comcnv.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER JW(*),NR
      CHARACTER*4 NAMER
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
     1'$Id: bcallc.F,v 1.1.1.1 1997/03/28 20:49:29 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BCALLC.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER MLINK,MBANK,INTCHA,NAMIND
      INTEGER IPR,KND,MIND,L,K,LA,LB,LC,JPR,NP,ND,JND,NNR,IND,I
      EQUIVALENCE (IPR,IW(4))
      CHARACTER*72 TXT
      CHARACTER*4 CHAINT,NAME
c_end_var
c----6----------------------------------------------------------------72
      JW(1)=0
      JW(2)=0
      JW(3)=0
      IF(IPR.EQ.1.AND.JW(LUP).GT.0) WRITE(JW(LUP),101) NAMER,NR
      KND=MLINK(JW,NAMER,NR)
      IF(KND.EQ.0) GOTO 100
      MIND=0
      DO 90 L=1,JW(KND)/18
      DO 10 K=1,18
   10 TXT(4*K-3:4*K)=CHAINT(JW(KND+(L-1)*18+K))
      IF(TXT(1:1).EQ.'*') THEN
         IF(IPR.EQ.1.AND.JW(LUP).GT.0) WRITE(JW(LUP),103) '*',TXT
         GOTO 90
      END IF
      CALL CNVCHA(TXT)
      LA=INDEX(TXT,'$FMT')
      IF(LA.NE.0) THEN
         MIND=0
         LB=INDEX(TXT(LA+4:72),'''')+LA+3
         IF(LB.EQ.LA+3) THEN
            LC=732
         ELSE
            LC=INDEX(TXT(LB+1:72),'''')+LB
            IF(LC.EQ.LB) LC=73
         END IF
         NAME=CHAINT(LABEL)
         IF(IPR.EQ.1.AND.JW(LUP).NE.0) WRITE(JW(LUP),103) 'F',TXT
         CALL BKFMT(NAME,TXT(LB+1:LC-1))
         GOTO 90
      END IF
*     CHECK SPECIAL CARDS
      JPR=IPR
      IF(LABEL.EQ.INTCHA('PON')) THEN
         IPR=1
      ELSE IF(LABEL.EQ.INTCHA('POFF')) THEN
         IPR=0
      ELSE IF(LABEL.NE.INTCHA('UNIT').AND.LABEL.NE.INTCHA('ENDQ')) THEN
         GOTO 50
      END IF
      MIND=0
*     PRINT SPECIAL CARD
      IF(IPR+JPR.NE.0.AND.JW(LUP).GT.0) WRITE(JW(LUP),103) '+',TXT
      GOTO 90
*     CREATE BANK OR ADD TO BANK
   50 NP=0
      IF(MIND.EQ.0) THEN
         ND=0
         NAME=CHAINT(LABEL)
         NAMI=NAMIND(NAME)
C
C        RESET LINK BANKS
C
         JND=JW(IDL)+NAMI-NSYST
         IF(JW(JND).NE.0) JW(JW(JND))=0
         IF(ISL.EQ.0) THEN
            NNR=0
            IND=NAMIND(NAME)+1
   55       IND=JW(IND-1)
            IF(IND.NE.0) THEN
               NNR=JW(IND-2)+1
               GOTO 55
            END IF
         ELSE
            NP=1
            NNR=NFR(1)
         END IF
         MIND=MBANK(JW,NAME,NNR,NT-NP,1)
         IF(JW(3).EQ.0) JW(3)=MIND
         IF(IPR.EQ.1.AND.JW(LUP).GT.0) WRITE(JW(LUP),102) NAME,NNR,TXT
         IF(MIND.EQ.0) GOTO 200
      ELSE IF(CHAINT(LABEL).EQ.'    ') THEN
         ND=JW(MIND)
         MIND=MBANK(JW,NAME,NNR,ND+NT,1)
         IF(IPR.EQ.1.AND.JW(LUP).GT.0) WRITE(JW(LUP),103) ' ',TXT
         IF(MIND.EQ.0) GOTO 200
      ELSE
         MIND=0
         GOTO 50
      END IF
      DO 60 I=1,NT-NP
   60 JW(MIND+ND+I)=NFR(I+NP)
   90 CONTINUE
  100 RETURN
  200 IF(JW(LUP).GT.0) WRITE(JW(LUP),104)
      CALL BABEND('BCALLC')
      GOTO 100
  101 FORMAT('0BCALLC-FROM  ',A4,I10,'  +--',11('------'),'--+')
  102 FORMAT(' -----------  ',A4,I10,2X,A)
  103 FORMAT(28X,A1,1X,A)
  104 FORMAT('0BCALLC-----  INSUFFICIENT SPACE - ABEND')
      END
