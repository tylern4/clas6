CDECK  ID>, BBINS.
      SUBROUTINE BBINS(JW,LUN,LISTR)
c
c_begin_doc
c  RCS ID string
c  $Id: bbins.F,v 1.1.1.1 1997/03/28 20:55:40 marki Exp $
c
c  Purpose:
c  --------
c        DEFINE LIST OF BANK NAMES, INSERT POINTER INTO NEW BANKS
c        AND FINALLY DROP +REC BANK
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
#include "bossys.inc"
#include "param.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER JW(*),LUN
      CHARACTER*(*) LISTR
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
     1'$Id: bbins.F,v 1.1.1.1 1997/03/28 20:55:40 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BBINS.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER JW2,NLS,JLS,IRD,NTOT,NEXT,LENG,JND,I,IOA,NSFT,IND,NIND
      INTEGER MBANK,MLINK,MDROP,INTCHA,MDRP
      CHARACTER*4 NAME,CHAINT
c_end_var
c----6----------------------------------------------------------------72
      JW2=0
      JW(1)=0
      JW(2)=0
*     DEFINE LIST OF BANK NAMES
*     GET LIST-INDEX AND INITIALIZE LIST
      NLS=0
      JLS=6
      IF(LEN(LISTR).EQ.1) THEN
         JLS=INDEX('CERST',LISTR)
         IF(JLS.EQ.0) JLS=6
      END IF
      JLS=JLS+ILT
      IF(JW(JLS).EQ.0) THEN
         CALL WBANC(JW,JW(JLS),0,1,*91)
         JW(JW(JLS)-3)=INTCHA('+LST')
      END IF
      JW(JW(JLS))=0
 
*     START RECORD
 
      IRD=MLINK(JW,'+REC',LUN)
      IF(IRD.EQ.0) GOTO 100
      NTOT=JW(IRD)
      NEXT=IRD+1
      NAMI=JW(NEXT+(NHW-2))+1
 
C bank loop
 
   10 LENG=JW(NEXT+(NHW-1))+NHW
      IF(JW(NEXT+(NHW-2)).NE.NAMI) THEN
C NEW NAME
         NAMI=JW(NEXT+(NHW-2))
C RESET LINK BANKS
         JND=JW(IDL)+NAMI-NSYST
         IF(JW(JND).NE.0) JW(JW(JND))=0
C STORE NAME INDEX IN LIST
         DO I=1,NLS
           IF(JW(JW(JLS)+I).EQ.NAMI) GOTO 30
         ENDDO
         IF(JW(JW(JLS)-2)-NHW.LE.NLS) THEN
           JW(JW(JLS))=NLS
           CALL WBANC(JW,JW(JLS),NLS+NHW,1,*91)
         END IF
         NLS=NLS+1
         JW(JW(JLS)+NLS)=NAMI
C IO-STATISTIC FOR THIS NAME
         IOA=NAMI-NSYST
         IF(JW(IOS).EQ.0.OR.JW(JW(IOS)).LT.2*IOA) THEN
           CALL WBANC(JW,JW(IOS),2*IOA+10,1,*91)
         END IF
         JW(JW(IOS)+2*IOA-1)=JW(JW(IOS)+2*IOA-1)+1
      END IF
   30 NEXT=NEXT+LENG
      NTOT=NTOT-LENG
      IF(NTOT.GT.0) GOTO 10
 
C end bank loop
 
*     WORD COUNT ERROR
      IF(NTOT.LT.0) GOTO 92
*     SET LENGTH OF LIST
      JW(JW(JLS))=NLS
*     INSERT POINTER INTO NEW BANKS
*     START RECORD
      NTOT=JW(IRD)
      NSFT=0
      NEXT=IRD+1
      NAMI=JW(NEXT+(NHW-2))+1
 
C bank loop
 
   40 LENG=JW(NEXT+(NHW-1))+NHW
*     COMPARE NAME INDICES
      IF(JW(NEXT+(NHW-2)).NE.NAMI) THEN
*        NEW NAME
         NAMI=JW(NEXT+(NHW-2))
      ELSE
*       SAME NAME AS PREVIOUS
        IF(JW(IND-2).EQ.JW(NEXT+(NHW-3)).AND.IND+JW(IND)+1.EQ.NEXT) THEN
*           SAME NUMBER, ADD TO SAME BANK
            NSFT=NSFT+NHW
            JW(IND)=JW(IND)+LENG-NHW
            CALL UCOPY2(JW(NEXT+NHW),JW(NEXT),NTOT-NHW)
            NEXT=NEXT+LENG-NHW
            GOTO 70
         ELSE
            NIND=JW(IND-1)
            IF(JW(IND-2).LT.JW(NEXT+(NHW-3)).AND.NIND.EQ.0) GOTO 60
         END IF
      END IF
*     FIND LAST BANK, TO INSERT POINTER
      IND=NAMI+1
   50 NIND=JW(IND-1)
      IF(NIND.NE.0) THEN
         IF(JW(NIND-2).LT.JW(NEXT+(NHW-3))) THEN
            IND=NIND
            GOTO 50
         ELSE IF(JW(NIND-2).EQ.JW(NEXT+(NHW-3))) THEN
*           DROP BANK WITH SAME NAME,NR
            NAME=CHAINT(JW(NEXT+(NHW-4)))
            JW(1)=1
            MDRP=MDROP(JW,NAME,JW(NEXT+(NHW-3)))
            NIND=JW(IND-1)
         END IF
      END IF
*     INSERT POINTER
   60 JW(IND-1)=NEXT+(NHW-1)
      IND=NEXT+(NHW-1)
      JW(IND-1)=NIND
      NEXT=NEXT+LENG
*     TRANSITION TO NEXT BANK
   70 NTOT=NTOT-LENG
      IF(NTOT.NE.0) GOTO 40
 
C end bank loop
 
      IF(NSFT.NE.0) THEN
         NSFT=JW(IRD)-NSFT
         IRD=MBANK(JW,'+REC',LUN,NSFT,1)
      END IF
*     RECORD COMPLETE - DROP +REC BANK
      JW(IRD)=0
      IRD=MDROP(JW,'+REC',LUN)
*     DUMP IF FLAG
C     IF(IFFF.NE.0) CALL BOSDP(JW)
      GOTO 90
*     ERROR CONDITIONS
*     ERROR IN WORD LENGTH
   92 JW2=JW2+1
*     NO SPACE FOR WORK BANK
   91 JW2=JW2+1
      JW(JW(JLS))=0
      IRD=MDROP(JW,'+REC',LUN)
   90 JW(2)=JW2
  100 RETURN
      END
