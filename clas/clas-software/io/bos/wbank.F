CDECK  ID>, WBANK.
      SUBROUTINE WBANK(JW,ID,NCOL,NROW,*)
c
c_begin_doc
c  RCS ID string
c  $Id: wbank.F,v 1.1.1.1 1997/03/28 20:49:42 marki Exp $
c
c  Purpose:
c  --------
c     CREATE WORKBANK AT INDEX ID OR CHANGE LENGTH
c        JW(1)=1   NO RESET TO ZERO OF ADDITIONAL WORDS
c        JW(1)=4   IF LENGTH IS REDUCED, BANK IS REALLY MADE SHORTER
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
      INTEGER JW(*),ID(1),NCOL,NROW
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
     1'$Id: wbank.F,v 1.1.1.1 1997/03/28 20:49:42 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='WBANK.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER INTCHA,LOCF
      INTEGER ND,NRESRV,JW1,JW2,LD,NCACH,IZER,NZER,NDIF,ICOP,NCOP,JD
      INTEGER HWORK
      DATA HWORK/0/
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'WBANK')
C check input arguments
      ND=NCOL*NROW
      IF(ND.LT.0) THEN ! value of argument ND less then 0
        IF(JW(LUP).GT.0) WRITE(JW(LUP),1001) ND
 1001   FORMAT('0BOS--WBANK- WRONG ARGUMENT - STOP'/14X,'ND=',I10)
        CALL BABEND('WBANK')
      END IF
      NRESRV=NRESR ! NRESR - the number of reserved words
   10 ND=NCOL*NROW
      IF(HWORK.EQ.0) HWORK=INTCHA('WORK') ! first work bank are creating
      JW1  =JW(1)
      JW2  =0
      JW(1)=0
      LD=LOCF(ID(1))-JW(LJW)+1
C
   20 IF(ID(1).EQ.0) THEN         ! create new work bank
         IF(ND+NHW+NRESRV.GT.JW(IWK)-JW(IGP)) THEN
            NCACH=JW(IWK)-(JW(IGP)+ND+NHW+NRESRV)+JW(18)
            IF(NCACH.LT.0) CALL BDCACH(NCACH)
            CALL WGARB(JW)
            IF(ND+NHW+NRESRV.GT.JW(IWK)-JW(IGP)) GOTO 91
         END IF
         JW(IWK)=JW(IWK)-ND-NHW
         ID(1)=JW(IWK)+(NHW-1)
         JW(ID(1)-5)=NCOL
         JW(ID(1)-4)=NROW
         JW(ID(1)-3)=HWORK
         JW(ID(1)-2)=ND+NHW
         JW(ID(1)-1)=LD
         JW(ID(1)  )=ND
         IZER=ID(1)+1
         NZER=ND
      ELSE                      ! dealing with existing bank
C
C        VERIFY VALUE OF WORK BANK INDEX ID
C
         IF(ID(1).GT.JW(IWK).AND.ID(1).LT.JW(NJW)) THEN
            IF(JW(ID(1)).GE.0.AND.JW(ID(1)-1).EQ.LD) GOTO 25
         END IF
         IF(JW(LUP).GT.0) WRITE(JW(LUP),1002) ID(1)
 1002    FORMAT('0BOS--WBANK- INDEX ID INCORRECT - STOP'/14X,'ID=',I12)
         CALL BABEND('WBANK')
C
C        CHANGE LENGTH OF EXISTING BANK
C
   25    NDIF=ND+NHW-JW(ID(1)-2)
         IF(NDIF.LE.0.AND.JW1.NE.4) THEN
C
C           CHANGE IN PLACE
C
            IZER=ID(1)+JW(ID(1))+1
            NZER=ND-JW(ID(1))
            JW(ID(1)  )=ND
         ELSE
C
C           CHANGE REQUIRES SHIFT
C
            IF(NDIF.GT.0.AND.NDIF+NRESRV.GT.JW(IWK)-JW(IGP)) THEN
               IF(JW(11).EQ.1) THEN
                  NCACH=JW(IWK)-(JW(IGP)+NDIF+NRESRV)+JW(18)
                  IF(NCACH.LT.0) CALL BDCACH(NCACH)
               END IF
               CALL WGARB(JW)
*              FLR'S MOD for IBM compiler
               ID(1)=JW(LD)
               IF(NDIF+NRESRV.GT.JW(IWK)-JW(IGP)) GOTO 92
            END IF
            ICOP=JW(IWK)
            IF(NDIF.GT.0) THEN
               NCOP=ID(1)+JW(ID(1))+1-ICOP
            ELSE
               NCOP=ID(1)+ND+1-ICOP
            END IF
            NZER=ND-JW(ID(1))
            IZER=ID(1)+JW(ID(1)-2)-NZER-(NHW-1)
            CALL UCOPY2(JW(ICOP),JW(ICOP-NDIF),NCOP)
            IF(JW(IGW).NE.0.AND.JW(IGW).LT.ID(1))
     &                                      JW(IGW)=JW(IGW)-NDIF
            ID(1)=ID(1)-NDIF
            JW(ID(1)-5)=NCOL
            JW(ID(1)-4)=NROW
            JW(ID(1)-2)=ND+NHW
            JW(ID(1)  )=ND
            JW(IWK)=JW(IWK)-NDIF
            JD=JW(IWK)+(NHW-1)
   30       IF(JD.LT.ID(1)) THEN
               IF(JW(JD).GE.0) JW(JW(JD-1))=JD
               JD=JD+JW(JD-2)
               GOTO 30
            END IF
         END IF
      END IF
C
C     PRESET TO ZERO
C
C if defined(ALLIANT)||defined(SUN)
      IF(NZER.GT.0.AND.JW1.NE.1) CALL VZERO(JW(IZER),NZER)
C else
C      IF(NZER.GT.0.AND.JW1.NE.1) THEN
C        DO M=1,NZER
C          JW(IZER-1+M)=0
C        ENDDO
C      END IF
C endif
      GOTO 90
C
C     CODE 2   INSUFFICIENT SPACE TO INCREASE LENGTH OF EXISTING BANK
C
   92 JW2=JW2+1
C
C     CODE 1   INSUFFICIENT SPACE TO CREATE NEW BANK
C
   91 JW2=JW2+1
      IF(JW(LUP).NE.0.AND.JW(LUW).NE.0)
     1   CALL BBRET(JW,JW2,'WBANK','WORK')
      JW(2)=JW2
      JW(IRC+JW2)=JW(IRC+JW2)+1
      GOTO 101
C
C     INTERNAL ENTRY
C     CREATE WORKBANK AT INDEX ID OR CHANGE LENGTH (NO RESERVED WORDS)
C
      ENTRY WBANC(JW,ID,NCOL,NROW,*)
      NRESRV=0
      GOTO 10
C exit
   90 JW(2)=0
  100 RETURN
  101 RETURN 1
      END
