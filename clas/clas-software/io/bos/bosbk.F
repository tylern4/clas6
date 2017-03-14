CDECK  ID>, BOSBK.
      SUBROUTINE BOSBK(JW)
c
c_begin_doc
c  RCS ID string
c  $Id: bosbk.F,v 1.1.1.1 1997/03/28 20:49:31 marki Exp $
c
c  Purpose:
c  --------
c     PRINT TABLE OF ALL BANKS
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
     1'$Id: bosbk.F,v 1.1.1.1 1997/03/28 20:49:31 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BOSBK.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER LUNP,I,J,K,L,NDEL,IND,LINE(3,5)
      CHARACTER*4 CHAINT
      CHARACTER*25 TEXTN,TEXTW
      CHARACTER*11 TEXT
      DATA    TEXTN/'     NAME.......NR....ND  '/
      DATA    TEXTW/'     NAME.......NT....ND  '/
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'BOSBK')
      LUNP=JW(LUP)
      IF(LUNP.EQ.0) GOTO 100
      WRITE(LUNP,1001) JW(NJA),CHAINT(JW(IAR+1)),CHAINT(JW(IAR+2))
C
C     NAMED BANKS
C
      TEXT='NAMED BANKS'
      WRITE(LUNP,1002) TEXT
      WRITE(LUNP,1003) (TEXTN,L=1,3)
      NDEL=0
      I   =0
      IND=JW(INM)+(NHW-1)
   10 IF(IND.GT.JW(IGP)) GOTO 20
      IF(JW(IND).LT.0) THEN
C
C        DELETED BANK
C
         NDEL=NDEL-JW(IND)
         IND =IND -JW(IND)
      ELSE
C
C        NOT DELETED BANK
C
         IF(I.GE.3) THEN
            WRITE(LUNP,1004) (CHAINT(LINE(1,J)),(LINE(K,J),K=2,3),J=1,I)
            I=0
         END IF
         I=I+1
         LINE(1,I)=JW(IND-3)
         LINE(2,I)=JW(IND-2)
         LINE(3,I)=JW(IND  )
         IND=IND+JW(IND)+NHW
      END IF
      GOTO 10
   20 IF(I.NE.0) WRITE(LUNP,1004) (CHAINT(LINE(1,J)),(LINE(K,J),
     +           K=2,3),J=1,I)
      WRITE(LUNP,1005) NDEL
C
C     WORK BANKS
C
      TEXT='WORK BANKS '
      WRITE(LUNP,1002) TEXT
      WRITE(LUNP,1003) (TEXTW,L=1,3)
      NDEL=0
      I   =0
      IND=JW(IWK)+(NHW-1)
   30 IF(IND.GT.JW(NJW)) GOTO 40
      IF(JW(IND).LT.0) THEN
C
C        DELETED BANK
C
         NDEL=NDEL-JW(IND)
      ELSE
C
C        NOT DELETED BANK
C
         IF(I.GE.3) THEN
            WRITE(LUNP,1004) (CHAINT(LINE(1,J)),(LINE(K,J),K=2,3),J=1,I)
            I=0
         END IF
         I=I+1
         LINE(1,I)=JW(IND-3)
         LINE(2,I)=JW(IND-2)
         LINE(3,I)=JW(IND  )
      END IF
      IND=IND+JW(IND-2)
      GOTO 30
   40 IF(I.NE.0) WRITE(LUNP,1004) (CHAINT(LINE(1,J)),(LINE(K,J),
     +           K=2,3),J=1,I)
      WRITE(LUNP,1005) NDEL
C
  100 RETURN
 1001 FORMAT(/'BOS++BOSBK+  ARRAY NR',I3,5X,2A4,5X,'TABLE OF ALL BANKS')
 1002 FORMAT(/'-----------  ',A)
 1003 FORMAT(/3(A))
 1004 FORMAT(1X,3(4X,A4,I9,I6,2X))
 1005 FORMAT(/'    PLUS',I10,' DELETED WORDS')
      END
