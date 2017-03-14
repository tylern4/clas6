CDECK  ID>, BOSFM.
      SUBROUTINE BOSFM
c
c_begin_doc
c  RCS ID string
c  $Id: bosfm.F,v 1.1.1.1 1997/03/28 20:49:32 marki Exp $
c
c  Purpose:
c  --------
c     PRINT FORMATS
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
     1'$Id: bosfm.F,v 1.1.1.1 1997/03/28 20:49:32 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BOSFM.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER M,N,LP,I,J,ID,L,L2,JL,JR,NDIV,LSTART,KL,K
      CHARACTER*4 CHAINT,NAMA,NAMN(5)
      CHARACTER*110 TEXT
      CHARACTER*1 NUMBER(0:9)
      CHARACTER*18 TEXTN(5)
      DATA NUMBER/'0','1','2','3','4','5','6','7','8','9'/
c_end_var
c----6----------------------------------------------------------------72
      IF(IW(LUP).LE.0) GOTO 100
      WRITE(IW(LUP),1001)
      N=IW(IDNAM)
      LP=0
      DO 40 I=1,N
      NAMA=CHAINT(IW(IDNAM+I))
      TEXT=' '
      ID=IDFMT+I
      IF(IW(ID).EQ.0) THEN
         TEXT(1:11)='F (default)'
         L=11
         GOTO 30
      END IF
      ID=IW(ID)
      IF(IW(ID+3).NE.0) THEN
         L=3
         IF(IW(ID+3).EQ.1) TEXT(1:3)='B32'
         IF(IW(ID+3).EQ.2) TEXT(1:3)='B16'
         IF(IW(ID+3).EQ.3) TEXT(1:3)='B08'
      ELSE
         L=0
         J=4
   10    IF(IW(ID).GE.J) THEN
            IF(L.GE.100) THEN
               L2=L+2
               TEXT(L:L2)='...'
               GOTO 30
            END IF
            JL=IW(ID+J)/10
            JR=MOD(IW(ID+J),10)
            J =J+1
            IF(JL.EQ.0) THEN
               L=L+1
               TEXT(L:L)=')'
               GOTO 10
            ELSE
            IF(L.NE.0) THEN
               IF(TEXT(L:L).NE.'(') THEN
                  L=L+1
                  TEXT(L:L)=','
               END IF
            END IF
C
C              INSERT NUMBER
C
               IF(JL.NE.1.AND.JL.NE.100000) THEN
                  NDIV=10000
                  LSTART=L
                  DO 20 KL=1,5
                  K=MOD(JL/NDIV,10)
                  NDIV=NDIV/10
                  IF(K.NE.0.OR.L.NE.LSTART) THEN
                     L=L+1
                     TEXT(L:L)=NUMBER(K)
                  END IF
   20             CONTINUE
               END IF
               IF(JR.EQ.0) THEN
                  L=L+1
                  TEXT(L:L)='('
               ELSE
                  L=L+1
                  IF(JR.EQ.1) TEXT(L:L)='F'
                  IF(JR.EQ.2) TEXT(L:L)='I'
                  IF(JR.EQ.3) TEXT(L:L)='A'
               END IF
               GOTO 10
            END IF
         END IF
      END IF
   30 IF(L.LE.18) THEN
         LP=LP+1
         NAMN(LP)=NAMA
         TEXTN(LP)=TEXT(1:18)
         IF(LP.EQ.5) THEN
            WRITE(IW(LUP),1002) (NAMN(M),TEXTN(M),M=1,5)
            LP=0
         END IF
      ELSE
         IF(LP.NE.0) THEN
            WRITE(IW(LUP),1002) (NAMN(M),TEXTN(M),M=1,LP)
            LP=0
         END IF
         WRITE(IW(LUP),1003) NAMA,TEXT
      END IF
   40 CONTINUE
      IF(LP.NE.0) THEN
         WRITE(IW(LUP),1002) (NAMN(M),TEXTN(M),M=1,LP)
      END IF
  100 RETURN
 1001 FORMAT('0  ',5('NAME FORMAT........',6X)/)
 1002 FORMAT(3X,5(A4,1X,A18,2X))
 1003 FORMAT(3X,A4,1X,A110)
      END
