CDECK  ID>, BOSFMT.
      SUBROUTINE BOSFMT(NAME,TEXTA)
c
c_begin_doc
c  RCS ID string
c  $Id: bosfmt.F,v 1.1.1.1 1997/03/28 20:49:33 marki Exp $
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
      CHARACTER*4 NAME
      CHARACTER*(*) TEXTA
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
     1'$Id: bosfmt.F,v 1.1.1.1 1997/03/28 20:49:33 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BOSFMT.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NAMIND,I,NAMA,ID,L,J,L2,JL,JR,NDIV,LSTART,KL,K
      CHARACTER*4 TEXT*110, NUMBER(0:9)*1
      DATA NUMBER/'0','1','2','3','4','5','6','7','8','9'/
c_end_var
c----6----------------------------------------------------------------72
      TEXT=' '
      I   =NAMIND(NAME)
      NAMA=IW(IDNAM+I)
      ID=IDFMT+I-NSYST
      IF(IW(ID).EQ.0) THEN
         TEXT(1:11)='B32 default'
         L=11
         GOTO 30
      END IF
      ID=IW(ID)
      IF(IW(ID+3).NE.0) THEN
         L=3
         IF(IW(ID+3).EQ.1) TEXT='B32'
         IF(IW(ID+3).EQ.2) TEXT='B16'
         IF(IW(ID+3).EQ.3) TEXT='B08'
      ELSE
         L=0
         J=4
   10    IF(IW(ID).GE.J) THEN
            IF(L.GE.100) THEN
               L2=L+2
               TEXT(L:L2)='...'
               L=L2
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
                  IF(JR.EQ.4) TEXT(L:L)='J'
                  IF(JR.EQ.5) TEXT(L:L)='K'
               END IF
               GOTO 10
            END IF
         END IF
      END IF
   30 TEXTA=TEXT
      IF(L.GT.LEN(TEXTA)) THEN
         L=LEN(TEXTA)
         TEXTA(L-2:L)='...'
      END IF
  100 RETURN
      END
