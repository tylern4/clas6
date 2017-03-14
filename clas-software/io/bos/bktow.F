CDECK  ID>, BKTOW.
      SUBROUTINE BKTOW(JW1,NAME,NR,JW2,ID,*)
c
c_begin_doc
c  RCS ID string
c  $Id: bktow.F,v 1.1.1.1 1997/03/28 20:49:30 marki Exp $
c
c  Purpose:
c  --------
c     Copy named bank to work bank
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
      INTEGER JW1(*),JW2(*),ID(*),NR
      CHARACTER*4 NAME
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
     1'$Id: bktow.F,v 1.1.1.1 1997/03/28 20:49:30 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BKTOW.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NBYTWD
      PARAMETER ( NBYTWD = 4 )
      INTEGER IND,ND1,IFM,IDF,ND2,JWTWO,MLINK
c_end_var
c----6----------------------------------------------------------------72
      JW1(1)=0
      JW2(1)=0
      IND= MLINK(JW1,NAME,NR)
      IF(IND.EQ.0) GOTO 10
      ND1=JW1(IND)
      IFM=0
      IDF=IW(IDFMT+NAMI-NSYST)
      IF(IDF.NE.0) IFM=IW(IDF+3)
      IF(IFM.EQ.3) THEN
         ND2=ND1*NBYTWD
      ELSE IF(IFM.EQ.2) THEN
         ND2=ND1*NBYTWD/2
      ELSE IF(IFM.EQ.1) THEN
         ND2=ND1*NBYTWD/4
      ELSE
         ND2=ND1
      END IF
C-- create work bank with NROW=1 and copy information from named bank
      CALL WBANK(JW2,ID,ND2,1,*20)
      IF(IFM.EQ.3) THEN
         CALL BLO8W(JW1(IND+1),1,JW2(ID(1)+1),1,ND2)
      ELSE IF(IFM.EQ.2) THEN
         CALL BLO16W(JW1(IND+1),1,JW2(ID(1)+1),1,ND2)
      ELSE IF(IFM.EQ.1) THEN
         CALL BLO32W(JW1(IND+1),1,JW2(ID(1)+1),1,ND2)
      ELSE
         CALL UCOPY(JW1(IND+1),JW2(ID(1)+1),ND2)
      END IF
C-- set NCOL and NROW
      JW2(ID(1)-5)=JW1(IND-5)
      JW2(ID(1)-4)=JW1(IND-4)
      GOTO 100
C
C     NAMED BANK NOT FOUND
C
   10 JW2(2)=0
      JW1(2)=6
      JW1(IRC+JW1(2))=JW1(IRC+JW1(2))+1
      GOTO 101
C
C     INSUFFICIENT SPACE
C
   20 JWTWO=JW2(2)
      JW2(2)=0
      JW1(2)=JWTWO
      JW1(IRC+JW1(2))=JW1(IRC+JW1(2))+1
      IF(JW1(LUP).NE.0.AND.JW1(LUW).NE.0)
     +   CALL BBRET(JW1,JW1(2),'BKTOW',NAME)
      GOTO 101
C
  100 RETURN
  101 RETURN 1
      END
