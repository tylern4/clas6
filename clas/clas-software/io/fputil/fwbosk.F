CDECK  ID>, FWBOSK.
      SUBROUTINE FWBOSK(JW,LIST,KEYNAM,NUM1,NUM2,NDATWD)
c
c_begin_doc
c  RCS ID string
c  $Id: fwbosk.F,v 1.1.1.1 1997/03/28 20:55:48 marki Exp $
c
c  Purpose:
c  --------
c        define key for list LIST (prepare for FWBOS)
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER JW(*),NUM1,NUM2,NDATWD
      CHARACTER*(*) LIST,KEYNAM
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
     1'$Id: fwbosk.F,v 1.1.1.1 1997/03/28 20:55:48 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FWBOSK.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NBYTWD
      PARAMETER (NBYTWD=4)
      INTEGER MBANK,MLINK,IKY,ICH
      CHARACTER NAMKEY*8
      CHARACTER*4 KEYB, CHA
      DATA KEYB/'+KYE'/
c_end_var
c----6----------------------------------------------------------------72
      NAMKEY=KEYNAM
      KEYB(4:4)=LIST(1:1)
*     create key-bank WITH 40 words
      IKY=MBANK(JW,KEYB,0,40,1)
      IF(IKY.EQ.0) STOP 'FWBOSK: Record key bank can not be created'
*
*      1      NAMKEY(1:4)
*      2      NAMKEY(5:8)
*      3      NUM1
*      4      NUM2
*      5      NDATWD
*      6
*      7      0
*      8      0
*      9      0
*     10      0
*     11...40 class flags
      CHA=NAMKEY(1:4)
*  char*4 --> hollerith 4H
      CALL UCTOH(CHA,ICH,4,4)
      JW(IKY+1)=ICH
      CHA=NAMKEY(5:8)
*  char*4 --> hollerith 4H
      CALL UCTOH(CHA,ICH,4,4)
      JW(IKY+2)=ICH
      JW(IKY+3)=NUM1
      JW(IKY+4)=NUM2
      JW(IKY+5)=NDATWD
      JW(IKY+6)=0
      JW(IKY+7)=0
      JW(IKY+8)=0
      JW(IKY+9)=0
      JW(IKY+10)=0
*     ... and unpack class flags
      CALL FCLUP(NDATWD,JW(IKY+11))
      GOTO 100
      ENTRY FWBOSR(JW,LIST,KEYNAM,NUM1,NUM2,NDATWD)
      NAMKEY=KEYNAM
      KEYB(4:4)=LIST(1:1)
      IKY=MLINK(JW,KEYB,0)
      IF(IKY.EQ.0) THEN
         KEYNAM=' '
         NUM1=0
         NUM2=0
         NDATWD=0
         GOTO 100
      END IF
      ICH=JW(IKY+1)
*  hollerith 4H --> char*4
      CALL UHTOC(ICH,4,CHA,4)
      NAMKEY(1:4)=CHA
      ICH=JW(IKY+2)
*  hollerith 4H --> char*4
      CALL UHTOC(ICH,4,CHA,4)
      NAMKEY(5:8)=CHA
      KEYNAM=NAMKEY
      NUM1=JW(IKY+3)
      NUM2=JW(IKY+4)
      NDATWD=JW(IKY+5)
  100 RETURN
      END
