CDECK  ID>, WSWAP.
      SUBROUTINE WSWAP(JW,ID1,ID2)
c
c_begin_doc
c  RCS ID string
c  $Id: wswap.F,v 1.1.1.1 1997/03/28 20:49:43 marki Exp $
c
c  Purpose:
c  --------
c     SWAP THE WORK BANK INDICES ID1 AND ID2
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
      INTEGER JW(*),ID1(1),ID2(1)
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
     1'$Id: wswap.F,v 1.1.1.1 1997/03/28 20:49:43 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='WSWAP.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER LOCF,LD1,LD2,JD
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'WSWAP')
      JW(1)=0
      JW(2)=0
C
C     VERIFY VALUE OF WORK BANK INDEX ID1
C
      LD1=LOCF(ID1(1))-JW(LJW)+1
      IF(ID1(1).GT.JW(IWK).AND.ID1(1).LT.JW(NJW)) THEN
         IF(JW(ID1(1)-1).EQ.LD1) GOTO 10
      END IF
      IF(JW(LUP).GT.0) WRITE(JW(LUP),1001) ID1(1)
      CALL BABEND('WSWAP')
C
C     VERIFY VALUE OF WORK BANK INDEX ID2
C
   10 LD2=LOCF(ID2(1))-JW(LJW)+1
      IF(ID2(1).GT.JW(IWK).AND.ID2(1).LT.JW(NJW)) THEN
         IF(JW(ID2(1)-1).EQ.LD2) GOTO 20
      END IF
      IF(JW(LUP).GT.0) WRITE(JW(LUP),1001) ID2(1)
      CALL BABEND('WSWAP')
C
C     INSERT NEW POINTERS AND EXCHANGE INDICES
C
   20 IF(ID1(1).NE.0) JW(ID1(1)-1)=LD2
      IF(ID2(1).NE.0) JW(ID2(1)-1)=LD1
      JD    =ID1(1)
      ID1(1)=ID2(1)
      ID2(1)=JD
C
  100 RETURN
 1001 FORMAT('0BOS--WSWAP-  INDEX ID INCORRECT - STOP'/
     1 14X,'ID= ',I12)
      END
