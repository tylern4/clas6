CDECK  ID>, MSWAP.
      INTEGER FUNCTION MSWAP(JW,NAME1,NR1,NAME2,NR2)
c
c_begin_doc
c  RCS ID string
c  $Id: mswap.F,v 1.1.1.1 1997/03/28 20:49:38 marki Exp $
c
c  Purpose:
c  --------
c     if both banks exist: exchange name,nr and return index of
c        (new) bank (NAME1,NR1)
c
c     if only one bank exists, change NAME,NR to the other NAME and
c        NR and return index of this bank
c
c     if both banks do not exist, return 0
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
      INTEGER    JW(*),NR1,NR2
      CHARACTER*4 NAME1,NAME2
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
     1'$Id: mswap.F,v 1.1.1.1 1997/03/28 20:49:38 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='MSWAP.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NAMIND,INTCHA
      INTEGER NAMI1,INDI1,INDJ1,NAMI2,INDI2,INDJ2,INDK1,INDK2
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'MSWAP')
      JW(1)=0
      JW(2)=0
*     get INDI1 and INDJ1 for first bank
      NAMI1=NAMIND(NAME1)
      INDI1=NAMI1+1
   10 INDJ1=INDI1
      INDI1=JW(INDJ1-1)
      IF(INDI1.NE.0) THEN
         IF(JW(INDI1-2).LT.NR1) GOTO 10
         IF(JW(INDI1-2).GT.NR1) INDI1=0
      END IF
*     bridge index("dropping"), if bank existing
      IF(INDI1.NE.0) JW(INDJ1-1)=JW(INDI1-1)
 
*     get INDI2 and INDJ2 for second bank
      NAMI2=NAMIND(NAME2)
      INDI2=NAMI2+1
   20 INDJ2=INDI2
      INDI2=JW(INDJ2-1)
      IF(INDI2.NE.0) THEN
         IF(JW(INDI2-2).LT.NR2) GOTO 20
         IF(JW(INDI2-2).GT.NR2) INDI2=0
      END IF
*     bridge index("dropping"), if bank existing
      IF(INDI2.NE.0) JW(INDJ2-1)=JW(INDI2-1)
 
*     treat (previous) first bank
      IF(INDI1.NE.0) THEN
*        insert new bank name and number, ...
         JW(INDI1-3)=INTCHA(NAME2)
         JW(INDI1-2)=NR2
*        ... determine "from_index" INDJ2 again ...
         INDK2=NAMI2+1
   30    INDJ2=INDK2
         INDK2=JW(INDJ2-1)
         IF(INDK2.NE.0) THEN
            IF(JW(INDK2-2).LT.NR2) GOTO 30
         END IF
*        ... and connect at second place
         JW(INDI1-1)=JW(INDJ2-1)
         JW(INDJ2-1)=INDI1
      END IF
 
*     treat (previous) second bank
      IF(INDI2.NE.0) THEN
*        insert new bank name and number, ...
         JW(INDI2-3)=INTCHA(NAME1)
         JW(INDI2-2)=NR1
*        ... determine "from_index" INDJ1 again ...
         INDK1=NAMI1+1
   40    INDJ1=INDK1
         INDK1=JW(INDJ1-1)
         IF(INDK1.NE.0) THEN
            IF(JW(INDK1-2).LT.NR1) GOTO 40
         END IF
*        ... and connect at first place
         JW(INDI2-1)=JW(INDJ1-1)
         JW(INDJ1-1)=INDI2
      END IF
 
      IF(INDI2.NE.0) THEN
          MSWAP=INDI2
      ELSE
          MSWAP=INDI1
      END IF
 
      RETURN
      END
