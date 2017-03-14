CDECK  ID>, NAMIND.
      INTEGER FUNCTION NAMIND(NAME)
c
c_begin_doc
c  RCS ID string
c  $Id: namind.F,v 1.1.1.1 1997/03/28 20:49:39 marki Exp $
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
     1'$Id: namind.F,v 1.1.1.1 1997/03/28 20:49:39 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='NAMIND.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NAMINC,NWNAM,INAM,JNAM,NUSED,NAMAA
      INTEGER NAMA
      CHARACTER*4 NAME
c_end_var
c----6----------------------------------------------------------------72
      IF(IW(ICK).NE.12345) CALL BBNOT(IW,'NAMIND')
      CALL UCTOH(NAME,NAMA,4,4)
   10 NWNAM=IW(IDNAM) ! IDNAM - index of work bank with names
      IW(IDNAM)=NAMA
C
C     CALCULATE HASH INDEX FROM NAME
C
      INAM=IW(IDNAM-2)+MOD(IABS(NAMA),NPRIM)-3
    1 JNAM=INAM
      INAM=IW(IDPTR+INAM)
C
C     COMPARE WITH ENTRY IN BANK OF NAMES
C
      IF(IW(IDNAM+INAM).NE.NAMA) GOTO 1
      IW(IDNAM)=NWNAM
      IF(INAM.EQ.0) THEN
C
C        ADD NEW ENTRY INTO BANK OF NAMES
C
         IF(NWNAM+NHW.GE.IW(IDNAM-2)) THEN
C
C           TOO MANY NAMES USED
C
            NUSED=IW(IDNAM-2)-NHW
            IF(IW(LUP).NE.0) THEN
               WRITE(IW(LUP),1001)
               CALL BBUFM(IW(IDNAM+1),1,NUSED,IW(LUP))
            END IF
            CALL BABEND('NAMIND')
         END IF
         INAM=NWNAM+1
         IW(IDNAM)=INAM
         IW(IDNAM+INAM)=NAMA
         IW(IDPTR+JNAM)=INAM
      END IF
      NAMIND=INAM+NSYST
      IW(1)=0
      IW(2)=0
      GOTO 100
C
C
C
      ENTRY NAMINC(NAMAA)
      NAMA=NAMAA
      GOTO 10
  100 RETURN
 1001 FORMAT('BOS++NAMIND+  TOO MANY DIFFFERENT NAMES USED - STOP'/
     +14X,'USER RESPONSE: CALL BNAMES(NMAX) BEFORE INITIALIZATION',
     2  ' FOR A MAXIMUM OF NMAX DIFFERENT NAMES'/
     3  14X,'LIST OF ALL USED NAMES FOLLOWS'/)
      END
