CDECK  ID>, BBNIN.
      SUBROUTINE BBNIN(JW,LUN)
c
c_begin_doc
c  RCS ID string
c  $Id: bbnin.F,v 1.1.1.1 1997/03/28 20:55:40 marki Exp $
c
c  Purpose:
c  --------
c        CHECK BANK LENGTH AND INSERT NAMEINDEX INTO POS. JW(IND-1)
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
      INTEGER JW(*),LUN
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
     1'$Id: bbnin.F,v 1.1.1.1 1997/03/28 20:55:40 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BBNIN.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER MLINK,MDROP,NAMIND,JW2,IRD,NTOT,IND,NAMA,NW,NWNAM
      CHARACTER*4 NAME,CHAINT
c_end_var
c----6----------------------------------------------------------------72
      JW2=1
      IRD=MLINK(JW,'+REC',LUN)
      IF(IRD.EQ.0) GOTO 90
      NTOT=JW(IRD)
      IND=IRD+NHW
      NAMA=JW(IND-3)+1
C
C     NEXT BANK - CHECK LENGTH
C
   10 NW=JW(IND)+NHW
      IF(MIN(NTOT,NW).LT.NHW.OR.NW.GT.NTOT) THEN
         IRD=MDROP(JW,'+REC',LUN)
         GOTO 90
      END IF
      IF(JW(IND-3).NE.NAMA) THEN
C
C        NEW NAME - GET NAMEINDEX
C
         NAMA=JW(IND-3)
C------ KEEP, NAMEZ -------------------------------------------------
C     STATEMENTS INSERTED IN SUBPROGRAM
C     RETURNS NAMI = NAME-INDEX FOR INTEGER NAMA
C                  = 0 IF NAME NOT YET USED
      NWNAM=IW(IDNAM)
      IW(IDNAM)=NAMA
      NAMI=IW(IDNAM-2)+MOD(IABS(IW(IDNAM)),NPRIM)-3
    1 NAMI=IW(IDPTR+NAMI)
      IF(IW(IDNAM+NAMI).NE.IW(IDNAM)) GOTO 1
      IW(IDNAM)=NWNAM
      IF(NAMI.NE.0) NAMI=NAMI+NSYST
C--------------------------------------------------------------------
 
         IF(NAMI.EQ.0) THEN
            NAME=CHAINT(NAMA)
            NAMI=NAMIND(NAME)
         END IF
      END IF
      JW(IND-1)=NAMI
      IND=IND+NW
      NTOT=NTOT-NW
      IF(NTOT.NE.0) GOTO 10
      JW2=0
   90 JW(2)=JW2
  100 RETURN
      END
