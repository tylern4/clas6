CDECK  ID>, BSWAP.
      SUBROUTINE BSWAP(JW,NAM1,NAM2)
c
c_begin_doc
c  RCS ID string
c  $Id: bswap.F,v 1.1.1.1 1997/03/28 20:49:35 marki Exp $
c
c  Purpose:
c  --------
c     EXCHANGE NAMES NAM1 AND NAM2 IN NAMED BANKS
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
      INTEGER    JW(*)
      CHARACTER*4 NAM1,NAM2
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
     1'$Id: bswap.F,v 1.1.1.1 1997/03/28 20:49:35 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BSWAP.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER JW1,JW2,NAMA1,NI1,IN1,NAMA2,NI2,IN2,JND,IND,INTCHA,NAMIND
c_end_var
c----6----------------------------------------------------------------72
C-----------------------------------------------------------------------
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'BSWAP')
      JW1=JW(1)
      JW(1)=0
      JW2=0
      NAMA1=INTCHA(NAM1)
      NI1  =NAMIND(NAM1)
      IN1  =JW(NI1)
      NAMA2=INTCHA(NAM2)
      NI2  =NAMIND(NAM2)
      IN2  =JW(NI2)
C     RESET LINK BANKS
      JND=JW(IDL)+NI1-NSYST
      IF(JW(JND).NE.0) JW(JW(JND))=0
      JND=JW(IDL)+NI2-NSYST
      IF(JW(JND).NE.0) JW(JW(JND))=0
      IND=NI1+1
   10 IND=JW(IND-1)
      IF(IND.NE.0) THEN
         JW(IND-3)=NAMA2
         GOTO 10
      END IF
      IND=NI2+1
   20 IND=JW(IND-1)
      IF(IND.NE.0) THEN
         JW(IND-3)=NAMA1
         GOTO 20
      END IF
      JW(NI1)=IN2
      JW(NI2)=IN1
  100 RETURN
      END
