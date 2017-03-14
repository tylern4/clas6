CDECK  ID>, MDROP.
      INTEGER FUNCTION MDROP(JW,NAME,NR)
c
c_begin_doc
c  RCS ID string
c  $Id: mdrop.F,v 1.1.1.1 1997/03/28 20:49:37 marki Exp $
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
      INTEGER JW(*),NR
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
     1'$Id: mdrop.F,v 1.1.1.1 1997/03/28 20:49:37 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='MDROP.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER JW1,NWNAM,NLPL,NDIF,JND
      INTEGER NAMA
      CHARACTER*4 NAME
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'MDROP')
      CALL UCTOH(NAME,NAMA,4,4)
      JW1=JW(1)
      JW(1)=0
      JW(2)=0
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
 
      IF(NAMI.EQ.0) GOTO 100
C------ KEEP, INDEX -------------------------------------------------
C     STATEMENTS INSERTED IN SUBPROGRAM
C     RETURNS INDI = INDEX OF BANK WITH NAME-INDEX NAMI AND NUMBER NR
C                    OR ZERO
C             INDJ = PREVIOUS INDEX (JW(INDJ-1) CONTAINS INDI)
C
      INDI=NAMI+1
      NLPL=0
    2 INDJ=INDI
      INDI=JW(INDJ-1)
      IF(INDI.NE.0) THEN
         NLPL=NLPL+1
         IF(JW(INDI-2).LT.NR) GOTO 2
         IF(JW(INDI-2).GT.NR) INDI=0
      END IF
C--------------------------------------------------------------------
 
      IF(INDI.NE.0) THEN
C
C        DROP BANK
C
         JW(LCH)=JW(LCH)+1
         JW(INDJ-1)=JW(INDI-1)
         NDIF=JW(INDI)+NHW
         IF(JW1.NE.1.AND.JW(IGP).EQ.INDI+NDIF-(NHW-1)) THEN
C
C           LAST BANK
C
            JW(IGP)=JW(IGP)-NDIF
         ELSE
C
C           NOT LAST BANK OR JW(1)=1 (FORCE DROP)
C
            JW(INDI)=-NDIF
            JW(NDN)=JW(NDN)+NDIF
            JW(IGN)=MIN(JW(IGN),INDI)
         END IF
C
C        RESET LINK BANKS
C
         JND=JW(IDL)+NAMI-NSYST
         IF(JW(JND).NE.0) JW(JW(JND))=0
      END IF
  100 MDROP=0
      RETURN
      END
