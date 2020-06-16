CDECK  ID>, NBANK.
      INTEGER FUNCTION NBANK(NAME,NR,NCOL,NROW)
c
c_begin_doc
c  RCS ID string
c  $Id: nbank.F,v 1.1.1.1 1997/03/28 20:49:39 marki Exp $
c
c  Purpose:
c  --------
c     CREATE NAMED BANK OR CHANGE LENGTH OF EXISTING BANK
c
c     JW(1)=1 ADDITIONAL WORDS NOT ZEROD
c     JW(1)=2 MOVE (UNLESS LAST BANK)
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
      INTEGER NR,NCOL,NROW
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
     1'$Id: nbank.F,v 1.1.1.1 1997/03/28 20:49:39 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='NBANK.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER JW1,JW2,ND,NWNAM,IFD,IFL,IND,NN,IDR,M,NLPL,NDIF,NCACH,NCOP
      INTEGER MDROP,IFMT,NAMIND,IZERO,JND
      INTEGER JW(Nbcs)
      EQUIVALENCE (IW(1),JW(1))
      CHARACTER*4 CHNA,CHAINT
      INTEGER NAMA
c_end_var
c----6----------------------------------------------------------------72
      IF(JW(ICK).NE.12345) CALL BBNOT(JW,'NBANK')
      CALL UCTOH(NAME,NAMA,4,4)
      JW1=JW(1)
      JW2=0
      JW(1)=0
C------ KEEP, NCOLTOND ----------------------------------------------
C
C calculate ND from NCOL and NROW in according to bank FORMAT
C
      NAMI=NAMIND(NAME)
      IFMT=IW(IDFMT+NAMI-NSYST)
      IF(IFMT.NE.0) THEN
        IFMT=IW(IFMT+3)
        IF(IFMT.EQ.2) ND=(NCOL*NROW+1)/2
        IF(IFMT.EQ.3) ND=(NCOL*NROW+3)/4
        IF(IFMT.NE.2.AND.IFMT.NE.3) ND=NCOL*NROW
       ELSE
        ND=NCOL*NROW
      ENDIF
C--------------------------------------------------------------------
 
C
      IF(ND.LT.0) THEN
C
C        ARGUMENT ND LESS THAN ZERO
C
         IF(JW(LUP).GT.0) WRITE(JW(LUP),1001) NAME,NR,ND
         CALL BABEND('NBANK')
      END IF
C------ KEEP, NAMEN -------------------------------------------------
C     STATEMENTS INSERTED IN SUBPROGRAM
C     RETURNS NAMI = NAME-INDEX FOR INTEGER NAMA
C                    (ALWAYS NONZERO)
      NWNAM=IW(IDNAM)
      IW(IDNAM)=NAMA
      NAMI=IW(IDNAM-2)+MOD(IABS(IW(IDNAM)),NPRIM)-3
    1 NAMI=IW(IDPTR+NAMI)
      IF(IW(IDNAM+NAMI).NE.IW(IDNAM)) GOTO 1
      IW(IDNAM)=NWNAM
      NAMI=NAMI+NSYST
      IF(NAMI.EQ.NSYST) NAMI=NAMIND(NAME)
C--------------------------------------------------------------------
 
      IFD=0
C     DROP OTHER BANKS/RENUMBER BANK
      IF(JW1.EQ.3) THEN
         JW1=0
         IFL=0
         IND=JW(NAMI)
         IF(IND.NE.0) THEN
   20       NN=JW(IND-2)
            IF(JW(IND-1).NE.0) THEN
               IF(NN.EQ.NR) THEN
                  IFL=IND
                  IND=JW(IND-1)
               ELSE
                  IND=JW(IND-1)
                  IDR=MDROP(JW,NAME,NN)
               END IF
               GOTO 20
            END IF
            IF(IFL.EQ.0.AND.JW(IND-2).NE.NR) THEN
               IF(JW(IND-2).NE.NR) THEN
                  JW(IND-2)=NR
                  DO 22 M=1,JW(IND)
   22             JW(IND+M)=0
                  IFD=1
               END IF
            ELSE
               NN=JW(IND-2)
               IDR=MDROP(JW,NAME,NN)
            END IF
         END IF
      END IF
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
 
      IND=INDI
      IF(IND.NE.0) GOTO 10
C
C     BANK NOT FOUND, CREATE NEW BANK
C
      NDIF=ND+NHW
      IF(JW(IGP)+NDIF.GT.JW(IWK)) THEN
         NCACH=JW(IWK)-(JW(IGP)+NDIF)+JW(18)
         IF(NCACH.LT.0) CALL BDCACH(NCACH)
         CALL WGARB(JW)
         IF(JW(IGP)+NDIF.GT.JW(IWK)) GOTO 91
      END IF
      IND=JW(IGP)+(NHW-1)
      JW(IND-3)=NAMA
      JW(IND-2)=  NR
      JW(IND-1)=JW(INDJ-1)
      JW(IND  )=  ND
      JW(INDJ-1)= IND
      JW(IGP   ) =JW(IGP)+NDIF
      IF(ND.GT.0.AND.JW1.EQ.0) THEN
         DO 24 M=1,ND
   24    JW(IND+M)=0
      END IF
      GOTO 90
C
C     BANK FOUND, CHANGE LENGTH OF BANK
C
   10 NDIF=ND-JW(IND)
      IF(NDIF.EQ.0.AND.JW1.NE.2) GOTO 90
      IF(IND+JW(IND)+1.EQ.JW(IGP)) THEN
C
C        LAST BANK, CHECK SPACE
C
         IF(JW(IGP)+NDIF.GT.JW(IWK)) THEN
            NCACH=JW(IWK)-(JW(IGP)+NDIF)+JW(18)
            IF(NCACH.LT.0) CALL BDCACH(NCACH)
            CALL WGARB(JW)
            IF(JW(IGP)+NDIF.GT.JW(IWK)) GOTO 92
         END IF
      ELSE
C
C        NOT LAST BANK
C
         IF(NDIF+NHW.LE.0.AND.JW1.NE.2) THEN
C
C           INSERT DUMMY DROPPED BANK
C
            JW(IND)=ND
            JW(IND+ND+NHW)=NDIF
            JW(IGN)=MIN(JW(IGN),IND+ND+NHW)
            JW( NDN)=JW(NDN)-NDIF
            GOTO 90
         ELSE
C
C           MOVE BANK
C
            IF(JW(IGP)+ND+NHW.GE.JW(IWK)) THEN
               NCACH=JW(IWK)-(JW(IGP)+ND+NHW)+JW(18)
               IF(NCACH.LT.0) CALL BDCACH(NCACH)
               CALL WGARB(JW)
               IF(JW(IGP)+ND+NHW.GE.JW(IWK)) GOTO 92
            END IF
            NCOP=NHW+MIN(ND,JW(IND))
            CALL UCOPY2(JW(IND-(NHW-1)),JW(JW(IGP)),NCOP)
            JW(IND)=-JW(IND)-NHW
            JW( NDN)=JW(NDN)-JW(IND)
            JW(IGN)=MIN(JW(IGN),IND)
            IND=JW(IGP)+(NHW-1)
            JW( IGP)=JW(IGP)+NCOP
            JW(IND)=NCOP-NHW
            JW(INDJ-1)=IND
            IF(NDIF.LE.0) GOTO 90
         END IF
      END IF
C
C     CHANGE LAST BANK
C
      IZERO=IND+JW(IND)
      JW(IND)=JW(IND)+NDIF
      JW(IGP)=JW(IGP)+NDIF
      IF(NDIF.GT.0.AND.JW1.NE.1) THEN
         DO 80 M=1,NDIF
   80    JW(IZERO+M)=0
      END IF
C
      GOTO 90
C
C     CODE 2   INSUFFICIENT SPACE TO INCREASE LENGTH OF BANK
C
   92 JW2=JW2+1
C
C     CODE 1   INSUFFICIENT SPACE FOR NEW BANK
C
   91 JW2=JW2+1
      IND=0
      JW(IRC+JW2)=JW(IRC+JW2)+1
      IF(JW(LUP).EQ.0.OR.JW(LUW).EQ.0) GOTO 90
      CHNA=CHAINT(NAMA)
      CALL BBRET(JW,JW2,'NBANK',CHNA)
C
C
C
   90 JW(1)=0
      JW(2)=JW2
      IF(IND.NE.INDI) THEN
C
C        RESET LINK BANKS
C
         JND=JW(IDL)+NAMI-NSYST
         IF(JW(JND).NE.0) JW(JW(JND))=0
      END IF
C
  100 NBANK=IND
      IF(IND.NE.0) THEN      ! write NCOL and NROW if O'k
        JW(IND-5)=NCOL
        JW(IND-4)=NROW
      ENDIF
      RETURN
 1001 FORMAT('0BOS--NBANK-  WRONG ARGUMENT - STOP'/
     1                 14X,'NAME,NR,ND= ',A4,2I10)
      END
