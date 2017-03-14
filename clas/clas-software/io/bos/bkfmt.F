CDECK  ID>, BKFMT.
      SUBROUTINE BKFMT(NAME,FMT)
c
c_begin_doc
c  RCS ID string
c  $Id: bkfmt.F,v 1.1.1.1 1997/03/28 20:49:30 marki Exp $
c
c  Purpose:
c  --------
c     Define format of banks 'NAME'
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
      CHARACTER*(*) FMT
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
     1'$Id: bkfmt.F,v 1.1.1.1 1997/03/28 20:49:30 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='BKFMT.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER ID,NFMT,ISAV,NAMIND,INTCHA
      INTEGER IFMTSZ
      PARAMETER(IFMTSZ=196)
      INTEGER IFMT(IFMTSZ)
      EQUIVALENCE (INTA(1),IFMT(1))
c_end_var
c----6----------------------------------------------------------------72
      IF(IW(ICK).NE.12345) CALL BBNOT(IW,'BKFMT')
      NAMI=NAMIND(NAME)
      ID=IDFMT+NAMI-NSYST
*
*     IGNORE CALL, IF FORMAT ALREADY SPECIFIED BY A CALL    no
*                                                 now allowed at any
*          CONTENT OF +FMT BANK                           time
*
*      1   NAME
*      2   ORIGIN (=3 DEFINED BY CALL, =4 DEFINED IN BOS)
*      3   BIT CODE (=1 B32, =2 B16, =3 B08)
*      4   FMT CODES, IF BIT CODE = 0
*
*      FMT CODE   TRANSLATED FROM...
*
*      N*10       N'('
*             0    ')'
*      N*10 + 1   N'F'
*      N*10 + 2   N'I'
*      N*10 + 3   N'A'
*                             next three lines commented out
C     IF(IW(ID).NE.0) THEN
C         IF(IW(IW(ID)+2).EQ.3) GOTO 100
C     END IF
*
*                ARRAY IFMT
*
      CALL TRFMTL(FMT,IFMT,NFMT,IFMTSZ)
      IFMT(1)=IW(IDNAM+NAMI-NSYST)
      IF(NFMT.EQ.0) THEN
         IF(IW(LUP).GT.0) WRITE(IW(LUP),1001) NAME,FMT
         IEFMT=-1
         GOTO 100
      END IF
*
*     STORE IN WORK BANK
*
*     Do modified BBFMT inline
      ID=IDFMT+NAMI-NSYST
*     IF(IW(ID).NE.0) THEN
*        IF(IFMT(2).LE.IW(IW(ID)+2)) GOTO 100
*     END IF
      CALL WBANC(IW,IW(ID),NFMT,1,*90)
      IW(IW(ID)-3)=INTCHA('+FMT')
      IF(NFMT.LE.IFMTSZ) THEN
         CALL UCOPY(IFMT,IW(IW(ID)+1),NFMT)
      ELSE
         ISAV = IFMT(1)
         CALL TRFMT(FMT, IW(IW(ID)+1),NFMT)
         IW(IW(ID)+1) = ISAV
      ENDIF
*
*     FORMAT FLAG IEFMT  = -1   ERROR IN BKFMT CALL
*                        >  0   NUMBER OF DEFINED FORMATS
*
*
      IF(IEFMT.GE.0) IEFMT=IEFMT+1
      GOTO 100
   90 CALL BBSPC(IW,'BKFMT')
  100 RETURN
 1001 FORMAT('0BOS--BKFMT-  FORMAT ERROR'/
     +       14X,'CALL BKFMT(',A4,',',A,')')
      END
