CDECK  ID>, TRFMTL.
      SUBROUTINE TRFMTL(FMT,IFMT,NFMT,LIM)
c
c_begin_doc
c  RCS ID string
c  $Id: trfmtl.F,v 1.1.1.1 1997/03/28 20:49:42 marki Exp $
c
c  Purpose:
c  --------
c     translate format FMT into integer array IFMT(1)...IFMT(NFMT)
c     array size is LIM
c     NFMT = 0 for wrong syntax in format statement
c
c      1   0
c      2   =3 DEFINED BY CALL
c      3   BIT CODE (=1 B32, =2 B16, =3 B08)
c      4...FMT CODES, IF BIT CODE = 0
c
c      FMT CODE   translated from...
c
c      N*10       N'('
c             0    ')'
c      N*10 + 1   N'F'          floating point
c      N*10 + 2   N'I'          integer
c      N*10 + 3   N'A'          text (4char per word)
c      N*10 + 4   N'J'          16-bit integer
c      N*10 + 5   N'K'          8-bit integer
c
c     initialize array ifmt
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      CHARACTER*(*) FMT
      INTEGER IFMT(*),NFMT,LIM
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
     1'$Id: trfmtl.F,v 1.1.1.1 1997/03/28 20:49:42 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='TRFMTL.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER IEFMT,N,KF,IER,LNF,LEV,NR,L,NX
      CHARACTER*(*) FM*1
c_end_var
c----6----------------------------------------------------------------72
      IEFMT=0
      IF(1.LE.LIM) IFMT(1)=0
      IF(2.LE.LIM) IFMT(2)=3
      IF(3.LE.LIM) IFMT(3)=0
      N=3
*     test special format 'B08' etc
      KF=0
      IF(FMT.EQ.'B32') KF=1
      IF(FMT.EQ.'B16') KF=2
      IF(FMT.EQ.'B08') KF=3
      IF(FMT.EQ.'B8')  KF=3
      IF(KF.NE.0) THEN
        IF(3.LE.LIM) IFMT(3)=KF
        GOTO 20
      ENDIF
*     translate format
      IER=0
      LNF=LEN(FMT)
      LEV=0
      NR =0
*     loop over all characters
      DO 10 L=1,LNF
      FM=FMT(L:L)
      IF(FM.EQ.' ') GOTO 10
      NX=ICHAR(FM)-ICHAR('0')
*     a number
      IF(NX.GE.0.AND.NX.LE.9) THEN
         IF(NR.LT.0) IER=IER+1
         NR=NX+10*MAX0(0,NR)
         GOTO 10
*     a left parenthesis               NR/0
      ELSE IF(FM.EQ.'(') THEN
         IF(NR.LT.0) IER=IER+1
         LEV=LEV+1
         N=N+1
         IF(N.LE.LIM) IFMT(N)=10*MAX0(1,NR)
         NR=0
         GOTO 10
*     a right parenthesis               0/0
      ELSE IF(FM.EQ.')') THEN
         IF(NR.GE.0) IER=IER+1
         LEV=LEV-1
         N=N+1
         IF(N.LE.LIM) IFMT(N)=0
*     a komma
      ELSE IF(FM.EQ.',') THEN
         IF(NR.GE.0) IER=IER+1
         NR=0
         GOTO 10
*     a F or I or A or J or K OR Z     NR/1 or 2 or 3 or 4 or 5 or 6
      ELSE
         KF=0
         IF(FM.EQ.'F') KF=1
         IF(FM.EQ.'I') KF=2
         IF(FM.EQ.'A') KF=3
         IF(FM.EQ.'J') KF=4
         IF(FM.EQ.'K') KF=5
         IF(KF.NE.0) THEN
            IF(NR.LT.0) IER=IER+1
            N=N+1
            IF(N.LE.LIM) IFMT(N)=KF+10*MAX0(1,NR)
         ELSE
            IF(FM.NE.' ') IER=IER+1
         END IF
      END IF
      NR=-1
   10 CONTINUE
*     end of do-loop
      IF(LEV.NE.0) IER=IER+1
      IF(IER.NE.0) THEN
         IEFMT=-1
         GOTO  30
      END IF
*
   20 CONTINUE
   30 NFMT=N
      IF(IEFMT.NE.0) NFMT=0
  100 RETURN
      END
