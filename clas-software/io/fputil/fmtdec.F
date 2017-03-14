CDECK  ID>, FMTDEC.
      SUBROUTINE FMTDEC(FMT,IFMT,NFMT)
c
c_begin_doc
c  RCS ID string
c  $Id: fmtdec.F,v 1.1.1.1 1997/03/28 20:55:44 marki Exp $
c
c  Purpose:
c  --------
c Turn format string FMT into internal format representation.
c Input: FMT
c Output: IFMT(1..NFMT),NFMT
c This routine is largely a copy of the TRFMT Routine in BOS.
c The main difference is the treatment of a new format definition
c for floating point numbers: Gn.m
c                             Y special treatment of "packed" floats
c The first two words of the 3 word header in IFMT are not used here.
c A value of NFMT=0 indicates an illegal format.
c
c Coding scheme:
c      1   BIT CODE (=1 B32, =2 B16, =3 B08)
c      2...FMT CODES, IF BIT CODE = 0
c
c      FMT CODE   translated from...
c
c      N*10       N'('
c             0    ')'
c      N*10 + 1   N'F'     floating point
c      N*10 + 2   N'I'     integer
c      N*10 + 3   N'A'     text (4char per word)
c      N*10 + 4   N'Gn.m'  float with reduced precision
c              +10000*n+1000000*m
c      N*10 + 5   N'Y'     special treatment of data packed in F
c      N*10 + 6   +-J      partner column N odd  means +J=(N-1)/2+1
c                                         N even means -J=(N-1)/2+1
c                                         N=2*ABS(J)-(1-(1-SIGN(J))/2)
c                                          =2*ABS(J)-JPOS
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
      INTEGER IFMT(*),NFMT
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
     1'$Id: fmtdec.F,v 1.1.1.1 1997/03/28 20:55:44 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FMTDEC.F')
c
c  Local User defined variables:
c  -----------------------------
      CHARACTER*(*) FM*1
      INTEGER IEFMT,N,KF,IER,LNF,LEV,NR,JPOS,L,NX,NN,NNN,MMM,NY
c_end_var
c----6----------------------------------------------------------------72
*     initialize array ifmt
      IEFMT=0
      N=1
*     test special format 'B08' etc
      KF=0
      IF(FMT.EQ.'B32') KF=1
      IF(FMT.EQ.'B16') KF=2
      IF(FMT.EQ.'B08') KF=3
      IF(FMT.EQ.'B8')  KF=3
      IF(KF.NE.0) THEN
         IFMT(1)=KF
         GOTO 120
      ELSE
         IFMT(1)=0
      END IF
*     translate format
      IER=0
      LNF=LEN(FMT)
      LEV=0
      NR =0
      JPOS=-1
*     loop over all characters
      L=0
*     loop over all characters
10    L=L+1
      IF(L.GT.LNF)GOTO 90
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
         IFMT(N)=10*MAX0(1,NR)
         NR=0
         GOTO 10
*     a right parenthesis               0/0
      ELSE IF(FM.EQ.')') THEN
         LEV=LEV-1
*        this may end a +-J number, store it
         IF(JPOS.GE.0)THEN
           N=N+1
           NN=2*NR-JPOS
           IFMT(N)=6+10*NN
           NR=0
           JPOS=-1
         ELSE
           IF(NR.GE.0) IER=IER+1
         ENDIF
         N=N+1
         IFMT(N)=0
*     a komma
      ELSE IF(FM.EQ.',') THEN
*        this may end a +-J number, store it
         IF(JPOS.GE.0)THEN
           N=N+1
           NN=2*NR-JPOS
           IFMT(N)=6+10*NN
           JPOS=-1
         ELSE
           IF(NR.GE.0) IER=IER+1
         ENDIF
         NR=0
         GOTO 10
*     a +  sign
      ELSE IF(FM.EQ.'+') THEN
         IF(NR.GE.0) IER=IER+1
         NR=0
         JPOS=1
         GOTO 10
*     a -  sign
      ELSE IF(FM.EQ.'-') THEN
         IF(NR.GE.0) IER=IER+1
         NR=0
         JPOS=0
         GOTO 10
*     a F or I or A or G or Y         NR/1 or 2 or 3 or 4 or 5
      ELSE
         KF=0
         IF(FM.EQ.'F')THEN
           KF=1
         ELSE IF(FM.EQ.'I') THEN
           KF=2
         ELSE IF(FM.EQ.'A') THEN
           KF=3
*    the sequence Gn.m is treated as one character
         ELSE IF(FM.EQ.'G')THEN
           KF=4
           NNN=0
           MMM=0
20         L=L+1
           FM=FMT(L:L)
           IF(FM.EQ.' ') GOTO 20
           IF(L.GT.LNF.OR.FM.EQ.'.') GOTO 30
           NY=ICHAR(FM)-ICHAR('0')
           IF(NY.LT.0.OR.NY.GT.9)IER=IER+1
           NNN=NY+10*NNN
           GOTO 20
30         L=L+1
           FM=FMT(L:L)
           IF(FM.EQ.' ') GOTO 30
           IF(
     + L.GT.LNF.OR.FM.EQ.')'.OR.FM.EQ.','.OR.FM.EQ.'+'.OR.FM.EQ.'-')
     +      GOTO 40
           NY=ICHAR(FM)-ICHAR('0')
           IF(NY.LT.0.OR.NY.GT.9)IER=IER+1
           MMM=NY+10*MMM
           GOTO 30
40         L=L-1
         ELSE IF(FM.EQ.'Y')THEN
           KF=5
         END IF
         IF(KF.NE.0) THEN
            IF(NR.LT.0) IER=IER+1
            N=N+1
            IFMT(N)=KF+10*MAX0(1,NR)
            IF(KF.EQ.4)IFMT(N)=IFMT(N)+10000*NNN+1000000*MMM
         ELSE
            IF(FM.NE.' ') IER=IER+1
         END IF
         NR=-1
         GOTO 10
      ENDIF
 
*     end of character loop
 90   CONTINUE
*     check if any remaining words to be stored:
      IF(JPOS.GE.0)THEN
        N=N+1
        NN=2*NR-JPOS
        IFMT(N)=6+10*NN
      ENDIF
      IF(LEV.NE.0) IER=IER+1
      IF(IER.NE.0) THEN
         WRITE(6,101) FMT
         IEFMT=-1
         GOTO  130
      END IF
*
  120 CONTINUE
  130 NFMT=N
      IF(IEFMT.NE.0) NFMT=0
  100 RETURN
  101 FORMAT('0FORMAT ERROR'/
     +       14X,'CALL FMTDEC(',A,')')
      END
