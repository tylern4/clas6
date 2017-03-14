CDECK  ID>, CNVCHA.
      SUBROUTINE CNVCHA(TXT)
c
c_begin_doc
c  RCS ID string
c  $Id: cnvcha.F,v 1.1.1.1 1997/03/28 20:49:35 marki Exp $
c
c  Purpose:
c  --------
*     CONVERT FROM CHARACTER STRING TO ARRAY OF INTEGERS, REAL NUMBERS,
*     TEXT DATA FOR FREE FORMAT INPUT
*     INTEGERS:
*        STRING OF DECIMAL DIGITS, CONTAINING NO DECIMAL POINT AND NO
*        BLANK, OPTIONALLY SIGNED.
*     REAL NUMBERS:
*        STRING OF DECIMAL DIGITS WITH A DECIMAL POINT AND WITHOUT
*        BLANK, OPTIONALLY SIGNED AND OPTIONALLY FOLLOWED BY EXPONENT
*        WITH A LETTER E OR D AND AN INTEGER.
*     TEXT DATA:
*       STRING OF CHARACTERS (EXCEPT APOSTROPH), ENCLOSED IN
*       APOSTROPHS (STORED WITH 4 CHARACTERS PER WORD).
*     THE RESULT OF THE CONVERSION IS STORED IN THE COMMON/COMCNV/
*     LABEL = SEE BELOW
*     ISL   = 1 IF FIRST DATA IS INTEGER AND FOLLOWED BY A SLASH
*           = 0 OTHERWISE
*     NT    = NUMBER OF DATA ITEMS
*     NFR/FFR(128)= DATA WORDS
*     LFR(128)    = FLAG FOR DATA WORD =1 FOR INTEGER, =2 FOR REAL NUMB.
*                   = 3 FOR TEXT DATA
*     LABEL:
*        THE CHARACTERS OF THE FIRST FOUR COLUMNS BEFORE THE FIRST DIGIT
*        OR ANY OF THE CHARCATERS '+-.
*     COMMENT:
*        ALL CHARACTERS AFTER AN APOSTROPH AS WELL AS ANY ADDITIONAL
*        CHARACTERS ARE TREATED AS COMMENT AND IGNORED.
*
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
#include "comcnv.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      CHARACTER*(*) TXT
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
     1'$Id: cnvcha.F,v 1.1.1.1 1997/03/28 20:49:35 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='CNVCHA.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NBYTWD
      PARAMETER ( NBYTWD = 4 )
      INTEGER IZ,NQ,I,I4,IN,ICHAR0,IA,N,NUMB,IUMB,NPNT,J,IS,M,NR,ITNR,
     &        NDIG,IE,IEFL,INTCHA
      REAL FFR(128),FUMB
      EQUIVALENCE (FFR(1),NFR(1))
      CHARACTER*4 CLAB,CTXT,CH*1
c_end_var
c----6----------------------------------------------------------------72
      NT=0
      ISL=0
*     Correcxtion to recognize ! correctly (outside quotes):
*        the next two lines are replaced by ....
C     IZ=INDEX(TXT,'!')-1
C     IF(IZ.LT.0) IZ=LEN(TXT)
*     ... the next line plus the seven lines ...
      IZ=LEN(TXT)
*     reduce search length
      IF(IZ.GT.10) THEN
         IF(TXT(IZ/2+1:IZ).EQ.' ') THEN
            IZ=IZ/2
         END IF
      END IF
      IF(IZ.GT.10) THEN
         IF(TXT(2*IZ/3+1:IZ).EQ.' ') THEN
            IZ=2*IZ/3
         END IF
      END IF
*     ... seven lines below
      NQ=0
      DO 05 I=1,IZ
      IF(TXT(I:I).EQ.'''') NQ=NQ+1
      IF(MOD(NQ,2).EQ.0.AND.TXT(I:I).EQ.'!') GOTO 06
   05 CONTINUE
      I =IZ+1
   06 IZ=I-1
*     ------- end of change to recognize ! correctly
      I4=4
      IN=1
      ICHAR0=ICHAR('0')
      CLAB=' '
      IA=1
*     FIND START OF NUMBER OR OF TEXT
   10 IN=IA
      DO 12 I=IA,IZ
      IF(TXT(I:I).EQ.'''') GOTO 20
      N=ICHAR(TXT(I:I))-ICHAR0
      IF(N.GE.0.AND.N.LE.9) THEN
          IF(I.LE.I4.AND.CLAB.NE.'    ') THEN
             CLAB(I:I)=TXT(I:I)
             IN=I+1
             GOTO 12
          END IF
          GOTO 30
      END IF
      IF(I.LE.I4) THEN
         IF(TXT(I:I).EQ.'+'.OR.TXT(I:I).EQ.'-'.OR.TXT(I:I).EQ.'.') I4=0
      END IF
*     HEXADECIMAL - 4 to    8 CHARS 0...9,A...F WITH A BLANK IN
*     FRONT, IF NOT FIRST COL), AND ENDS WITH
*     BLANK OR END
*     test hexadecimal
      CH=' '
      IF(I.GT.1) CH=TXT(I-1:I-1)
      IF(CH.EQ.'''') CH=' '
      IF(TXT(I:I).EQ.'Z'.AND.(CH.EQ.' '.OR.CH.EQ.',').AND.IZ-I.GE.4)THEN
*        candidate for hexadecimal
         NUMB=0
         IUMB=0
         NPNT=-1
         DO 11 J=I+1,IZ
         N=ICHAR(TXT(J:J))-ICHAR0
         IF(N.LT.0.OR.N.GT.9) THEN
            N=-1
            IF(TXT(J:J).EQ.'A') N=10
            IF(TXT(J:J).EQ.'B') N=11
            IF(TXT(J:J).EQ.'C') N=12
            IF(TXT(J:J).EQ.'D') N=13
            IF(TXT(J:J).EQ.'E') N=14
            IF(TXT(J:J).EQ.'F') N=15
         END IF
         IF(N.GE.0) THEN
            IUMB=IUMB+1
            NUMB=IOR(ISHFT(NUMB,4),N)
         ELSE
*           no hexadecimal digit
            CH=TXT(J:J)
            IF(CH.EQ.''''.OR.CH.EQ.' '.OR.CH.EQ.',') GOTO 13
            IUMB=0
            GOTO 13
         END IF
   11    CONTINUE
         J=IZ+1
   13    IF(IUMB.GE.4.AND.IUMB.LE.8) THEN
*           hexadecimal digit
            IS=J
            GOTO 41
         END IF
      END IF
      IF(I.LE.I4) THEN
         CLAB(I:I)=TXT(I:I)
         IN=I+1
      END IF
   12 CONTINUE
      GOTO 100
*     TEXT STARTED
   20 IA=I+1
      I4=0
      M =0
      NR=NT
      CTXT=' '
      ITNR=0
      DO 22 I=IA,IZ
      IF(M.EQ.4.OR.TXT(I:I).EQ.'''') THEN
         NR=NR+1
         NFR(NR)=INTCHA(CTXT)
         ITNR=ITNR+1
         LFR(NR)=ITNR
         M=0
         CTXT=' '
      END IF
      IF(TXT(I:I).EQ.'''') GOTO 24
      M=M+1
   22 CTXT(M:M)=TXT(I:I)
      GOTO 100
   24 NT=NR
      IA=I+1
      GOTO 10
*     NUMBER STARTED
   30 IA=I
      I4=0
      NUMB=0
      NDIG=0
      NPNT=-1
      IF(IA.GT.1) THEN
         IF(TXT(IA-1:IA-1).EQ.'.') IA=IA-1
      END IF
      DO 32 I=IA,IZ
      N=ICHAR(TXT(I:I))-ICHAR0
      IF(N.GE.0.AND.N.LE.9) THEN
         NDIG=NDIG+1
      NUMB=10*NUMB+N
      ELSE IF(TXT(I:I).EQ.'.') THEN
         NPNT=NDIG
      ELSE
         GOTO 34
      END IF
   32 CONTINUE
      I=IZ+1
   34 IS=I
*     CHECK SIGN
      DO 36 I=IA-1,IN,-1
      IF(TXT(I:I).EQ.'+'.OR.TXT(I:I).EQ.'-') THEN
         IF(TXT(I:I).EQ.'-') NUMB=-NUMB
         IE=I-1
      ELSE IF(TXT(I:I).EQ.' ') THEN
         GOTO 36
      ELSE
         IE=I
      END IF
      GOTO 38
   36 CONTINUE
      IE=IN-1
*     CHECK FOR 'E' OR 'D'
   38 IEFL=0
      DO 40 I=IE,IN,-1
      IF(TXT(I:I).EQ.'E'.OR.TXT(I:I).EQ.'D') THEN
         IF(IEFL.EQ.0) THEN
            IEFL=1
         ELSE
            IEFL=-1
         END IF
      END IF
   40 CONTINUE
*     NEW NUMBER
   41 NT=NT+1
      IF(NPNT.LT.0) THEN
         NFR(NT)=NUMB
         LFR(NT)=-1
         IF(NT.EQ.1) THEN
            DO 42 I=IS,IZ
            IF(TXT(I:I).EQ.' ') GOTO 42
            IF(TXT(I:I).EQ.'/') ISL=1
            GOTO 44
   42       CONTINUE
   44       CONTINUE
         END IF
      ELSE
         FUMB=NUMB
         FFR(NT)=FUMB/10.0**(NDIG-NPNT)
         LFR(NT)=-2
      END IF
*     EXPONENT
      IA=IS
      IF(NT.LE.1) GOTO 10
      IF(IEFL.EQ.1.AND.LFR(NT)+1.EQ.0.AND.LFR(NT-1)+2.EQ.0) THEN
         NT=NT-1
         FFR(NT)=FFR(NT)*10.0**NFR(NT+1)
      END IF
      GOTO 10
  100 LABEL=INTCHA(CLAB)
      RETURN
      END
