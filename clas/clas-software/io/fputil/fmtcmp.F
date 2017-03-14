CDECK  ID>, FMTCMP.
      SUBROUTINE FMTCMP(IFMT,NFMT,LFMT,LFBIT)
c
c_begin_doc
c  RCS ID string
c  $Id: fmtcmp.F,v 1.1.1.1 1997/03/28 20:55:44 marki Exp $
c
c  Purpose:
c  --------
c Compress internal format representation for input/output
c Input: IFMT(1..NFMT),NFMT: uncompressed internal format
c Output: LFMT(1..LF),LFBIT: compressed representation
c                            (bitstream)
c
c Structure of compressed format definition:
c
c    Bits 0,1:  00:normal format  -> in IFMT(1)
c               01:B32
c               10:B16
c               11:B08
c                                 -> in IFMT(2..NFMT)
c      N*10       N'('
c             0    ')'  -> translated to 7 to be unique, since N<>0
c      N*10 + 1   N'F'          floating point
c      N*10 + 2   N'I'          integer
c      N*10 + 3   N'A'          text (4char per word)
c      N*10 + 4   N'Gn.m'       float with reduced precision
c              +10000*n+1000000*m
c      N*10 + 5   N'Y'          special treatment of data packed in F
c      N*10 + 6   +-J           partner column N even means +J=N/2+1
c                                              N odd  means -J=N/2+1
c                               partner refers to previously defined column(s)
c
c   The identifier KF always takes three bits, whereas N,n and m
c   are coded with variable number of bits, the first two
c   bits determine the total number of bits:
c                   00 N=1
c                 xx01 N=2..5
c               xxxx10 N=6..21
c   xxxxxxxxxxxxxxxx11 N=22..65557
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
      INTEGER NFMT,LFBIT,IFMT(*),LFMT(*)
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
     1'$Id: fmtcmp.F,v 1.1.1.1 1997/03/28 20:55:44 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='FMTCMP.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NBYTWD
      PARAMETER (NBYTWD=4)
      INTEGER LF,JBIT,I,IWORD,KF,NBIT,IOUT,JBITN,N,NCODE
c_end_var
c----6----------------------------------------------------------------72
      LF=1
      LFMT(LF)=IFMT(1)
      JBIT=2
      DO I=2,NFMT
        IWORD=IFMT(I)
        KF=MOD(IWORD,10)
        IF(IWORD.EQ.0)THEN
*         right paranthesis
          NBIT=3
          IOUT=7
*     put next NBIT least significant bits from IOUT into LFMT(LF)
*     JBIT counts the number of bits already used in this word.
*     If necessary LF is updated and the remaining bits are filled
*     into the next word. All other bits in IOUT must be 0.
      JBITN=JBIT+NBIT
      IF(JBITN.LE.32)THEN
*       all bits still in current word
        LFMT(LF)=IOR(LFMT(LF),ISHFT(IOUT,JBIT))
        JBIT=JBITN
      ELSE
*       put LSB into this word, remaining bits into next word
        IF(JBIT.LT.32) LFMT(LF)=IOR(LFMT(LF),ISHFT(IOUT,JBIT))
        JBIT=JBITN-32
        LF=LF+1
        LFMT(LF)=ISHFT(IOUT,JBIT-NBIT)
      ENDIF
        ELSE
          IWORD=IWORD/10
          N=MOD(IWORD,1000)
*         code n, determine NBIT
*                   00 N=1
*                 xx01 N=2..5
*               xxxx10 N=6..21
*   xxxxxxxxxxxxxxxx11 N=22..65557
            IF(N.EQ.1)THEN
              NBIT=2
              NCODE=0
            ELSE IF(N.LE.5)THEN
              NBIT=4
              NCODE=1+4*(N-2)
            ELSE IF(N.LE.21)THEN
              NBIT=6
              NCODE=2+4*(N-6)
            ELSE
              NBIT=18
              NCODE=3+4*(N-22)
            ENDIF
          IOUT=8*NCODE+KF
          NBIT=NBIT+3
*     put next NBIT least significant bits from IOUT into LFMT(LF)
*     JBIT counts the number of bits already used in this word.
*     If necessary LF is updated and the remaining bits are filled
*     into the next word. All other bits in IOUT must be 0.
      JBITN=JBIT+NBIT
      IF(JBITN.LE.32)THEN
*       all bits still in current word
        LFMT(LF)=IOR(LFMT(LF),ISHFT(IOUT,JBIT))
        JBIT=JBITN
      ELSE
*       put LSB into this word, remaining bits into next word
        IF(JBIT.LT.32) LFMT(LF)=IOR(LFMT(LF),ISHFT(IOUT,JBIT))
        JBIT=JBITN-32
        LF=LF+1
        LFMT(LF)=ISHFT(IOUT,JBIT-NBIT)
      ENDIF
        ENDIF
        IF(KF.EQ.4)THEN
*         add n and m
          IWORD=IWORD/1000
          N=MOD(IWORD,100)
          IF(N.LT.1)THEN
            WRITE(6,*)' FMTCMP: Gn.m: n,m>=1 necessary'
            STOP
          ENDIF
*                   00 N=1
*                 xx01 N=2..5
*               xxxx10 N=6..21
*   xxxxxxxxxxxxxxxx11 N=22..65557
            IF(N.EQ.1)THEN
              NBIT=2
              NCODE=0
            ELSE IF(N.LE.5)THEN
              NBIT=4
              NCODE=1+4*(N-2)
            ELSE IF(N.LE.21)THEN
              NBIT=6
              NCODE=2+4*(N-6)
            ELSE
              NBIT=18
              NCODE=3+4*(N-22)
            ENDIF
          IOUT=NCODE
*     put next NBIT least significant bits from IOUT into LFMT(LF)
*     JBIT counts the number of bits already used in this word.
*     If necessary LF is updated and the remaining bits are filled
*     into the next word. All other bits in IOUT must be 0.
      JBITN=JBIT+NBIT
      IF(JBITN.LE.32)THEN
*       all bits still in current word
        LFMT(LF)=IOR(LFMT(LF),ISHFT(IOUT,JBIT))
        JBIT=JBITN
      ELSE
*       put LSB into this word, remaining bits into next word
        IF(JBIT.LT.32) LFMT(LF)=IOR(LFMT(LF),ISHFT(IOUT,JBIT))
        JBIT=JBITN-32
        LF=LF+1
        LFMT(LF)=ISHFT(IOUT,JBIT-NBIT)
      ENDIF
          N=IWORD/100
          IF(N.LT.1)THEN
            WRITE(6,*)' FMTCMP: Gn.m: n,m>=1 necessary'
            STOP
          ENDIF
*                   00 N=1
*                 xx01 N=2..5
*               xxxx10 N=6..21
*   xxxxxxxxxxxxxxxx11 N=22..65557
            IF(N.EQ.1)THEN
              NBIT=2
              NCODE=0
            ELSE IF(N.LE.5)THEN
              NBIT=4
              NCODE=1+4*(N-2)
            ELSE IF(N.LE.21)THEN
              NBIT=6
              NCODE=2+4*(N-6)
            ELSE
              NBIT=18
              NCODE=3+4*(N-22)
            ENDIF
          IOUT=NCODE
*     put next NBIT least significant bits from IOUT into LFMT(LF)
*     JBIT counts the number of bits already used in this word.
*     If necessary LF is updated and the remaining bits are filled
*     into the next word. All other bits in IOUT must be 0.
      JBITN=JBIT+NBIT
      IF(JBITN.LE.32)THEN
*       all bits still in current word
        LFMT(LF)=IOR(LFMT(LF),ISHFT(IOUT,JBIT))
        JBIT=JBITN
      ELSE
*       put LSB into this word, remaining bits into next word
        IF(JBIT.LT.32) LFMT(LF)=IOR(LFMT(LF),ISHFT(IOUT,JBIT))
        JBIT=JBITN-32
        LF=LF+1
        LFMT(LF)=ISHFT(IOUT,JBIT-NBIT)
      ENDIF
        ENDIF
      ENDDO
      LFBIT=32*(LF-1)+JBIT
1000  RETURN
      END
