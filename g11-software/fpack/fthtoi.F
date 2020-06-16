*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE FTHTOI(IHFMT,IHF,IFMT,NFMT)
*
      INTEGER IHFMT(*), IFMT(*)
*
************************************************************************
*
*     Translate FormaT from Hollerith TO Integer array
*     ( takes about 25 mikro seconds )
*
************************************************************************
*
      CHARACTER  FMT*4, FM*1, CHA*4
*
*     initialize array
      IEFMT=0
      N=0
*
*     translate format
      IER=0
      LNF=4*IHF
      ICH=IHFMT(1)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
#ifndef Linux
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
#else /* Linux */
      CHA=CHAR(IAND      (ICH     ,255))//CHAR(IAND(ISHFT(ICH,- 8),255))
     +  //CHAR(IAND(ISHFT(ICH,-16),255))//CHAR(IAND(ISHFT(ICH,-24),255))
#endif /* Linux */
*KEND.
      FMT=CHA
*
*     test special formats
      IF(LNF.GE.1) THEN
         IF(FMT(1:1).EQ.'B'.OR.FMT(1:1).EQ.'b') THEN
*           B... code
            KF=0
            IF(LNF.GE.2) THEN
               IF(FMT(2:2).EQ.'8')  KF=5
            END IF
            IF(KF.EQ.0.AND.LNF.GE.3) THEN
               IF(FMT(2:3).EQ.'08') KF=5
               IF(FMT(2:3).EQ.'16') KF=4
               IF(FMT(2:3).EQ.'32') KF=6
            END IF
            IF(KF.NE.0) THEN
               IFMT(1)=KF
               NFMT=1
               GOTO 100
            END IF
         END IF
      END IF
      LEV=0
      NR =0
*
*     loop over all characters
      LI=0
      IHP=1
      ICHAR0=ICHAR('0')
*
      DO 10 L=1,4*IHF
      LI=LI+1
      IF(LI.EQ.5) THEN
         LI=1
         IHP=IHP+1
         ICH=IHFMT(IHP)
*KEEP,CHAICH.
*     get char*4 from hollerith 4H
#ifndef Linux
      CHA=CHAR(IAND(ISHFT(ICH,-24),255))//CHAR(IAND(ISHFT(ICH,-16),255))
     +  //CHAR(IAND(ISHFT(ICH,- 8),255))//CHAR(IAND      (ICH     ,255))
#else /* Linux */
      CHA=CHAR(IAND      (ICH     ,255))//CHAR(IAND(ISHFT(ICH,- 8),255))
     +  //CHAR(IAND(ISHFT(ICH,-16),255))//CHAR(IAND(ISHFT(ICH,-24),255))
#endif /* Linux */
*KEND.
         FMT=CHA
      END IF
      FM=FMT(LI:LI)
      IF(FM.EQ.' ') GOTO 10
      NX=ICHAR(FM)-ICHAR0
*     a number
      IF(NX.GE.0.AND.NX.LE.9) THEN
         IF(NR.LT.0) IER=IER+1
         NR=NX+10*MAX0(0,NR)
         GOTO 10
*     a left parenthesis            NR/0
      ELSE IF(FM.EQ.'(') THEN
         IF(NR.LT.0) IER=IER+1
         LEV=LEV+1
         N=N+1
         IFMT(N)=10*MAX0(1,NR)
         NR=0
         GOTO 10
*     a right parenthesis           0/0
      ELSE IF(FM.EQ.')') THEN
         IF(NR.GE.0) IER=IER+1
         LEV=LEV-1
         N=N+1
         IFMT(N)=0
*     a komma
      ELSE IF(FM.EQ.',') THEN
         IF(NR.GE.0) IER=IER+1
         NR=0
         GOTO 10
*     a F or I or A or J or K OR Z ...
      ELSE
         KF=INDEX('FIAJKZC',FM)
         IF(KF.EQ.0) KF=INDEX('fiajkzc',FM)
         IF(KF.NE.0) THEN
            IF(NR.LT.0) IER=IER+1
            N=N+1
            IFMT(N)=KF+10*MAX0(1,NR)
         ELSE
*           illegal character
            IER=IER+1
         END IF
      END IF
      NR=-1
   10 CONTINUE
*
      IF(LEV.NE.0) IER=IER+1
      IF(IER.NE.0) IEFMT=-1
   30 NFMT=N
      IF(IEFMT.NE.0) THEN
         IFMT(1)=9
         NFMT=1
      END IF
*
  100 RETURN
      END
