*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE PNVF(FLT,STR,JS)
*
      CHARACTER*(*) STR
*
************************************************************************
*
*     Convert floating point number FLT into string STR with JS
*     non-blank characters. JS = 2 ... 12
*
*     Examples:
*     CALL PNVF( -1.200,STR,JS)   -> STR='-1.2' ; JS = 4
*     CALL PNVF(1.0/3.0,STR,JS)   -> STR='0.333333' ; JS = 8
*     CALL PNVF(-123.00,STR,JS)   -> STR='-123.' ; JS = 5
*     STR will contain up to 6 digits, trailing zeros are suppressed.
*     STR will contain one decimal point.
*     Range of floating point numbers is 10E-36 ... 10E+36 (due to
*     limitations on certain machines.
*
*     Binary representation of NaN and Infinity on the SGI :
*     NaN = (7F810000)
*     Inf = (7F800000)
*
************************************************************************
*
#if defined(IRIX)
      CHARACTER*20 TEMP
#endif
      CHARACTER*1 DIG(0:9)
      INTEGER ND(12),KD(6)
      DOUBLE PRECISION FEX(-3:5)
      DATA DIG/'0','1','2','3','4','5','6','7','8','9'/
      DATA FEX/1.0D8,1.0D7,1.0D6,1.0D5,1.0D4,1.0D3,1.0D2,1.0D1,1.0D0/
      DATA KD/100000,10000,1000,100,10,1/
      DATA IJ0/0/
*
#ifdef SunOS
*     check for NaN:
      IF(IR_ISNAN(FLT).EQ.1) THEN
         STR = 'NaN'
         JS  = 3
         RETURN
      ENDIF
*     check for Infinity:
      IF(IR_ISINF(FLT).EQ.1) THEN
         STR = 'Inf'
         JS  = 3
         RETURN
      ENDIF
#endif /* SunOS */
#if defined(IRIX)
*
*     check for NaN:
      WRITE(TEMP,'(F20.5)') FLT
      IF(INDEX(TEMP,'NaN').NE.0.OR.INDEX(TEMP,'nan').NE.0) THEN
         STR = 'NaN'
         JS  = 3
         RETURN
      ENDIF
*     check for Infinity:
      WRITE(TEMP,'(F20.5)') FLT
      IF(INDEX(TEMP,'Infinity').NE.0) THEN
         STR = 'Inf'
         JS  = 3
         RETURN
      ENDIF
#endif
      IF(IJ0.EQ.0) THEN
         IJ0=ICHAR('0')
         IJ9=ICHAR('9')
      END IF
      JS=0
      X=FLT
      IF(X.NE.0.0) THEN
*        limited range on certain machines
         IF(ABS(X).LT.1.0E-36) X=SIGN(1.0E-36,FLT)
         IF(ABS(X).GT.1.0E+36) X=SIGN(1.0E+36,FLT)
      ELSE
         GOTO 22
      END IF
      N=IFIX(ALOG10(ABS(X))+100.0)-100
   20 IF(N.LE.5.AND.N+3.GE.0) THEN
         K=DBLE(ABS(X))*FEX(N)+0.5D0
      ELSE
         K=DBLE(ABS(X))*10.0D0**(5-N)+0.5D0
      END IF
   21 IF(K.GT.999999) THEN
         N=N+1
         GOTO 20
      END IF
*     rounding
      IF(MOD(K,100).EQ.99) THEN
         K=K+1
         GOTO 21
      ELSE IF(MOD(K,100).EQ.01) THEN
         K=K-1
      END IF
*     determine number of digits
      KDUP=K
      DO 40 I=1,6
      IF(MOD(KDUP,10).NE.0) GOTO 50
      KDUP=KDUP/10
   40 CONTINUE
      I=6
   50 NDIG=7-I
*
   22 IF(X.EQ.0.0) THEN
         GOTO 23
      ELSE IF(N.GT.5) THEN
*        N greater than +5
         NFL=(N/3)*3
         M=MOD(N,3)+1
      ELSE IF(N+3.GE.0) THEN
*        N between +5 and -3
         NFL=0
         M=MAX0(0,N+1)
      ELSE
*        N less than -3
         NFL=-((2-N)/3)*3
         M=3-MOD(2-N,3)
*        calculate length, if exponent modified by 3
         NLG=MAX0(M-3,NDIG)+4
         IF(X.LT.0.0)     NLG=NLG+1
         IF(M.LT.3)       NLG=NLG+1
         IF(M.LT.2)       NLG=NLG+1
         IF(NFL.LT.(-12)) NLG=NLG+1
*        if not more than 12 bytes, add 3 to exponent
         IF(NLG.LE.12) THEN
            NFL=NFL+3
            M  =M-3
            N  =N+3
         END IF
      END IF
*
*     K = 6-digit number
*     N = exponent (or +3)
*     NFL = printed exponent
*     M   = position of point
*     NDIG = number of nonzero digits
*
   23 STR=' '
*
      IF(X.EQ.0.0) THEN
         JS=JS+1
         STR(JS:JS+1)='0.'
         JS=JS+1
         GOTO 100
      ELSE IF(X.LT.0) THEN
         JS=JS+1
         STR(JS:JS)='-'
      END IF
*
      IF(NFL.GE.0.AND.(N+1.LE.0.AND.N+3.GE.0)) THEN
*         special case for N= -1 or -2 or -3 (0. or 0.0 or 0.00)
         JS=JS+1
         STR(JS:JS-N+1)='0.00'
         JS=JS-N
*        added code to avois some problems
      ELSE IF(NFL.LT.0.AND.M.LE.0) THEN
         NFL=NFL-3
         M=M+3
      END IF
*
      DO 24 I=1,6
      J=K/KD(I)
      K=K-KD(I)*J
      JS=JS+1
      STR(JS:JS)=DIG(J)
      IF(I.EQ.M) THEN
         JS=JS+1
         STR(JS:JS)='.'
      END IF
      IF(I.GE.M.AND.K.EQ.0) GOTO 26
   24 CONTINUE
*
   26 IF(NFL.EQ.0) GOTO 100
      JS=JS+1
      STR(JS:JS)='E'
*
      K=IABS(NFL)
      DO 30 I=1,2
      ND(I)=MOD(K,10)
      K    =K/10
      IF(K.EQ.0) GOTO 32
   30 CONTINUE
      I=2
   32 IF(NFL.LT.0) THEN
         JS=JS+1
         STR(JS:JS)='-'
      END IF
      DO 34 J=I,1,-1
      JS=JS+1
   34 STR(JS:JS)=DIG(ND(J))
*
  100 RETURN
      END
