*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE STRNUM(I,NUM,N)
*
      INTEGER NUM(*)
*
************************************************************************
*
*     Collect group of integers (class 2),
*     separated optionally by commas.
*     First pattern is always three words:
*     1:  0 or 1 or 2 or 3  (no - lower - upper - lower+upper limit)
*     2:  lower limit
*     3:  upper limit
*     Further patters have length 2 or 3
*     Pattern      stored as        N = total number of words
*     X                      0  X
*     X :                    1  X
*       : Y                  2  Y
*     X : Y                  3  X  Y
*
*     see subroutine NUMTAG for check of integer
*
************************************************************************
*
      CHARACTER CH*1
*KEEP,CTEXTIR.
*     one statement up to 5280 = 66 * 80 characters
      PARAMETER (LSTMT=5280)
      CHARACTER STMT*(LSTMT),ERRMES*72,NORMES*72
      PARAMETER (NDIMIT=640)
      REAL RTX(4,NDIMIT)
      COMMON/CTXTIR/NST,MST,KST,INU,NTX,ITX(4,NDIMIT)
      COMMON/CTXTST/STMT,ERRMES,NORMES
      EQUIVALENCE (ITX(1,1),RTX(1,1))
      SAVE/CTXTIR/,/CTXTST/
*KEND.
*
      N=3
      I=I-1
*     IDD is a : marker
      IDD=0
   10 IF(I.EQ.NTX.OR.N.GT.1500) GOTO 20
      IP= 1
*     check preceeding :
      IF(I.EQ.NTX) GOTO 20
      IF(IDD.EQ.0.AND.ITX(3,I+1).EQ.1) THEN
         CH=STMT(ITX(1,I+1):ITX(2,I+1))
         IF(CH.EQ.':') THEN
            IDD=1
            IP=IP+1
         END IF
      END IF
*     treat sign
      IS=+1
      IF(ITX(3,I+IP).EQ.1) THEN
         IF(I+IP.EQ.NTX) GOTO 20
         CH=STMT(ITX(1,I+IP):ITX(2,I+IP))
         IF(CH.EQ.'-') THEN
            IS=-1
            IP=IP+1
         ELSE IF(CH.EQ.'+') THEN
            IS=+1
            IP=IP+1
         END IF
      END IF
*     accept numbers only
      IF(ITX(3,I+IP).LT.4.OR.ITX(3,I+IP).GT.6) GOTO 20
      I=I+IP
      IF(ITX(3,I).EQ.6) ITX(4,I)=RTX(4,I)
*     store number
      IF(IDD.EQ.2) THEN
         NUM(N-1)=3
         N=N+1
         NUM(N  )=IS*ITX(4,I)
      ELSE
         N=N+2
         NUM(N-1)=0
         NUM(N  )=IS*ITX(4,I)
      END IF
*
      IF(IDD.EQ.1) THEN
         NUM(N-1)=2
         IDD=0
      ELSE IF(IDD.EQ.2) THEN
         IDD=0
      ELSE
*        check trailing :
         IF(I.EQ.NTX) GOTO 20
         IF(ITX(3,I+1).EQ.1) THEN
            CH=STMT(ITX(1,I+1):ITX(2,I+1))
            IF(CH.EQ.':') THEN
               NUM(N-1)=1
               IDD=2
               I=I+1
            END IF
         END IF
      END IF
*     allow comma as separator
      IF(I.EQ.NTX) GOTO 20
      IF(ITX(3,I+1).EQ.1) THEN
         CH=STMT(ITX(1,I+1):ITX(2,I+1))
         IF(CH.EQ.',') THEN
            I=I+1
            IDD=0
         END IF
      END IF
      GOTO 10
*     finally define total limit
   20 J=3
      LIMA=0
      LIMB=0
      LIM1=0
      LIM2=0
   30 IF(J.GE.N) GOTO 40
      IF(NUM(J+1).EQ.0) THEN
*        single number
         IF(LIMA.EQ.0) THEN
            LIMA=1
            LIM1=NUM(J+2)
         ELSE
            LIM1=MIN0(LIM1,NUM(J+2))
         END IF
         IF(LIMB.EQ.0) THEN
            LIMB=1
            LIM2=NUM(J+2)
         ELSE
            LIM2=MAX0(LIM2,NUM(J+2))
         END IF
      ELSE IF(NUM(J+1).EQ.1) THEN
*        lower limit
         IF(LIMA.EQ.0) THEN
            LIMA=1
            LIM1=NUM(J+2)
         ELSE
            LIM1=MIN(LIM1,NUM(J+2))
         END IF
         LIMB=-1
      ELSE IF(NUM(J+1).EQ.2) THEN
*        upper limit
         IF(LIMB.EQ.0) THEN
            LIMB=1
            LIM2=NUM(J+2)
         ELSE
            LIM2=MAX(LIM2,NUM(J+2))
         END IF
         LIMA=-1
      ELSE IF(NUM(J+1).EQ.3) THEN
*        lower and upper limit
         IF(LIMA.EQ.0) THEN
            LIMA=1
            LIM1=NUM(J+2)
         ELSE
            LIM1=MIN(LIM1,NUM(J+2))
         END IF
         IF(LIMB.EQ.0) THEN
            LIMB=1
            LIM2=NUM(J+3)
         ELSE
            LIM2=MAX(LIM2,NUM(J+3))
         END IF
         J=J+1
      END IF
      J=J+2
      GOTO 30
   40 IF(LIMA.LT.0) THEN
*        open lower limit
         IF(LIMB.LT.0) THEN
*           open upper limit => unlimited
            NUM(1)=0
            NUM(2)=0
            NUM(3)=0
         ELSE
*           no open upper limit => upper limit
            NUM(1)=2
            NUM(2)=0
            NUM(3)=LIM2
         END IF
      ELSE
*        no open lower limit
         IF(LIMB.LT.0) THEN
*           open upper limit => lower limit
            NUM(1)=1
            NUM(2)=LIM1
            NUM(3)=0
         ELSE
*           no open upper limit => lower and upper limit
            NUM(1)=3
            NUM(2)=LIM1
            NUM(3)=LIM2
         END IF
      END IF
*
  100 RETURN
      END
