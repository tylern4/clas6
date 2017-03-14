*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      SUBROUTINE STMTNU
*
************************************************************************
*
*     Recognize numerical values for data or language
*
************************************************************************
*
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
*     scan for numerical constants and recognize floating point numbers
      ICHAR0=ICHAR('0')
      IA=0
*     number (class = 4) is searched for
   10 DO 11 I=IA+1,NTX
      IF(ITX(3,I).EQ.4) GOTO 12
   11 CONTINUE
      I=0
   12 IA=I
      IF(I.EQ.0) GOTO 18
*     it is a number
      NUMB=ITX(4,I)
*     search for dot on the left ...
      IDOT=0
      IL=JSCAN(I,1,0)
      IF(IL.NE.0) THEN
         IF(ITX(3,IL).EQ.1.AND.ITX(1,I)-1.EQ.ITX(2,IL)) THEN
*           is it a dot?
            IF(ITX(4,IL).EQ.10) THEN
               IEP=1+ITX(2,I)-ITX(1,I)
               RTX(4,I)=FLOAT(ITX(4,I))/FLOAT(10**IEP)
               ITX(3,I)=6
               ITX(1,I)=ITX(1,IL)
               ITX(3,IL)=0
               IDOT=1
            END IF
         END IF
      END IF
*     ... and on the right
      IR=JSCAN(I,NTX,0)
      IF(IR.NE.0.AND.IDOT.EQ.0) THEN
         IF(ITX(3,IR).EQ.1.AND.ITX(2,I)+1.EQ.ITX(1,IR)) THEN
            IF(ITX(4,IR).EQ.10) THEN
               RTX(4,I)=FLOAT(ITX(4,I))
               ITX(3,I)=6
               ITX(2,I)=ITX(2,IR)
               ITX(3,IR)=0
               IDOT=2
               IR=JSCAN(IR,NTX,0)
               IF(IR.NE.0) THEN
                  IF(ITX(3,IR).EQ.4.AND.ITX(2,I)+1.EQ.ITX(1,IR)) THEN
*                    a number
                     IEP=1+ITX(2,IR)-ITX(1,IR)
                     RTX(4,I)=RTX(4,I)+FLOAT(ITX(4,IR))/FLOAT(10**IEP)
                     ITX(2,I)=ITX(2,IR)
                     ITX(3,IR)=0
                  END IF
               END IF
            END IF
         END IF
      END IF
      GOTO 10
*     scan for text E and recognize fl.pt. numbers with exponents
   18 IA=0
*     text (class = 3) is searched for
   20 DO 21 I=IA+1,NTX
      IF(ITX(3,I).EQ.3) THEN
         IF(STMT(ITX(1,I):ITX(1,I)).EQ.'E') THEN
            INUM=-1
            IF(ITX(1,I).EQ.ITX(2,I)) GOTO 22
*           text starts with E
            INUM=0
            DO 19 L=ITX(1,I)+1,ITX(2,I)
            IN=ICHAR(STMT(L:L))-ICHAR0
            IF(IN.LT.0.OR.IN.GT.9) GOTO 21
            INUM=INUM*10+IN
   19       CONTINUE
            GOTO 22
         END IF
      END IF
   21 CONTINUE
      I=0
   22 IA=I
      IF(IA.EQ.0) GOTO 40
*     exponent found, must be preceeded by number
      IVA=IA
   23 IVA=IVA-1
*     search for number before
      IF(IVA.GE.1) THEN
         IF(ITX(3,IVA).EQ.0) GOTO 23
         IF(ITX(3,IVA).EQ.4.OR.ITX(3,IVA).EQ.6) GOTO 24
      END IF
      GOTO 20
   24 CONTINUE
*     IVA is index of number before - search for sign and number foll.
      IF(INUM.GE.0) THEN
*        store modified number
         IF(ITX(3,IVA).EQ.4) THEN
            ITX(3,IVA)=6
            RTX(4,IVA)=FLOAT(ITX(4,IVA))*10.0**(INUM)
C           ITX(3,IV)=0
         ELSE
            RTX(4,IVA)=RTX(4,IVA)*10.0**(INUM)
         END IF
         ITX(2,IVA)=ITX(2,IA)
         ITX(3,IA )=0
         GOTO 20
      END IF
      IVB=IA
      ISG=0
   25 IVB=IVB+1
      IF(IVB.LE.NTX) THEN
         IF(ITX(3,IVB).EQ.0) GOTO 25
         IF(ITX(3,IVB).EQ.1.AND.
     +      (ITX(4,IVB).EQ.1.OR.ITX(4,IVB).EQ.2)) THEN
*           plus of minus
            IF(ISG.NE.0) GOTO 20
            ISG=3-2*ITX(4,IVB)
            GOTO 25
         ELSE IF(ITX(3,IVB).EQ.4) THEN
*           integer number (exponent)
            IF(ISG.EQ.0) ISG=1
*           store modified number
            IF(ITX(3,IVA).EQ.4) THEN
               ITX(3,IVA)=6
               RTX(4,IVA)=FLOAT(ITX(4,IVA))*10.0**(ISG*ITX(4,IVB))
            ELSE
               RTX(4,IVA)=RTX(4,IVA)*10.0**(ISG*ITX(4,IVB))
            END IF
            ITX(2,IVA)=ITX(2,IVB)
   27       ITX(3,IVB)=0
            IVB=IVB-1
            IF(IVB.GT.IVA) GOTO 27
         END IF
      END IF
      GOTO 20
*     search for hexadecimals only in data mode
C  40 IF(INU.EQ.1) GOTO 80
   40 IF(INU.EQ.1) GOTO 80
      IA=0
*     text (class = 3) is searched for
   50 DO 51 I=IA+1,NTX
      IF(ITX(3,I).EQ.3) GOTO 52
   51 CONTINUE
      I=0
   52 IA=I
      IF(I.EQ.0) GOTO 59
*     test for hexadecimal number
      IF(STMT(ITX(1,IA):ITX(1,IA)).EQ.'Z'.AND.
     +   ITX(2,IA)-ITX(1,IA).GE.3) THEN
         INUM=0
         DO 54 I=ITX(1,IA)+1,ITX(2,IA)
         IN=ICHAR(STMT(I:I))-ICHAR0
         IF(IN.LT.0.OR.IN.GT.9) THEN
            IN=-1
            IF(STMT(I:I).EQ.'A') IN=10
            IF(STMT(I:I).EQ.'B') IN=11
            IF(STMT(I:I).EQ.'C') IN=12
            IF(STMT(I:I).EQ.'D') IN=13
            IF(STMT(I:I).EQ.'E') IN=14
            IF(STMT(I:I).EQ.'F') IN=15
         END IF
         IF(IN.LT.0) GOTO 50
#ifndef SunOS
   54    INUM=IOR(ISHFT(INUM,4),IN)
#else /* SunOS */
   54    INUM= OR(LSHIFT(INUM,4),IN)
#endif /* SunOS */
*        store hexadecimal number
         ITX(3,IA)=5
         ITX(4,IA)=INUM
      END IF
      GOTO 50
   59 IA=1
*     numbers (class = 4 and 6) is searched for
   60 DO 61 I=IA+1,NTX
      IF(ITX(3,I).EQ.4.or.ITX(3,I).EQ.6) GOTO 62
   61 CONTINUE
      I=0
   62 IA=I
      IF(I.EQ.0) GOTO 80
      IVA=IA
   66 IVA=IVA-1
*     search for sign before
      IF(IVA.GE.1) THEN
         IF(ITX(3,IVA).EQ.0) GOTO 66
         IF(ITX(3,IVA).EQ.1) THEN
            IF(ITX(4,IVA).EQ.1.OR.ITX(4,IVA).EQ.2) THEN
               IF(ITX(4,IVA).EQ.2) THEN
                  IF(ITX(3,IA).EQ.4) ITX(4,IA)=-ITX(4,IA)
                  IF(ITX(3,IA).EQ.6) RTX(4,IA)=-RTX(4,IA)
               END IF
               ITX(1,IA)=ITX(1,IVA)
               ITX(3,IVA)=0
            END IF
         END IF
      END IF
   64 CONTINUE
      GOTO 60
*     finally compress
   80 J=0
      DO 90 I=1,NTX
      IF(ITX(3,I).EQ.0) GOTO 90
      J=J+1
      ITX(1,J)=ITX(1,I)
      ITX(2,J)=ITX(2,I)
      ITX(3,J)=ITX(3,I)
      ITX(4,J)=ITX(4,I)
   90 CONTINUE
      NTX=J
*
  100 RETURN
      END
