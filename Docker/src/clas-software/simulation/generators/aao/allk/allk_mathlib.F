c  map [-1;1] -> [0,infty]
c
c    dfmap(x) = d fmap(x)/dx
c
c   lmap = 1 f = (1+x)/(1-x)     df/dx = 2/(1-x)**2
c          2 f = tan(pi/4*(1+x)) df/dx = pi/4*(1+tan^2(pi/4*(1+x)))
c
      real*8 function dfmap(x,lmap)
      implicit real*8(a-b,d-h,o-z)
      parameter(pi=3.14159265d0)
c
      if(lmap.eq.1) then
        dfmap = 2.d0/(1.d0-x)**2
        else if(lmap.eq.2) then 
        dfmap = pi/4.d0*(1.d0+tan(pi/4.d0*(1.d0+x))**2)
      end if
      return
      end
c
      real*8 function fmap(x,lmap)
      implicit real*8(a-b,d-h,o-z)
      parameter(pi=3.14159265d0)
c
      if(lmap.eq.1) then
        fmap = (1.d0+x)/(1.d0-x)
        else if(lmap.eq.2) then 
        fmap = tan(pi/4.d0*(1.d0+x))
      end if
      return
      end
c
      SUBROUTINE GAUSSL(N,X,W)
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      REAL*8 X(50),W(50)
      DIMENSION XX(50),WW(50)
      A=-1.0D+00
      B=+1.0D+00
      CALL GAUSAB(XX,WW,A,B,N)
      DO 1 I=1,N
      X(I)=XX(I)
    1 W(I)=WW(I)
      RETURN
      END
C=======================================================================
     
C=======================================================================
     
      SUBROUTINE GAUSAB( X, W, A, B, NPT )
c     
C=======================================================================
C                               /b
C     Gaussian quadratures for  | f(x)dx.   NPT must be even & <100.
C                              /a
C
C=======================================================================
c     
c      REAL X(1), W(1)
c      REAL*16 XX, WW, C1, C2
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
      REAL*8 X(1), W(1)
      COMMON /GAUSCM/ XX(100), WW(100)
C-----------------------------------------------------------------------
      NPT2 = NPT/2
C     
      IF( A .EQ. 0.D+00 .AND. B .GT. .5D20 ) THEN
        CALL GAUS(NPT,1)
        DO 5 N = 1, NPT
          X(N) = XX(N)
          W(N) = WW(N)
 5      CONTINUE
        RETURN
      END IF
C     
      CALL GAUS(NPT,0)
C     
      C1 = (B-A)/2.0D+00
      C2 = (B+A)/2.0D+00
C     
      DO 10 N = 1, NPT2
        M = NPT2 + 1 - N
        X(N) = - XX(M)*C1 + C2
        W(N) =   WW(M)*C1
 10   CONTINUE
C     
      DO 20 N = NPT2+1, NPT
        M = N - NPT2
        X(N) = XX(M)*C1 + C2
        W(N) = WW(M)*C1
 20   CONTINUE
C     
      RETURN
C     
      END
C=======================================================================
        SUBROUTINE GAUS(NN,ITYPE)
C
C       STANDARD SETTINGS ARE: IALF=IBTA=0,0<NN<100,
C               ITYPE=0 : -1 TO +1 POINTS
C               ITYPE=1 :  0 TO INFINITY
C
C      IMPLICIT REAL*16 (A-H,O-Z)
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
C     
C***********************************************************************
C
C     GAUJAC COMPUTES GAUSS - JACOBI INTEGRATION WEIGHTS AND NODES
C
C***********************************************************************
C
      DIMENSION X(100),A(100),B(100),C(100)
      DIMENSION XX(100), WW(100)
      DIMENSION G(100)
C
        COMMON/GAUSCM/XX,WW
      DATA IALF, IBTA / 0, 0 /
C
      PI=3.141592653589793238D0
C
      ISUM=IALF+IBTA
      IDIF=IBTA-IALF
      FNUM=ISUM*IDIF
      ALF=IALF
      BTA=IBTA
C
C     CALCULATE COEFFICIENTS REQUIRED BY JACOBI
C
      DO 20 I=1,100
        I2=I+I
        IM=I-1
        DEN=(ISUM+I2)*(ISUM+I2-2)
        IF(ISUM.EQ.0) GO TO 67
        B(I)=FNUM/DEN
        GO TO 68
 67     B(I)=0.D0
 68     FNUMP=4*IM*(IALF+IM)*(IBTA+IM)*(ISUM+IM)
        DENP=(ISUM+I2-1)*(ISUM+I2-2)**2*(ISUM+I2-3)
        IF(((ISUM.EQ.0).OR.(ISUM.EQ.1)).AND.(I.EQ.1)) GO TO 69
        C(I)=FNUMP/DENP
        GO TO 20
 69     C(I)=0.D0
 20   CONTINUE
     
C-----------------------------------------------------------------------
C     For outputting quadratures
C-----------------------------------------------------------------------
C      TYPE 34
C34    FORMAT(1X,'FILENAME FOR INTERMEDIATE RESULTS =? (A10)')
C      ACCEPT 35,FNAMX
C35    FORMAT(A10)
C      OPEN(UNIT=3,FILE=FNAMX,STATUS='NEW')
C-----------------------------------------------------------------------
C     
      CALL JACOBI(NN,X,G,ALF,BTA,B,C,EPS,CSX,CSA,TSX,TSA)
C     
C-----------------------------------------------------------------------
C     For outputting quadratures
C-----------------------------------------------------------------------
C      TYPE 70
C70    FORMAT(1X,'OUTPUT FILENAME ? (A10)')
C      ACCEPT 71,FNAM
C71    FORMAT(A10)
C      OPEN(UNIT=7,FILE=FNAM,STATUS='NEW')
C-----------------------------------------------------------------------
     
C     OUTPUT DATA STATEMENT OF WEIGHTS AND NODES
C
      IF(ITYPE.EQ.0) GO TO 130
      DO 100 I=1,NN
        II=NN+1-I
C     XX(I)=(1.0D+00+X(II))/(1.0D+00-X(II))
C     WW(I)=2.0D+00*G(II)/((1.0D+00-X(II))**(IALF+2)*
C    1     (1.0D+00+X(II))**IBTA)
        ZZ=PI*(1.0D+00+X(II))/4.0D+00
        XX(I)=DSIN(ZZ)/DCOS(ZZ)
        WW(I)=G(II)*PI*0.25D+00/(DCOS(0.25D+00*PI*(1.0D+00
     1      +X(II)))**2)
     
C-----------------------------------------------------------------------
C      WRITE(7,120)I,I,XX(I),WW(I)
 120    FORMAT(6X,6HDATAX(,I2,6H,M),W(,I2,4H,M)/,D22.15,1H,,
     1      D22.15,1H/)
C-----------------------------------------------------------------------
     
 100  CONTINUE
        RETURN
 130  CONTINUE
      NI=NN/2
      DO 140 I=1,NI
        II=NI+1-I
        XX(I)=X(II)
        WW(I)=G(II)
C     
C-----------------------------------------------------------------------
C      WRITE(7,120)I,I,XX(I),WW(I)
C-----------------------------------------------------------------------
C     
 140  CONTINUE
      RETURN
      END
      SUBROUTINE JACOBI(NN,X,A,ALF,BTA,B,C,EPS,CSX,CSA,TSX,TSA)
C      IMPLICIT REAL*16 (A-H,O-Z)
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
C
C***********************************************************************
C
C     CALCULATES THE ZEROS X(I) OF THE NN-TH ORDER JACOBI POLYNOMIAL
C     PN(ALF,BTA) FOR THE SEGMENT (-1,1)
C     THE LARGEST ZERO WILL BE STORED IN X(1).
C     ALSO CALCULATES THE CORRESPONDING COEFFICIENTS A(I) OF THE
C     NN-TH ORDER GAUSS-JACOBI QUADRATURE FORMULA OF DEGREE 2*NN-1.
C     THIS SUBROUTINE MUST BE GIVEN THE COEFFICIENTS:
C
C                (ALF+BTA)(BTA-ALF)
C        B(N) = -----------------------------
C               (ALF+BTA+2N)(ALF+BTA+2N-2)
C
C                4(N-1)(ALF+N-1)(BTA+N-1)(ALF+BTA+N-1)
C        C(N) = ---------------------------------------------
C               (ALF+BTA+2N-1)(ALF+BTA+2N-2)**2(ALF+BTA+2N-3)
C
C
C     IN THE RECURSION RELATION
C
C        P(N) = (X-B(N))*P(N-1)-C(N)*P(N-2)
C
C     FOR ALL N LESS THAN OR EQUAL TO THE HIGHEST DEGREE NN.
C
C        CSX = CALC SUM X(I)      TSX = TRUE SUM X(I)
C        CSA = CALC SUM A(I)      TSA = TRUE SUM A(I)
C
C***********************************************************************
C
      DIMENSION X(100),A(100),B(100),C(100)
      FN=NN
      CSX=0.D0
      CSA=0.D0
      BETA=DEXP(FLGAMA(ALF+1.D0)+FLGAMA(BTA+1.D0)-FLGAMA(ALF+BTA+2.D0))
      CC=2.0D+00**(ALF+BTA+1.0D+00)*BETA
      TSX=FN*(BTA-ALF)/(ALF+BTA+2.0D+00*FN)
      TSA=CC
      DO 1 J=2,NN
 1    CC=CC*C(J)
      DO 12 I=1,NN
      IF(I-1)12,2,3
C        LARGEST ZERO
 2    AN=ALF/FN
      BN=BTA/FN
      R1=(1.D0+ALF)*(2.78D+00/(4.0D+00+FN*FN)+.768D+00*AN/FN)
      R2=1.D0+1.48D+00*AN+.96D+00*BN+.452D+00*AN*AN+.83D+00*AN*BN
      XT=1.D0-R1/R2
      GO TO 11
 3    IF(I-2)12,4,5
C        SECOND ZERO
 4    R1=(4.1D+00+ALF)/((1.0D+00+ALF)*(1.0D+00+.156D+00*ALF))
      R2=1.D0+.06D+00*(FN-8.0D+00)*(1.D0+.12D+00*ALF)/FN
      R3=1.D0+.012D+00*BTA*(1.D0+.25D+00*ABS(ALF))/FN
      RATIO=R1*R2*R3
      XT=XT-RATIO*(1.D0-XT)
      GO TO 11
 5    IF(I-3) 12,6,7
C        THIRD ZERO
 6    R1=(1.67D+00+.28D+00*ALF)/(1.D0+.37D+00*ALF)
      R2=1.D0+.22D+00*(FN-8.D0)/FN
      R3=1.D0+8.0D+00*BTA/((6.28D0+BTA)*FN*FN)
      RATIO=R1*R2*R3
      XT=XT-RATIO*(X(1)-XT)
      GO TO 11
 7    IF(NN-I-1)10,9,8
C        MIDDLE ZEROS
 8    XT=3.0D+00*X(I-1)-3.0D+00*X(I-2)+X(I-3)
      GO TO 11
C        SECOND LAST ZERO
 9    R1=(1.D0+.235D+00*BTA)/(.766D0+.119D+00*BTA)
      R2=1.0D+00/(1.D0+.639D+00*(FN-4.D0)/(1.D0+.71D+00*(FN-4.D0)))
      R3=1.0D+00/(1.D0+20.d0*ALF/((7.5D0+ALF)*FN*FN))
      RATIO=R1*R2*R3
      XT=XT+RATIO*(XT-X(I-2))
      GO TO 11
C        LAST ZERO
 10   R1=(1.D0+.37D+00*BTA)/(1.67D0+.28D+00*BTA)
      R2=1.0D+00/(1.D0+.22D+00*(FN-8.D0)/FN)
      R3=1.0D+00/(1.D0+8.0D+00*ALF/((6.28D0+ALF)*FN*FN))
      RATIO=R1*R2*R3
      XT=XT+RATIO*(XT-X(I-2))
C
 11   CALL GSROOT(XT,NN,ALF,BTA,DPN,PN1,B,C,EPS)
      X(I)=XT
      A(I)=CC/(DPN*PN1)
C-----------------------------------------------------------------------
C      WRITE(3,20)ALF,BTA,NN,I,XT,A(I)
C-----------------------------------------------------------------------
C     
      CSX=CSX+XT
      CSA=CSA+A(I)
12    CONTINUE
C     
C-----------------------------------------------------------------------
C      WRITE(3,22)CSX,CSA,TSX,TSA
 20   FORMAT(1X,2F6.2,2I3,2(1X,D26.18),1X,(1X,D26.18))
 22   FORMAT(1H0,/,5X,'CSX =',D25.18,5X,'CSA =',D25.18,/,
     1 5X,'TSX =',D25.18,5X,'TSA =',D25.18)
C-----------------------------------------------------------------------
C     
      RETURN
      END
C
      SUBROUTINE GSROOT(X,NN,ALF,BTA,DPN,PN1,B,C,EPS)
C      IMPLICIT REAL*16 (A-H,O-Z)
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
C
C***********************************************************************
C
C     IMPROVES THE APPROXIMATE ROOT X
C     IN ADDITION WE ALSO OBTAIN:
C      DPN = DERIVATIVE OF P(N) AT X
C      PN1 = VALUE OF P(N-1) AT X
C
C***********************************************************************
C
      DIMENSION B(100),C(100)
      ITER=0
 1    ITER=ITER+1
      CALL RECR(P,DP,PN1,X,NN,ALF,BTA,B,C)
      D=P/DP
      X=X-D
      IF(DABS(D)-EPS)3,3,2
 2    IF(ITER-10)1,3,3
 3    DPN=DP
      RETURN
      END
C
      SUBROUTINE RECR(PN,DPN,PN1,X,NN,ALF,BTA,B,C)
C      IMPLICIT REAL*16 (A-H,O-Z)
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
C
      DIMENSION B(100),C(100)
      P1=1.D0
      P=X+(ALF-BTA)/(ALF+BTA+2.D0)
      DP1=0.D0
      DP=1.D0
      DO 1 J=2,NN
        Q=(X-B(J))*P-C(J)*P1
        DQ=(X-B(J))*DP+P-C(J)*DP1
        P1=P
        P=Q
        DP1=DP
        DP=DQ
1     CONTINUE
      PN=P
      DPN=DP
      PN1=P1
      RETURN
      END
C
      FUNCTION FLGAMA(W)
C      IMPLICIT REAL*16 (A-H,O-Z)
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
C
C   RECHECK
C***********************************************************************
C
C     CALCULATES LOG(BASE E)GAMMA(W) FOR W REAL AND GAMMA(W) POSITIVE.
C     USES STIRLING'S APPROXIMATION
C     ACCURATE TO ABOUT 12 SIGNIFICANT PLACES
C
C***********************************************************************
C
      PI=3.141592653589793238D0
      X=W
      M=0
      FK=-1.D0
      IF(X-.5D0)1,1,2
C        W LESS EQ .5
1     M=1
      XPI=X*PI
      X=1.D0-X
2     FK=FK+1.D0
      IF(X+FK-6.D0)2,2,3
3     Z=X+FK
      ZZ=Z*Z
C        LOG GAMMA(Z), Z GREATER 6.
      Y=(Z-.5D+00)*DLOG(Z)-Z+.9189385332047D+00
     1 +(((((((-10861851.D+00/ZZ
     2 +2356200.D+00)/ZZ-704820.D+00)/ZZ+309400.D+00)
     3 /ZZ-218790.D+00)/ZZ+291720.D+00)/ZZ-1021020.D+00)
     4 /ZZ+30630600.D+00)/Z/367567200.D+00
      IF(FK)6,6,4
4     IK=FK
      DO 5 I=1,IK
      FK=FK-1.D+00
5     Y=Y-DLOG(X+FK)
6     IF(M)7,11,7
7     P=PI/DSIN(XPI)
      IF(P)8,8,10
8     WRITE(6,9)W
9     FORMAT(2X,'GAMMA(',D11.4,') IS NEGATIVE')
      Y=0.
      GO TO 11
10    Y=DLOG(P)-Y
11    FLGAMA=Y
      RETURN
      END
C------------------------------------------------------------
      SUBROUTINE CROUT(A,B,N,ID,IS)
C------------------------------------------------------------
C      
C      A = INPUT MATRIX : OUTPUT INVERSE MATRIX         
C      B = INPUT VECTOR : OUTPUT SOLUTION OF LINEAR EQUATIONS
C      B(N+1) = SIGN DET(A) : B(N+2) = LOG10 ABS DET(A)
C      N = SIZE OF THE MATRIX AND THE VECTOR
C      ID = SIZE OF ARRAYS A & B
C      IS = 0 : LU-DECOMPOSITION & DETERMINANT
C      < 0 : INVERSE MATRIX
C      > 0 : LINEAR EQUATIONS
C      < -1, > 1 : CHECK OF THE RESULTS
C      
C-     - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      PARAMETER (JD=200)
      implicit real*8(a-h,o-z)
      DIMENSION A(ID,ID),B(ID+2)
      dimension DA(JD,JD+2),DB(JD,JD),DET(2)
C-     - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      N1=N+1 
      N2=N+2
      DO 10 I=1,N 
        DA(I,N1)=B(I) 
        DA(I,N2)=I
        DO 10 J=1,N 
          DA(I,J)=A(I,J) 
          DB(I,J)=DA(I,J) 
   10 CONTINUE
C        
      CALL LUDCM(DA,DET,N,JD)
      B(N+1)=DET(1) 
      B(N+2)=DET(2)
      IF(IS.EQ.0) GO TO 1 
C      
      IF(IS.LT.0) THEN
        CALL INVER(DA,N,JD)
      ELSE
        CALL LINEQ(DA,N,JD)
      ENDIF
C      
      IF(IABS(IS).GT.1) CALL MCHEK(DA,DB,N,JD,IS)
C      
    1 CONTINUE
      DO 20 I=1,N 
        B(I)=DA(I,N1)
        DO 20 J=1,N 
          A(I,J)=DA(I,J)
   20 CONTINUE
C        
      RETURN 
      END
C------------------------------------------------------------
      SUBROUTINE LUDCM(DA,DET,N,ID)
C------------------------------------------------------------
      implicit real*8(a-h,o-z)
      dimension DA(ID,ID+2),DET(2)
c,DX,DI,DT
C-     - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C      
CC     L-U DECOMPOSITION OF THE MATRIX
C      
      N1=N+1 
      N2=N+2 
      KK=1 
      DI=1
      CALL PIVOT(DA,DI,KK,N,ID) 
      DO 11 J=2,N1 
   11 DA(1,J)=DA(1,J)*DA(1,1)
C      
      DO 12 K=2,N 
        KK=K 
        KP1=K+1 
        KM1=K-1
        DO 13 I=K,N 
          DX=DA(I,K)
          DO 14 J=1,KM1 
   14     DX=DX-DA(I,J)*DA(J,K)
   13   DA(I,K)=DX
        CALL PIVOT(DA,DI,KK,N,ID) 
        IF(K.EQ.N) GO TO 1
C        
        DO 15 J=KP1,N 
          DX=DA(K,J)
          DO 16 I=1,KM1 
   16     DX=DX-DA(K,I)*DA(I,J)
   15   DA(K,J)=DX*DA(K,K)
   12 CONTINUE
C      
CC     DET(1)=SIGN(DET)  DET(2)=LOG10(| DET |)
C      
    1 DT=0
      DO 20 I=1,N 
        DX=DA(I,I)
        DT=DT-DLOG10(DABS(DX))
        IF(DX.LT.0.0D0) DI=-DI
   20 CONTINUE
      DET(1)=DI 
      DET(2)=DT
C      
      RETURN 
      END
C---------------------------------------------------------
      SUBROUTINE PIVOT(DA,DI,IK,N,ID)
C---------------------------------------------------------
      implicit real*8(a-h,o-z)
      dimension DA(ID,ID+2)
c,DI,DK,DW,DX
C-     - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C      
CC     PARTIAL PIVOTING
C      
      N1=N+1 
      N2=N+2
      DW=DABS(DA(IK,IK)) 
      IW=IK
      DO 10 I=IK+1,N 
        DK=DABS(DA(I,IK))
        IF(DW.LT.DK) THEN 
          IW=I 
          DW=DK 
        ENDIF
   10 CONTINUE
      IF(IW.EQ.IK) GO TO 1
C      
      DO 20 I=1,N2
        DX=DA(IK,I) 
        DA(IK,I)=DA(IW,I) 
        DA(IW,I)=DX
   20 CONTINUE
      DI=-DI
    1 DA(IK,IK)=1.0D0/DA(IK,IK)
C      
      RETURN 
      END
C------------------------------------------------------------
      SUBROUTINE INVER(DA,N,ID)
C------------------------------------------------------------
      implicit real*8(a-h,o-z)
      dimension DA(ID,ID+2)
c,DX
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C      
CC     INVERSE MATRIX
C      
      N1=N+1 
      N2=N+2 
      NM1=N-1
      DO 11 J=1,N 
        DA(J,N1)=DA(J,J)
        IF(J.EQ.N1) GO TO 1
        DO 12 I=J+1,N 
          IM1=I-1 
          DX=0
          DO 13 K=J,IM1 
   13     DX=DX-DA(I,K)*DA(K,N1)
          DA(I,N1)=DA(I,I)*DX 
          DA(I,J)=DA(I,N1)
   12   CONTINUE
   11 CONTINUE
C      
    1 CONTINUE
      DO 14 I=NM1,1,-1 
        IP1=I+1
        DO 15 J=1,N 
          DX=0
          DO 16 K=IP1,N 
   16     DX=DX-DA(I,K)*DA(K,J)
   15   DA(J,N1)=DX
        DO 17 J=1,I 
   17   DA(I,J)=DA(I,J)+DA(J,N1)
        DO 18 J=IP1,N 
   18   DA(I,J)=DA(J,N1)
   14 CONTINUE
C      
      DO 21 I=1,N
        DO 22 J=1,N 
          JJ=DA(J,N2)+0.1 
   22   DA(JJ,N1)=DA(I,J)
        DO 23 J=1,N 
   23   DA(I,J)=DA(J,N1)
   21 CONTINUE
C      
      RETURN 
      END
C------------------------------------------------------------
      SUBROUTINE LINEQ(DA,N,ID)
C------------------------------------------------------------
      implicit real*8(a-h,o-z)
      dimension DA(ID,ID+2)
c,DX
C-     - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C      
CC     LINEAR EQUATIONS
C      
      N1=N+1 
      NM1=N-1
      DO 11 I=2,N 
        IM1=I-1 
        DX=DA(I,N1)
        DO 12 J=1,IM1 
   12   DX=DX-DA(I,J)*DA(J,N1)
   11 DA(I,N1)=DX*DA(I,I)
C      
      DO 13 I=NM1,1,-1 
        IP1=I+1 
        DX=DA(I,N1)
        DO 14 J=IP1,N 
   14   DX=DX-DA(I,J)*DA(J,N1)
   13 DA(I,N1)=DX
C      
      RETURN 
      END
C------------------------------------------------------------
      SUBROUTINE MCHEK(DA,DB,N,ID,IS)
C------------------------------------------------------------
      PARAMETER (JD=200)
      implicit real*8(a-h,o-z)
      dimension DA(ID,ID+2),DB(ID,ID),DC(JD,JD+1)
c,DX
C-     - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C      
CC     CHECK OF (A)(1/A)=(I) & (A)B=X
C      
      N1=N+1 
      N2=N+2
      IF(IS.LT.0) THEN
        DO 11 I=1,N 
          DO 11 J=1,N 
            DX=0
            DO 12 K=1,N 
   12       DX=DX+DA(I,K)*DB(K,J)
   11   DC(I,J)=DX
C          
        WRITE(6,661)
  661   FORMAT(1H ,'   ---   CHECK OF A*(1/A) = I   ---',/)
        WRITE(6,100) ((DC(I,J),J=1,N),I=1,N)
  100   FORMAT(1H ,8D14.6)
C          
      ELSE
        DO 13 I=1,N 
          DX=0
          DO 14 K=1,N 
   14     DX=DX+DB(I,K)*DA(K,N1)
   13   DC(I,N1)=DX
C        
        WRITE(6,662)
  662   FORMAT(1H ,'   ---   CHECK OF A*X = B   ---',/)
        WRITE(6,100) (DC(I,N1),I=1,N)
      ENDIF
C      
      RETURN 
      END
C------------------------------------------------------------
      SUBROUTINE PRINTM(H,ID,JD,I,J,ND)
C------------------------------------------------------------
      implicit real*8(a-h,o-z)
      DIMENSION H(ID,JD)
C-     - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C      
CC     MATRIX PRINT OUT
C      
      IF(ND.EQ.1) GO TO 1
      NJ=(J-1)/8+1
      DO 10 N=1,NJ 
        N8=8*(N-1)
        LMIN=N8+1 
        LMAX=MIN0(8,J-N8)+N8
        WRITE(6,100) (L,L=LMIN,LMAX)
  100   FORMAT(1H ,7X,8(5X,I2,8X)/)
C        
        DO 10 K=1,I
          WRITE(6,200) K,(H(K,L),L=LMIN,LMAX)
  200     FORMAT(1H ,5X,I2,8E15.6)
   10 CONTINUE
      RETURN
C        
    1 CONTINUE
      DO 30 M=1,I
        WRITE(6,300) M,H(M,M)
  300   FORMAT(1H ,5X,I2,E15.6)
   30 CONTINUE
C      
      RETURN 
      END
      