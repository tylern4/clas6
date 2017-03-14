C
C
c
      SUBROUTINE MATINC (A,N,B,M,DETERM,NMAX)
      IMPLICIT REAL*8(A-H,O-Z)
C
C     MATRIX INVERSION WITH ACCOMPANYING SOLUTION OF LINEAR EQUATIONS
C     (COMPLEX ARITHMETIC VERSION)
C
      COMPLEX*16 A,B,DETERM,SWAP,T
      REAL*4 PIVOT
      DIMENSION A(NMAX,N), B(NMAX,1)
      COMMON /F402/ PIVOT(392), INDEX(392)
C     (CARDS BELOW WITH ******** IN COLUMNS 73-80 IMPLEMENT THE IMPROVED
C     COMPUTATION OF THE DETERMINANT IN THE MODIFICATION OF MARCH 1975.)
      DATA DETMAX,DETMIN/1.D+50, 1.D-50/
C
C     INITIALIZE DETERMINANT AND PIVOT ELEMENT ARRAY
C
      DETERM=1.0
      IDET=0
      DO 20 I=1,N
      PIVOT(I)=0.0
   20 CONTINUE
C
C     PERFORM SUCCESSIVE PIVOT OPERATIONS (GRAND LOOP)
C
      DO 550 I=1,N
C
C     SEARCH FOR PIVOT ELEMENT AND EXTEND DETERMINANT PARTIAL PRODUCT
C
      AMAX=0.0
      DO 105 J=1,N
      IF (PIVOT(J).NE.0.0) GO TO 105
      DO 100 K=1,N
      IF (PIVOT(K).NE.0.0) GO TO 100
      TEMP=ABS(A(J,K))
      IF (TEMP.LT.AMAX) GO TO 100
      IROW=J
      ICOLUM=K
      AMAX=TEMP
  100 CONTINUE
  105 CONTINUE
      INDEX(I)=4096*IROW+ICOLUM
      J=IROW
      T=A(J,ICOLUM)
      DETERM=T*DETERM
      IF(ABS(DETERM).LT.DETMAX) GO TO 130
      DETERM=DETERM*DETMIN
      IDET=IDET+1
      GO TO 140
  130 IF(ABS(DETERM).GT.DETMIN) GO TO 140
      DETERM=DETERM*DETMAX
      IDET=IDET-1
  140 CONTINUE
C
C     RETURN IF MATRIX IS SINGULAR (ZERO PIVOT) AFTER COLUMN INTERCHANGE
C
      IF (ABS(DETERM).EQ.0.0) GO TO 600
C
      PIVOT(ICOLUM)=AMAX
C
C     INTERCHANGE ROWS TO PUT PIVOT ELEMENT ON DIAGONAL
C
      IF (IROW.EQ.ICOLUM) GO TO 260
      DETERM=-DETERM
      DO 200 K=1,N
      SWAP=A(J,K)
      A(J,K)=A(ICOLUM,K)
      A(ICOLUM,K)=SWAP
  200 CONTINUE
      IF (M.LE.0) GO TO 260
      DO 250 K=1,M
      SWAP=B(J,K)
      B(J,K)=B(ICOLUM,K)
      B(ICOLUM,K)=SWAP
  250 CONTINUE
C
C     DIVIDE PIVOT ROW BY PIVOT ELEMENT
C
  260 DO 350 K=1,N
      IF (K.EQ.ICOLUM) A(ICOLUM,K)=1.0
      A(ICOLUM,K)=A(ICOLUM,K)/T
  350 CONTINUE
      IF (M.LE.0) GO TO 380
      DO 370 K=1,M
      B(ICOLUM,K)=B(ICOLUM,K)/T
  370 CONTINUE
C
C     REDUCE NON-PIVOT ROWS
C
  380 DO 550 J=1,N
      IF (J.EQ.ICOLUM) GO TO 550
      T=A( J,ICOLUM)
      A( J,ICOLUM)=0.0
      DO 450 K=1,N
      A( J,K)=A( J,K)-A(ICOLUM,K)*T
  450 CONTINUE
      IF (M.LE.0) GO TO 550
      DO 500 K=1,M
      B( J,K)=B( J,K)-B(ICOLUM,K)*T
  500 CONTINUE
  550 CONTINUE
C
C     INTERCHANGE COLUMNS AFTER ALL PIVOT OPERATIONS HAVE BEEN PERFORMED
C
  600 DO 710 I=1,N
      I1=N+1-I
      K=INDEX(I1)/4096
      ICOLUM=INDEX(I1)-4096*K
      IF (K.EQ.ICOLUM) GO TO 710
      DO 705 J=1,N
      SWAP=A(J,K)
      A(J,K)=A(J,ICOLUM)
      A(J,ICOLUM)=SWAP
  705 CONTINUE
  710 CONTINUE
C
      PIVOT(1)=IDET
      RETURN
      END
 
       SUBROUTINE GAUSSL(N,X,W)
       IMPLICIT REAL*8(A-H,O-Z)
       DIMENSION X(100),W(100)
       DIMENSION XX(100),WW(100)
       CALL GAUSAB(XX,WW,-1.d0,1.d0,N)
       DO 11 I=1,N
       X(I)=XX(I)
   11  W(I)=WW(I)
       RETURN
       END
      SUBROUTINE GAUSAB( X, W, A, B, NPT )

C=======================================================================
C                               /b
C     Gaussian quadratures for  | f(x)dx.   NPT must be even & <100.
C                              /a
C
C=======================================================================

       IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 X(1), W(1)

      REAL*8 XX, WW, C1, C2
      COMMON / GAUSCM / XX(100), WW(100)
C-----------------------------------------------------------------------
      NPT2 = NPT/2

      IF( A .EQ. 0. .AND. B .GT. .5E20 ) THEN
      CALL GAUS(NPT,1)
      DO 5 N = 1, NPT
      X(N) = XX(N)
      W(N) = WW(N)
5     CONTINUE
      RETURN
      END IF

      CALL GAUS(NPT,0)

      C1 = (B-A)/2.
      C2 = (B+A)/2.

      DO 10 N = 1, NPT2
      M = NPT2 + 1 - N
      X(N) = - XX(M)*C1 + C2
      W(N) =   WW(M)*C1
10    CONTINUE

      DO 20 N = NPT2+1, NPT
      M = N - NPT2
      X(N) = XX(M)*C1 + C2
      W(N) = WW(M)*C1
20    CONTINUE

      RETURN

      END
C=======================================================================
        SUBROUTINE GAUS(NN,ITYPE)
C
C       STANDARD SETTINGS ARE: IALF=IBTA=0,0<NN<100,
C               ITYPE=0 : -1 TO +1 POINTS
C               ITYPE=1 :  0 TO INFINITY
C
      IMPLICIT REAL*8 (A-H,O-Z)

      DATA IALF, IBTA / 0, 0 /
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
67    B(I)=0.D0
68    FNUMP=4*IM*(IALF+IM)*(IBTA+IM)*(ISUM+IM)
      DENP=(ISUM+I2-1)*(ISUM+I2-2)**2*(ISUM+I2-3)
      IF(((ISUM.EQ.0).OR.(ISUM.EQ.1)).AND.(I.EQ.1)) GO TO 69
      C(I)=FNUMP/DENP
      GO TO 20
69    C(I)=0.D0
20    CONTINUE

C-----------------------------------------------------------------------
C     For outputting quadratures
C-----------------------------------------------------------------------
C      TYPE 34
C34    FORMAT(1X,'FILENAME FOR INTERMEDIATE RESULTS =? (A10)')
C      ACCEPT 35,FNAMX
C35    FORMAT(A10)
C      OPEN(UNIT=3,FILE=FNAMX,STATUS='NEW')
C-----------------------------------------------------------------------

      CALL JACOBI(NN,X,G,ALF,BTA,B,C,EPS,CSX,CSA,TSX,TSA)

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
      XX(I)=SIN(ZZ)/COS(ZZ)
      WW(I)=G(II)*PI*0.25D+00/(COS(0.25D+00*PI*(1.0D+00
     1      +X(II)))**2)

C-----------------------------------------------------------------------
C      WRITE(7,120)I,I,XX(I),WW(I)
120   FORMAT(6X,6HDATAX(,I2,6H,M),W(,I2,4H,M)/,D22.15,1H,,
     1      D22.15,1H/)
C-----------------------------------------------------------------------

100   CONTINUE
        RETURN
 130  CONTINUE
      NI=NN/2
      DO 140 I=1,NI
      II=NI+1-I
      XX(I)=X(II)
      WW(I)=G(II)

C-----------------------------------------------------------------------
C      WRITE(7,120)I,I,XX(I),WW(I)
C-----------------------------------------------------------------------

140   CONTINUE
      RETURN
      END
      SUBROUTINE JACOBI(NN,X,A,ALF,BTA,B,C,EPS,CSX,CSA,TSX,TSA)
      IMPLICIT REAL*8 (A-H,O-Z)
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
      BETA=EXP(FLGAMA(ALF+1.D0)+FLGAMA(BTA+1.D0)-FLGAMA(ALF+BTA+2.D0))
      CC=2.**(ALF+BTA+1.)*BETA
      TSX=FN*(BTA-ALF)/(ALF+BTA+2.*FN)
      TSA=CC
      DO 1 J=2,NN
1     CC=CC*C(J)
      DO 12 I=1,NN
      IF(I-1)12,2,3
C        LARGEST ZERO
2     AN=ALF/FN
      BN=BTA/FN
      R1=(1.D0+ALF)*(2.78/(4.+FN*FN)+.768*AN/FN)
      R2=1.D0+1.48*AN+.96*BN+.452*AN*AN+.83*AN*BN
      XT=1.D0-R1/R2
      GO TO 11
3     IF(I-2)12,4,5
C        SECOND ZERO
4     R1=(4.1+ALF)/((1.+ALF)*(1.+.156*ALF))
      R2=1.D0+.06*(FN-8.)*(1.D0+.12*ALF)/FN
      R3=1.D0+.012*BTA*(1.D0+.25*ABS(ALF))/FN
      RATIO=R1*R2*R3
      XT=XT-RATIO*(1.D0-XT)
      GO TO 11
5     IF(I-3) 12,6,7
C        THIRD ZERO
6     R1=(1.67+.28*ALF)/(1.D0+.37*ALF)
      R2=1.D0+.22*(FN-8.D0)/FN
      R3=1.D0+8.*BTA/((6.28D0+BTA)*FN*FN)
      RATIO=R1*R2*R3
      XT=XT-RATIO*(X(1)-XT)
      GO TO 11
7     IF(NN-I-1)10,9,8
C        MIDDLE ZEROS
8     XT=3.*X(I-1)-3.*X(I-2)+X(I-3)
      GO TO 11
C        SECOND LAST ZERO
9     R1=(1.D0+.235*BTA)/(.766D0+.119*BTA)
      R2=1./(1.D0+.639*(FN-4.D0)/(1.D0+.71*(FN-4.D0)))
      R3=1./(1.D0+20.*ALF/((7.5D0+ALF)*FN*FN))
      RATIO=R1*R2*R3
      XT=XT+RATIO*(XT-X(I-2))
      GO TO 11
C        LAST ZERO
10    R1=(1.D0+.37*BTA)/(1.67D0+.28*BTA)
      R2=1./(1.D0+.22*(FN-8.D0)/FN)
      R3=1./(1.D0+8.*ALF/((6.28D0+ALF)*FN*FN))
      RATIO=R1*R2*R3
      XT=XT+RATIO*(XT-X(I-2))
C
11    CALL GSROOT(XT,NN,ALF,BTA,DPN,PN1,B,C,EPS)
      X(I)=XT
      A(I)=CC/(DPN*PN1)
C-----------------------------------------------------------------------
C      WRITE(3,20)ALF,BTA,NN,I,XT,A(I)
C-----------------------------------------------------------------------

      CSX=CSX+XT
      CSA=CSA+A(I)
12    CONTINUE

C-----------------------------------------------------------------------
C      WRITE(3,22)CSX,CSA,TSX,TSA
20    FORMAT(1X,2F6.2,2I3,2(1X,D26.18),1X,(1X,D26.18))
22    FORMAT(1H0,/,5X,'CSX =',D25.18,5X,'CSA =',D25.18,/,
     1 5X,'TSX =',D25.18,5X,'TSA =',D25.18)
C-----------------------------------------------------------------------

      RETURN
      END
      SUBROUTINE GSROOT(X,NN,ALF,BTA,DPN,PN1,B,C,EPS)
      IMPLICIT REAL*8 (A-H,O-Z)
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
1     ITER=ITER+1
      CALL RECR(P,DP,PN1,X,NN,ALF,BTA,B,C)
      D=P/DP
      X=X-D
      IF(ABS(D)-EPS)3,3,2
2     IF(ITER-10)1,3,3
3     DPN=DP
      RETURN
      END
      SUBROUTINE RECR(PN,DPN,PN1,X,NN,ALF,BTA,B,C)
      IMPLICIT REAL*8 (A-H,O-Z)
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
       FUNCTION FLGAMA(W)
      IMPLICIT REAL*8 (A-H,O-Z)
C
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
      Y=(Z-.5)*LOG(Z)-Z+.9189385332047+(((((((-10861851./ZZ
     1 +2356200.)/ZZ-704820.)/ZZ+309400.)/ZZ-218790.)/ZZ
     2 +291720.)/ZZ-1021020.)/ZZ+30630600.)/Z/367567200.
      IF(FK)6,6,4
4     IK=FK
      DO 5 I=1,IK
      FK=FK-1.
5     Y=Y-LOG(X+FK)
6     IF(M)7,11,7
7     P=PI/SIN(XPI)
      IF(P)8,8,10
8     WRITE(6,9)W
9     FORMAT(' W IS NEGATIVE:',2e12.4)
      Y=0.
      GO TO 11
10    Y=LOG(P)-Y
11    FLGAMA=Y
      RETURN
      END

c--------------------
cc library program
c--------------------
      SUBROUTINE DCLNLU(A,N,N1,B,EPS,W,IP,NSTOP)                          
C                                                                               
C                   SOLUTION OF THE SIMULTANEOUS LINEAR EQUATIONS               
C                  BY CROUT METHOD ( LU FACTORIZATION ).                        
C                                                                               
C         * USAGE - -                                                           
C                CALL DCLNLU(A,N,N1,B,EPS,W,IP,NSTOP)                           
C           INPUT - -                                                           
C                A     C*16- 2-DIM. ARRAY CONTAINING THE COEFFICIENTS.          
C                N     I*4 - ORDER OF THE MATRIX 'A'.                           
C                N1    I*4 - SIZE OF THE 2-DIM. ARRAY 'A' DEFINED               
C                            IN 'DIMENSION' STATEMENT (FIRST INDEX).            
C                B     C*16- 1-DIM. ARRAY CONTAINING THE RIGHT-HAND             
C                            SIDE VECTOR.                                       
C                EPS   R*8 - PARAMETER TO CHECK SINGULARITY OF THE              
C                            MATRIX 'A'. (STANDARD VALUE : 1.0D-14)             
C           OUTPUT -                                                            
C                A     C*16- RESULT OF THE LU FACTORIZATION.                    
C                B     C*16- SOLUTION.                                          
C                NSTOP I*4 - FINAL STATE OF THE EXECUTION;                      
C                               0 : NORMAL END                                  
C                               1 : MATRIX SINGULAR                             
C                               2 : ARGUMENT ERROR                              
C           WORKING AREA - -                                                    
C                IP    I*4 - 1-DIM. ARRAY.                                      
C                W     C*16- 1-DIM. ARRAY.                                      
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      COMPLEX*16 A,B,AWK,W                                                    
      REAL*8 MAX                                                                
      DIMENSION A(N1,1),B(1),IP(1),W(1)                                     
C                                                                               
C     ARGUMENT CHECK.                                                           
C                                                                               
      NM1 = N-1                                                                 
      IF ( N1-N ) 1010,100,100                                                  
  100 CONTINUE                                                                  
      IF ( NM1 ) 1010,1000,110                                                  
  110 CONTINUE                                                                  
      IF ( EPS ) 120,130,130                                                    
  120 CONTINUE                                                                  
      EPS = 1.0D-14                                                             
  130 CONTINUE                                                                  
C                                                                               
C     INITIALIZATION.                                                           
C                                                                               
      DO 140 I=1,N                                                              
      W(I) = CDABS(A(I,1))                                                      
      IP(I) = I                                                                 
  140 CONTINUE                                                                  
C                                                                               
C     FIND MAXIMUM ELEMENT FOR EVERY ROW.                                       
C                                                                               
      DO 160 J=2,N                                                              
      DO 160 I=1,N                                                              
      ABSS = CDABS(A(I,J))                                                      
      WRK = W(I)                                                                
      IF ( WRK-ABSS ) 150,160,160                                               
  150 CONTINUE                                                                  
      W(I) = ABSS                                                               
  160 CONTINUE                                                                  
      DO 180 I=1,N                                                              
      WRK = W(I)                                                                
      IF ( WRK) 170,1020,170                                                    
  170 CONTINUE                                                                  
      W(I) = 1.0D0/W(I)                                                         
  180 CONTINUE                                                                  
C                                                                               
C     START LU FACTORIZATION.                                                   
C                                                                               
      DO 260 K=1,N                                                              
      K1 = K-1                                                                  
      IF ( K1) 190,210,190                                                      
  190 CONTINUE                                                                  
      DO 200 J=1,K1                                                             
      IROW = IP(J)                                                              
      AWK       = - A(IROW,K) * A(IROW,J)                                       
      A(IROW,K) = - AWK                                                         
      KK = J+1                                                                  
      DO 200 II=KK,N                                                            
      I = IP(II)                                                                
      A(I,K) = A(I,K) + A(I,J)*AWK                                              
  200 CONTINUE                                                                  
C                                                                               
C     FIND MAXIMUM ELEMENT IN THE K-TH COLUMN.                                  
C                                                                               
  210 CONTINUE                                                                  
      MAX = 0.0D0                                                               
      DO 230 II=K,N                                                             
      I = IP(II)                                                                
      WRK = CDABS(A(I,K))*W(I)                                                  
      IF ( MAX-WRK ) 220,230,230                                                
  220 CONTINUE                                                                  
      IIROW = II                                                                
      MAX = WRK                                                                 
  230 CONTINUE                                                                  
C                                                                               
C     CHECK SINGULARITY.                                                        
C                                                                               
      IF ( MAX-EPS ) 240,250,250                                                
  240 CONTINUE                                                                  
      IF ( MAX ) 1030,1020,1030                                                 
  250 CONTINUE                                                                  
      IROW = IP(IIROW)                                                          
      IP(IIROW) = IP(K)                                                         
      IP(K) = IROW                                                              
      A(IROW,K) = 1.0D0/A(IROW,K)                                               
  260 CONTINUE                                                                  
c     ENTRY DCLSUB(B,NSTOP)                                                     
      IF ( NM1 ) 1010,1000,270                                                  
C                                                                               
C     FORWARD SUBSTITUTION.                                                     
C                                                                               
  270 CONTINUE                                                                  
      DO 280 K=1,NM1                                                            
      IROW = IP(K)                                                              
      AWK = -B(IROW)*A(IROW,K)                                                  
      B(IROW) = -AWK                                                            
      K1 = K+1                                                                  
      DO 280 II=K1,N                                                            
      I = IP(II)                                                                
      B(I) = A(I,K)*AWK+B(I)                                                    
  280 CONTINUE                                                                  
      B(I) = B(I)*A(I,N)                                                        
C                                                                               
C     BACKWARD SUBSTITUTION.                                                    
C                                                                               
      DO 290 KK=2,N                                                             
      K = N-KK+2                                                                
      AWK = -B(I)                                                               
      K1 = K-1                                                                  
      DO 290 II=1,K1                                                            
      I = IP(II)                                                                
      B(I) = A(I,K)*AWK+B(I)                                                    
  290 CONTINUE                                                                  
      DO 300 K=1,N                                                              
      W(K) = B(K)                                                               
  300 CONTINUE                                                                  
      DO 310 K=1,N                                                              
      I = IP(K)                                                                 
      B(K) = W(I)                                                               
  310 CONTINUE                                                                  
      NSTOP = 0                                                                 
      RETURN                                                                    
C                                                                               
C     MATRIX ELEMENT IS ONLY ONE.                                               
C                                                                               
 1000 CONTINUE                                                                  
      IF ( CDABS(A(1,1)) ) 1001,1020,1001                                       
 1001 CONTINUE                                                                  
      NSTOP = 0                                                                 
      B(1) = B(1)/A(1,1)                                                        
      IP(1) = 1                                                                 
      RETURN                                                                    
C                                                                               
C     ARGUMENT ERROR RETURN.                                                    
C                                                                               
 1010 CONTINUE                                                                  
      WRITE(6,10) N,N1                                                          
      NSTOP = 3                                                                 
      RETURN                                                                    
C                                                                               
C     MATRIX IS SINGULAR.                                                       
C                                                                               
 1020 CONTINUE                                                                  
      WRITE(6,20)                                                               
      NSTOP = 1                                                                 
      RETURN                                                                    
 1030 CONTINUE                                                                  
      WRITE(6,30) K                                                             
      NSTOP = 2                                                                 
      RETURN                                                                    
   10 FORMAT(1H0,'(SUBR.DCLNLU) INVALID ARGUMENT. N,N1 =',2I5)                  
   20 FORMAT(1H0,'(SUBR.DCLNLU) MATRIX IS SINGULAR.')                           
   30 FORMAT(1H0,'(SUBR.DCLNLU) MATRIX IS SINGULAR AT STEP #',I4)               
      END                                                                       
      SUBROUTINE DCLSUB(A,N,N1,B,EPS,W,IP,NSTOP)                          
C                                                                               
C                   SOLUTION OF THE SIMULTANEOUS LINEAR EQUATIONS               
C                  BY CROUT METHOD ( LU FACTORIZATION ).                        
C                                                                               
C         * USAGE - -                                                           
C                CALL DCLNLU(A,N,N1,B,EPS,W,IP,NSTOP)                           
C           INPUT - -                                                           
C                A     C*16- 2-DIM. ARRAY CONTAINING THE COEFFICIENTS.          
C                N     I*4 - ORDER OF THE MATRIX 'A'.                           
C                N1    I*4 - SIZE OF THE 2-DIM. ARRAY 'A' DEFINED               
C                            IN 'DIMENSION' STATEMENT (FIRST INDEX).            
C                B     C*16- 1-DIM. ARRAY CONTAINING THE RIGHT-HAND             
C                            SIDE VECTOR.                                       
C                EPS   R*8 - PARAMETER TO CHECK SINGULARITY OF THE              
C                            MATRIX 'A'. (STANDARD VALUE : 1.0D-14)             
C           OUTPUT -                                                            
C                A     C*16- RESULT OF THE LU FACTORIZATION.                    
C                B     C*16- SOLUTION.                                          
C                NSTOP I*4 - FINAL STATE OF THE EXECUTION;                      
C                               0 : NORMAL END                                  
C                               1 : MATRIX SINGULAR                             
C                               2 : ARGUMENT ERROR                              
C           WORKING AREA - -                                                    
C                IP    I*4 - 1-DIM. ARRAY.                                      
C                W     C*16- 1-DIM. ARRAY.                                      
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      COMPLEX*16 A,B,AWK,W                                                    
      REAL*8 MAX                                                                
      DIMENSION A(N1,1),B(1),IP(1),W(1)                                     
C                                                                               
C     ARGUMENT CHECK.                                                           
C                                                                               
      NM1 = N-1                                                                 
      IF ( NM1 ) 1010,1000,270                                                  
C                                                                               
C     FORWARD SUBSTITUTION.                                                     
C                                                                               
  270 CONTINUE                                                                  
      DO 280 K=1,NM1                                                            
      IROW = IP(K)                                                              
      AWK = -B(IROW)*A(IROW,K)                                                  
      B(IROW) = -AWK                                                            
      K1 = K+1                                                                  
      DO 280 II=K1,N                                                            
      I = IP(II)                                                                
      B(I) = A(I,K)*AWK+B(I)                                                    
  280 CONTINUE                                                                  
      B(I) = B(I)*A(I,N)                                                        
C                                                                               
C     BACKWARD SUBSTITUTION.                                                    
C                                                                               
      DO 290 KK=2,N                                                             
      K = N-KK+2                                                                
      AWK = -B(I)                                                               
      K1 = K-1                                                                  
      DO 290 II=1,K1                                                            
      I = IP(II)                                                                
      B(I) = A(I,K)*AWK+B(I)                                                    
  290 CONTINUE                                                                  
      DO 300 K=1,N                                                              
      W(K) = B(K)                                                               
  300 CONTINUE                                                                  
      DO 310 K=1,N                                                              
      I = IP(K)                                                                 
      B(K) = W(I)                                                               
  310 CONTINUE                                                                  
      NSTOP = 0                                                                 
      RETURN                                                                    
C                                                                               
C     MATRIX ELEMENT IS ONLY ONE.                                               
C                                                                               
 1000 CONTINUE                                                                  
      IF ( CDABS(A(1,1)) ) 1001,1020,1001                                       
 1001 CONTINUE                                                                  
      NSTOP = 0                                                                 
      B(1) = B(1)/A(1,1)                                                        
      IP(1) = 1                                                                 
      RETURN                                                                    
C                                                                               
C     ARGUMENT ERROR RETURN.                                                    
C                                                                               
 1010 CONTINUE                                                                  
      WRITE(6,10) N,N1                                                          
      NSTOP = 3                                                                 
      RETURN                                                                    
C                                                                               
C     MATRIX IS SINGULAR.                                                       
C                                                                               
 1020 CONTINUE                                                                  
      WRITE(6,20)                                                               
      NSTOP = 1                                                                 
      RETURN                                                                    
c1030 CONTINUE                                                                  
c     WRITE(6,30) K                                                             
c     NSTOP = 2                                                                 
c     RETURN                                                                    
   10 FORMAT(1H0,'(SUBR.DCLNLU) INVALID ARGUMENT. N,N1 =',2I5)                  
   20 FORMAT(1H0,'(SUBR.DCLNLU) MATRIX IS SINGULAR.')                           
   30 FORMAT(1H0,'(SUBR.DCLNLU) MATRIX IS SINGULAR AT STEP #',I4)               
      END                                                                       
      SUBROUTINE BCODIN(NM)                                                     
      IMPLICIT REAL*8 (A-H,O-Z)                                                 
C C C C C                                                                       
      COMMON /BCODCM/ Q(52,52)                                                  
C C C C C=                                                                      
      IF(NM.NE.52) GO TO 99                                                     
      Q(1,1)=1.0D0                                                              
      Q(2,1)=1.0D0                                                              
      Q(1,2)=1.0D0                                                              
      Q(2,2)=1.0D0                                                              
      DO 10 I=3,52                                                              
      Q(I,1)=1.0D0                                                              
      Q(1,I)=1.0D0                                                              
      Q(I,I)=1.0D0                                                              
      DO 11 J=2,I-1                                                             
      Q(J,I)=Q(J-1,I-1)+Q(J,I-1)                                                
   11 CONTINUE                                                                  
   10 CONTINUE                                                                  
      DO 20 J=3,52                                                              
      DO 20 I=1,J-1                                                             
      Q(J,I)=1.0D0/SQRT(Q(I,J))                                                 
   20 CONTINUE                                                                  
      RETURN                                                                    
   99 WRITE(6,100)                                                              
  100 FORMAT(1H ,'* ERR IN BCODIN *')                                           
      RETURN                                                                    
      END                                                                       
      SUBROUTINE LEGAUS(XS,XL,IN,X,W)                                           
CEGENDRE-GAUSS-INTEGRAL-LEGENDR-GAUSS-INTEGRAL-LEGENDRE-GAUSS-INTEGRAL          
C982.1.15.T.S.CHEON.1982.1.15.T.S.CHEON.1982.1.15.T.S.CHEON.1982.1.15.          
CEGENDRE-GAUSS-INTEGRAL-LEGENDR-GAUSS-INTEGRAL-LEGENDRE-GAUSS-INTEGRAL          
      IMPLICIT REAL*8(A-H,K-Z)                                                  
      DIMENSION X(60),W(60)                                                     
      ZERO=0.0d0                                                                
      HALB=0.5d0                                                                
      EIN=1.0d0                                                                 
      ZWEI=2.0d0                                                                
      IF (IN.LE.0) THEN                                                         
      WRITE(6,600) IN                                                           
  600 FORMAT(' --> ',I5,' POINT LEGENDR-GAUSS DOES NOT EXIST | <--')            
      RETURN                                                                    
      END IF                                                                    
      IF (IN-2.LT.0) THEN                                                       
      X(1)=ZERO                                                                 
      W(1)=HALB                                                                 
      GO TO 140                                                                 
      END IF                                                                    
      I=1                                                                       
      G=-EIN                                                                    
      IC=(IN+1)/2                                                               
   50 CONTINUE                                                                  
      S=G                                                                       
      T=EIN                                                                     
      U=EIN                                                                     
      V=ZERO                                                                    
      DO 60 IK=2,IN                                                             
      A=IK                                                                      
      FACT1=(ZWEI*A-EIN)/A                                                      
      FACT2=(A-EIN)/A                                                           
      P=FACT1*G*S-FACT2*T                                                       
      DP=FACT1*(S+G*U)-FACT2*V                                                  
      T=S                                                                       
      S=P                                                                       
      V=U                                                                       
      U=DP                                                                      
   60 CONTINUE                                                                  
      SUM=ZERO                                                                  
      IF (I-1.GT.0) THEN                                                        
      IM1=I-1                                                                   
      DO 80 IK=1,IM1                                                            
      SUM=SUM+EIN/(G-X(IK))                                                     
   80 CONTINUE                                                                  
      END IF                                                                    
      TEST=G                                                                    
      G=G-P/(DP-P*SUM)                                                          
      R=DABS(TEST-G)                                                            
      IF (R.LT.1.0D-13) GO TO 100                                               
      GO TO 50                                                                  
  100 CONTINUE                                                                  
      R=IN                                                                      
      X(I)=G                                                                    
      W(I)=ZWEI/R/T/DP                                                          
      IF (IC-I.GT.0) THEN                                                       
      RUM=ZERO                                                                  
      IF (I-1.GT.0) THEN                                                        
      DO 112 IK=1,IM1                                                           
  112 RUM=RUM+EIN/(G-X(IK))**2                                                  
      END IF                                                                    
      G=G-(DP-P*SUM)/((ZWEI*G*DP-A*(A+EIN)*P)/(EIN-G*G)-ZWEI*DP*SUM             
     1  +P*SUM**2-RUM*P)                                                        
      I=I+1                                                                     
      GO TO 50                                                                  
      END IF                                                                    
      IK0=2*IC-IN+2*(IN/2)+1                                                    
      IC=IC+1                                                                   
      DO 130 I=IC,IN                                                            
      IK=IK0-I                                                                  
      X(I)=-X(IK)                                                               
      W(I)=W(IK)                                                                
  130 CONTINUE                                                                  
  140 FACT1=(XL-XS)/ZWEI                                                        
      FACT2=(XL+XS)/ZWEI                                                        
      DO 150 I=1,IN                                                             
      W(I)=W(I)*FACT1                                                           
      X(I)=X(I)*FACT1+FACT2                                                     
  150 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DCINV(A,N,M,N1,M1,EPS,DET,W,IP,NSTOP)
C
C                   MATRIX INVERSION BY SWEEP OUT METHOD.
C
C         * USAGE - -
C                CALL DCINV(A,N,M,N1,M1,EPS,DET,W,IP,NSTOP)
C           INPUT - -
C                A     C*16- 2-DIM. ARRAY CONTAINING THE COEFFICIENTS
C                            AND THE RIGHT-HAND SIDE VECTORS.
C                N     I*4 - ORDER OF MATRIX 'A'.
C                M     I*4 - NUMBER OF LINEAR EQUATIONS.
C                N1    I*4 - SIZE OF THE 2-DIM. ARRAY 'A' DEFINED
C                            IN 'DIMENSION' STATEMENT (FIRST INDEX).
C                M1    I*4 - SIZE OF THE 2-DIM. ARRAY 'A' DEFINED
C                            IN 'DIMENSION' STATEMENT (SECOND INDEX).
C                EPS   R*8 - PARAMETER TO CHECK SINGULARITY OF THE
C                            MATRIX 'A'. (STANDARD VALUE : 1.0D-14)
C           OUTPUT - -
C                A     C*16- INVERSE MATRIX AND SOLUTION.
C                NSTOP I*4 - FINAL STATE OF THE EXECUTION.
C                               0 : NORMAL END
C                               1 : MATRIX SINGULAR
C                               2 : ARGUMENT ERROR
C           WORKING AREA - -
C                W     C*16     1-DIM. ARRAY.
C                IP    I*4      1-DIM. ARRAY.
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 A,DET,PIVOT,W,AWK,PIVI
      REAL*8 MAX
      DIMENSION A(N1,1),IP(1),W(1)
C
C     ARGUMENT CHECK.
C
      NM = N+M
      IF ( N ) 1000,1000,100
  100 CONTINUE
      IF ( N-N1 ) 110,110,1000
  110 CONTINUE
      IF ( M ) 1000,120,120
  120 CONTINUE
      IF ( NM-M1 ) 130,130,1000
  130 CONTINUE
      IF ( EPS ) 140,150,150
  140 CONTINUE
      EPS = 1.0D-14
  150 CONTINUE
C
C     INITIALIZATION.
C
      EPSS = 1.0D-2*EPS
      DET = 1.0D0
      DO 160 I=1,N
      IP(I) = 0
  160 CONTINUE
C
C     PIVOT SERCH.
C
      DO 270 K=1,N
      MAX = -1.0D0
      DO 190 I=1,N
      IF ( IP(I) ) 170,170,190
  170 CONTINUE
      ABSS = CDABS(A(I,K))
      IF ( MAX-ABSS) 180,190,190
  180 CONTINUE
      MAX = ABSS
      L = I
  190 CONTINUE
C
C     CHECK SINGULARITY AND CALCULATE DETERMINANT.
C
      IF ( MAX-EPS ) 1010,1010,200
  200 CONTINUE
      PIVOT = A(L,K)
      DET = DET*PIVOT
C
C     START SWEEP OUT.
C
      IP(L) = K
      PIVI = - 1.0D0/PIVOT
      DO 250 J=1,NM
      IF ( J-K ) 210,250,210
  210 CONTINUE
      AWK = A(L,J)*PIVI
      IF ( CDABS(AWK)-EPSS ) 240,240,220
  220 CONTINUE
      DO 230 I=1,N
      A(I,J) = A(I,J) + A(I,K)*AWK
  230 CONTINUE
  240 CONTINUE
      A(L,J) = -AWK
  250 CONTINUE
      DO 260 I=1,N
      A(I,K) = A(I,K)*PIVI
  260 CONTINUE
      A(L,K) = -PIVI
  270 CONTINUE
C
C     EXCHANGE COLUMN.
C
      DO 290 J=1,NM
      DO 280 I=1,N
      W(I) = A(I,J)
  280 CONTINUE
      DO 290 I=1,N
      A(IP(I),J) = W(I)
  290 CONTINUE
      IF ( N-1 ) 350,350,300
  300 CONTINUE
      DO 320 I=1,N
      DO 310 J=1,N
      W(J) = A(I,J)
  310 CONTINUE
      DO 320 J=1,N
      A(I,J) = W(IP(J))
  320 CONTINUE
      NM = N-1
      DO 340 I=1,NM
      K = I+1
      DO 340 J=K,N
      IF ( IP(I)-IP(J) ) 340,340,330
  330 CONTINUE
      DET = -DET
  340 CONTINUE
  350 CONTINUE
      NSTOP = 0
      RETURN
C
C     ARGUMENT ERROR RETURN.
C
 1000 CONTINUE
      NSTOP = 3
      WRITE(6,10) N,M,N1,M1
      RETURN
C
C     MATRIX IS SINGULAR.
C
 1010 CONTINUE
      DET = 0.0D0
      IF ( MAX ) 1011,1020,1011
 1011 CONTINUE
      NSTOP = 2
      WRITE(6,20) K
      RETURN
 1020 CONTINUE
      NSTOP = 1
      WRITE(6,30)
      RETURN
   10 FORMAT(1H0,'(SUBR.DCINV) INVALID ARGUMENT. N,M,N1,M1 =',4I5)
   20 FORMAT(1H0,'(SUBR.DCINV) MATRIX IS SINGULAR AT STEP #',I5)
   30 FORMAT(1H0,'(SUBR.DCINV) MATRIX IS SINGULAR.')
      END
