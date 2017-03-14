
      FUNCTION  CLBS(J1,M1,J2,M2,J3)                                            
      COMMON/CLBSF/K(2)                                                         
      DIMENSION M(41)                                                           
      M3=M1+M2                                                                  
      IF(J3.LT.IABS(J1-J2).OR.J3.GT.(J1+J2))  GO TO 11                          
      IF(IABS(M1).GT.J1)  GO TO 11                                              
      IF(IABS(M2).GT.J2)  GO TO 11                                              
      IF(IABS(M3).GT.J3)  GO TO 11                                              
      IF(MOD(J1+IABS(M1),2).EQ.1)  GO TO 11                                     
      IF(MOD(J2+IABS(M2),2).EQ.1)  GO TO 11                                     
      IF(MOD(J3+IABS(M3),2).EQ.1)  GO TO 11                                     
      GO TO 12                                                                  
   11 CONTINUE                                                                  
      CLBS=0.                                                                   
      RETURN                                                                    
   12 CONTINUE                                                                  
      CALL CLEBS(J1,J2,J3,M1,M2,M3,K,M)                                         
      SGN=ISIGN(1,K(1))                                                         
      UP=IABS(K(1))                                                             
      DN=K(2)                                                                   
      CLBS=SGN*SQRT(UP/DN)                                                      
      RETURN                                                                    
      END                                                                       
      SUBROUTINE CLEBS(J1,J2,J3,JM1,JM2,JM3,K,M)                                
C --- COMPUTES CLEBSCH-GORDAN COEFFICIENTS.                                     
C                    I                                                          
C     (J1,JM1,J2,JM2 I J3M,JM3) =  K(1)/K(2) IN RATIONAL FORM                   
C                    I                                                          
C --- END COMMENT.                                                              
      DIMENSION L1(40),L2(40),IB(40),K(2),LS(40,11),LD(40,6),IIL(40,2),         
     F                LLSN(40),LLSD(40),IL(2),LL(40),LR(40,100),LQ(40),         
     F IS(100),ISK(40),M(41)                                                    
      DIMENSION LR1(40),ICGC(40)                                                
      IF((JM1+JM2).NE.JM3)     GO TO 1                                          
      CALL PRIME(J3+1,LS(1,1),ID)                                               
      CALL FCTRAL((J3+J1-J2)/2,LS(1,2))                                         
      CALL FCTRAL((J3-J1+J2)/2,LS(1,3))                                         
      CALL FCTRAL((J1+J2-J3)/2,LS(1,4))                                         
      CALL FCTRAL((J3+JM3)/2,LS(1,5))                                           
      CALL FCTRAL((J3-JM3)/2,LS(1,6))                                           
      CALL FCTRAL((J1+J2+J3)/2+1,LS(1,7))                                       
      CALL FCTRAL((J1-JM1)/2,LS(1,8))                                           
      CALL FCTRAL((J1+JM1)/2,LS(1,9))                                           
      CALL FCTRAL((J2-JM2)/2,LS(1,10))                                          
      CALL FCTRAL((J2+JM2)/2,LS(1,11))                                          
      DO 2    LZ=1,11                                                           
      IF (LS(1,LZ) .EQ. (-77))   GO TO 45                                       
    2 CONTINUE                                                                  
      CALL IZERO (M,41)                                                         
      CALL IZERO (LLSN,40)                                                      
      CALL IZERO (LLSD, 40)                                                     
      DO 5  J=1,40                                                              
      DO 5 I=1,6                                                                
    5 LLSN(J)=LLSN(J)+LS(J,I)                                                   
      DO 6  J=1,40                                                              
      DO 6  I=7,11                                                              
    6 LLSD(J)=LLSD(J)+LS(J,I)                                                   
      CALL DIVIDS(LLSN,LLSD,LL)                                                 
      MJ=1                                                                      
      N=-1                                                                      
      MM=0                                                                      
    3 N=N+1                                                                     
      CALL FCTRAL((J2+J3+JM1)/2-N,LD(1,1))                                      
      CALL FCTRAL((J1-JM1)/2+N,LD(1,2))                                         
      CALL FCTRAL(N,LD(1,3))                                                    
      CALL FCTRAL((J3-J1+J2)/2-N,LD(1,4))                                       
      CALL FCTRAL((J3+JM3)/2-N,LD(1,5))                                         
      CALL FCTRAL((J1-J2-JM3)/2+N,LD(1,6))                                      
      IF(LD(1,1).EQ.(-77))    GO TO 4                                           
      IF(LD(1,4).EQ.(-77))    GO TO 4                                           
      IF(LD(1,5).EQ.(-77))    GO TO 4                                           
      IF(LD(1,2).EQ.(-77))  GO TO 3                                             
      IF(LD(1,6).EQ.(-77))  GO TO 3                                             
      MM=MM+1                                                                   
      IS(MM)=(-1)**(N+(J2+JM2)/2)                                               
      DO 7    JK=1,40                                                           
      IIL(JK,1)=LD(JK,1)+LD(JK,2)                                               
    7 IIL(JK,2)=LD(JK,3)+LD(JK,4)+LD(JK,5)+LD(JK,6)                             
      CALL DIVIDS (IIL(1,1),IIL(1,2),LR(1,MM))                                  
      GO TO 3                                                                   
    4 DO 31 JJ=1,40                                                             
      IB(JJ)=10000                                                              
      DO 31  JK=1,MM                                                            
      IF (LR(JJ,JK) .LT. IB(JJ))   IB(JJ)=LR(JJ,JK)                             
   31 CONTINUE                                                                  
      ISUM=0                                                                    
      DO 32 JM=1,MM                                                             
      DO 33 IO=1,40                                                             
   33 LQ(IO)=LR(IO,JM)-IB(IO)                                                   
      CALL PRMTOD(LQ,K(1),K(2))                                                 
   32 ISUM=ISUM+K(1)*IS(JM)                                                     
      IX=IABS(ISUM)                                                             
      IF(IX.EQ.0) GO TO 1                                                       
      CALL PRIME(IX,ISK,IP)                                                     
      DO 22 LK=1,40                                                             
   22 ICGC(LK)=LL(LK)+2*IB(LK)+2*ISK(LK)                                        
      IXX=IP*   ISUM/IABS(ISUM)                                                 
      ISIGN=ISUM/IABS(ISUM)                                                     
      IXXX=IP*ISIGN                                                             
      M(41)=IXXX                                                                
      CALL VCOPY (ICGC, M, 40)                                                  
      CALL PRMTOD(ICGC,K(1),K(2))                                               
      K(1)=K(1)*IXX                                                             
      RETURN 
 45    CONTINUE
C   45 PRINT 50                                                                  
   50 FORMAT(1H ,31HCHECK YOUR PARAMETERS ONCE MORE,///)                        
    1 K(1)=0                                                                    
      K(2)=1                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DIVIDS (J,K,L)                                                 
      DIMENSION  J(40), K(40), L(40)                                            
      DO 1 LL=1,40                                                              
    1 L(LL)=J(LL)-K(LL)                                                         
      RETURN                                                                    
      END                                                                       
      SUBROUTINE PRMTOD(J,IN,ID)                                                
      DIMENSION  J(40),IPRIME(40)                                               
      DATA IPRIME/2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,         
     + 71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,           
     + 157,163,167,173/                                                         
      IN=1                                                                      
      ID=1                                                                      
      DO 1 LL=1,40                                                              
      IF(J(LL).GT.0)    IN=IN*IPRIME(LL)**  J(LL)                               
    1 IF(J(LL).LT.0)    ID=ID*IPRIME(LL)**(-J(LL))                              
      RETURN                                                                    
      END                                                                       
      SUBROUTINE FCTRAL (I,K)                                                   
      DIMENSION K(40), L(40)                                                    
      CALL IZERO (K, 40)                                                        
      IF(I)  4,3,2                                                              
    4 K(1)=-77                                                                  
      RETURN                                                                    
    2 DO 5  JK=1,I                                                              
      CALL PRIME(JK,L,M)                                                        
      DO 5 JM=1,40                                                              
    5 K(JM)=K(JM)+L(JM)                                                         
    3 RETURN                                                                    
      END                                                                       
      SUBROUTINE PRIME (I,K,M)                                                  
      DIMENSION IPRIME(40),K(40)                                                
      DATA IPRIME/2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,         
     + 71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,           
     + 157,163,167,173/                                                         
      II=I                                                                      
      M =1                                                                      
      IF(I.LT.1)  GO TO 6                                                       
      CALL IZERO (K, 40)                                                        
      IF(I.EQ.1)  RETURN                                                        
      DO 2 KL=1,40                                                              
    1 IF(MOD(I,IPRIME(KL)).NE.0)   GO TO 2                                      
      K(KL)=K(KL)+1                                                             
      I=I/IPRIME(KL)                                                            
      IF(I.EQ.1)  GO TO 3                                                       
      GO TO 1                                                                   
    2 CONTINUE                                                                  
      IF(I.NE.1)    PRINT 4    ,I                                               
    4 FORMAT(' PRIME NO. LARGER THAN THE LIST,THE FACTOR LEFT=',I15)            
      M=I                                                                       
    3 I=II                                                                      
      RETURN                                                                    
    6 PRINT 7                                                                   
    7 FORMAT(' PLEASE OBSERVE THE CONVENTION OF YOUR ASKING NO. BEING           
     FLARGER THAN +0.')                                                         
      RETURN                                                                    
      END 

	SUBROUTINE DFUNC(ALPHA,BETA,GAMMA,J,N,M,RE,IM)
	    REAL*4 RE,IM
	    COMPLEX DVAL,DDFG
	    DVAL = DDFG(ALPHA,BETA,GAMMA,J,N,M)
	    RE = REAL(DVAL)
	    IM = IMAG(DVAL)
	    RETURN
	    END
	                                                                      
      COMPLEX FUNCTION DDF(ALPHA,BETA,GAMMA,J,N,M)                              
C-----                                                                          
C     CALCULATION OF THE D FUNCTION                                             
C-----                                                                          
      DD=DSF(BETA,J,N,M)                                                        
      ANGLE = FLOAT(N)*ALPHA + FLOAT(M)*GAMMA                                   
      DREAL =  DD*COS(ANGLE)                                                    
      DIMAG = -DD*SIN(ANGLE)                                                    
      DDF=CMPLX(DREAL,DIMAG)                                                    
      RETURN                                                                    
      END                                                                       
      FUNCTION DSF(BETA,J,N,M)                                                  
      REAL N1,N2,N3,N4,N5,N6,N7,N8                                              
C-----                                                                          
C     CALCULATION OF THE SMALL D FUNCTION                                       
C-----                                                                          
      IF(J.LT.0)  GO TO 1000                                                    
      IF(M.GT.J.OR.NOT.GT.J.OR.M.LT.(-J).OR.NOT.LT.(-J))  GO TO 1000            
      IF(J.GT.0)  GO TO 10                                                      
      DSF=1.                                                                    
      RETURN                                                                    
   10 CONTINUE                                                                  
      LIMLOW = 0                                                                
      IF((M-N).GT.0)  LIMLOW = M-N                                              
      LIMUPP = J + M                                                            
      IF((J-N).LT.(J+M))   LIMUPP = J-N                                         
      DD = 0.0                                                                  
      B1 = COS(BETA/2.0)                                                        
      B2 = SIN(BETA/2.0)                                                        
      CALL FCH(J+M,N1)                                                          
      CALL FCH(J-M,N2)                                                          
      CALL FCH(J+N,N3)                                                          
      CALL FCH(J-N,N4)                                                          
      C1=SQRT(N1)                                                               
      C2=SQRT(N2)                                                               
      C3=SQRT(N3)                                                               
      C4=SQRT(N4)                                                               
      DO 101 K = LIMLOW,LIMUPP                                                  
      CALL FCH(J-N-K,N5)                                                        
      CALL FCH(J+M-K,N6)                                                        
      CALL FCH(K,N7)                                                            
      CALL FCH(K+N-M,N8)                                                        
      G1=C1/N5                                                                  
      G2=C2/N6                                                                  
      G3=C3/N7                                                                  
      G4=C4/N8                                                                  
      C=G1*G2*G3*G4                                                             
      CC = FLOAT((-1)**(K+N-M))                                                 
      DD  =   DD  +        CC*C*B1**(2*J+M-N-2*K)*B2**(2*K+N-M)                 
  101 CONTINUE                                                                  
      DSF=DD                                                                    
      RETURN                                                                    
 1000 PRINT 2000,J,N,M                                                          
 2000 FORMAT(/,5X,' ERROR IN LABELS IN D-FUNCTION CALL  ',                      
     1  5X,' J,N,M = ',3I5,/)                                                   
      STOP                                                                      
      END                                                                       
      SUBROUTINE FCH(N,FACT)                                                    
C-----                                                                          
C     N FACTORIAL                                                               
C-----                                                                          
      FACT=1.                                                                   
      IF(N.LT.2)  RETURN                                                        
      DO 101 I = 2,N                                                            
      FACT=FACT*FLOAT(I)                                                        
  101 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       


      COMPLEX FUNCTION DDFG(AL,BT,GM,J,M,N)                                     
C --- COMPUTE CAPITAL D-FUNCTION.                                               
C                                                                               
C      J/2                                                                      
C     D      (AL,BT,GM)                                                         
C      M/2,N/2                                                                  
C                                                                               
C     NOTE THAT J,M AND N ARE ALWAYS INTEGERS.                                  
C --- END COMMENT.                                                              

      DD=DSFG(BT,J,M,N)                                                         
      ANGLE=FLOAT(M)/2.*AL+FLOAT(N)/2.*GM                                       
      DREAL =  DD*COS(ANGLE)                                                    
      DIMAG = -DD*SIN(ANGLE)                                                    
      DDFG=CMPLX(DREAL,DIMAG)                                                   
      RETURN                                                                    
      END                                                                       


      FUNCTION DSFG(BT,J,M,N)                                                   
C --- COMPUTE SMALL D-FUNCTION.                                                 
C                                                                               
C      J/2                                                                      
C     D      (BT)   WHERE BT IS IN RADIANS                                      
C      M/2,N/2                                                                  
C                                                                               
C     NOTE THAT J,M AND N ARE INTEGERS.                                         
C --- END OF COMMENT.                                                           
      DATA PII/3.1415926/                                                       
      IF(IABS(M).GT.J)  GO TO 11                                                
      IF(IABS(N).GT.J)  GO TO 11                                                
      JM=J+M                                                                    
      JN=J+N                                                                    
      IF(MOD(JM,2).EQ.1.OR.MOD(JN,2).EQ.1)  GO TO 11                            
      GO TO 12                                                                  
   11 CONTINUE                                                                  
      DSFG=0.                                                                   
      RETURN                                                                    
   12 CONTINUE                                                                  

       AJ=FLOAT(J)/2.
       AM=FLOAT(M)/2.
       AN=FLOAT(N)/2.

C      BETA=BT*180./PII
C      DSFG=DSMALL(AJ,AM,AN,BETA)

c      DSFG=DJMNB(AJ,AM,AN,BETA)  MN: to investigate NAN problems. 
c                                     But now use since when tried to compiled, 
c                                     got a CERN message: DJMNB not available on this machine. 

c --- replacing with call to su urks new, improved d-function 
c     routine.
c
       costh = cos(BT)
       dsfg = dscal(J/2,M/2,N/2,costh)
       if(BT.lt.0.0 .and. mod(M-N,4).ne.0)  dsfg=-1.*dsfg
       RETURN                                                                    
       END                                                                       

      FUNCTION DSMALL(AJ,AM,AN,BETA)                                            
C       COMPUTES THE ELEMENTS OF THE FINITE ROTATION MATRIX AS ADAPTED          
C       FROM E.P. WIGNER, GROUP THEORY AND ITS APPLICATION TO THE QUAN-         
C       TUM MECHANICS OF ATOMIC SPECTRA, 1959                                   
C       BY D. TEODORO AND K.S. KOELBIG (CERN),  OCTOBER 1977                    
      DIMENSION Z(90),A(90),Z1(30),Z2(30),Z3(30)                                
      EQUIVALENCE (Z(1),Z1(1)),(Z(31),Z2(1)),(Z(61),Z3(1)),(A(1),Z(1))          
      DATA Z1 /                                                                 
     1 0.                 , 0.                 , 6.9314718055993E-01,           
     2 1.7917594692280E+00, 3.1780538303478E+00, 4.7874917427819E+00,           
     3 6.5792512120101E+00, 8.5251613610651E+00, 1.0604602902745E+01,           
     4 1.2801827480081E+01, 1.5104412573076E+01, 1.7502307845874E+01,           
     5 1.9987214495662E+01, 2.2552163853123E+01, 2.5191221182738E+01,           
     6 2.7899271383841E+01, 3.0671860106080E+01, 3.3505073450136E+01,           
     7 3.6395445208033E+01, 3.9339884187199E+01, 4.2335616460753E+01,           
     8 4.5380138898477E+01, 4.8471181351835E+01, 5.1606675567764E+01,           
     9 5.4784729398112E+01, 5.8003605222980E+01, 6.1261701761002E+01,           
     A 6.4557538627005E+01, 6.7889743137181E+01, 7.1257038967167E+01/           
      DATA Z2 /                                                                 
     1 7.4658236348829E+01, 7.8092223553314E+01, 8.1557959456115E+01,           
     2 8.5054467017581E+01, 8.8580827542197E+01, 9.2136175603686E+01,           
     3 9.5719694542142E+01, 9.9330612454787E+01, 1.0296819861451E+02,           
     4 1.0663176026064E+02, 1.1032063971476E+02, 1.1403421178146E+02,           
     5 1.1777188139974E+02, 1.2153308151544E+02, 1.2531727114936E+02,           
     6 1.2912393363913E+02, 1.3295257503562E+02, 1.3680272263733E+02,           
     7 1.4067392364823E+02, 1.4456574394634E+02, 1.4847776695177E+02,           
     8 1.5240959258449E+02, 1.5636083630308E+02, 1.6033112821663E+02,           
     9 1.6432011226319E+02, 1.6832744544842E+02, 1.7235279713916E+02,           
     A 1.7639584840699E+02, 1.8045629141754E+02, 1.8453382886145E+02/           
      DATA Z3 /                                                                 
     1 1.8862817342367E+02, 1.9273904728784E+02, 1.9686618167289E+02,           
     2 2.0100931639928E+02, 2.0516819948264E+02, 2.0934258675253E+02,           
     3 2.1353224149456E+02, 2.1773693411395E+02, 2.2195644181913E+02,           
     4 2.2619054832373E+02, 2.3043904356577E+02, 2.3470172344282E+02,           
     5 2.3897838956183E+02, 2.4326884900298E+02, 2.4757291409618E+02,           
     6 2.5189040220972E+02, 2.5622113555001E+02, 2.6056494097186E+02,           
     7 2.6492164979855E+02, 2.6929109765102E+02, 2.7367312428569E+02,           
     8 2.7806757344036E+02, 2.8247429268763E+02, 2.8689313329543E+02,           
     9 2.9132395009427E+02, 2.9576660135076E+02, 3.0022094864701E+02,           
     A 3.0468685676567E+02, 3.0916419358014E+02, 3.1365282994988E+02/           
      DSMALL=0.                                                                 
      IF(AJ.GE.45.0) GOTO 5                                                     
      JPM=AJ+AM+SIGN(0.1,AJ+AM)                                                 
      JPN=AJ+AN+SIGN(0.1,AJ+AN)                                                 
      JMM=AJ-AM+SIGN(0.1,AJ-AM)                                                 
      JMN=AJ-AN+SIGN(0.1,AJ-AN)                                                 
      IF(JPM.LT.0 .OR. JPN.LT.0 .OR. JMM.LT.0 .OR. JMN.LT.0) GOTO 5             
      MPN=AM+AN+SIGN(0.1,AM+AN)                                                 
      IF(BETA.EQ.0.) GOTO 2                                                     
      IF(BETA.EQ.180.) GOTO 3                                                   
      IF(BETA.EQ.360.) GOTO 4                                                   
      B=0.00872 66462 599716*BETA                                               
      S=SIN(B)                                                                  
      C=COS(B)                                                                  
      R=(C/S)**2                                                                
      W=0.5*(A(JPM+1)+A(JMM+1)+A(JPN+1)+A(JMN+1))                                       
      NUA=MAX0(0,MPN)                                                           
      NUZ=MIN0(JPM,JPN)                                                         
      H=(-R)**NUA                                                               
      SUM=0.                                                                    
      NU=NUA-1                                                                  
    1 NU=NU+1                                                                   
      SUM=SUM+H*EXP(W-A(NU+1)-A(JPM-NU+1)-A(JPN-NU+1)-A(NU-MPN+1))                      
      H=-H*R                                                                    
      IF(NU.LT.NUZ) GOTO 1                                                      
      DSMALL=(-1.0)**JPM*SUM*S**(JPM+JPN)/C**MPN                                
      RETURN                                                                    
    2 IF(JPM.EQ.JPN) DSMALL=1.                                                  
      RETURN                                                                    
    3 IF(JPM.EQ.JMN) DSMALL=(-1.0)**JPM                                         
      RETURN                                                                    
    4 IF(JPM.EQ.JPN) DSMALL=(-1.0)**MPN                                         
      RETURN                                                                    
    5 PRINT 100, AJ,AM,AN,BETA                                                  
  100 FORMAT(31H DSMALL ... ILLEGAL ARGUMENT(S),5X,4E15.4)                      
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DCOPY(A,B,N)                                                   
      DIMENSION A(1),B(1)                                                       
      DO 10 I=1,N                                                               
      B(I)=A(I)                                                                 
   10 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE IZERO(IA,N)                                                    
      DIMENSION IA(1)                                                           
      DO 10 I=1,N                                                               
      IA(I)=0                                                                   
   10 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DZERO(A,N)                                                     
      DIMENSION A(*) 
      DO 10 I=1,N                                                               
      A(I)=0.                                                                   
   10 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DXDUMP(DVEC,NW)                                                
      DIMENSION DVEC(1)                                                         
      CHARACTER*10 IFMT(12)                                                     
      EQUIVALENCE(IVC,DVC)                                                      
      DATA IFMT/12*'          '/                                                
      DATA IHEX/X'70000000'/                                                      
C                                                                               
C     IBM VERSION FOR DUMPING ARRAYS.                                           
C                                                                               
C     AN INTEGER(POSITIVE OR NEGATIVE) IS DISTINGUISHED FROM                    
C     A FLOATING-POINT NUMBER ONLY IF ITS ABSOLUTE VALUE IS                     
C     LESS THAN EIGHT HEXADECIMAL DIGITS (Z0FFFFFFF).                           
C                                                                               
C     -268 435 456  .GE.  ALLOWED INTEGER  .LE.  268 435 455                    
C                                                                               
      IFMT(1)='(1X,I4,1X,'                                                      
      PRINT 100,(K,K=1,10)                                                      
  100 FORMAT(/,6X,10(3X,3H---,I2,3H---,1X))                                     
      KT=0                                                                      
      DO 20 J=1,NW,10                                                           
      JTOP=MIN0(J+9,NW)                                                         
      DO 10 I=J,JTOP                                                            
      K=MOD(I-1,10)+2                                                           
      DVC=DVEC(I)                                                               
      IF(IVC.LT.0)  IVC=NOT(IVC)                                                
      IVA=IAND(IVC,IHEX)                                                        
      IF(IVA.NE.0)  THEN                                                        
      IFMT(K)='E11.4,1X,'                                                       
      ELSE                                                                      
      IFMT(K)='I11,1X,'                                                         
      END IF                                                                    
   10 CONTINUE                                                                  
      DSUM=0.                                                                   
      DO 11 I=J,JTOP                                                            
      DSUM=DSUM+DVEC(I)                                                         
   11 CONTINUE                                                                  
      IF(DSUM.NE.0.)  THEN                                                      
      IF(MOD(JTOP,10).EQ.0)  THEN                                               
      IFMT(12)='1X,I4)'                                                         
      PRINT IFMT,KT,(DVEC(I),I=J,JTOP),KT                                       
      ELSE                                                                      
      IFMT(12)='1X)'                                                            
      PRINT IFMT,KT,(DVEC(I),I=J,JTOP)                                          
      END IF                                                                    
      END IF                                                                    
      KT=KT+10                                                                  
   20 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       



      SUBROUTINE VCOPY(A,B,N)                                                   
      DIMENSION A(1),B(1)                                                       
      DO 10 I=1,N                                                               
      B(I)=A(I)                                                                 
   10 CONTINUE                                                                  
      RETURN                                                                    
      END

