************************************************************
*     SUBROUTINE TO CALCULATES THE QUARK MODEL  PREDICTIONS
*     OF THE COUPLINGS USING FORM FACTOR FROM FOSTER ET. AL.
*     
*     CALLED BY AO, AOT
************************************************************
      SUBROUTINE QKMC(ATEST,Q2,AP3,AP1,AN3,AN1)


      IMPLICIT NONE
      include "ao.inc"

      REAL AP12S11_1535              
      REAL AP12S11_1650              
      REAL AP12P11_1440,CP12P11_1440              
      REAL AP12P11_1710             
      REAL AP12P13_1720, AP32P13_1720
      REAL AP12D13_1520, AP32D13_1520
      REAL AP12D13_1700, AP32D13_1700
      REAL AP12D15_1675, AP32D15_1675
      REAL AP12F15_1680, AP32F15_1680 
      REAL AP12P13_1870, AP32P13_1870
      REAL AP12F15_1955, AP32F15_1955
      REAL AP12P13_1955, AP32P13_1955   
      REAL AP12P13_1980, AP32P13_1980
      REAL AP12F17_1990, AP32F17_1990
      REAL AP12F15_2000, AP32F15_2000
      REAL AP12G17_2190, AP32G17_2190
      REAL AP12G19_2250, AP32G19_2250
      REAL AP12H19_2220, AP32H19_2220
      REAL AP12P11_2100             
      REAL AP12I111_2600,AP32I111_2600
      REAL AN12S11_1535              
      REAL AN12S11_1650              
      REAL AN12P11_1440              
      REAL AN12P11_1710             
      REAL AN12P13_1720, AN32P13_1720
      REAL AN12D13_1520, AN32D13_1520
      REAL AN12D13_1700, AN32D13_1700
      REAL AN12D15_1675, AN32D15_1675
      REAL AN12F15_1680, AN32F15_1680 
      REAL AN12F15_1955, AN32F15_1955
      REAL AN12P13_1870, AN32P13_1870
      REAL AN12P13_1955, AN32P13_1955   
      REAL AN12P13_1980, AN32P13_1980
      REAL AN12F17_1990, AN32F17_1990
      REAL AN12F15_2000, AN32F15_2000
      REAL AN12G17_2190, AN32G17_2190
      REAL AN12G19_2250, AN32G19_2250
      REAL AN12H19_2220, AN32H19_2220
      REAL AN12P11_2100             
      REAL AN12I111_2600,AN32I111_2600
      REAL AP12S31_1620              
      REAL AP12S31_1900           
      REAL AP12P31_1910          
      REAL AP12P33_1232, AP32P33_1232
      REAL AP12P33_1920, AP32P33_1920
      REAL AP12P33_1600, AP32P33_1600
      REAL AP12D33_1700, AP32D33_1700
      REAL AP12D35_1930, AP32D35_1930
      REAL AP12F35_1905, AP32F35_1905
      REAL AP12P31_1925          
      REAL AP12F37_1950, AP32F37_1950
      REAL AP12P33_1975, AP32P33_1975
      REAL AP12F35_2000, AP32F35_2000
      REAL AP12H311_2420,AP32H311_2420

      REAL FFACT
      REAL Q2,AP3,AP1,AN3,AN1   
      CHARACTER*8 ATEST 
      REAL CS11P,CP11P,CP333,CP331,CM
      REAL AL,ALG
      REAL THETAS,THETAD,THETAF
      REAL WC,RKEVF,GK,K,G,WMQ,RK0
      INTEGER NL
      REAl PI
      REAL XMPROT,XMNTRON,XMET,XMN,XMPIP,XMPI0
      DATA CS11P,CP11P,CP333,CP331/1.,1.,1.,1./     
      DATA AL,XMPROT,XMNTRON,XMPIP,XMPI0,XMET/     
     *     0.41,0.9383,0.9396,0.1395,0.1349,0.5488/                         
      DATA PI/3.14159/

c     
      THETAS=32.*PI/180. 
      THETAD=10.*PI/180.
      THETAF=28.*PI/180.
C     THETAF=0.                                                      
      XMN=XMPROT                                                    
      CM=1.0 
C     *  CM=1 FOR CONSIDERING THE GROUND STATE IMPURITY EFFECT          
C     *  START OF (56,0) GROUP WITH N=0,L=0  
      if(atest.eq.'P33_1232')then
         WC=LP33_1232
         K=RKEVF(LP33_1232,Q2)        
         GK=K/G(WC,K)
         AP32P33_1232=-2.0*SQRT(3.0)/3.0*K*FFACT(WC,K,Q2) 
         AP12P33_1232=-2.0/3.0*K*FFACT(WC,K,Q2) 
         AP3=AP32P33_1232*CP333
         AP1=AP12P33_1232 *CP331
         AN3=0.
         AN1=0.                                               
         NL=33  
      endif
C     * START OF (70,1) GROUP WITH N=1,L=1    
      IF(ATEST.EQ.'D15_1675') THEN            
         WC=LD15_1675
         K=RKEVF(LD15_1675,Q2)  
         GK=K/G(WC,K)                                                      
         AP32D15_1675=-CM*0.27*(-2.0*SQRT(15.)/45.*GK**2/AL)*FFACT(WC,K,Q2)
         AP12D15_1675=0.0-CM*0.27*(-SQRT(30.)/45.*GK**2/AL)*FFACT(WC,K,Q2) 
         AN32D15_1675=-SQRT(10.)/15.*GK**2/AL*FFACT(WC,K,Q2)+CM*0.0   
         AN12D15_1675=-SQRT(5.0)/15.0*GK**2/AL*FFACT(WC,K,Q2)+CM*0.0 
         AP3=AP32D15_1675
         AP1=AP12D15_1675
         AN3=AN32D15_1675
         AN1=AN12D15_1675                      
         NL=15                                                                     
      ENDIF                                                                     
      IF(ATEST.EQ.'D13_1700') THEN                                              
         WC=LD13_1700
         K=RKEVF(LD13_1700,Q2)
         GK=K/G(WC,K)
         CM=0.                                                        
         AP32D13_1700=SIN(THETAD)*SQRT(6.)/3.*AL*FFACT(WC,K,Q2)
     *        -CM*0.27*COS(THETAD)*SQRT(10.)/15.*GK**2/AL*FFACT(WC,K,Q2)
         AP12D13_1700=SIN(THETAD)*SQRT(2.)/3.*(1.-GK**2/AL**2)*AL
     *        *FFACT(WC,K,Q2)
     *        -CM*COS(THETAD)*0.27*SQRT(30.)/135.*GK**2/AL*FFACT(WC,K,Q2)                           
         AN32D13_1700=SQRT(15.0)/15.0*GK**2/AL*FFACT(WC,K,Q2)*COS(THETAD)
     *        -SIN(THETAD)*(-SQRT(6.)/3.)*AL*FFACT(WC,K,Q2)                              
         AN12D13_1700=SQRT(5.0)/45.0*GK**2/AL*FFACT(WC,K,Q2)*COS(THETAD)
     *        -SIN(THETAD)*(-SQRT(2.)/3.)*(1.-GK**2/AL**2/3.)*AL*FFACT(WC,K,Q2) 
         AP3=AP32D13_1700
         AP1=AP12D13_1700
         AN3=-AN32D13_1700
         AN1=-AN12D13_1700                              
         NL=13                                                                     
      ENDIF                                                                     
      IF(ATEST.EQ.'S11_1650') THEN       
         WC=LS11_1650
         K=RKEVF(LS11_1650,Q2)                                                        
         GK=K/G(WC,K)
         AP12S11_1650=SIN(THETAS)*2./3.*(1.+GK**2/2./AL**2)*AL
     *        *FFACT(WC,K,Q2)            
     *        -CM*0.27*COS(THETAS)*SQRT(6.)/27.*GK**2/AL*FFACT(WC,K,Q2)
         AN12S11_1650=COS(THETAS)*1./9.*GK**2/AL*FFACT(WC,K,Q2)-
     *        SIN(THETAS)*2./3.*(1.+GK**2/6./AL**2)*AL*FFACT(WC,K,Q2)    
         AP3=0.
         AP1=AP12S11_1650
         AN3=0.
         AN1=AN12S11_1650                                      
         NL=11                                                                     
      ENDIF                                                                     
      IF(ATEST.EQ.'D13_1520') THEN                                              
         WC=LD13_1520
         K=RKEVF(LD13_1520,Q2)                   
         GK=K/G(WC,K)                                     
         AP32D13_1520=SQRT(6.0)/3.0*AL*FFACT(WC,K,Q2)*COS(THETAD)
     *        +SIN(THETAD)*0.                                     
         AP12D13_1520=SQRT(2.0)/3.0*(1.0-GK**2/AL**2)*AL*FFACT(WC,K,Q2)
     *        *COS(THETAD)-SIN(THETAD)*0.*FFACT(WC,K,Q2)                 
         AN32D13_1520=-AP32D13_1520
     *        +SIN(THETAD)*SQRT(15.)/15.*GK**2/AL*FFACT(WC,K,Q2)                                    
         AN12D13_1520=-SQRT(2.0)/3.0*(1.0-GK**2/3.0/AL**2)*AL
     *        *FFACT(WC,K,Q2)+SIN(THETAD)*SQRT(5.)/45.*GK**2/AL*FFACT(WC,K,Q2)
         AP3=AP32D13_1520
         AP1=AP12D13_1520
         AN3=AN32D13_1520
         AN1=AN12D13_1520
         NL=13                                                                     
      ENDIF                                                                     
      IF (ATEST.EQ.'S11_1535') THEN                                             
         WC=LS11_1535
         K=RKEVF(LS11_1535,Q2)                  
         GK=K/G(WC,K)                                      
         AP12S11_1535=COS(THETAS)*2./3.*(1.+GK**2/2./AL**2)*AL
     *        *FFACT(WC,K,Q2)
     *        -CM*SIN(THETAS)*0.27*SQRT(6.)/27.*GK**2/AL*FFACT(WC,K,Q2) 
     *        +CM*0.27*COS(THETAS)*SQRT(6.)/108.*GK**2*K/AL**2*FFACT(WC,K,Q2)                
         AN12S11_1535=-COS(THETAS)*2./3.*(1.+GK**2/6./AL**2)*AL
     *        *FFACT(WC,K,Q2)
     *        +SIN(THETAS)*1./9.*GK**2/AL*FFACT(WC,K,Q2)
     *        +CM*0.27*COS(THETAS)*SQRT(6.)/18.*GK**2*K/AL**2*FFACT(WC,K,Q2)      
         AP3=0.
         AP1=AP12S11_1535*CS11P
         AN3=0.
         AN1=AN12S11_1535                         
         NL=11                                                                     
      ENDIF                                                                     
      IF (ATEST.EQ.'D33_1700') THEN                                             
         WC=LD33_1700
         K=RKEVF(LD33_1700,Q2)                                                        
         GK=K/G(WC,K)
         AP32D33_1700=-SQRT(6.0)/3.*AL*FFACT(WC,K,Q2)                                     
         AP12D33_1700=-SQRT(2.)/3.*(1.+GK**2/3./AL**2)*AL*FFACT(WC,K,Q2) 
     *        -CM*0.27*2.*SQRT(3.)/27.*GK**2/AL*FFACT(WC,K,Q2)
         AP3=-AP32D33_1700
         AP1=-AP12D33_1700
         AN3=0.
         AN1=0.                  
         NL=33         
      ENDIF                                                            
      IF(ATEST.EQ.'S31_1620') THEN
         WC=LS31_1620
         K=RKEVF(LS31_1620,Q2)
         GK=K/G(WC,K)
         AP12S31_1620=-2./3.*(1.-GK**2/6./AL**2)*AL*FFACT(WC,K,Q2)
     *        +CM*0.27*2.*SQRT(6.)/54.*GK**2/AL*FFACT(WC,K,Q2)
         AP3=0.
         AP1=-AP12S31_1620
         AN3=0.
         AN1=0.
         NL=31
      ENDIF
C     * START OF (56,2) GROUP WITH N=2,L=0    
                                                         
      IF (ATEST .EQ.'P11_1440') THEN                                           
         WC=LP11_1440
         K=RKEVF(LP11_1440,Q2) 
         GK=K/G(WC,K)
         RK0=K**2-Q2
         IF (NF.EQ.1) THEN                                                       
            AP12P11_1440=-SQRT(6.)/18.*GK**2*K/AL**2*FFACT(WC,K,Q2)
            AN12P11_1440=SQRT(6.)/27.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         ENDIF
         IF(NF.EQ.2) THEN
            AL=0.3
            WMQ=0.33
            AP12P11_1440=-0.90*SQRT(6.)/18.*(GK**2/AL**2
     *           -2./3.*RK0/WMQ)*FFACT(WC,K,Q2)
     *           -CM*0.34*SQRT(2.)*(1.-1./9.*GK**2/AL**2
     *           +1./108.*GK**4/AL**4)*FFACT(WC,K,Q2)
     *           -CM*0.27*1./9.*(GK**2/AL**2-1./36.*GK**2/AL**2*RK0/WMQ)
     *           *FFACT(WC,K,Q2)
     *           +CM*0.16*SQRT(3.)/18.*(GK**2/AL**2-RK0/WMQ)*FFACT(WC,K,Q2)
            AP12P11_1440=K*AP12P11_1440
            AN12P11_1440=-0.90*SQRT(6.)/27.*(GK**2/AL**2
     *           -1./2.*RK0/WMQ)*FFACT(WC,K,Q2)
     *           +CM*0.34*2.*SQRT(2.)/3.*(1.-1./9.*GK**2/AL**2
     *           +1./108.*GK**4/AL**4)*FFACT(WC,K,Q2)
     *           +CM*0.27*2./27.*(GK**2/AL**2-RK0/WMQ)*FFACT(WC,K,Q2)
            AN12P11_1440=K*AN12P11_1440
            CP12P11_1440=-((1.-CM*0.1)/SQRT(3.)/6.*GK**2/AL**2
     *           +CM*0.34*(1.-1./9.*GK**2/AL**2+1./108.*(GK/AL)**4)
     *           +CM*0.27*SQRT(2.)/18.*GK**2/AL**2
     *           -CM*0.16*1./6./SQRT(6.)*GK**2/AL**2)
     *           *2.*WMQ*FFACT(WC,K,Q2)
         ENDIF
         IF(NF.EQ.3) THEN
            AP12P11_1440=-4./3.*K*0.35*FFACT(WC,K,Q2)
            AN12P11_1440=8./9.*K*0.35*FFACT(WC,K,Q2)
            ALG=SQRT(0.3*0.6)
            CP12P11_1440=-0.35*6.*0.33*SQRT(2.)
     *           *(1.-EXP(-GK**2/36./ALG**2))*FFACT(WC,K,Q2)
         ENDIF
         AP3=CP12P11_1440
         AP1=AP12P11_1440*CP11P
         AN3=0.
         AN1=AN12P11_1440                              
         NL=11    
      ENDIF            
      
      IF (ATEST .EQ. 'P33_1600') THEN                                           
         WC=LP33_1600
         K=RKEVF(LP33_1600,Q2)                                                        
         GK=K/G(WC,K)
         AP32P33_1600=-1./9.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        +CM*0.27*SQRT(6.)/27.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        +0.18*SQRT(10.)/45.*GK**2*K/AL**2*FFACT(WC,K,Q2)                                
         AP12P33_1600=-SQRT(3.)/27.*GK**2*K/AL**2*FFACT(WC,K,Q2) 
     *        -CM*0.27*SQRT(2.)/27.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        -0.18*SQRT(30.)/135.*GK**2*K/AL**2*FFACT(WC,K,Q2)
c     *  use the hybrid form for P33(1600)  1/15/92
C     *       NF=3
C     *       AP12P33_1600=-2./9.*K*0.35*FFACT(WC,K,Q2)
C     *      AP32P33_1600=SQRT(3.)*AP12P33_1600
         AP3=AP32P33_1600
         AP1=AP12P33_1600
         AN3=0.
         AN1=0.                             
         NL=11                                                                     
      ENDIF                                                                     
C     * START OF (70,2) WITH N=2,L=0                                                 
      IF(ATEST .EQ. 'P13_1870') THEN                                            
C**   NOT SEEN                                                                    
         K=RKEVF(LP13_1870,Q2)                                                        
         WC=LP13_1870
         GK=K/G(WC,K)
         AN32P13_1870=1./18.*GK**2*K/AL**2*FFACT(WC,K,Q2)                                    
         AN12P13_1870=SQRT(3.)/54.*GK**2*K/AL**2*FFACT(WC,K,Q2)   
         AP3=0.
         AP1=0.
         AN3=AN32P13_1870
         AN1=AN12P13_1870                           
         NL=13                                                                     
      ENDIF                                                                     
      IF(ATEST .EQ. 'P11_1710') THEN                                            
         WC=LP11_1710
         K=RKEVF(LP11_1710,Q2)      
         GK=K/G(WC,K)                                                  
         AP12P11_1710=-0.94*SQRT(3.)/18.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        +0.15*(-SQRT(6.)/18.*GK**2*K/AL**2*FFACT(WC,K,Q2))
     *        +CM*0.27*SQRT(2.)/3.*K*(1.-1./9.*(GK/AL)**2)*AL*FFACT(WC,K,Q2)                          
         AN12P11_1710=0.94*SQRT(3.)/54.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        +0.15*SQRT(6.)/27.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        +CM*0.27*SQRT(2.)/6.*K*(1.-1./9.*(GK/AL)**2)*FFACT(WC,K,Q2)    
         AP3=0.
         AP1=AP12P11_1710
         AN3=0.
         AN1=AN12P11_1710                          
         NL=11                                                                     
      ENDIF                                                                     
      IF(ATEST .EQ. 'P31_1925') THEN                                            
         WC=LP31_1925
C**   NOT SEEN                                                                    
         K=RKEVF(LP31_1925,Q2)                  
         GK=K/G(WC,K)                                      
         AP12P31_1925=0.77*SQRT(3.)/54.*GK**2*K/AL**2*FFACT(WC,K,Q2)  
     *        -0.64*SQRT(30.)/135.*GK**2*K/AL**2*FFACT(WC,K,Q2)             
         AP3=0.
         AP1=AP12P31_1925
         AN3=0.
         AN1=0.                            
         NL=31                                                                     
      ENDIF                                                                     
C     * START OF (56,2) GROUP WITH N=2,L=2                                           
      IF(ATEST .EQ. 'F15_1680') THEN                                            
         WC=LF15_1680
         K=RKEVF(LF15_1680,Q2)                                                        
         GK=K/G(WC,K)
         AP32F15_1680=COS(THETAF)*(2.*SQRT(10.)/15.*K*FFACT(WC,K,Q2)
     *        +CM*0.34*SQRT(30.)/135.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        -CM*0.27*SQRT(15.)/135.*GK**2*K/AL**2*FFACT(WC,K,Q2))
     *        +SIN(THETAF)*2.*SQRT(5.)/15.*K*FFACT(WC,K,Q2)                                   
         AP12F15_1680=
     *        COS(THETAF)*(2.*SQRT(5.)/15.*(1.-GK**2/2./AL**2)*K*FFACT(WC,K,Q2)
     *        +CM*0.34*SQRT(15.)/45.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        -CM*0.27*SQRT(30.)/270.*GK**2*K/AL**2*FFACT(WC,K,Q2))
     *        +SIN(THETAF)*SQRT(10.)/15.*(1.-GK**2/2./AL**2)*K*FFACT(WC,K,Q2)
         AN32F15_1680=-SIN(THETAF)*2.*SQRT(5.)/15.*K*FFACT(WC,K,Q2)                 
         AN12F15_1680=2.*SQRT(5.)/45.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        *COS(THETAF)
     *        -SIN(THETAF)*SQRT(10.)/15.*(1.-GK**2/3./AL**2)*K*FFACT(WC,K,Q2)        
         AP3=AP32F15_1680
         AP1=AP12F15_1680
         AN3=AN32F15_1680 
         AN1=AN12F15_1680                       
         NL=15                                                                     
      ENDIF                                                                     
      IF(ATEST .EQ. 'P13_1720') THEN                                            
         WC=LP13_1720
         K=RKEVF(LP13_1720,Q2)               
         GK=K/G(WC,K)                                         
         AP32P13_1720=-SQRT(10.)/15.*K*FFACT(WC,K,Q2)                                     
         AP12P13_1720=SQRT(30.)/15.*(1.+GK**2/3./AL**2)*K*FFACT(WC,K,Q2)    
         AN32P13_1720=0.               
         AN12P13_1720=-2.*SQRT(30.)/135.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         AP3=AP32P13_1720
         AP1=AP12P13_1720
         AN3=AN32P13_1720
         AN1=AN12P13_1720                        
         NL=13                                                                     
      ENDIF                                                                     
      IF(ATEST .EQ. 'F37_1950') THEN                                            
         WC=LF37_1950
         K=RKEVF(LF37_1950,Q2)                  
         GK=K/G(WC,K)                                      
         AP32F37_1950=-2.*SQRT(7.)/63.*GK**2*K/AL**2*FFACT(WC,K,Q2)                            
     *        -CM*0.27*2.*SQRT(42.)/89.*GK**2*K/AL**2*FFACT(WC,K,Q2)  
         AP12F37_1950=-2.*SQRT(105.)/315.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        -CM*0.27*2.*SQRT(70.)/315.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         AP3=AP32F37_1950
         AP1=AP12F37_1950
         AN3=0.
         AN1=0.                      
         NL=37                                                                     
      ENDIF                                                                     
      IF (ATEST.EQ.'F35_1905') THEN                                             
         WC=LF35_1905
         K=RKEVF(LF35_1905,Q2)                                                        
         GK=K/G(WC,K)
         AP32F35_1905=(-2.*SQRT(35.)/105.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        +CM*0.27*2.*SQRT(210.)/315.*GK**2*K/AL**2*FFACT(WC,K,Q2))*0.94 
     *        +0.38*2.*SQRT(5.)/15.*K*FFACT(WC,K,Q2)    
         AP12F35_1905=-SQRT(70.)/315.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        +CM*0.27*2.*SQRT(105.)/945.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        +0.38*SQRT(10.)/15.*(1.+GK**2/6./AL**2)*K*FFACT(WC,K,Q2)
         AP3=AP32F35_1905
         AP1=AP12F35_1905
         AN3=0.
         AN1=0.                           
         NL=35                                                                     
      ENDIF                                                                     
      IF(ATEST.EQ.'P33_1920') THEN                                              
         WC=LP33_1920
         K=RKEVF(LP33_1920,Q2)      
         GK=K/G(WC,K)                                                  
         AP32P33_1920=SQRT(10.)/45.*GK**2*K/AL**2*FFACT(WC,K,Q2)                             
     *        -CM*0.27*2.*SQRT(15.)/135.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         AP12P33_1920=-SQRT(30.)/135.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        +CM*0.27*2.*SQRT(5.)/135.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         AP3=AP32P33_1920
         AP1=AP12P33_1920
         AN3=0.
         AN1=0.                           
         NL=33                                                                     
      ENDIF                                                                     
      IF(ATEST.EQ.'P31_1910') THEN                                              
         WC=LP31_1910
         K=RKEVF(LP31_1910,Q2)               
         GK=K/G(WC,K)                                         
         AP12P31_1910=-0.64*SQRT(30.)/135.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        +0.77*SQRT(3.)/54.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         AP3=0.
         AP1=AP12P31_1910
         AN3=0.
         AN1=0.                                    
         NL=31                                                                     
      ENDIF                                                                     
C     * START OF (70,2) GROUP WITH N=2,L=2                                           
      IF(ATEST.EQ.'F17_1990') THEN                                              
         WC=LF17_1990
         K=RKEVF(LF17_1990,Q2)                  
         GK=K/G(WC,K)                                      
         AP32F17_1990=-CM*0.27*2.*SQRT(42.)/189.*GK**2*K/AL**2
     *        *FFACT(WC,K,Q2)                                                       
         AP12F17_1990=-CM*0.27*2.*SQRT(70.)/315.*GK**2*K/AL**2
     *        *FFACT(WC,K,Q2)       
         AN32F17_1990=-SQRT(7.)/63.*GK**2*K/AL**2*FFACT(WC,K,Q2)                              
         AN12F17_1990=-SQRT(105.)/315.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         AP3=AP32F17_1990
         AP1=AP12F17_1990
         AN3=AN32F17_1990
         AN1=AN12F17_1990                           
         NL=17                                                                     
      ENDIF                                                                     
      IF (ATEST.EQ.'F15_2000') THEN                                             
         WC=LF15_2000
         K=RKEVF(LF15_2000,Q2)               
         GK=K/G(WC,K)                                         
         AP32F15_2000=CM*0.0                                                       
         AP12F15_2000=CM*0.0
         AN32F15_2000=-SQRT(35.)/105.*GK**2*K/AL**2*FFACT(WC,K,Q2)                           
         AN12F15_2000=-SQRT(70.)/630.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         AP3=AP32F15_2000
         AP1=AP12F15_2000
         AN3=AN32F15_2000
         AN1=AN12F15_2000                         
         NL=15                                                                     
      ENDIF            
      IF(ATEST.EQ.'P13_1980') THEN
         WC=LP13_1980
         K=RKEVF(LP13_1980,Q2)
         GK=K/G(WC,K)
         AP32P13_1980=CM*0.
         AP12P13_1980=CM*0.
         AN32P13_1980=SQRT(10.)/90.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         AN12P13_1980=-SQRT(30.)/270.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         AP3=AP32P13_1980
         AP1=AP12P13_1980                  
         AN3=AN32P13_1980
         AN1=AN12P13_1980
         NL=13
      ENDIF
      IF(ATEST.EQ.'F15_1955') THEN
         WC=LF15_1955
         K=RKEVF(LF15_1955,Q2)
         GK=K/G(WC,K)
         AP32F15_1955=-2.*SQRT(5.)/15.*K*FFACT(WC,K,Q2)             
         AP12F15_1955=-SQRT(10.)/15.*(1.-GK**2/2./AL**2)*K*
     *        FFACT(WC,K,Q2)
         AN32F15_1955=2.*SQRT(5.)/15.*K*FFACT(WC,K,Q2)
         AN12F15_1955=SQRT(10.)/15.*(1-GK**2/3./AL**2)*K*
     *        FFACT(WC,K,Q2)
         AP3=AP32F15_1955
         AP1=AP12F15_1955
         AN3=AN32F15_1955
         AN1=AN12F15_1955
         NL=15
      END IF
      IF(ATEST.EQ.'P13_1955') THEN
         WC=LP13_1955
         K=RKEVF(LP13_1955,Q2)
         GK=K/G(WC,K)
         AP32P13_1955=SQRT(5.)/15.*K*FFACT(WC,K,Q2)
         AP12P13_1955=-SQRT(15.)/15.*(1.+GK**2/3./AL**2)*K*
     *        FFACT(WC,K,Q2)
         AN32P13_1955=-SQRT(5.)/15.*K*FFACT(WC,K,Q2)
         AN12P13_1955=SQRT(15.)/15.*(1.+GK**2/9./AL**2)*K*
     *        FFACT(WC,K,Q2)
         AP3=AP32P13_1955
         AP1=AP12P13_1955
         AN3=AN32P13_1955
         AN1=AN12P13_1955
         NL=13
      END IF
      IF(ATEST.EQ.'P33_1975') THEN
         WC=LP33_1975
         K=RKEVF(LP33_1975,Q2)
         GK=K/G(WC,K)
         AP32P33_1975=-SQRT(5.)/15.*K*FFACT(WC,K,Q2)
         AP12P33_1975=SQRT(15.)/15.*(1-GK**2/AL**2/9.)*K*
     *        FFACT(WC,K,Q2)
         AP3=AP32P33_1975
         AP1=AP12P33_1975
         AN3=0.
         AN1=0.
      END IF
      IF(ATEST.EQ.'F35_2000') THEN
         WC=LF35_2000
         K=RKEVF(LF35_2000,Q2)
         GK=K/G(WC,K)
         AP32F35_2000=2.*SQRT(5.)/15.*K*FFACT(WC,K,Q2)
         AP12F35_2000=SQRT(10.)/15.*(1.+GK**2/6./AL**2)*K*
     *        FFACT(WC,K,Q2)    
         AP3=AP32F35_2000
         AP1=AP12F35_2000
         AN3=0.
         AN1=0.
         NL=35
      END IF
      IF ( ATEST.EQ.'P11_2100') THEN
         WC=LP11_2100
         K=RKEVF(LP11_2100,Q2)
         GK=K/G(WC,K)
         AP12P11_2100=-0.02*SQRT(6.)/18.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        -0.08*SQRT(3.)/18.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         AN12P11_2100=0.02*SQRT(6.)/27.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        +0.08*SQRT(3.)/54.*GK**2*K/AL**2*FFACT(WC,K,Q2)
     *        +0.89*SQRT(30.)/270.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         AP3=0.
         AP1=AP12P11_2100
         AN3=0.
         AN1=AN12P11_2100
      ENDIF
      IF (ATEST.EQ.'F35_2000') THEN
         WC=LF35_2000
         K=RKEVF(LF35_2000,Q2)
         GK=K/G(WC,K)
         AP32F35_2000=0.94*(2.*SQRT(5.)/15.*K*FFACT(WC,K,Q2)
     *        +CM*0.27*SQRT(30.)/135.*GK**2*K/AL**2*FFACT(WC,K,Q2))
     *        -0.38*2.*SQRT(35.)/105.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         AP12F35_2000=0.94*(SQRT(10.)/15.*(1.+GK**2/AL**2/6.)*K
     *        *FFACT(WC,K,Q2)
     *        -CM*0.27*SQRT(7.)/18./15.*GK**2*K/AL**2*FFACT(WC,K,Q2))
     *        -0.38*SQRT(70.)/315.*GK**2*K/AL**2*FFACT(WC,K,Q2)
         AP3=AP32F35_2000
         AP1=AP12F35_2000
         AN3=0.0
         AN1=0.0
      ENDIF
      RETURN                                                                     
      END                                                                       
C************************************************************                   
      REAL FUNCTION RKEVF(WC,Q2)                                                   
      DATA XMPROT,XMNTRON,XMPIP,XMPI0,XMET/                                     
     *  0.9383,0.9369,0.1395,0.1349,0.5488/                                     
      WN=XMPROT      
      NR=1                                                          
      IF(NR.EQ.1.OR.NR.EQ.3)  WPI=XMPIP                                         
      IF(NR.EQ.2.OR.NR.EQ.4)  WPI=XMPI0                                         
C** NR IS THE REACRTION TYPE NR=1,2,3,4 FOR PI+N, PI0P,PI-P,PI0N   
      IF (Q2.NE.0.0) THEN             
      RK2=((WC**2-WN**2)**2+Q2*(WC+WN)**2)/4./WC/WN                           
      RKEVF=SQRT(RK2)   
      ELSE
      RKEVF=(WC**2-WN**2)/2./WC 
      ENDIF                                                       
      RETURN                                                                    
      END                                                                       
C***************************************************************                
      REAL FUNCTION G(WC,K)
      REAL K
      DATA WN/0.9383/
      QT=K-(WC**2-WN**2)/2./WC
      IF(QT.LT.1.E-06) THEN
      G=1.
      ELSE
      G=SQRT(1.+K**2/(WC+WN)**2)
      ENDIF
      RETURN
      END
C* *************************************************************
      REAL FUNCTION FFACT(WC,K,Q2)                                                     
      REAL K,K0                                                                   
      COMMON /IP/IT,IB,NF,IBORN,CUT,IP11
      DATA AL,UP,WN/0.41,0.13,0.938/
      PI=4.0*ATAN(1.)   
      K0=SQRT(K**2-Q2)     
      GA=SQRT(1.+K**2/(WC+WN)**2)
      Q0K=(WC**2-WN**2)/SQRT(4.*WC*WN)
      GK=K/GA
      F2=SQRT(2.*PI/K0)*UP
      FF1=EXP(-1./6.*K**2/AL**2)
      IF(NF.EQ.3) THEN
      AL=0.25
      IF(Q2.NE.0.)FFACT=F2*1/GA**2*EXP(-1./6.*K**2/AL**2/GA**2) 
      IF(Q2.EQ.0.) THEN
      K=(WC**2-WN**2)/2./WC
      FFACT=F2*EXP(-1./6.*K**2/AL**2)
      END IF
      GO TO 99
      ENDIF

       IF(NF.EQ.2.OR.NF.EQ.1) THEN
        IF(Q2.EQ.0.) FFACT=F2*EXP(-1./6.*K**2/AL**2)
        IF(Q2.NE.0.) FFACT=F2/GA**2*EXP(-1./6.*GK**2/AL**2)
      GO TO 99
      ENDIF  

99    RETURN                                                                    
      END                                                                       
                                                                                
