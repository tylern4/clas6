*******************************************************************************
* subroutine to calculate helicity amplitudes A(3/2),A^{p}(1/2) and A^{n}(1/2)
*                         at resonance position
* the mass used is for gamma p->pi0 n or gamma n to pi0 n
* The isospin relation used is (c1=sqrt(1/3) c2=sqrt(2/3)):
* A+=-c1A(3/2)-c2A^{p}(1/2), A0=-c2A(3/2)+c1A^{p}(1/2)
* A-=-c1A(3/2)+c2A^{n}(1/2), A0=-c2A(3/2)-c1A^{n}(1/2)
*            Z. Li   May 28, 93
************************************************************************
      SUBROUTINE RAMP(EPIREA,STATE,EPQ2,A,B,C,CA,CB)
      
      IMPLICIT NONE
      include "ao.inc"
     
      INTEGER EPIREA
      CHARACTER*8 STATE
      REAL EPQ2,A,B,C,CA,CB
      REAL C1P,C1N,C1,C3
      REAL W0,PIB,GAM,RJ2,SA,SB
      REAL FACT,AFACT,F2
      
C* CALCULATES THE HELICITY AMPLITUDES AT RESONANCE POSITION

      C1P=-sqrt(2./3.)
      C1N=sqrt(2./3.)
      IF(EPIREA.EQ.1.OR.EPIREA.EQ.3) C1=C1P
      IF(EPIREA.EQ.2.OR.EPIREA.EQ.4) C1=C1N        
      c3=-sqrt(1./3.)                             
      IF(IT.EQ.1) THEN
         IF(IP11.EQ.1) CALL EXPA(EPIREA,EPQ2)
         IF(IP11.EQ.2) THEN
            CALL EXPA(EPIREA,EPQ2)
            RAP11_1440=0.0
         ENDIF
         IF(IP11.EQ.3.OR.IP11.EQ.4) THEN
            CALL EXPA(EPIREA,EPQ2)
            CALL QKMA(EPIREA,EPQ2)
         ENDIF
      ENDIF
      IF(IT.EQ.2) THEN
         CALL QKMA(EPIREA,EPQ2)
         CALL EXPA(EPIREA,EPQ2)
      ENDIF
      
      IF (IT.EQ.3) THEN
         CALL QKMA(EPIREA,EPQ2)
         CALL EXPA(EPIREA,EPQ2)
      ENDIF

      IF(IT.EQ.4) THEN
C* here I get ride of the terms which is not used in
c* the EXPT
         CALL QKMA(EPIREA,EPQ2)
      ENDIF
C* THIS FOLLOWING RESONANCES ARE PREDICTED BY QK MODEL ONLY
      IF(STATE.EQ.'P13_1870') THEN
 15      A=RAP13_1870/C1
         B=RBP13_1870/C1
         C=RCP13_1870/C1
         W0=LP13_1870
         PIB=PIBP13_1870
         GAM=WP13_1870
         RJ2=3.
         SA=-1.
      ENDIF

      IF(STATE.EQ.'P31_1925') THEN
         A=RAP31_1925/C3
         B=RBP31_1925/C3
         C=RCP31_1925/C3
         W0=LP31_1925
         PIB=PIBP31_1925
         GAM=WP31_1925
         RJ2=1.
         SA=+1.
      ENDIF
   
      IF(STATE.EQ.'P13_1980') THEN
         A=RAP13_1980/C1
         B=RBP13_1980/C1
         C=RCP13_1980/C1
         W0=LP13_1980
         PIB=PIBP13_1980
         GAM=WP13_1980
         RJ2=3.
         SA=-1.
      ENDIF
        
      IF (STATE.EQ.'F15_1955') THEN
         A=RAF15_1955/C1
         B=RBF15_1955/C1
         B=RCF15_1955/C1
         W0=LF15_1955
         PIB=PIBF15_1955
         GAM=WF15_1955
         RJ2=5.
         SA=+1.
      ENDIF
         
      IF(STATE.EQ.'P13_1955') THEN
         A=RAP13_1955/C1
         B=RBP13_1955/C1
         C=RCP13_1955/C1
         W0=LP13_1955
         PIB=PIBP13_1955
         GAM=WP13_1955
         RJ2=3.
         SA=-1.
      ENDIF

      IF(STATE.EQ.'P33_1975') THEN    
         A=RAP33_1975/C3
         B=RBP33_1975/C3
         C=RCP33_1975/C3
         W0=LP33_1975
         PIB=PIBP33_1975
         GAM=WP33_1975
         RJ2=3.
         SA=-1.
      ENDIF

C*  THE FOLLOWING RESONANCES ARE SEEN AS ONE OR TWO STAR
      IF(STATE.EQ.'P33_1600') THEN
         A=RAP33_1600/C3
         B=RBP33_1600/C3
         C=RCP33_1600/C3
         W0=LP33_1600
         PIB=PIBP33_1600
         GAM=WP33_1600
         RJ2=3.
         SA=-1.
      ENDIF

      IF(STATE.EQ.'F17_1990') THEN
         A=RAF17_1990/C1
         B=RBF17_1990/C1
         C=RCF17_1990/C1
         W0=LF17_1990
         PIB=PIBF17_1990
         GAM=WF17_1990
         RJ2=7.
         SA=-1.
      ENDIF
        
      IF(STATE.EQ.'F15_2000') THEN
         A=RAF15_2000/C1
         B=RBF15_2000/C1
         C=RCF15_2000/C1
         W0=LF15_2000
         PIB=PIBF15_2000
         GAM=WF15_2000
         RJ2=5.
         SA=1.
      ENDIF
    
      IF(STATE.EQ.'P11_2100') THEN
         A=RAP11_2100/C1
         B=RBP11_2100/C1
         C=RCP11_2100/C1
         W0=LP11_2100
         PIB=PIBP11_2100
         GAM=WP11_2100
         RJ2=1.
         SA=1.
      ENDIF

      IF(STATE.EQ.'F35_2000') THEN
         A=RAF35_2000/C3
         B=RBF35_2000/C3
         C=RCF35_2000/C3
         W0=LF35_2000
         PIB=PIBF35_2000
         GAM=WF35_2000
         RJ2=5.
         SA=1.
      ENDIF

         
C* THE FOLLWING RESONANCES ARE SEEN BY EXPERIMENT                                                                        
      IF (STATE.EQ.'S11_1535') THEN
         A=RAS11_1535/C1
         B=0.0
         C=RCS11_1535/C1
         W0=LS11_1535
         PIB=PIBS11_1535
         GAM=WS11_1535
         RJ2=1.
         SA=-1.
      ENDIF

         IF(STATE.EQ.'S11_1650') THEN
         A=RAS11_1650/C1
         B=0.0
         C=RCS11_1650/C1
         W0=LS11_1650
         PIB=PIBS11_1650
         GAM=WS11_1650
         RJ2=1.
         SA=-1.
      ENDIF

      IF(STATE.EQ.'P11_1440') THEN
         A=RAP11_1440/C1
         B=0.
         C=RCP11_1440/C1
         W0=LP11_1440
         PIB=PIBP11_1440
         GAM=WP11_1440
         RJ2=1.
         SA=1.
      ENDIF

      IF(STATE.EQ.'P13_1720') THEN
         A=RAP13_1720/C1
         B=RBP13_1720/C1
         C=RCP13_1720/C1
         W0=LP13_1720
         PIB=PIBP13_1720
         GAM=WP13_1720
         RJ2=3.
         SA=-1.
      ENDIF

      IF(STATE.EQ.'D13_1520') THEN
         A=RAD13_1520/C1
         B=RBD13_1520/C1
         C=RCD13_1520/C1
         W0=LD13_1520
         PIB=PIBD13_1520
         GAM=WD13_1520
         RJ2=3.
         SA=1.
      ENDIF

      IF(STATE.EQ.'D13_1700') THEN
         A=RAD13_1700/C1
         B=RBD13_1700/C1
         C=RCD13_1700/C1
         W0=LD13_1700
         PIB=PIBD13_1700
         GAM=WD13_1700
         RJ2=3.
         SA=1.
      ENDIF
  
      IF(STATE.EQ.'D15_1675') THEN
         A=RAD15_1675/C1
         B=RBD15_1675/C1
         C=RCD15_1675/C1
         W0=LD15_1675
         PIB=PIBD15_1675
         GAM=WD15_1675
         RJ2=5.
         SA=-1.
      ENDIF

      IF(STATE.EQ.'F15_1680') THEN
         A=RAF15_1680/C1
         B=RBF15_1680/C1
         C=RCF15_1680/C1
         W0=LF15_1680
         PIB=PIBF15_1680
         GAM=WF15_1680
         RJ2=5.
         SA=1.
      ENDIF

      IF(STATE.EQ.'G17_2190') THEN
         A=RAG17_2190/C1
         B=RBG17_2190/C1
         C=RCG17_2190/C1
         W0=LG17_2190
         PIB=PIBG17_2190
         GAM=WG17_2190
         RJ2=7.
         SA=1.
      ENDIF

      IF(STATE.EQ.'G19_2250') THEN
         A=RAG19_2250/C1
         B=RBG19_2250/C1
         C=RCG19_2250/C1
         W0=LG19_2250
         PIB=PIBG19_2250
         GAM=WG19_2250
         RJ2=9.
         SA=-1.
      ENDIF

      IF(STATE.EQ.'H19_2200') THEN
         A=RAH19_2220/C1
         B=RBH19_2220/C1
         C=RCH19_2220/C1
         W0=LH19_2220
         PIB=PIBH19_2220
         GAM=WH19_2220
         RJ2=9.
         SA=1.
      ENDIF

      IF(STATE.EQ.'I111_2600') THEN
         A=RAI111_2600/C1
         B=RBI111_2600/C1
         C=RCI111_2600/C1
         W0=LI111_2600
         PIB=PIBI111_2600
         GAM=WI111_2600
         RJ2=11.
         SA=1.
      ENDIF

      IF(STATE.EQ.'S31_1620') THEN
         A=RAS31_1620/C3
         B=0.
         C=RCS31_1620/C3
         W0=LS31_1620
         PIB=PIBS31_1620
         GAM=WS31_1620
         RJ2=1.
         SA=-1.
      ENDIF

      IF (STATE.EQ.'S31_1900') THEN
         A=RAS31_1900/C3
         B=0
         C=RCS31_1900/C3
         W0=LS31_1900
         PIB=PIBS31_1900
         GAM=WS31_1900
         RJ2=1.
         SA=-1.
      ENDIF

      IF(STATE.EQ.'P31_1910') THEN
         A=RAP31_1910/C3
         B=0.
         C=RCP31_1910/C3
         W0=LP31_1910
         PIB=PIBP31_1910
         GAM=WP31_1910
         RJ2=1.
         SA=1.
      ENDIF

      IF(STATE.EQ.'P33_1232') THEN
         A=RAP33_1232/C3
         B=RBP33_1232/C3
         C=RCP33_1232/C3
         W0=LP33_1232
         PIB=PIBP33_1232
         GAM=WP33_1232
         RJ2=3.
         SA=-1.
      ENDIF

      IF(STATE.EQ.'P33_1920') THEN
         A=RAP33_1920/C3
         B=RBP33_1920/C3
         C=RCP33_1920/C3
         W0=LP33_1920
         PIB=PIBP33_1920
         GAM=WP33_1920
         RJ2=3.
         SA=-1.
      ENDIF

      IF(STATE.EQ.'D33_1700') THEN
         A=RAD33_1700/C3
         B=RBD33_1700/C3
         C=RCD33_1700/C3
         W0=LD33_1700
         PIB=PIBD33_1700
         GAM=WD33_1700
         RJ2=3.
         SA=1.
      ENDIF

      IF(STATE.EQ.'D35_1930') THEN
         A=RAD35_1930/C3
         B=RBD35_1930/C3
         C=RCD35_1930/C3
         W0=LD35_1930
         PIB=PIBD35_1930
         GAM=WD35_1930
         RJ2=5.
         SA=-1.
      ENDIF

      IF(STATE.EQ.'F35_1905') THEN
         A=RAF35_1905/C3
         B=RBF35_1905/C3
         C=RCF35_1905/C3
         W0=LF35_1905
         PIB=PIBF35_1905
         GAM=WF35_1905
         RJ2=5.
         SA=1.
      ENDIF
   
      IF(STATE.EQ.'F37_1950') THEN
         A=RAF37_1950/C3
         B=RBF37_1950/C3
         C=RCF37_1950/C3
         W0=LF37_1950
         PIB=PIBF37_1950
         GAM=WF37_1950
         RJ2=7.
         SA=-1.
      ENDIF

      IF(STATE.EQ.'H311_2420') THEN
         A=RAH311_2420/C3
         B=RBH311_2420/C3
         C=RCH311_2420/C3
         W0=LH311_2420
         PIB=PIBH311_2420
         GAM=WH311_2420
         RJ2=11.
         SA=-1.
      ENDIF
C* CALCULATE THE COUPLINGS 

      FACT=AFACT(EPIREA,EPQ2,W0,PIB,GAM,RJ2)         
      CA=A*SA/FACT/19.73*1000.
      SB=-SA
      IF(RJ2.EQ.1.) THEN
         CB=0.
      ELSE 
         F2=SQRT(16./(RJ2-1.)/(RJ2+3.))
         CB=B*SB/FACT/F2/19.73*1000. 
      ENDIF
      RETURN 
      END                                                          
******************************************************************
* 
      FUNCTION AFACT(EPIREA,EPQ2,W0,PIB,GAM,RJ)

c RJ=2*j
      IMPLICIT NONE
      INTEGER EPIREA
      REAL AFACT
      REAL EPQ2,W0,PIB,GAM,RJ
      REAL PVEC_GAM,PVEC_PI
      REAL XMP,XMPI0,XMPIP,XMPI,PI
      DATA XMP,XMPI0,XMPIP/0.938,0.1349,0.1395     /     
      DATA PI/3.14159/

      IF(EPIREA.EQ.1.OR.EPIREA.EQ.2)XMPI=XMPI0
      IF(EPIREA.EQ.3.OR.EPIREA.EQ.4)XMPI=XMPIP

      PVEC_GAM = SQRT((EPQ2+(W0+XMP)**2)*(EPQ2+(W0-XMP)**2))/(2.*W0)
      PVEC_PI= SQRT((W0**2+XMPI**2-XMP**2)**2/(4.*W0**2)-XMPI**2)
      AFACT = SQRT(1./(RJ+1.)/PI*PVEC_GAM/PVEC_PI*XMP/W0*PIB/GAM)

      RETURN
      END
