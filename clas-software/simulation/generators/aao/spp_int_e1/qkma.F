***************************************************************************
      SUBROUTINE QKMA(EPIREA,Q2)


      IMPLICIT NONE
      include "ao.inc"

      INTEGER EPIREA,IR
      REAL Q2
      REAL AP3,AP1,AN3,AN1,F1,F2
      REAL W0,RX,GT,RJ
      INTEGER ISO


C       Now calculate the resonant helicity amplitudes 
C
C* NF IS THE FORM NUMBER DETERMINS THE MODEL
C* IP11 IS A NUMBER TO DETERMIN HOW TO TREAT P11-WAVE
C* IP11= 1 FOR NORMAL EXMPT, 2 FOR EXCLUDE P11, 3 FOR USING LI&CLOSE MODEL
C*       4 FOR HYBRID MODEL         
      IR=EPIREA
      IF(IT.EQ.1) THEN
         IF(IP11.EQ.3) NF=1
         IF(IP11.EQ.4) NF=3
         CALL QKMC('P11_1440',Q2,AP3,AP1,AN3,AN1)
         W0  = LP11_1440
         RX  = PIBP11_1440
         GT  = WP11_1440
         RJ  = 1.
         ISO =1
         CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
         IF(IR.EQ.1.OR.IR.EQ.3) THEN
            RAP11_1440=F1*AP1
            RCP11_1440=0.0
         ELSE
            RAP11_1440=F1*AN1
            RCP11_1440=0.0
         END IF
         GO TO 99
      ENDIF

      IF (IT.EQ.2) GO TO 20
      IF (IT.EQ.3) GO TO 30
         
      CALL QKMC('S11_1535', Q2,AP3,AP1,AN3,AN1)
      W0  = LS11_1535
      RX  = PIBS11_1535
      GT  = WS11_1535
      RJ  = 1.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAS11_1535 =-F1*AP1
         RCS11_1535 = 0.0
      ELSE
         RAS11_1535 =-F1*AN1
         RCS11_1535 = 0.0
      ENDIF
      
      CALL QKMC('S11_1650',Q2,AP3,AP1,AN3,AN1)
      W0  = LS11_1650
      RX  = PIBS11_1650
      GT  = WS11_1650
      RJ  = 1.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAS11_1650 =-F1*AP1
         RCS11_1650 = 0.0
      ELSE
         RAS11_1650 =-F1*AN1
         RCS11_1650 = 0.0
      ENDIF
         
      CALL QKMC('P11_1440',Q2,AP3,AP1,AN3,AN1)
      W0  = LP11_1440
      RX  = PIBP11_1440
      GT  = WP11_1440
      RJ  = 1.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAP11_1440 = F1*AP1
         RCP11_1440 = 0.0
      ELSE
         RAP11_1440 = F1*AN1
         RCP11_1440 = 0.0
      ENDIF
      
      CALL QKMC('P11_1710',Q2,AP3,AP1,AN3,AN1)
      W0  = LP11_1710
      RX  = PIBP11_1710
      GT  = WP11_1710
      RJ  = 1.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAP11_1710 = F1*AP1
         RCP11_1710 = 0.0
      ELSE
         RAP11_1710 = F1*AN1
         RCP11_1710 = 0.0
      ENDIF
      
      CALL QKMC('P13_1720',Q2,AP3,AP1,AN3,AN1)
      W0  = LP13_1720
      RX  = PIBP13_1720
      GT  = WP13_1720
      RJ  = 3.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAP13_1720 =-F1*AP1
         RBP13_1720 = F2*AP3
         RCP13_1720 = 0.0
      ELSE
         RAP13_1720 =-F1*AN1
         RBP13_1720 = F2*AN3
         RCP13_1720 = 0.0
      ENDIF
      
      CALL QKMC('D13_1520',Q2,AP3,AP1,AN3,AN1)
      W0  = LD13_1520
      RX  = PIBD13_1520
      GT  = WD13_1520
      RJ  = 3.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAD13_1520 = F1*AP1
         RBD13_1520 =-F2*AP3
         RCD13_1520 = 0.0
      ELSE
         RAD13_1520 = F1*AN1
         RBD13_1520 =-F2*AN3
         RCD13_1520 = 0.0
      ENDIF 
      
      CALL QKMC('D13_1700',Q2,AP3,AP1,AN3,AN1)
      W0  = LD13_1700
      RX  = PIBD13_1700
      GT  = WD13_1700
      RJ  = 3.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAD13_1700 = F1*AP1
         RBD13_1700 =-F2*AP3
         RCD13_1700 = 0.0
      ELSE
         RAD13_1700 = F1*AN1
         RBD13_1700 =-F2*AN3
         RCD13_1700 = 0.0
      ENDIF

      CALL QKMC('D15_1675',Q2,AP3,AP1,AN3,AN1)
      W0  = LD15_1675
      RX  = PIBD15_1675
      GT  = WD15_1675
      RJ  = 5.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAD15_1675 =-F1*AP1
         RBD15_1675 = F2*AP3
         RCD15_1675 = 0.0
      ELSE
         RAD15_1675 =-F1*AN1
         RBD15_1675 = F2*AN3
         RCD15_1675 = 0.0
      ENDIF
      
      CALL QKMC('F15_1680',Q2,AP3,AP1,AN3,AN1)
      W0  = LF15_1680
      RX  = PIBF15_1680
      GT  = WF15_1680
      RJ  = 5.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAF15_1680 = F1*AP1
         RBF15_1680 =-F2*AP3
         RCF15_1680 = 0.0
      ELSE
         RAF15_1680 = F1*AN1
         RBF15_1680 =-F2*AN3
         RCF15_1680 = 0.0
      ENDIF

      CALL QKMC('S31_1620',Q2,AP3,AP1,AN3,AN1)
      W0  = LS31_1620
      RX  = PIBS31_1620
      GT  = WS31_1620
      RJ  = 1.
      ISO = 3
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAS31_1620 =-F1*AP1
         RCS31_1620 = 0.0
      ELSE
         RAS31_1620 =-F1*AN1
         RCS31_1620 = 0.0
      ENDIF
      
      CALL QKMC('P33_1232',Q2,AP3,AP1,AN3,AN1)
      W0  = LP33_1232
      RX  = PIBP33_1232
      GT  = WP33_1232
      RJ  = 3.
      ISO = 3
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAP33_1232 =-F1*AP1
         RBP33_1232 = F2*AP3
         RCP33_1232 = 0.0
      ELSE
         RAP33_1232 =-F1*AN1
         RBP33_1232 = F2*AN3
         RCP33_1232 = 0.0
      ENDIF
      
      CALL QKMC('P33_1920',Q2,AP3,AP1,AN3,AN1)
      W0  = LP33_1920
      RX  = PIBP33_1920
      GT  = WP33_1920
      RJ  = 3.
      ISO = 3
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAP33_1920 =-F1*AP1
         RBP33_1920 = F2*AP3
         RCP33_1920 = 0.0
      ELSE
         RAP33_1920 =-F1*AN1
         RBP33_1920 = F2*AN3
         RCP33_1920 = 0.0
      ENDIF

      CALL QKMC('D33_1700',Q2,AP3,AP1,AN3,AN1)
      W0  = LD33_1700
      RX  = PIBD33_1700
      GT  = WD33_1700
      RJ  = 1.
      ISO = 3
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAD33_1700 = F1*AP1
         RBD33_1700 =-F2*AP3
         RCD33_1700 = 0.0
      ELSE
         RAD33_1700 = F1*AN1
         RBD33_1700 =-F2*AN3
         RCD33_1700 = 0.0
      ENDIF
      
      CALL QKMC('D35_1930',Q2,AP3,AP1,AN3,AN1)
      W0  = LD35_1930
      RX  = PIBD35_1930
      GT  = WD35_1930
      RJ  = 5.
      ISO = 3
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAD35_1930 =-F1*AP1
         RBD35_1930 = F2*AP3
         RCD35_1930 = 0.0
      ELSE
         RAD35_1930 =-F1*AN1
         RBD35_1930 = F2*AN3
         RCD35_1930 = 0.0
      ENDIF
      
      CALL QKMC('F35_1905',Q2,AP3,AP1,AN3,AN1)
      W0  = LF35_1905
      RX  = PIBF35_1905
      GT  = WF35_1905
      RJ  = 5.
      ISO = 3
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAF35_1905 = F1*AP1
         RBF35_1905 =-F2*AP3
         RCF35_1905 = 0.0
      ELSE
         RAF35_1905 = F1*AN1
         RBF35_1905 =-F2*AN3
         RCF35_1905 = 0.0
      ENDIF
      
      CALL QKMC('F37_1950',Q2,AP3,AP1,AN3,AN1)
      W0  = LF37_1950
      RX  = PIBF37_1950
      GT  = WF37_1950
      RJ  = 7.
      ISO = 3
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAF37_1950 =-F1*AP1
         RBF37_1950 = F2*AP3
         RCF37_1950 = 0.0
      ELSE
         RAF37_1950 =-F1*AN1
         RBF37_1950 = F2*AN3
         RCF37_1950 = 0.0
      ENDIF
      
C * THE FOLLOWING BLOCK CONTAINS THE RES. ONLY PREDICTED BY QKM
 30   CALL QKMC('P13_1870',Q2,AP3,AP1,AN3,AN1)
      W0  = LP13_1870
      RX  = PIBP13_1870
      GT  = WP13_1870
      RJ  = 3.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAP13_1870 =-F1*AP1
         RBP13_1870 = F2*AP3
         RCP13_1870 = 0.0
      ELSE
         RAP13_1870 =-F1*AN1
         RBP13_1870 = F2*AN3
         RCP13_1870 = 0.0
      ENDIF
      
      CALL QKMC('P13_1980',Q2,AP3,AP1,AN3,AN1)
      W0  = LP13_1980
      RX  = PIBP13_1980
      GT  = WP13_1980 
      RJ  = 3.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAP13_1980 =-F1*AP1
         RBP13_1980 = F2*AP3
         RCP13_1980 = 0.0
      ELSE
         RAP13_1980 =-F1*AN1
         RBP13_1980 = F2*AN3
         RCP13_1980 = 0.0
      ENDIF 
      
      CALL QKMC('F15_1955',Q2,AP3,AP1,AN3,AN1)
      W0  = LF15_1955
      RX  = PIBF15_1955
      GT  = WF15_1955
      RJ  = 5.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAF15_1955 = F1*AP1
         RBF15_1955 =-F2*AP3
         RCF15_1955 = 0.0
      ELSE
         RAF15_1955 = F1*AN1
         RBF15_1955 =-F2*AN3
         RCF15_1955 = 0.0
      ENDIF
      
      CALL QKMC('P13_1955',Q2,AP3,AP1,AN3,AN1)
      W0  = LP13_1955
      RX  = PIBP13_1955
      GT  = WP13_1955
      RJ  = 3.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAP13_1955 =-F1*AP1
         RBP13_1955 = F2*AP3
         RCP13_1955 = 0.0
      ELSE
         RAP13_1955 =-F1*AN1
         RBP13_1955 = F2*AN3
         RCP13_1955 = 0.0
      ENDIF 
      
      CALL QKMC('P31_1925',Q2,AP3,AP1,AN3,AN1)
      W0  = LP31_1925
      RX  = PIBP31_1925
      GT  = WP31_1925
      RJ  = 1.
      ISO = 3
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAP31_1925 = F1*AP1
         RCP31_1925 = 0.0
      ELSE
         RAP31_1925 = F1*AN1
         RCP31_1925 = 0.0
      ENDIF

      CALL QKMC('P33_1975',Q2,AP3,AP1,AN3,AN1)
      W0  = LP33_1975
      RX  = PIBP33_1975
      GT  = WP33_1975
      RJ  = 3.
      ISO = 3
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAP33_1975 =-F1*AP1
         RBP33_1975 = F2*AP3
         RCP33_1975 = 0.0
      ELSE
         RAP33_1975 =-F1*AN1
         RBP33_1975 = F2*AN3
         RCP33_1975 = 0.0
      ENDIF
C* THE FOLOWING BLOCK CONTAINS THE TWO STAR RES. NOT IN THE EXPT

 20   CALL QKMC('F17_1990',Q2,AP3,AP1,AN3,AN1)
      W0  = LF17_1990
      RX  = PIBF17_1990
      GT  = WF17_1990
      RJ  = 7.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAF17_1990 =-F1*AP1
         RBF17_1990 = F2*AP3
         RCF17_1990 = 0.0
      ELSE
         RAF17_1990 =-F1*AN1
         RBF17_1990 = F2*AN3
         RCF17_1990 = 0.0
      ENDIF
      
      CALL QKMC('F15_2000',Q2,AP3,AP1,AN3,AN1)
      W0  = LF15_2000
      RX  = PIBF15_2000
      GT  = WF15_2000
      RJ  = 5.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAF15_2000 = F1*AP1
         RBF15_2000 =-F2*AP3
         RCF15_2000 = 0.0
      ELSE
         RAF15_2000 = F1*AN1
         RBF15_2000 =-F2*AN3
         RCF15_2000 = 0.0
      ENDIF
      
      CALL QKMC('P11_2100',Q2,AP3,AP1,AN3,AN1)
      W0  = LP11_2100
      RX  = PIBP11_2100
      GT  = WP11_2100
      RJ  = 1.
      ISO = 1
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAP11_2100 = F1*AP1
         RCP11_2100 = 0.0
      ELSE
         RAP11_2100 = F1*AN1
         RCP11_2100 = 0.0
      ENDIF
      
      CALL QKMC('P33_1600',Q2,AP3,AP1,AN3,AN1)
      W0  = LP33_1600
      RX  = PIBP33_1600
      GT  = WP33_1600
      RJ  = 3.
      ISO = 3
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAP33_1600 =-F1*AP1
         RBP33_1600 = F2*AP3
         RCP33_1600 = 0.0
      ELSE
         RAP33_1600 =-F1*AN1
         RBP33_1600 = F2*AN3
         RCP33_1600 = 0.0
      ENDIF
      
      CALL QKMC('F35_2000',Q2,AP3,AP1,AN3,AN1)
      W0  = LF35_2000
      RX  = PIBF35_2000
      GT  = WF35_2000
      RJ  = 5.
      ISO = 3
      CALL HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
      IF(IR.EQ.1.OR.IR.EQ.3) THEN
         RAF35_2000 = F1*AP1
         RBF35_2000 =-F2*AP3
         RCF35_2000 = 0.0
      ELSE
         RAF35_2000 = F1*AN1
         RBF35_2000 =-F2*AN3
         RCF35_2000 = 0.0
      ENDIF
      
 99   RETURN
      END

      



      SUBROUTINE HFACT(EPIREA,Q2,W0,RX,GT,RJ,ISO,F1,F2)
	 
	 
      IMPLICIT NONE	 
      INTEGER EPIREA,ISO
      REAL Q2,W0,RX,GT,RJ,F1,F2
      REAL RK,RQ,F,CPIN
      REAL WN,XMPIP,XMPI0,XMPI,PI
      DATA WN,XMPIP,XMPI0/0.938,0.1359,0.1349/
      
      IF(EPIREA.EQ.1.OR.EPIREA.EQ.2) XMPI=XMPI0
      IF(EPIREA.EQ.3.OR.EPIREA.EQ.4) XMPI=XMPIP
      
      PI=4.*ATAN(1.)
      RK=SQRT(Q2+(W0**2-Q2-WN**2)**2/4./W0**2)
      RQ=SQRT((W0**2+XMPI**2-WN**2)**2/4./W0**2-XMPI**2)
      F=SQRT(1./PI*RK/RQ*WN/W0*RX/GT)
      IF(ISO.EQ.1) THEN
         IF(EPIREA.EQ.1.OR.EPIREA.EQ.3) CPIN=-SQRT(2./3.)
         IF(EPIREA.EQ.2.OR.EPIREA.EQ.4) CPIN= SQRT(2./3.)
      ENDIF
      IF(ISO.EQ.3)                      CPIN=-SQRT(1./3.)        
      F1=CPIN*F/SQRT(RJ+1.)*19.73
      IF(RJ.GT.1.0) F2=F1*SQRT(16./(RJ-1.)/(RJ+3.))

      RETURN
      END
