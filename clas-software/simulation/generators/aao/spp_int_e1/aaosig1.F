      subroutine aao(epq2,epw,epeps,epcos,epphi,epirea,
     *pol_elec,pol_targ,sigu,sigt,sigl,sigi,sige,siget,sigpt)
     
      implicit none
      include "ao.inc"
      
      real pi
      real pol_elec,pol_targ
      real epq2,epw,epeps,epcos,epphi,sigma0,sigu,sigt,sigl,sigi,sige
      real siget,sigpt
      real tandd,fkt,sig0,sigmao
      real xmprot
      integer epirea

***************************************************************************
*      AUTHOR:        V. BURKERT AND Z. LI
*      FIRST VERSION: SUMMER, 1991.  RECENT UPDATE: MAY.1993
* AO IS WRITTEN BASED ON THE ORIGINAL PROGRAM A_AND_O FROM V.BURKERT
* THIS PROGRAM SHOULD BE LINKED TO EITHER QKMC OR QKXM FOR THE CALCULATION
* OF QUARK MODEL.  
* QKMC AND QKXM USE DIFFERENT FORM FACTORS:
* QKMC USES THE TREATMENT OF FOSTER ET. AL.
* QKXM USES THE DIPOLE FORM FACTOR 
* QKMC IS THE DEFAULT CHOICE IN THIS PACKAGE.
* AO CAN BE USED TO EXAM: Q2-DEPENDENCE OF HELICITY AMP. AT RES. POSITION (GO1)
*                         OUTPUT:GDH.TOP: A B CA(A_1/2) CB(A_3/2)
*                         GDH SUM RULE (GO2):OUTPUT GDH.TOP
*                         OBERVABLES  (RETURN):OUTPUT TEST.TOP
**************************************************************************
*           AO.FOR
* AO CONTAINS THE FOLLOWING SUBROUTINES/FUNCTIONS:
* SIGMA--CALCULATES OBSERVABLES
* EPRES--CALCULATES BREIT-WIGNER RESONANCE AMPLITUDES
* EXPA --CALCULATES THE HELICITY AMPLITUTES FROM EXPT (V.BURKERT ORIGINAL)
* BORNT--CALCULATES THE BORN TERM CONTRIBUTIONS
* BACK --CALCULATES BORN AND NON-BORN BACK GROUND TERMSS
* QKMA --CALCULATES THE HELICITY AMPLITUTES FROM QUARK MODEL
* RAMPF --CALCULATES THE Q2-DEPENDENCE OF THE HELICITY AMP. AT RES. POSITIONS
* HAMP --CALCULATES THE ENERGY-DEPENDECE OF THE RESONANCE HELICITY AMPLITUDES
* QKMC --CALCULATES THE COUPLING CONSTANTS FROM QUARK MODEL
************************************************************************
**************************************************************************
* UPDTATED: NOV., 1992
*         * WITH NEW OPTIONS TO TURN THE BORN TERM ON AND OFF
*         * BORN TERM ARE MODIFIED WITH A CUT OFF FACTOR AT HIGHER 
*           ENERGIES (WCM>1.3 GEV)
*         * SOME CORRECTIONS HAVE BEEN MADE TO THE EXPA SUBROUTINE
***************************************************************************  

C* IT:		1=EXP 2=EXP+QKM(*,**),3=EXP+QKM(NOEXP),4=QKM
C* IB=1,0:	1=BORN,0=NOBORN
C* IBORN=1,0:	1=BORN,0=NOBORN
C* NF=1,3:	1=NRQM,2=LI&CLOSE,3=HYBRID
C* IP11=1,4:	1=EXP,2=NO P11_1440,3=NRQM,4=HYBRID

      call ao_init

      pi	= 4.*atan(1.)
      nf	= 1
      it	= 1
      ip11	= 1
      ib	= 0
      iborn	= 1
      cut	= 0.4       
      sigma0	= 0.

      sigma0 = SIGMAO(EPW,EPQ2,EPEPS,EPCOS,EPPHI,EPIREA,
     *pol_elec,pol_targ,sigu,sigt,sigl,sigi,sige,siget,sigpt)
     
      end
      
C**************************************************************************
      COMPLEX  FUNCTION EPRES(IREA,XMP,XMPI,W,W0,GAM,X,L,J,ISO,     
     *                        PIMP,GK,TSTS11)   
     
      IMPLICIT NONE
      
      INTEGER L,J,ISO,IREA                                           
      REAL W,W0,GAM,X,PIMP,PIMP0,GK,GK0                                                          
      LOGICAL TSTS11                                                 
      COMPLEX QETA,GAMMAT,Q0ETA                                      
      REAL PENER0,XETA
      REAL GAMMA,GAMMAG
      COMPLEX IM                                                     
      REAL XMP,XMPI,XMETA
      DATA XMETA /.5488    /
      DATA IM/(0.,1.)/                                               
      
      PENER0=(W0**2-XMP**2+XMPI**2)/(2.*W0)                          
      PIMP0 = SQRT ( PENER0**2 - XMPI**2 )                           
      GK0 = (W0**2-XMP**2)/(2.*W0)                                
      
      GAMMA =GAM *((PIMP/PIMP0)**(2.*L+1))*(((PIMP0**2+X**2)/(PIMP**2+X**2))**L)                          
      GAMMAG=GAM *((GK/GK0)**(2.*J))*(((GK0**2+X**2)/(GK**2+X**2))**J)                        
                                                                       
      IF(TSTS11)THEN                                                 
         XETA=(W**2-XMP**2+XMETA**2)/(2.*W)                           
         XETA=XETA**2-XMETA**2                                        
         IF(XETA.GE.0.)THEN                                           
            QETA=CMPLX(SQRT(XETA),0.)                                  
         ELSE                                                         
            QETA=CMPLX(0.,SQRT(-XETA))                                 
         END IF                                                       
         Q0ETA=(W0**2-XMP**2+XMETA**2)/(2.*W0)                        
         Q0ETA=SQRT(Q0ETA**2-XMETA**2)                                
         GAMMAT=GAM*(.55*(QETA/Q0ETA)+.4*(PIMP/PIMP0)+.05)            
C     GAMMAT=GAM*(PIMP/PIMP0)
      ELSE                                                           
         GAMMAT=CMPLX(GAMMA,0.)                                       
      END IF                                                         
      EPRES=SQRT((GK0*PIMP0)/(GK*PIMP))                        
      EPRES=EPRES*W0*SQRT(GAMMA*GAMMAG)                              
      EPRES=EPRES/(W0**2-W**2-IM*W0*GAMMAT)                          
      IF(IREA.EQ.1.OR.IREA.EQ.2)THEN                                              
         IF(ISO.EQ.1)THEN                                             
            EPRES=-EPRES/SQRT(2.)                                      
         ELSE                                                         
            EPRES=EPRES*SQRT(2.)                                       
         END IF                                                       
      END IF                                                         
 99   RETURN                                                         
      END      
      
*************************************************************************   
      SUBROUTINE EXPA(EPIREA,EPQ2)
      
      IMPLICIT NONE
      include "ao.inc"
      include "res.inc"
     
      INTEGER EPIREA
      REAL    EPQ2,EPW                                                            
      REAL    XMPI,ELCHAR,FACT,FFAC
      REAL    QME11,QMM11,QMM12,QME22,QMM21,QMM22,QMM23
      REAL    CSD,SND 
      REAL    Q2EVF,Q2EVF0,DIP_EVF,PVEC_GAM,PVEC_GAM0,PVEC_PI,Q0_GAM    
      REAL    AFACT
      REAL    CPIN
      REAL    XMPROT,XMPIP,XMPI0,XMETA	
      REAL    PI
      DATA    PI         /3.14159  /     
      DATA    XMPROT     /0.938     /     
      DATA    XMPIP      /0.1395    /     
      DATA    XMPI0      /0.1349    /     
      DATA    XMETA      /0.5488    /     


      CPIN=sqrt(2./3.)
 
C
C       Now calculate the resonant helicity amplitudes 
C
      IF(EPIREA.EQ.1.OR.EPIREA.EQ.2) XMPI=XMPI0
      IF(EPIREA.EQ.3.OR.EPIREA.EQ.4) XMPI=XMPIP
      ELCHAR = SQRT(4.*PI/137.)

      IF (EPIREA.EQ.1.OR.EPIREA.EQ.3) THEN
         CALL QKEM(LS11_1535,EPQ2,QME11,QMM11,QMM12)
         A12S11_1535 = ELCHAR/2./SQRT(XMPROT*(LS11_1535**2-XMPROT**2))* 
     *                 (SQRT(1./3.)*QME11 - SQRT(2./3.)*QMM11)*CSD(THETA_S11)
         CALL QKEM(LS11_1650,EPQ2,QME11,QMM11,QMM12)
         A12S11_1650 = ELCHAR/2./SQRT(XMPROT*(LS11_1650**2-XMPROT**2)) * 
     *                 (SQRT(1./3.)*QME11 - SQRT(2./3.)*QMM11)*SND(THETA_S11)
  
C
C     Assume no mixing for D13 and D15
C
         CALL QKEM(LD13_1520,EPQ2,QME11,QMM11,QMM12)
         A12D13_1520=ELCHAR/2./SQRT(XMPROT*(LD13_1520**2-XMPROT**2))*
     *               (SQRT(1./6.)*QME11 + SQRT(1./12.)*QMM11 - SQRT(3./4.)*QMM12)
         A32D13_1520=ELCHAR/2./SQRT(XMPROT*(LD13_1520**2-XMPROT**2))*
     *               (SQRT(1./2.)*QME11 + 0.5*QMM11 + 0.5*QMM12)

         A12D13_1700 = 0. 
         A32D13_1700 = 0. 
         
         A12D15_1675 = 0. 
         A32D15_1675 = 0. 

         CALL QKEM(LD33_1700,EPQ2,QME11,QMM11,QMM12)
         A12D33_1700 = -ELCHAR/2./SQRT(XMPROT*(LD33_1700**2-XMPROT**2))*
     *                 (-SQRT(1./12.) * QMM12 - SQRT(1./6.) * QME11 +
     *                 0.5*SQRT(1./27.)*QMM11)
         A32D33_1700 = -ELCHAR/2./SQRT(XMPROT*(LD33_1700**2-XMPROT**2))*
     *                 (1./6.*QMM12 - SQRT(1./2.)*QME11 + 1./6.*QMM11)

         CALL QKEM(LS31_1620,EPQ2,QME11,QMM11,QMM12)
         A12S31_1620 = ELCHAR/2./SQRT(XMPROT*(LS31_1620**2-XMPROT**2))*
     *                 ( SQRT(1./3.)*QME11 + SQRT(2./27.)*QMM11)

C*  SET ALL TO ZERO EXCEPT S11(1535) AND D13(1520)
CC         A12S11_1535=0.
CC         A12D13_1520=0.
CC         A32D13_1520=0.
CC         A12S11_1650=0.
CC         A12S31_1620=0.
CC         A12D33_1700=0.
CC         A32D33_1700=0.
         

C
C         Now do the {56,0+} ---> {56,2+} transition.
C         Here we use the quark multipole moments of Cottingham & Dunbar
C         directly. Note that the quark multipole QMM21 is assumed = 0.
C         This assumption has no justification (data do not allow its
C         determination), other than that in photoproduction the extracted 
C         value is consistent with 0.
C                  
         CALL QKEM2(LF15_1680,EPQ2,QME22,QMM21,QMM22,QMM23)
         FACT = ELCHAR/2./SQRT(XMPROT*(LF15_1680**2 - XMPROT**2))
         A12F15_1680 = FACT*(-SQRT(2./3.)*QMM23 + SQRT(1./5.)*QME22+
     *                 SQRT(2./15.)*QMM22)
         A32F15_1680 = FACT*(SQRT(1./3.)*QMM23 + SQRT(2./5.)*QME22+
     *                 SQRT(4./15.)*QMM22)

         CALL QKEM2(LP13_1720,EPQ2,QME22,QMM21,QMM22,QMM23)
         FACT = ELCHAR/2./SQRT(XMPROT*(LP13_1720**2 - XMPROT**2))
         A12P13_1720 = -FACT*(SQRT(3./4.)*QMM21 + SQRT(1./10.)*QME22-
     *                  SQRT(3./20.)*QMM22)
         A32P13_1720 =  FACT*(0.5*QMM21 - SQRT(3./10.)*QME22 + 
     *                  SQRT(9./20.)*QMM22)

         CALL QKEM2(LF35_1905,EPQ2,QME22,QMM21,QMM22,QMM23)
         FACT = ELCHAR/2./SQRT(XMPROT*(LF35_1905**2 - XMPROT**2))
         A12F35_1905 =  FACT*(SQRT(16./189.)*QMM23 + 
     *                  SQRT(28./135.)*QMM22)
         A32F35_1905 = -FACT*(SQRT(8./189.)*QMM23 - 
     *                  SQRT(56./135.)*QMM22)

         CALL QKEM2(LP33_1920,EPQ2,QME22,QMM21,QMM22,QMM23)
         FACT = ELCHAR/2./SQRT(XMPROT*(LP33_1920**2 - XMPROT**2))
         A12P33_1920 = -FACT*(SQRT(3./15.)*QMM22 + 1./3.*QMM21)
         A32P33_1920 =  FACT*(SQRT(1./15.)*QMM22 - SQRT(1./3.)*QMM21)

         CALL QKEM2(LF37_1950,EPQ2,QME22,QMM21,QMM22,QMM23)
         FACT = ELCHAR/2./SQRT(XMPROT*(LF37_1950**2 - XMPROT**2))
         A12F37_1950 = -FACT*SQRT(2./7.)*QMM23
         A32F37_1950 = -FACT*SQRT(10./21.)*QMM23
cccc
cccc        multiply A12F37_1950 and A32F37_1950 by factor 2 (temporarily).
cccc        this will compensate for the shortfall of the SQTM results for that
cccc        state in photoproduction analysis
cccc 
         A12F37_1950 = A12F37_1950 * 2.
         A32F37_1950 = A32F37_1950 * 2.
cccc
cccc
cccc
         CALL QKEM2(LP31_1910,EPQ2,QME22,QMM21,QMM22,QMM23)
         FACT = ELCHAR/2./SQRT(XMPROT*(LP31_1910**2 - XMPROT**2))
         A12P31_1910 = -FACT*2./3.*QMM21

C*   SET ALL STATES ZERO EXCEPT F15(1680)
c          A12F15_1680=0.
c          A32F15_1680=0.
c          A12P13_1720=0.
c          A32P13_1720=0.
c          A12F37_1950=0.
c          A32F37_1950=0.
c          A12F35_1905=0.
c          A32F35_1905=0.
c          A12P33_1920=0.
c          A32P33_1920=0.
c          A12P31_1910=0.
C


      ELSE                      

C* THE FOLLOWING IS FOR NEUTRON TARGET
         A12P11_1440=0.056

         CALL QKEM(LS11_1535,EPQ2,QME11,QMM11,QMM12)
         FACT = ELCHAR/2./SQRT(XMPROT*(LS11_1535**2-XMPROT**2))
         A12S11_1535 = FACT*((-SQRT(1./3.)*QME11 +SQRT(2./27.)*QMM11)*CSD(THETA_S11)
     *                                         +SND(THETA_S11)*(-SQRT(2./27.)*QMM11))

         CALL QKEM(LS11_1650,EPQ2,QME11,QMM11,QMM12)
         FACT = ELCHAR/2./SQRT(XMPROT*(LS11_1650**2-XMPROT**2))
         A12S11_1650 = FACT*((-SQRT(1./3.)*QME11 +SQRT(2./27.)*QMM11)*SND(THETA_S11)
     *                                         +CSD(THETA_S11)*(-SQRT(2./27.)*QMM11))

C        Assume no mixing for D13 and D15
         CALL QKEM(LD13_1520,EPQ2,QME11,QMM11,QMM12)
         FACT = ELCHAR/2./SQRT(XMPROT*(LD13_1520**2-XMPROT**2))
         A12D13_1520 = FACT*(-SQRT(1./6.)*QME11-SQRT(1./108.)*QMM11 +SQRT(1./12.)*QMM12)
         A32D13_1520 = FACT*(-SQRT(1./2.)*QME11 - 1./6.*QMM11 -1./6.*QMM12)


         CALL QKEM(LD13_1700,EPQ2,QME11,QMM11,QMM12)
         FACT = ELCHAR/2./SQRT(XMPROT*(LD13_1700**2-XMPROT**2))
         A12D13_1700 = FACT*(-SQRT(5./54.)*QMM11-SQRT(1./120.)*QMM12)
         A32D13_1700 = FACT*(-SQRT(5./18.)*QMM11+SQRT(1./360.)*QMM12)


         CALL QKEM(LD15_1675,EPQ2,QME11,QMM11,QMM12)
         FACT = ELCHAR/2./SQRT(XMPROT*(LD15_1675**2-XMPROT**2))
         A12D15_1675 = FACT*(-SQRT(2./15.)*QMM12)
         A32D15_1675 = FACT*(-SQRT(4./15.)*QMM12)


         CALL QKEM(LD33_1700,EPQ2,QME11,QMM11,QMM12)
         FACT = ELCHAR/2./SQRT(XMPROT*(LD33_1700**2-XMPROT**2))
         A12D33_1700 =-FACT*(-SQRT(1./12.)* QMM12 - SQRT(1./6.)*QME11 +
     *                     0.5*SQRT(1./27.)*QMM11)
         A32D33_1700 =-FACT*(1./6.*QMM12 - SQRT(1./2.)*QME11 + 1./6.*QMM11)


         CALL QKEM(LS31_1620,EPQ2,QME11,QMM11,QMM12)
         FACT = ELCHAR/2./SQRT(XMPROT*(LS31_1620**2-XMPROT**2)) 
         A12S31_1620 = FACT*( SQRT(1./3.)*QME11 + SQRT(2./27.)*QMM11)

C*  SET ALL TO ZERO EXCEPT S11(1535) AND D13(1520)
cc           A12S11_1535=0.
cc           A12D13_1520=0.
cc           A32D13_1520=0.
cc         A12S11_1650=0.
cc         A12S31_1620=0.
cc         A12D33_1700=0.
cc         A32D33_1700=0.
        

C
C         Now do the {56,0+} ---> {56,2+} transition.
C         Here we use the quark multipole moments of Cottingham & Dunbar
C         directly. Note thet quark multipole QMM21 is assumed = 0.
C         This assumption has no justification (data do not allow its
C         determination).
C                  

         CALL QKEM2(LF15_1680,EPQ2,QME22,QMM21,QMM22,QMM23)
         FACT = ELCHAR/2./SQRT(XMPROT*(LF15_1680**2 - XMPROT**2))
         A12F15_1680 = FACT*(SQRT(8./27.)*QMM23-SQRT(8./135.)*QMM22)
         A32F15_1680 = FACT*(-SQRT(4./27.)*QMM23-SQRT(16./135.)*QMM22)


         CALL QKEM2(LP13_1720,EPQ2,QME22,QMM21,QMM22,QMM23)
         FACT = ELCHAR/2./SQRT(XMPROT*(LP13_1720**2 - XMPROT**2))
         A12P13_1720 = -FACT*(-SQRT(1./3.)*QMM21 + SQRT(1./15.)*QMM22)
         A32P13_1720 = -FACT*(1./3.*QMM21+SQRT(1./5.)*QMM22)  


         CALL QKEM2(LF35_1905,EPQ2,QME22,QMM21,QMM22,QMM23)
         FACT = ELCHAR/2./SQRT(XMPROT*(LF35_1905**2 - XMPROT**2))
         A12F35_1905 =  FACT*(SQRT(16./189.)*QMM23 + SQRT(28./135.)*QMM22)
         A32F35_1905 = -FACT*(SQRT(8. /189.)*QMM23 - SQRT(56./135.)*QMM22)


         CALL QKEM2(LP33_1920,EPQ2,QME22,QMM21,QMM22,QMM23)
         FACT = ELCHAR/2./SQRT(XMPROT*(LP33_1920**2 - XMPROT**2))
         A12P33_1920 = -FACT*(SQRT(3./15.)*QMM22 + 1./3.*QMM21)
         A32P33_1920 =  FACT*(SQRT(1./15.)*QMM22 - SQRT(1./3.)*QMM21)


         CALL QKEM2(LF37_1950,EPQ2,QME22,QMM21,QMM22,QMM23)
         FACT = ELCHAR/2./SQRT(XMPROT*(LF37_1950**2 - XMPROT**2))
         A12F37_1950 = -FACT*SQRT(2./7.)*QMM23
         A32F37_1950 = -FACT*SQRT(10./21.)*QMM23
cccc
cccc        multiply A12F37_1950 and A32F37_1950 by factor 2 (temporarily !!!)
cccc        to bring them in quantitative agreement with photoproduction
cccc        analysis
cccc
         A12F37_1950 = A12F37_1950 * 2.
         A32F37_1950 = A32F37_1950 * 2.
cccc
cccc
cccc
         CALL QKEM2(LP31_1910,EPQ2,QME22,QMM21,QMM22,QMM23)
         FACT = ELCHAR/2./SQRT(XMPROT*(LP31_1910**2 - XMPROT**2))
         A12P31_1910 = -FACT*2./3.*QMM21
      
      END IF

C
C       Now calculate the resonant partial wave helicity elements
C


      IF(EPIREA.EQ.1.OR.EPIREA.EQ.3)    CPIN=-CPIN

      FACT = AFACT(EPIREA,EPQ2,LS11_1535,PIBS11_1535,WS11_1535,1.)
      RAS11_1535 =-FACT*A12S11_1535*19.73*CPIN
      RCS11_1535 = 0.


      FACT = AFACT(EPIREA,EPQ2,LS11_1650,PIBS11_1650,WS11_1650,1.)
      RAS11_1650 = -FACT*A12S11_1650*19.73*CPIN
      RCS11_1650 = 0.


      FACT = AFACT(EPIREA,EPQ2,LP11_1440,PIBP11_1440,WP11_1440,1.)
      EPW=LP11_1440
      Q2EVF=((EPW**2-XMPROT**2)**2+EPQ2*(EPW+XMPROT)**2)/(4.*EPW*XMPROT)
      Q2EVF0=((EPW**2-XMPROT**2)**2)/(4.*EPW*XMPROT)
      DIP_EVF = (1.+ Q2EVF/0.71)**(-2.)
      RAP11_1440 = FACT*A12P11_1440*(Q2EVF/Q2EVF0)*DIP_EVF*19.73*CPIN        
      RCP11_1440 =               0.0000D+00


      RAP11_1710 =               0.0000D+00
      RCP11_1710 =               0.0000D+00


      FACT = AFACT(EPIREA,EPQ2,LP13_1720,PIBP13_1720,WP13_1720,3.)
      RAP13_1720 = - FACT              *A12P13_1720*19.73*CPIN
      RBP13_1720 =   FACT*SQRT(16./12.)*A32P13_1720*19.73*CPIN
      RCP13_1720 =               0.0000D+00


      FACT = AFACT(EPIREA,EPQ2,LD13_1520,PIBD13_1520,WD13_1520,3.)
      RAD13_1520 =  FACT              *A12D13_1520*19.73*CPIN
      RBD13_1520 = -FACT*SQRT(16./12.)*A32D13_1520*19.73*CPIN
      RCD13_1520 =               0.0000D+00


      FACT = AFACT(EPIREA,EPQ2,LD13_1700,PIBD13_1700,WD13_1700,3.)
      RAD13_1700 =  FACT              *A12D13_1700*19.73*CPIN
      RBD13_1700 = -FACT*SQRT(16./12.)*A32D13_1700*19.73*CPIN
      RCD13_1700 =               0.0000D+00


      FACT = AFACT(EPIREA,EPQ2,LD15_1675,PIBD15_1675,WD15_1675,5.)
      RAD15_1675 = -FACT              *A12D15_1675*19.73*CPIN
      RBD15_1675 =  FACT*SQRT(16./32.)*A32D15_1675*19.73*CPIN
      RCD15_1675 =               0.0000D+00


      FACT = AFACT(EPIREA,EPQ2,LF15_1680,PIBF15_1680,WF15_1680,5.)
      RAF15_1680 =  FACT              *A12F15_1680*19.73*CPIN
      RBF15_1680 = -FACT*SQRT(16./32.)*A32F15_1680*19.73*CPIN
      RCF15_1680 =              0.0000D+00


      RAG17_2190 =              0.0000D+00
      RBG17_2190 =              0.0000D+00
      RCG17_2190 =              0.0000D+00


      RAG19_2250 =              0.0000D+00
      RBG19_2250 =              0.0000D+00
      RCG19_2250 =              0.0000D+00


      RAH19_2220 =              0.0000D+00
      RBH19_2220 =              0.0000D+00
      RCH19_2220 =              0.0000D+00


      RAI111_2600 =             0.0000D+00
      RBI111_2600 =             0.0000D+00
      RCI111_2600 =             0.0000D+00

          
      FACT = AFACT(EPIREA,EPQ2,LS31_1620,PIBS31_1620,WS31_1620,1.)
      RAS31_1620 = FACT*A12S31_1620*19.73*SQRT(1./3.)
      RCS31_1620 =              0.0000D+00


      RAS31_1900 =              0.0000D+00
      RCS31_1900 =              0.0000D+00


      FACT = AFACT(EPIREA,EPQ2,LP31_1910,PIBP31_1910,WP31_1910,1.)
      RAP31_1910 = -FACT*A12P31_1910*19.73*SQRT(1./3.)
      RCP31_1910 =              0.0000D+00

         
      PVEC_GAM = SQRT((EPQ2 + (LP33_1232+XMPROT)**2)*(EPQ2+(LP33_1232-XMPROT)**2))/(2.*LP33_1232)
      PVEC_GAM0 = SQRT(((LP33_1232+XMPROT)**2)*((LP33_1232-XMPROT)**2))/(2.*LP33_1232)
      Q0_GAM=(LP33_1232**2 - XMPROT**2+EPQ2)/(2.*LP33_1232)
      PVEC_PI= SQRT((LP33_1232**2+XMPI**2-XMPROT**2)**2/(4.*LP33_1232**2) - XMPI**2)
      FACT = SQRT(1./137.*LP33_1232*PIBP33_1232/6./XMPROT/PVEC_PI/XMPROT**2/WP33_1232)*PVEC_GAM
      FFAC = 1./(1+EPQ2/0.71)**2*EXP(-0.21	*EPQ2)
      RAP33_1232 = -0.5*3.1*FFAC*FACT*SQRT(1./3.)*19.73
      RBP33_1232 =      3.1*FFAC*FACT*SQRT(1./3.)*19.73
      RCP33_1232 =  -0.03*3.1*SQRT(EPQ2)/Q0_GAM*3.*FFAC*FACT*SQRT(1./3.)*19.73
c      RCP33_1232 =  -0.05*3.1*FFAC*FACT*SQRT(1./3.)*19.73
c      print *, 'Resonance:',q0_gam,sqrt(epq2)/q0_gam
 
         
      FACT = AFACT(EPIREA,EPQ2,LP33_1920,PIBP33_1920,WP33_1920,3.)
      RAP33_1920 =  FACT              *A12P33_1920*SQRT(1./3.)
      RBP33_1920 = -FACT*SQRT(16./12.)*A32P33_1920*SQRT(1./3.)
      RCP33_1920 =               0.0000D+00


      FACT = AFACT(EPIREA,EPQ2,LD33_1700,PIBD33_1700,WD33_1700,3.)
      RAD33_1700 = -FACT              *A12D33_1700*19.73*SQRT(1./3.)
      RBD33_1700 =  FACT*SQRT(16./12.)*A32D33_1700*19.73*SQRT(1./3.)
      RCD33_1700 =               0.0000D+00
         

      RAD35_1930 =               0.0000D+00
      RBD35_1930 =               0.0000D+00
      RCD35_1930 =               0.0000D+00


      FACT = AFACT(EPIREA,EPQ2,LF35_1905,PIBF35_1905,WF35_1905,5.)
      RAF35_1905 = -FACT              *A12F35_1905*19.73*SQRT(1./3.)
      RBF35_1905 =  FACT*SQRT(16./32.)*A32F35_1905*19.73*SQRT(1./3.)
      RCF35_1905 =               0.0000D+00

      
      FACT = AFACT(EPIREA,EPQ2,LF37_1950,PIBF37_1950,WF37_1950,7.)
      RAF37_1950 =  FACT              *A12F37_1950*19.73*SQRT(1./3.)
      RBF37_1950 = -FACT*SQRT(16./60.)*A32F37_1950*19.73*SQRT(1./3.)
      RCF37_1950 =               0.0000D+00


      RAH311_2420 =              0.0000D+00
      RBH311_2420 =              0.0000D+00
      RCH311_2420 =              0.0000D+00
      
      RETURN
      END

c*********************************************************************************
      SUBROUTINE QKEM(EPW,EPQ2,QME11,QMM11,QMM12) 
      
      IMPLICIT NONE
      REAL EPW,EPQ2,QME11,QMM11,QMM12 
      REAL Q2EVF,Q2EVF0,DIP_EVF,DIP_EVF0    
      REAL XMPROT     
      DATA XMPROT/0.938/
      
      Q2EVF=((EPW**2-XMPROT**2)**2+EPQ2*(EPW+XMPROT)**2)/(4.*EPW*XMPROT)
      Q2EVF0=((EPW**2-XMPROT**2)**2)/(4.*EPW*XMPROT)
      DIP_EVF = (1.+ Q2EVF/0.71)**(-2.)
      dip_evf0 = (1.+ Q2EVF0/0.71)**(-2.)
***********************************************************************************
C* PUT THE FOLLOWING TO USE THE COUPLING CONSTANT GIVEN IN DATA STATEMENT
C          GO TO 98
c                  QME11 = 3.  * DIP_EVF
c                  QMM11 =  3.8 * (0.4 - Q2EVF) * DIP_EVF
***********************************************************************************
*  NEW PARAMTERIZATION OF QUARK MULTIPOLES BASED ON THE CLAS DATA FOR THE S11(1535)
***********************************************************************************
***  MODIFICATION DONE ON 2/27/2001: to use old, decomment line above and comment lines below
      IF(Q2EVF.LE.1.4) THEN  
         QME11 = (3.074 + 0.812 * Q2EVF) * DIP_EVF
      ELSE
         QME11 = (4.387 - 0.238 * Q2EVF + 0.081 * Q2EVF**2) * DIP_EVF
      ENDIF
      QMM11 =  (1.896 - 6.212*Q2EVF + 0.913*Q2EVF**2 - 0.12*Q2EVF**3)* DIP_EVF
***********************************************************************************
      IF(Q2EVF.LE.1.2) THEN
         QMM12 = 5.* Q2EVF * DIP_EVF
      ELSE
         QMM12 = ( 7.2 - 1.0*Q2EVF ) * DIP_EVF
      ENDIF
      
      RETURN
      END


C******************************************************************
      SUBROUTINE QKEM2(EPW,EPQ2,QME22,QMM21,QMM22,QMM23) 
	 
      IMPLICIT NONE
      REAL EPW,EPQ2,QME22,QMM21,QMM22,QMM23
      REAL Q2EVF,Q2EVF0,DIP_EVF,DIP_EVF0    
      REAL XMPROT     
      DATA XMPROT/0.938/
      
      Q2EVF=((EPW**2-XMPROT**2)**2+EPQ2*(EPW+XMPROT)**2)/(4.*EPW*XMPROT)
      Q2EVF0=((EPW**2-XMPROT**2)**2)/(4.*EPW*XMPROT)
      DIP_EVF = (1.+ Q2EVF/0.71)**(-2.)
      DIP_EVF0 = (1.+ Q2EVF0/0.71)**(-2)
CCC
CCC       NOTE THAT QME22 HAS TO BE RENORMALIZED TO REPRODUCE DATA
CCC           AT THE PHOTON POINT E22 = 0.99, AND E22^(twiddle) IS ASSUMED
CCC            AS A C0NSTANT  !!!!
CCC           NOTE THAT IN THE PRD ARTICLE THERE IS A MISPRINT 
CCC                        (E22 -> E22^(twiddle))
CCC
      QME22 = 0.99*SQRT(Q2EVF)*DIP_EVF/SQRT(Q2EVF0)/DIP_EVF0
C
      QMM21 = 0.* SQRT(Q2EVF)*DIP_EVF
      QMM22 = (0.75*SQRT(5.)-1.5*SQRT(5.)*Q2EVF)*SQRT(Q2EVF)*DIP_EVF
      IF(Q2EVF.LE.1.2)THEN
         QMM23 = 5.*Q2EVF*SQRT(Q2EVF)*DIP_EVF
      ELSE
         QMM23 = (7.2 - Q2EVF)*SQRT(Q2EVF)*DIP_EVF
      ENDIF
      RETURN
      END

***************************************************************************
* DEVELOPED IN END OF SEPT. 1992 Z.LI
* With IFLAG and VFLAG both equal to 0, we calculate electric born only
* Both neutron and proton form factor took from original program hborn.
* The axial form factor may not correct.
* A cut off factor is added for the born terms when the center of
* mass energy is larger than 1.3 GEV:
* Fcut=cut**2/(cut**2-(w-1.3)**2), cut is set to 0.4 
**************************************************************************
      SUBROUTINE BORNT(Q2,W,SINX,COSX,SINX2,COSX2,BORN,IR,CUT)
              
      IMPLICIT NONE

      INTEGER I,J
      INTEGER IR
      REAL Q2,W,SINX,COSX,SINX2,COSX2,CUT,FCUT
      REAL Q,Q0,E1,E2,P,P0,T,U,Z1,Z2,FC,XX,PQ,S
      REAL F1P,F1N,F2P,F2N,F1V,F1S,F2V,F2S
      REAL BORN(6),GAMMA(6,3)
      REAL A(6,3),AR(6),FR(6),F1(3),F2(3),FP(3),F1A(3),F2A(3)
      REAL C(6),B(6,6),ETA(6),EPS(3)
      REAL XMPIP,XMPI0,XMP,XMPI
      REAL PI,SQ2
      REAL UP,UN,EC,GN,GG,GEP,CAP
      REAL VFLAG
      INTEGER IFLAG
      real qv_mag_cm,nu_cm

      DATA XMPIP/0.139563/,XMPI0/0.1349630/,XMP/0.93827/
      DATA UP,UN/1.793,-1.913/
      DATA EC/.302862/,GN/13.5/
      DATA ETA/1.,1.,-1.,1.,-1.,-1./,EPS/1.,1.,-1./
      DATA IFLAG/1/,VFLAG/0./

C* IFLAG=0, VFLAG=0. WOULD GET RID OF MAGNETIC AND PV EXTRA TERM 
C* RESPECTIVELY.
       IF(IR.EQ.1.OR.IR.EQ.2)XMPI=XMPI0
       IF(IR.EQ.3.OR.IR.EQ.4)XMPI=XMPIP
       PI=4.0*ATAN(1.0)
       GG=SQRT(4.*PI*GN)
C*  THE PI-N COUPLING CONSTANT
       GEP=1./(1.+3.04*Q2+1.54*Q2*Q2+0.068*Q2*Q2*Q2)
       CAP=Q2/(4.*XMP*XMP)
       F1P=EC*GEP*(1.+(1.+UP)*CAP)/(1.+CAP)
       F1N=EC*GEP*UN*CAP/(1.+CAP)
       IF(IFLAG.EQ.0) THEN
         F2P=0.
         F2N=0.
       ELSE
         F2P=EC/(2.*XMP)*GEP*UP/(1.+CAP)
         F2N=EC/(2.*XMP)*GEP*UN/(1.+CAP)
       END IF
       F1V=F1P-F1N
       F1S=F1P+F1N
       F2V=F2P-F2N
       F2S=F2P+F2N
       F1(1)=F1V
       F1(2)=F1S
       F1(3)=F1V
       F2(1)=F2V
       F2(2)=F2S
       F2(3)=F2V
C* 1,3,2, CORESSPONDING TO THE ISOVECTOR (+,-) AND ISOSCALAR (0).
       FP(3)=EC/(1.+Q2/0.5)
       FP(1)=0.
       FP(2)=0.
C* PION FORM FACTOR.
       S=W**2
       
       Q0=(S-XMP**2-Q2)/(2.*W)
       Q=SQRT(Q2+Q0**2)
       
c      qv_mag_cm  = ((w**2 + q2 + xmp**2)/2.0/w)**2 - xmp**2
c      qv_mag_cm  = sqrt(qv_mag_cm)
c      nu_cm      = sqrt(qv_mag_cm**2 - q2)
c      print *, w,q0,nu_cm,qv_mag_cm,q0/q
      
       E1=(S+XMP**2+Q2)/(2.*W)
       E2=(S+XMP**2-XMPI**2)/(2.*W)
       P=SQRT(E2**2-XMP**2)
       P0=SQRT(P**2+XMPI**2)
       T=2.*Q*P*COSX-2.*Q0*P0+XMPI**2-Q2
       U=-2.*Q*P*COSX-2.*Q0*E2+XMP**2-Q2
       DO 15,J=1,3
         GAMMA(1,J)=0.5*GG*F1(J)
         GAMMA(2,J)=-GG*F1(J)/(T-XMPI**2)
         GAMMA(3,J)=-0.5*GG*F2(J)
         GAMMA(4,J)=GAMMA(3,J)
         GAMMA(5,J)=0.5*GAMMA(2,J)
         GAMMA(6,J)=0.
15     CONTINUE   
       DO 25,I=1,4
       DO 35,J=1,3
         A(I,J)=(1./(S-XMP**2)+EPS(J)*ETA(I)/(U-XMP**2))*GAMMA(I,J)
35     CONTINUE
25     CONTINUE
       F2A(1)=F2(1)
       F2A(2)=F2(2)
       A(1,1)=VFLAG*0.5*GG/XMP*F2A(1)+A(1,1)
       A(1,2)=VFLAG*0.5*GG/XMP*F2A(2)+A(1,2)
c* Add in the pseudovector extra term, here we simply used F2(1,2) same
c* as in non-axial term.
       DO 45,J=1,3
       IF (Q2.EQ.0.) THEN
         A(5,J)=0.
         A(6,J)=0.
       ELSE
         A(5,J)=(1./(S-XMP**2)+ETA(5)/(U-XMP**2))*GAMMA(5,J)
     C   +0.5*(1.-EPS(J))*2.*GG/Q2*(FP(J)-F1(J))/(T-XMPI**2)
         A(6,J)=(1./(S-XMP**2)+ETA(6)/(U-XMP**2))*GAMMA(6,J)
       END IF
45     CONTINUE
       SQ2=SQRT(2.0)
       DO 50, I=1,6
         IF(IR.EQ.1) AR(I)=(A(I,2)+A(I,1))
         IF(IR.EQ.2) AR(I)=(-A(I,2)+A(I,1))
         IF(IR.EQ.3) AR(I)=-SQ2*(A(I,2)+A(I,3))
         IF(IR.EQ.4) AR(I)=SQ2*(A(I,2)-A(I,3))
50     CONTINUE
       Z1=SQRT(E1+XMP)
       Z2=SQRT(E2+XMP)
       FC=1./(8.*PI*W)
       C(1)=FC*(W-XMP)*Z1*Z2
       C(2)=FC*(W+XMP)*P*Q/Z1/Z2
       C(3)=FC*(W+XMP)*P*Q*Z2/Z1
       C(4)=FC*(W-XMP)*P**2*Z1/Z2
       C(5)=FC*Z1/Z2*P
       C(6)=FC*Q*Z2/Z1  
       XX=C(1)/C(2)
       PQ=(T-XMPI**2+Q2)/2.
       B(1,1)=1.
       B(1,2)=0.
       B(1,3)=-PQ/(W-XMP)
       B(1,4)=W-XMP+PQ/(W-XMP)
       B(1,5)=0.
       B(1,6)=Q2/(W-XMP)
       B(2,1)=-1.
       B(2,2)=0.
       B(2,3)=-PQ/(W+XMP)
       B(2,4)=W+XMP+PQ/(W+XMP)
       B(2,5)=0.
       B(2,6)=Q2/(W+XMP)
       B(3,1)=0.
       B(3,2)=(S-XMP**2+Q2/2.)/(W+XMP)
       B(3,3)=1.
       B(3,4)=-1.
       B(3,5)=-Q2/(W+XMP)
       B(3,6)=0.
       B(4,1)=0.
       B(4,2)=-(S-XMP**2+Q2/2.)/(W-XMP)
       B(4,3)=1.
       B(4,4)=-1.
       B(4,5)=Q2/(W-XMP)
       B(4,6)=0.
       B(5,1)=-E1+XMP
       B(5,2)=1./(2.*Q0)*(Q**2*(3.*PQ+2.*Q0*W)-(PQ+P0*Q0)*(2.*(S-XMP**2)+Q2))
       B(5,3)=PQ+P0*(W-XMP)
       B(5,4)=-PQ-P0*(W+XMP)+(E1-XMP)*(W+XMP)
       B(5,5)=P0*Q2-Q0*PQ
       B(5,6)=-(E1-XMP)*(W+XMP)
       B(6,1)=E1+XMP
       B(6,2)=-B(5,2)
       B(6,3)=PQ+P0*(W+XMP)
       B(6,4)=-PQ-P0*(W+XMP)+(E1+XMP)*(W-XMP)
       B(6,5)=-B(5,5)
       B(6,6)=-(E1+XMP)*(W-XMP)
       DO 55, I=1,6
       FR(I)=0.
       DO 65, J=1,6
         FR(I)=FR(I)+C(I)*B(I,J)*AR(J)*19.732857
65     CONTINUE
55     CONTINUE
       IF(W.LE.1.3) FCUT=1.
       IF(W.GT.1.3) FCUT=CUT**2/(CUT**2+(W-1.3)**2)
****************************************************************
*** eliminated cut
***     fcut=1.
*****************************************************************
       BORN(1)=-SINX*COSX2/SQ2*(FR(3)+FR(4))
       BORN(2)=-SQ2*COSX2*(FR(1)-FR(2))+SINX2*SINX*(FR(3)-FR(4))/SQ2
       BORN(3)=SINX2*SINX/SQ2*(FR(3)-FR(4))
       BORN(4)=SQ2*SINX2*(FR(1)+FR(2))+COSX2*SINX*(FR(3)+FR(4))/SQ2 
       BORN(5)=COSX2*(FR(5)+FR(6))
       BORN(6)=SINX2*(FR(5)-FR(6))
       DO 75, I=1,6
       BORN(I)=BORN(I)*FCUT
75     CONTINUE
       RETURN
       END   
                
      REAL FUNCTION SIGMAO(EPW,EPQ2,EPEPS,EPCOS,EPPHI,EPIREA,
     *POL_ELEC,POL_TARG,sigu,sigt,sigl,sigi,sige,siget,sigpt)

      implicit none
      real epw,epq2,epeps,epcos,epphi,pol_elec,pol_targ
      real sig0,sigu,sigt,sigl,sigi
      real fkt
      real snd,csd,pn,pl,pt,sigpt,siget
      real xila,sige,enue,theta_elec,e_elec_scatt
      real e_elec_prim
      real c2,c3
      real q2,sinx,cosx,sinx2,cosx2
      real eph1b,eph2b,eph3b,eph4b,eph5b,eph6b
      real xmpot
      real sq2,pi,xmprot,xmpip,xmpi0,xmpi,xmeta,dum
      real epsin,epsin2,epcos2,pener,pimp,gener,gimp,gamk
      real p1s,p2s,p3s,p4s,p5s,p6s,p7s,p2s2,p3s2,p4s2,p5s2
      real p6s2,p7s2
      real x,x0,f
      real bpipy0,bpipy1,bpipy2,bpipy3
      real vt,vl,vtt,vlt,vltp,vttp,cph,sph,c2ph,s2ph
      integer irea
      real vgamE,vgamP,rnorm1,ekin
      
      real sig_l,sig_t,sig_tt,sig_lt,sig_ltp
      real sig_t_y,sig_l_y
      real sig_lt_x,sig_lt_y,sig_lt_z
      real sig_tt_x,sig_tt_y,sig_tt_z
      real sig_ltp_x,sig_ltp_y,sig_ltp_z
      real sig_ttp_x,sig_ttp_z
         
      real sigma_l, sigma_t, sigma_tt, sigma_lt, sigma_ltp
      real sigma_t_y, sigma_l_y, sigma_lt_x, sigma_lt_y, sigma_lt_z
      real sigma_tt_x, sigma_tt_y, sigma_tt_z
      real sigma_ltp_x, sigma_ltp_y, sigma_ltp_z
      real sigma_ttp_x, sigma_ttp_z
      
      include 'helamp.inc'
      include 'ao.inc'

      COMPLEX                                                           
     * QAP11_1440,             QCP11_1440,
     * QAD13_1520, QBD13_1520, QCD13_1520, 
     * QAS11_1535,             QCS11_1535, 
     * QAS11_1650,             QCS11_1650, 
     * QAD15_1675, QBD15_1675, QCD15_1675, 
     * QAF15_1680, QBF15_1680, QCF15_1680,
     * QAD13_1700, QBD13_1700, QCD13_1700, 
     * QAP11_1710,             QCP11_1710,
     * QAP13_1720, QBP13_1720, QCP13_1720,
     * QAP13_1870, QBP13_1870, QCP13_1870,
     * QAP13_1955, QBP13_1955, QCP13_1955,
     * QAF15_1955, QBF15_1955, QCF15_1955,
     * QAP13_1980, QBP13_1980, QCP13_1980,
     * QAF17_1990, QBF17_1990, QCF17_1990,
     * QAF15_2000, QBF15_2000, QCF15_2000,
     * QAP11_2100, QBP11_2100, QCP11_2100,
     * QAG17_2190, QBG17_2190, QCG17_2190, 
     * QAH19_2220, QBH19_2220, QCH19_2220,
     * QAG19_2250, QBG19_2250, QCG19_2250, 
     * QAI111_2600,QBI111_2600,QCI111_2600, 
     * QAP33_1232, QBP33_1232, QCP33_1232,
     * QAP33_1600, QBP33_1600, QCP33_1600,
     * QAS31_1620,             QCS31_1620,
     * QAD33_1700, QBD33_1700, QCD33_1700,
     * QAS31_1900,             QCS31_1900,
     * QAF35_1905, QBF35_1905, QCF35_1905,
     * QAP31_1910,             QCP31_1910,
     * QAP33_1920, QBP33_1920, QCP33_1920,
     * QAP31_1925, QBP31_1925, QCP31_1925,
     * QAD35_1930, QBD35_1930, QCD35_1930,
     * QAF37_1950, QBF37_1950, QCF37_1950,
     * QAP33_1975, QBP33_1975, QCP33_1975,
     * QAF35_2000, QBF35_2000, QCF35_2000,
     * QAH311_2420,QBH311_2420,QCH311_2420,
     * XA0P  ,XA1P  ,XA2P  ,XA3P  ,XA4P  ,XA5P  ,                               
     * XA1M  ,XA2M  ,XA3M  ,XA4M  ,XA5M  ,XA6M  ,                                             
     * XB1P  ,XB2P  ,XB3P  ,XB4P  ,XB5P  ,                                             
     * XB2M  ,XB3M  ,XB4M  ,XB5M  ,XB6M  ,                                                   
     * XC0P  ,XC1P  ,XC2P  ,XC3P  ,XC4P  ,XC5P  ,                                      
     * XC1M  ,XC2M  ,XC3M  ,XC4M  ,XC5M  ,XC6M                                              
      COMPLEX                                                           
     * RP11_1440, RD13_1520, RS11_1535,
     * RS11_1650, RD15_1675, RF15_1680,
     * RD13_1700, RP11_1710, RP13_1720,   
     * RP13_1870, RP13_1955, RF15_1955,
     * RP13_1980, RF17_1990, RF15_2000,
     * RP11_2100, RG17_2190, RH19_2220,
     * RG19_2250, RI111_2600,
     * RP33_1232, RP33_1600, RS31_1620,
     * RD33_1700, RS31_1900, RF35_1905,
     * RP31_1910, RP33_1920, RP31_1925,
     * RD35_1930, RF37_1950, RP33_1975,
     * RF35_2000, RH311_2420

      COMPLEX EPH1,EPH2,EPH3,EPH4,EPH5,EPH6                             
      COMPLEX ADDA0,ADDA1,ADDA2,ADDA3,ADDA4,ADDA5
      COMPLEX ADDB1,ADDB2,ADDB3,ADDB4,ADDB5
      COMPLEX ADDC0,ADDC1,ADDC2,ADDC3,ADDC4,ADDC5
      COMPLEX SUBA0,SUBA1,SUBA2,SUBA3,SUBA4,SUBA5
      COMPLEX SUBB1,SUBB2,SUBB3,SUBB4,SUBB5
      COMPLEX SUBC0,SUBC1,SUBC2,SUBC3,SUBC4,SUBC5
      COMPLEX HNP,HNM,HFP,HFM,HN0,HF0                                   
      COMPLEX X1,X2,Y1,Y2,Y3,Y4,Z1,Z2                                   
      COMPLEX R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11
      REAL HB(6)
                                      
      COMPLEX ZERO                 
      INTEGER EPIREA               
      INTEGER IR                   
      COMPLEX EPRES                
      REAL YMPI,W,PIM,GK           
                                      
      DATA ZERO/(0.,0.)/           
      DATA SQ2        /1.41421  /     
      DATA PI         /3.14159  /     
      DATA XMPROT     /.938     /     
      DATA XMPIP      /.1395    /     
      DATA XMPI0      /.1349    /     
      DATA XMETA      /.5488    /     
        
      sig0=0.

      DUM    = ACOS(EPCOS)                                                
      EPSIN  = SIN(DUM)                                                 
      EPSIN2 = SIN(DUM/2.)                                             
      EPCOS2 = COS(DUM/2.)
                                                   
      IF(EPIREA.EQ.1.OR.EPIREA.EQ.2)XMPI=XMPI0                                      
      IF(EPIREA.EQ.3.OR.EPIREA.EQ.4)XMPI=XMPIP                                      
                                                                                 
      PENER = (EPW**2-XMPROT**2+XMPI   **2)/(2.*EPW   )            
      PIMP  = SQRT(PENER   **2 - XMPI   **2)                       
      GENER = (EPW   **2-XMPROT**2-EPQ2   )/(2.*EPW   )            
      GIMP  = SQRT(GENER**2 + EPQ2   )                             
      GAMK  = (EPW**2-XMPROT**2)/(2.*EPW   )                       

      X    = EPCOS                                                      
      P1S  = 1.   
      P2S  = 3.*X 
      P3S  = (15.*X*X-3.)/2.
      P4S  = (35.*X*X*X-15.*X)/2.
      P5S  = (315.*X*X*X*X-210.*X*X+15.)/8. 
      P6S  = (693.*X*X*X*X*X-630.*X*X*X)/8.
      P7S  = (3003.*X*X*X*X*X*X-3465.*X*X*X*X)/16.
      P2S2 = 3.
      P3S2 = 15.*X 
      P4S2 = (105.*X*X-15.)/2.
      P5S2 = (315.*X*X*X-105.*X)/2.
      P6S2 = (3465.*X*X*X*X-1890.*X*X)/8.                             
      P7S2 = (18018.*X*X*X*X*X-13860.*X*X*X)/16.
      
      X0     = 1./SQRT((PIMP   **2)+(.35**2))                            
      F      = (1.+(PIMP   **2)/.71)                                      
      BPIPY0 = 1./F                                               
      BPIPY1 = X0*PIMP/F                                       
      BPIPY2 = X0*X0*PIMP*PIMP/F                            
      BPIPY3 = X0*X0*X0*PIMP*PIMP*PIMP/F   
                                                    
      FKT   = PIMP/GAMK                                                
      Q2    = EPQ2                                                      
      W     = EPW                                                       
      SINX  = EPSIN                                                  
      COSX  = EPCOS                                                  
      SINX2 = EPSIN2                                                 
      COSX2 = EPCOS2                                                 
      IR    = EPIREA  
      IBORN = 1
                                                      
      IF(IBORN.EQ.1) THEN
         CALL BORNT(Q2,W,SINX,COSX,SINX2,COSX2,HB,IR,CUT)
         EPH1B   =HB(1)
         EPH2B   =HB(2)
         EPH3B   =HB(3)
         EPH4B   =HB(4)
         EPH5B   =HB(5)
         EPH6B   =HB(6)                                           
      ELSE
         EPH1B=0.
         EPH2B=0.
         EPH3B=0.
         EPH4B=0.
         EPH5B=0.
         EPH6B=0.
      END IF
      
      IR    =  EPIREA         
      YMPI  =  XMPI                                                
      W     =  EPW                                                 
      PIM   =  PIMP                                                
      GK    =  GAMK   
            
      RS11_1535  = EPRES(IR,XMPROT,YMPI,W,LS11_1535 ,WS11_1535 ,.35,0,1,1,PIM,GK,.TRUE.)
      RS11_1650  = EPRES(IR,XMPROT,YMPI,W,LS11_1650 ,WS11_1650 ,.35,0,1,1,PIM,GK,.FALSE.)
      RP11_1440  = EPRES(IR,XMPROT,YMPI,W,LP11_1440 ,WP11_1440 ,.35,1,1,1,PIM,GK,.FALSE.)
      RP11_1710  = EPRES(IR,XMPROT,YMPI,W,LP11_1710 ,WP11_1710 ,.35,1,1,1,PIM,GK,.FALSE.)
      RP13_1720  = EPRES(IR,XMPROT,YMPI,W,LP13_1720 ,WP13_1720 ,.35,1,1,1,PIM,GK,.FALSE.)
      RD13_1520  = EPRES(IR,XMPROT,YMPI,W,LD13_1520 ,WD13_1520 ,.35,2,1,1,PIM,GK,.FALSE.)
      RD13_1700  = EPRES(IR,XMPROT,YMPI,W,LD13_1700 ,WD13_1700 ,.35,2,1,1,PIM,GK,.FALSE.)
      RD15_1675  = EPRES(IR,XMPROT,YMPI,W,LD15_1675 ,WD15_1675 ,.35,2,2,1,PIM,GK,.FALSE.)
      RF15_1680  = EPRES(IR,XMPROT,YMPI,W,LF15_1680 ,WF15_1680 ,.35,3,2,1,PIM,GK,.FALSE.)
      RG17_2190  = EPRES(IR,XMPROT,YMPI,W,LG17_2190 ,WG17_2190 ,.35,4,3,1,PIM,GK,.FALSE.)
      RG19_2250  = EPRES(IR,XMPROT,YMPI,W,LG19_2250 ,WG19_2250 ,.35,4,4,1,PIM,GK,.FALSE.)
      RH19_2220  = EPRES(IR,XMPROT,YMPI,W,LH19_2220 ,WH19_2220 ,.35,5,4,1,PIM,GK,.FALSE.)
      RI111_2600 = EPRES(IR,XMPROT,YMPI,W,LI111_2600,WI111_2600,.35,6,5,1,PIM,GK,.FALSE.)

      RS31_1620  = EPRES(IR,XMPROT,YMPI,W,LS31_1620 ,WS31_1620 ,.35,0,1,3,PIM,GK,.FALSE.)
      RS31_1900  = EPRES(IR,XMPROT,YMPI,W,LS31_1900 ,WS31_1900 ,.35,0,1,3,PIM,GK,.FALSE.)
      RP31_1910  = EPRES(IR,XMPROT,YMPI,W,LP31_1910 ,WP31_1910 ,.35,1,1,3,PIM,GK,.FALSE.)
      RP33_1232  = EPRES(IR,XMPROT,YMPI,W,LP33_1232 ,WP33_1232 ,.185,1,1,3,PIM,GK,.FALSE.)
      RP33_1920  = EPRES(IR,XMPROT,YMPI,W,LP33_1920 ,WP33_1920 ,.35,1,1,3,PIM,GK,.FALSE.)
      RD33_1700  = EPRES(IR,XMPROT,YMPI,W,LD33_1700 ,WD33_1700 ,.35,2,1,3,PIM,GK,.FALSE.)
      RD35_1930  = EPRES(IR,XMPROT,YMPI,W,LD35_1930 ,WD35_1930 ,.35,2,2,3,PIM,GK,.FALSE.)
      RF35_1905  = EPRES(IR,XMPROT,YMPI,W,LF35_1905 ,WF35_1905 ,.35,3,2,3,PIM,GK,.FALSE.)
      RF37_1950  = EPRES(IR,XMPROT,YMPI,W,LF37_1950 ,WF37_1950 ,.35,3,3,3,PIM,GK,.FALSE.)
      RH311_2420 = EPRES(IR,XMPROT,YMPI,W,LH311_2420,WH311_2420,.35,5,5,3,PIM,GK,.FALSE.)
                                                                         
C* THE FOLOWING ARE ONE OR TWO STAR RESONANCES NOT INCLUDED IN THE ABOVE
      RP33_1600  = EPRES(IR,XMPROT,YMPI,W,LP33_1600 ,WP33_1600 ,.35 ,1,1,3,PIM,GK,.FALSE.)
      RF17_1990  = EPRES(IR,XMPROT,YMPI,W,LF17_1990 ,WF17_1990 ,.35 ,3,3,1,PIM,GK,.FALSE.)
      RF15_2000  = EPRES(IR,XMPROT,YMPI,W,LF15_2000 ,WF15_2000 ,.35 ,3,2,1,PIM,GK,.FALSE.)
      RP11_2100  = EPRES(IR,XMPROT,YMPI,W,LP11_2100 ,WP11_2100 ,.35 ,1,1,1,PIM,GK,.FALSE.)
      RF35_2000  = EPRES(IR,XMPROT,YMPI,W,LF35_2000 ,WF35_2000 ,.35 ,3,2,3,PIM,GK,.FALSE.)

C*  THE FOLOWING ARE RESONANCES PREDICTED BY THE QUARK MODEL ONLY
      RP13_1870  = EPRES(IR,XMPROT,YMPI,W,LP13_1870 ,WP13_1870 ,.35 ,1,1,1,PIM,GK,.FALSE.)
      RP31_1925  = EPRES(IR,XMPROT,YMPI,W,LP31_1925 ,WP31_1925 ,.35 ,1,1,3,PIM,GK,.FALSE.)
      RP13_1980  = EPRES(IR,XMPROT,YMPI,W,LP13_1980 ,WP13_1980 ,.35 ,1,1,1,PIM,GK,.FALSE.)
      RF15_1955  = EPRES(IR,XMPROT,YMPI,W,LF15_1955 ,WF15_1955 ,.35 ,3,2,1,PIM,GK,.FALSE.)
      RP13_1955  = EPRES(IR,XMPROT,YMPI,W,LP13_1955 ,WP13_1955 ,.35 ,1,1,1,PIM,GK,.FALSE.)
      RP33_1975  = EPRES(IR,XMPROT,YMPI,W,LP33_1975 ,WP33_1975 ,.35 ,1,1,3,PIM,GK,.FALSE.)
                                                     
      XA0P    = ZERO                            
      XA1P    = ZERO                            
      XA2P    = ZERO                            
      XA3P    = ZERO
      XA4P    = ZERO                            
      XA5P    = ZERO

      XB1P    = ZERO 
      XB2P    = ZERO 
      XB3P    = ZERO
      XC4P    = ZERO                            
      XC5P    = ZERO

      XC0P    = ZERO                            
      XC1P    = ZERO                            
      XC2P    = ZERO
      XC3P    = ZERO
      XC4P    = ZERO                            
      XC5P    = ZERO
                                            
      XA1M    = ZERO
      XA2M    = ZERO
      XA3M    = ZERO
      XA4M    = ZERO
      XA5M    = ZERO
      XA6M    = ZERO

      XB2M    = ZERO
      XB3M    = ZERO                            
      XB4M    = ZERO
      XB5M    = ZERO
      XB6M    = ZERO

      XC1M    = ZERO                            
      XC2M    = ZERO
      XC3M    = ZERO
      XC4M    = ZERO
      XC5M    = ZERO
      XC6M    = ZERO
      
C* IT=1 FOR EXPT ONLY, 
C* IT=2 FOR EXPT+ONE OR TWO STAR FROM QKM
C* IT=3 FOR EXPT+ALL THE OTHER FROM QKM
C* IT=4 FOR ALL FROM QKM
C* C2,C3 ARE USED TO TURN THE QKM PREDICTION OFF AND ON

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
         C2=0.
         C3=0.
         GO TO 35
      ENDIF

      IF(IT.EQ.2) THEN
         C2=1.
         C3=0.
         CALL QKMA(EPIREA,EPQ2)
         CALL EXPA(EPIREA,EPQ2)
         GO TO 25
      ENDIF

      IF (IT.EQ.3) THEN
         CALL QKMA(EPIREA,EPQ2)
         CALL EXPA(EPIREA,EPQ2)
         C2=1.
         C3=1.
      ENDIF
      
      IF(IT.EQ.4) THEN
         C2=0.
         C3=0.
C* here I get ride of the terms which is not used in
c* the EXPT
         CALL QKMA(EPIREA,EPQ2)
C     *         GO TO 35
      ENDIF
                                                                        

C* THIS FOLLOWING RESONANCES ARE PREDICTED BY QK MODEL ONLY
15       QAP13_1870   =RP13_1870   * RAP13_1870
         QBP13_1870   =RP13_1870   * RBP13_1870
         QCP13_1870   =RP13_1870   * 0.0

         QAP31_1925   =RP31_1925   * RAP31_1925
         QBP31_1925   =RP31_1925   * RBP31_1925
         QCP31_1925   =RP31_1925   * 0.0
 
         QAP13_1980   =RP13_1980   * RAP13_1980
         QBP13_1980   =RP13_1980   * RBP13_1980
         QCP13_1980   =RP13_1980   * 0.0
        
         QAF15_1955   =RF15_1955   * RAF15_1955
         QBF15_1955   =RF15_1955   * RBF15_1955
         QCF15_1955   =RF15_1955   * 0.0
         
         QAP13_1955   =RP13_1955   * RAP13_1955
         QBP13_1955   =RP13_1955   * RBP13_1955
         QCP13_1955   =RP13_1955   * 0.0
     
         QAP33_1975   =RP33_1975   * RAP33_1975
         QBP33_1975   =RP33_1975   * RBP33_1975
         QCP33_1975   =RP33_1975   * 0.0
         

C*  THE FOLLOWING RESONANCES ARE SEEN AS ONE OR TWO STAR
25       QAP33_1600   =RP33_1600   * RAP33_1600
         QBP33_1600   =RP33_1600   * RBP33_1600
         QCP33_1600   =RP33_1600   * 0.0

         QAF17_1990   =RF17_1990   * RAF17_1990
         QBF17_1990   =RF17_1990   * RBF17_1990
         QCF17_1990   =RF17_1990   * 0.0
        
         QAF15_2000   =RF15_2000   * RAF15_2000
         QBF15_2000   =RF15_2000   * RBF15_2000
         QCF15_2000   =RF15_2000   * 0.0
    
         QAP11_2100   =RP11_2100   * RAP11_2100
         QBP11_2100   =RP11_2100   * RBP11_2100
         QCP11_2100   =RP11_2100   * 0.0

         QAF35_2000   =RF35_2000   * RAF35_2000
         QBF35_2000   =RF35_2000   * RBF35_2000
         QCF35_2000   =RF35_2000   * RCF35_2000

         
C* THE FOLLWING RESONANCES ARE SEEN BY EXPERIMENT                                                                        
35       QAS11_1535   =RS11_1535   * RAS11_1535 
         QCS11_1535   =RS11_1535   * RCS11_1535

         QAS11_1650   =RS11_1650   * RAS11_1650
         QCS11_1650   =RS11_1650   * RCS11_1650

         QAP11_1440   =RP11_1440   * RAP11_1440  
         QCP11_1440   =RP11_1440   * RCP11_1440  

         QAP13_1720   =RP13_1720   * RAP13_1720
         QBP13_1720   =RP13_1720   * RBP13_1720
         QCP13_1720   =RP13_1720   * RCP13_1720

         QAD13_1520   =RD13_1520   * RAD13_1520
         QBD13_1520   =RD13_1520   * RBD13_1520
         QCD13_1520   =RD13_1520   * RCD13_1520

         QAD13_1700   =RD13_1700   * RAD13_1700
         QBD13_1700   =RD13_1700   * RBD13_1700
         QCD13_1700   =RD13_1700   * RCD13_1700

         QAD15_1675   =RD15_1675   * RAD15_1675
         QBD15_1675   =RD15_1675   * RBD15_1675
         QCD15_1675   =RD15_1675   * RCD15_1675

         QAF15_1680   =RF15_1680   * RAF15_1680  
         QBF15_1680   =RF15_1680   * RBF15_1680
         QCF15_1680   =RF15_1680   * RCF15_1680

         QAG17_2190   =RG17_2190   * RAG17_2190
         QBG17_2190   =RG17_2190   * RBG17_2190
         QCG17_2190   =RG17_2190   * RCG17_2190

         QAG19_2250   =RG19_2250   * RAG19_2250
         QBG19_2250   =RG19_2250   * RBG19_2250
         QCG19_2250   =RG19_2250   * RCG19_2250

         QAH19_2220   =RH19_2220   * RAH19_2220
         QBH19_2220   =RH19_2220   * RBH19_2220
         QCH19_2220   =RH19_2220   * RCH19_2220

         QAI111_2600  =RI111_2600  * RAI111_2600
         QBI111_2600  =RI111_2600  * RBI111_2600
         QCI111_2600  =RI111_2600  * RCI111_2600

         QAS31_1620   =RS31_1620   * RAS31_1620  
         QCS31_1620   =RS31_1620   * RCS31_1620  

         QAS31_1900   =RS31_1900   * RAS31_1900  
         QCS31_1900   =RS31_1900   * RCS31_1900  

         QAP31_1910   =RP31_1910   * RAP31_1910  
         QCP31_1910   =RP31_1910   * RCP31_1910  

         QAP33_1232   =RP33_1232   * RAP33_1232  
         QBP33_1232   =RP33_1232   * RBP33_1232  
         QCP33_1232   =RP33_1232   * RCP33_1232

         QAP33_1920   =RP33_1920   * RAP33_1920  
         QBP33_1920   =RP33_1920   * RBP33_1920  
         QCP33_1920   =RP33_1920   * RCP33_1920  


         QAD33_1700   =RD33_1700   * RAD33_1700  
         QBD33_1700   =RD33_1700   * RBD33_1700  
         QCD33_1700   =RD33_1700   * RCD33_1700  

         QAD35_1930   =RD35_1930   * RAD35_1930
         QBD35_1930   =RD35_1930   * RBD35_1930
         QCD35_1930   =RD35_1930   * RCD35_1930

         QAF35_1905   =RF35_1905   * RAF35_1905
         QBF35_1905   =RF35_1905   * RBF35_1905
         QCF35_1905   =RF35_1905   * RCF35_1905

         QAF37_1950   =RF37_1950   * RAF37_1950
         QBF37_1950   =RF37_1950   * RBF37_1950
         QCF37_1950   =RF37_1950   * RCF37_1950

         QAH311_2420  =RH311_2420  * RAH311_2420
         QBH311_2420  =RH311_2420  * RBH311_2420
         QCH311_2420  =RH311_2420  * RCH311_2420

c	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c	The following were not initialized in my copy of AO (RCM)
	qap11_1710=zero
    	qcp11_1710=zero
c	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
    				
         XA0P=XA0P + QAS11_1535 + QAS11_1650 + QAS31_1620 + QAS31_1900
         XC0P=XC0P + QCS11_1535 + QCS11_1650 + QCS31_1620 + QCS31_1900
                                                       
         XA1M=XA1M + QAP11_1440 + QAP11_1710 + QAP31_1910
     *       +C2*(QAP11_2100)+C3*(QAP31_1925)
         XC1M=XC1M + QCP11_1440 + QCP11_1710 + QCP31_1910
     *       +C2*(QCP11_2100)+C3*(QCP31_1925)
                                                                   
         XA1P=XA1P + QAP13_1720 + QAP33_1232 + QAP33_1920
     *+C2*(QAP33_1600)+C3*(QAP13_1870+QAP13_1980+QAP13_1955+QAP33_1975)
         XB1P=XB1P + QBP13_1720 + QBP33_1232 + QBP33_1920
     *+C2*(QBP33_1600)+C3*(QBP13_1870+QBP13_1980+QBP13_1955+QBP33_1975)
         XC1P=XC1P + QCP13_1720 + QCP33_1232 + QCP33_1920
     *+C2*(QCP33_1600)+C3*(QCP13_1870+QCP13_1980+QCP13_1955+QCP33_1975)
                                                                        
         XA2M = XA2M + QAD13_1520 + QAD13_1700 + QAD33_1700
         XB2M = XB2M + QBD13_1520 + QBD13_1700 + QBD33_1700
         XC2M = XC2M + QCD13_1520 + QCD13_1700 + QCD33_1700
         
         XA2P = XA2P + QAD15_1675 + QAD35_1930
         XB2P = XB2P + QBD15_1675 + QBD35_1930
         XC2P = XC2P + QCD15_1675 + QCD35_1930

         XA3M = XA3M + QAF15_1680 + QAF35_1905
     *        +C2*(QAF15_2000+QAF35_2000)+C3*(QAF15_1955)    
         XB3M = XB3M + QBF15_1680 + QBF35_1905
     *        +C2*(QBF15_2000+QBF35_2000)+C3*(QBF15_1955)    
         XC3M = XC3M + QCF15_1680 + QCF35_1905
     *        +C2*(QCF15_2000+QCF35_2000)+C3*(QCF15_1955)    
                                      
         XA3P = XA3P + QAF37_1950
     *        +C2*(QAF17_1990)
         XB3P = XB3P + QBF37_1950
     *        +C2*(QBF17_1990)
         XC3P = XC3P + QCF37_1950
     *        +C2*(QCF17_1990)

         XA4M = XA4M + QAG17_2190
         XB4M = XB4M + QBG17_2190
         XC4M = XC4M + QCG17_2190
                                  
         XA4P = XA4P + QAG19_2250
         XB4P = XB4P + QBG19_2250
         XC4P = XC4P + QCG19_2250

         XA5M = XA5M + QAH19_2220
         XB5M = XB5M + QBH19_2220
         XC5M = XC5M + QCH19_2220

         XA5P = XA5P + QAH311_2420
         XB5P = XB5P + QBH311_2420
         XC5P = XC5P + QCH311_2420

         XA6M = XA6M + QAI111_2600
         XB6M = XB6M + QBI111_2600
         XC6M = XC6M + QCI111_2600

         SUBA0=XA0P-XA1M 
         SUBA1=XA1P-XA2M 
         SUBA2=XA2P-XA3M 
         SUBA3=XA3P-XA4M 
         SUBA4=XA4P-XA5M 
         SUBA5=XA5P-XA6M 
                         
         SUBC0=XC0P-XC1M 
         SUBC1=XC1P-XC2M 
         SUBC2=XC2P-XC3M 
         SUBC3=XC3P-XC4M 
         SUBC4=XC4P-XC5M 
         SUBC5=XC5P-XC6M 
                         
         SUBB1=XB1P-XB2M 
         SUBB2=XB2P-XB3M 
         SUBB3=XB3P-XB4M 
         SUBB4=XB4P-XB5M 
         SUBB5=XB5P-XB6M 
                         
         ADDA0=XA0P+XA1M 
         ADDA1=XA1P+XA2M 
         ADDA2=XA2P+XA3M 
         ADDA3=XA3P+XA4M 
         ADDA4=XA4P+XA5M 
         ADDA5=XA5P+XA6M 
                         
         ADDC0=XC0P+XC1M 
         ADDC1=XC1P+XC2M 
         ADDC2=XC2P+XC3M 
         ADDC3=XC3P+XC4M 
         ADDC4=XC4P+XC5M 
         ADDC5=XC5P+XC6M 
                         
         ADDB1=XB1P+XB2M 
         ADDB2=XB2P+XB3M 
         ADDB3=XB3P+XB4M 
         ADDB4=XB4P+XB5M 
         ADDB5=XB5P+XB6M 
c
C   Partial wave expansion
C
      EPH1   = SUBB1*(       -P2S2   )                                  
     *       + SUBB2*(P2S2   -P3S2   )                                  
     *       + SUBB3*(P3S2   -P4S2   )                                  
     *       + SUBB4*(P4S2   -P5S2   )                                  
     *       + SUBB5*(P5S2   -P6S2   )
      EPH1   = EPH1   * EPSIN   * EPCOS2   /SQ2                           
                                                                                                               
      EPH2  =  SUBA0   *(       -P1S   )                                
     *       + SUBA1   *(P1S    -P2S   )                                
     *       + SUBA2   *(P2S    -P3S   )                                
     *       + SUBA3   *(P3S    -P4S   )
     *       + SUBA4   *(P4S    -P5S   )
     *       + SUBA5   *(P5S    -P6S   )
      EPH2  = EPH2    * EPCOS2   * SQ2

      EPH3   =  ADDB1    *(       +P2S2   )                             
     *       +  ADDB2    *(P2S2   +P3S2   )                             
     *       +  ADDB3    *(P3S2   +P4S2   )
     *       +  ADDB4    *(P4S2   +P5S2   )
     *       +  ADDB5    *(P5S2   +P6S2   )
      EPH3   = EPH3   *  EPSIN   * EPSIN2   /SQ2     
                                                                        
      EPH4  =  ADDA0     * (       +P1S   )                              
     *       + ADDA1     * (P1S    +P2S   )                             
     *       + ADDA2     * (P2S    +P3S   )                             
     *       + ADDA3     * (P3S    +P4S   )
     *       + ADDA4     * (P4S    +P5S   )
     *       + ADDA5     * (P5S    +P6S   )
      EPH4   = EPH4   * EPSIN2   * SQ2              
                                                                        
      EPH5   = SUBC0     *(       -P1S   )                              
     *       + SUBC1     *(P1S    -P2S   )                              
     *       + SUBC2     *(P2S    -P3S   )                              
     *       + SUBC3     *(P3S    -P4S   )
     *       + SUBC4     *(P4S    -P5S   )
     *       + SUBC5     *(P5S    -P6S   )
      EPH5   = EPH5   * EPCOS2   * SQ2                
                                                                        
                                                                        
      EPH6   = ADDC0     * (       +P1S   )                              
     *       + ADDC1     * (P1S    +P2S   )                             
     *       + ADDC2     * (P2S    +P3S   )                             
     *       + ADDC3     * (P3S    +P4S   )
     *       + ADDC4     * (P4S    +P5S   )
     *       + ADDC5     * (P5S    +P6S   )
      EPH6   =  EPH6   * EPSIN2   * SQ2    
      
      vgamP = ((epw**2+epq2+0.938**2)/2.0/epw)**2-0.938**2
      vgamP = sqrt(vgamP)
      vgamE = (epw**2-0.938**2-epq2)/(2*epw)    
c      ekin = sqrt(epq2)/sqrt((epw-vgamE)**2-0.938**2)
      ekin = vgamE/sqrt(epq2+vgamE**2)
           
c       print *, epw,epq2,epeps,epcos,epphi,ekin,vgamE/vgamP
           
      EPH1 = EPH1 - CMPLX(EPH1B,0.)                             
      EPH2 = EPH2 - CMPLX(EPH2B,0.)                             
      EPH3 = EPH3 - CMPLX(EPH3B,0.)                             
      EPH4 = EPH4 - CMPLX(EPH4B,0.)                             
      EPH5 = EPH5 - CMPLX(EPH5B,0.)*ekin             
      EPH6 = EPH6 - CMPLX(EPH6B,0.)*ekin

c      EPH1 = -CMPLX(EPH1B,0.)                             
c      EPH2 = -CMPLX(EPH2B,0.)                             
c      EPH3 = -CMPLX(EPH3B,0.)                             
c      EPH4 = -CMPLX(EPH4B,0.)                             
c      EPH5 = -CMPLX(EPH5B,0.)*ekin             
c      EPH6 = -CMPLX(EPH6B,0.)*ekin
               
c      rnorm1 = 0.141383	! Convert from microbarns to 10**-3/m_pi^2 (MAID tables)
      rnorm1 = 1.0	
     
      reph(1)=-real(eph1)/rnorm1
      reph(2)=-real(eph2)/rnorm1
      reph(3)=-real(eph3)/rnorm1
      reph(4)=-real(eph4)/rnorm1
      reph(5)=-real(eph5)/rnorm1
      reph(6)=-real(eph6)/rnorm1
         
      ieph(1)=-imag(eph1)/rnorm1
      ieph(2)=-imag(eph2)/rnorm1
      ieph(3)=-imag(eph3)/rnorm1
      ieph(4)=-imag(eph4)/rnorm1
      ieph(5)=-imag(eph5)/rnorm1
      ieph(6)=-imag(eph6)/rnorm1
      
c      if(epcos.lt.0.11.and.epcos.gt.0.09.and.epphi.eq.15) then
c      print *, EPW,EPQ2,EPCOS,EPPHI,epeps
c      print *, sigu,sigl,sigt,sigi
c      print *,eph1,eph2,eph3,eph4,eph5,eph6
c      print *,reph
c      print *,eph1b,eph2b,eph3b,eph4b,eph5b,eph6b
c      print *, ' '
c      endif
      
      sig_t  = (cabs(eph1)**2+cabs(eph2)**2
     1         +cabs(eph3)**2+cabs(eph4)**2)/2.
      sig_l  =  cabs(eph5)**2+cabs(eph6)**2
      sig_tt =  real(-conjg(eph1)*eph4+conjg(eph2)*eph3)
      sig_lt =  real(conjg(eph5)*(eph1-eph4)  + 
     1               conjg(eph6)*(eph2+eph3))/sqrt(2.0)
      sig_ltp = aimag(conjg(eph5)*(eph4-eph1)  - 
     1                conjg(eph6)*(eph2+eph3))/sqrt(2.0)
      sig_t_y   = -aimag(conjg(eph1)*eph2+conjg(eph3)*eph4)
      sig_l_y   = -2*aimag(conjg(eph5)*eph6)
      sig_lt_x  = aimag(conjg(eph5)*(eph3-eph2) - 
     1                  conjg(eph6)*(eph1+eph4))/sqrt(2.)
      sig_lt_y  = aimag(-conjg(eph5)*(eph3+eph2) + 
     1                   conjg(eph6)*(eph1-eph4))/sqrt(2.)
      sig_lt_z  = aimag(-conjg(eph5)*(eph1+eph4) + 
     1                   conjg(eph6)*(eph2-eph3))/sqrt(2.)
      sig_tt_x  = aimag(conjg(eph1)*eph3-conjg(eph2)*eph4)
      sig_tt_y  = -aimag(conjg(eph1)*eph3+conjg(eph2)*eph4)
      sig_tt_z  = -aimag(conjg(eph1)*eph4+conjg(eph2)*eph3)
      sig_ltp_x = real(conjg(eph5)*(-eph3+eph2) + 
     1                 conjg(eph6)*(eph1+eph4))/sqrt(2.)
      sig_ltp_y = real(-conjg(eph5)*(eph3+eph2) + 
     1                  conjg(eph6)*(eph1-eph4))/sqrt(2.)
      sig_ltp_z = real(conjg(eph5)*(eph1+eph4) + 
     1                 conjg(eph6)*(eph3-eph2))/sqrt(2.)
      sig_ttp_x = real(conjg(eph1)*eph2+conjg(eph3)*eph4)
      sig_ttp_z = (cabs(eph1)**2-cabs(eph2)**2
     1            +cabs(eph3)**2-cabs(eph4)**2)/2.
                                      
      ekin = 1.0                                                                        
      
      vt   = 1.0
      vl   = epeps * ekin**2
      vtt  = epeps
      vlt  = sqrt(2*epeps*(1+epeps))*ekin
      vltp = sqrt(2*epeps*(1-epeps))*ekin
      vttp = sqrt(1-epeps**2)
             
      cph  = csd(epphi)
      sph  = snd(epphi)
      c2ph = csd(2*epphi)
      s2ph = snd(2*epphi)
            
      sigu = fkt *               sig_t
      sigl = fkt * vl          * sig_l 
      sigt = fkt * vtt  * c2ph * sig_tt
      sigi = fkt * vlt  * cph  * sig_lt      
      sige = fkt * vltp * sph  * sig_ltp
      
c      print *, sigu,sigl
c      print *, ' '
      
      sige = pol_elec * sige 
       
      pt  = e_elec_scatt*sin(theta_elec)/sqrt(epq2+enue**2)
      pl  = sqrt(1-pt**2)
      pn  = -pt * sph
      pt  =  pt * cph
      
      sigma_t_y   = fkt  * vt   * pn * sig_t_y
      sigma_l_y   = fkt  * vl   * pn * sig_l_y          
      sigma_lt_x  = fkt  * vlt  * pt * sig_lt_x  * sph  
      sigma_lt_y  = fkt  * vlt  * pn * sig_lt_y  * cph  
      sigma_lt_z  = fkt  * vlt  * pl * sig_lt_z  * sph  
      sigma_tt_x  = fkt  * vtt  * pt * sig_tt_x  * s2ph
      sigma_tt_y  = fkt  * vtt  * pn * sig_tt_y  * c2ph
      sigma_tt_z  = fkt  * vtt  * pl * sig_tt_z  * s2ph
      sigma_ltp_x = fkt  * vltp * pt * sig_ltp_x * cph  
      sigma_ltp_y = fkt  * vltp * pn * sig_ltp_y * sph  
      sigma_ltp_z = fkt  * vltp * pl * sig_ltp_z * cph  
      sigma_ttp_x = fkt  * vttp * pt * sig_ttp_x
      sigma_ttp_z = fkt  * vttp * pl * sig_ttp_z
            
      sigpt  = sigma_t_y+sigma_l_y
      sigpt  = sigpt+sigma_lt_x+sigma_lt_y+sigma_lt_z
      sigpt  = sigpt+sigma_tt_x+sigma_tt_y+sigma_tt_z
      
      siget  = sigma_ltp_x+sigma_ltp_y+sigma_ltp_z
      siget  = siget+sigma_ttp_x+sigma_ttp_z
      
      sigpt  =  pol_targ*sigpt
      siget  =  pol_elec*pol_targ*siget
              
99    sigmao = sigu + sigl + sigt + sigi + sige + sigpt + siget
           
      RETURN                                                       
      END                                                          

      function snd(x)

      real pi,x,snd
      data pi/3.1415926/

      snd=sin(x*pi/180.)

      return
      end

      function csd(x)

      real pi,x,csd
      data pi/3.1415926/

      csd=cos(x*pi/180.)

      return
      end
