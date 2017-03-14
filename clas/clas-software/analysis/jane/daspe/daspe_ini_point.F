      SUBROUTINE DASPE_INI_point(Q2)
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine DASPE_INIT
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:         Created:  Wed Dec 11 17:16:27 EST 1996
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "gpar.inc"
#include "resonances.inc"
#include "multipoles.inc"
#include "fit_par.inc"
#include "delta_dat.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      REAL Q2
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile$')
      PARAMETER (CREVIS = '$Revision$')
      PARAMETER (CSTATE = '$State$')
      PARAMETER (CDATE = '$Date$')
      PARAMETER (CAUTHO = '$Author$')
      DATA CRCSID/   
     1'$Id$'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='DASPE_INI_POINT')
      PARAMETER (CRAUTH='')
c
c  Local User defined variables:
c  -----------------------------
c      INTEGER NWRITE
      INTEGER i,j,k1
      REAL mpp,mp0
      REAL GM,mr,Gamma,etta,c,kr,lr,jr,qr,Er,a
      REAL A12,A32,S12,RSM,REM
	REAL AA,BB,CC,XX,YY,ZZ,SINS,SIND,COSS,COSD
c_end_var
c
c  executable code for routine DASPE_INI_POINT:
c----6----------------------------------------------------------------72
c
      pi=2.*acos(0.E0)
      a2=sqrt(2.E0)
      mn=0.93827
      mpp=0.13957
      mp0=0.13498
	mp=(mpp+mp0)/2.
      mn22=mn**2
      mp22=mp**2
      mn2=2.*mn
      meta=0.547
      m2pion=2.*mp
c
        COSS=0.866
        SINS=0.5
c        COSD=0.94
c        SIND=0.342
        COSD=0.99
        SIND=0.11
c
C P11(1440) 
        mr=1.44
        Gamma=0.35
        etta=0.6
       c=-sqrt(1./3.)
        A12=-68.7866+21.3966*Q2+79.8415*sqrt(Q2)
        A12=A12/(1.-0.7178*Q2**2+0.5663*Q2**2*sqrt(Q2))
        A12=A12*ap11
        S12=31.19227+3.53338*Q2
        S12=S12/(1.-0.278265*Q2**2+0.3677575*Q2**2*sqrt(Q2))
        S12=S12*sp11
        kr=(mr**2-mn**2)/(2.*mr)
        Er=(mr**2+mn**2-mp**2)/(2.*mr)
        qr=sqrt(Er**2-mn**2)
        a=sqrt((kr*mn*etta)/(2.*pi*qr*mr*Gamma))/50.6
        AM1(1,1)=A12*(2.*a*c)/2.
        AS1(1,1)=-S12*(2.*a*c)/sqrt(2.)
      AM1(2,1)= 0.
      AS1(2,1)= 0.
C S11(1535) 
        lr=0.
        jr=1./2.
       c=-sqrt(1./3.)
        mr=1.535
        Gamma=0.15
        etta=0.485
        A12=92.5029+1.45023*Q2
        A12=A12/(1.+0.1095*Q2**2-0.000322*Q2**2*sqrt(Q2))
	XX=A12*6./COSS 
        A12=A12*as11
        S12=-9.758811-4.231412*Q2
        S12=S12/(1.-0.7341952*Q2**2+0.5087887*Q2**2*sqrt(Q2))
        S12=S12*ss11
        kr=(mr**2-mn**2)/(2.*mr)
        Er=(mr**2+mn**2-mp**2)/(2.*mr)
        qr=sqrt(Er**2-mn**2)
        a=sqrt((kr*mn*etta)/((2.*jr+1.)*pi*qr*mr*Gamma))/50.6
        AE1(1,2)=-A12*(2.*a*c)/(2.*(lr+1.))
        AS1(1,2)=-S12*(2.*a*c)/sqrt(2.)/(lr+1.)
      AE1(2,2)= 0.
      AS1(2,2)= 0.
C D13(1520) 
        lr=2.
        lr=lr-1.
        jr=3./2.
       c=-sqrt(1./3.)
        mr=1.52
        Gamma=0.12
        etta=0.5
        A12= -23.357-151.199533*Q2
        A12=A12/(1.+2.01489898*Q2**2-0.2654327*Q2**2*sqrt(Q2))
        YY=A12*6.*sqrt(2.)/COSD
        A12=A12*a1d13
	A32=3.322979*Q2-2.0339966*Q2**2+1.622563*Q2**2*sqrt(Q2)
        A32=162.458285/(1.+A32)
        ZZ=A32*2.*sqrt(6.)/COSD
        A32=A32*a3d13
	S12=1.7269289*Q2-3.25975587*Q2**2+2.905189*Q2**2*sqrt(Q2)
        S12=-67.3195419/(1.+S12)
        S12=S12*sd13
        kr=(mr**2-mn**2)/(2.*mr)
        Er=(mr**2+mn**2-mp**2)/(2.*mr)
        qr=sqrt(Er**2-mn**2)
        a=sqrt((kr*mn*etta)/((2.*jr+1.)*pi*qr*mr*Gamma))/50.6
        A12=(2.*a*c)*A12
        A32=-A32*(2.*a*c)/sqrt(lr*(lr+2.))
        AE1(1,3)=((lr+2.)*A32-A12)/(2.*(lr+1.))
        AM1(1,3)=(A12+lr*A32)/(2.*(lr+1.))
        AS1(1,3)=-S12*(2.*a*c)/sqrt(2.)/(lr+1.)
      AE1(2,3)=0.
      AM1(2,3)=0.
      AS1(2,3)=0.
        BB=(XX-YY)/3.
        AA=BB+(ZZ+YY)/2.
        CC=-BB+(ZZ-YY)/2.
C S11(1650) 
        lr=0.
        jr=1./2.
       c=-sqrt(1./3.)
        mr=1.655
        Gamma=0.165
        etta=0.75
	A12=(AA+BB-CC)*SINS/6.
        A12=A12*cas11
        S12=css11
        kr=(mr**2-mn**2)/(2.*mr)
        Er=(mr**2+mn**2-mp**2)/(2.*mr)
        qr=sqrt(Er**2-mn**2)
        a=sqrt((kr*mn*etta)/((2.*jr+1.)*pi*qr*mr*Gamma))/50.6
        AE1(1,5)=-A12*(2.*a*c)/(2.*(lr+1.))
        AS1(1,5)=-S12*(2.*a*c)/sqrt(2.)/(lr+1.)
        AE1(2,5)=0.
        AS1(2,5)=0.
C S31(1620) 
        lr=0.
        jr=1./2.
       c=sqrt(2./3.)
        mr=1.63
        Gamma=0.145
        etta=0.25
	A12=(3.*AA-BB+CC)/18.
        A12=A12*cas31
        S12=-30.0*css31
        kr=(mr**2-mn**2)/(2.*mr)
        Er=(mr**2+mn**2-mp**2)/(2.*mr)
        qr=sqrt(Er**2-mn**2)
        a=sqrt((kr*mn*etta)/((2.*jr+1.)*pi*qr*mr*Gamma))/50.6
        AE3(5)=-A12*(2.*a*c)/(2.*(lr+1.))
        AS3(5)=-S12*(2.*a*c)/sqrt(2.)/(lr+1.)
C F15(1680)  
        lr=3.
        lr=lr-1.
        jr=5./2.
       c=-sqrt(1./3.)
        mr=1.685
        Gamma=0.130
        etta=0.65
	A12=-(15.+70.*Q2)/(1.+1.8*Q2**2)
        A12=A12*a1f15
	A32=3.322979*Q2-2.0339966*Q2**2+1.622563*Q2**2*sqrt(Q2)
        A32=0.8*162.458285/(1.+A32)
        A32=A32*a3f15
	S12=-(10.+30.*Q2)/(1.+1.8*Q2**2)
        S12=S12*sf15
        kr=(mr**2-mn**2)/(2.*mr)
        Er=(mr**2+mn**2-mp**2)/(2.*mr)
        qr=sqrt(Er**2-mn**2)
        a=sqrt((kr*mn*etta)/((2.*jr+1.)*pi*qr*mr*Gamma))/50.6
        A12=(2.*a*c)*A12
        A32=-A32*(2.*a*c)/sqrt(lr*(lr+2.))
        AE1(1,6)=((lr+2.)*A32-A12)/(2.*(lr+1.))
        AM1(1,6)=(A12+lr*A32)/(2.*(lr+1.))
        AS1(1,6)=-S12*(2.*a*c)/sqrt(2.)/(lr+1.)
        AE1(2,6)=0.
        AM1(2,6)=0.
        AS1(2,6)=0.
C D33(1700) 
        lr=2.
        lr=lr-1.
        jr=3./2.
       c=sqrt(2./3.)
        mr=1.7
        Gamma=0.3
        etta=0.15
        A12=(3.*AA+2.*BB+CC)/18./sqrt(2.)
        A32=(3.*AA-CC)/6./sqrt(6.)
        A12=A12*ca1d33
        A32=A32*ca3d33
        S12=10.0*csd33
        kr=(mr**2-mn**2)/(2.*mr)
        Er=(mr**2+mn**2-mp**2)/(2.*mr)
        qr=sqrt(Er**2-mn**2)
        a=sqrt((kr*mn*etta)/((2.*jr+1.)*pi*qr*mr*Gamma))/50.6
        A12=(2.*a*c)*A12
        A32=-A32*(2.*a*c)/sqrt(lr*(lr+2.))
        AE3(2)=((lr+2.)*A32-A12)/(2.*(lr+1.))
        AM3(2)=(A12+lr*A32)/(2.*(lr+1.))
        AS3(2)=-S12*(2.*a*c)/sqrt(2.)/(lr+1.)
C D13(1700)
        lr=2.
        lr=lr-1.
        jr=3./2.
       c=-sqrt(1./3.)
        mr=1.7
        Gamma=0.1
        etta=0.1
        A12=-(AA-2.*BB-CC)*SIND/6./sqrt(2.)
        A32=-(AA+CC)*SIND/sqrt(6.)/2.
        A12=A12*ca1d13
        A32=A32*ca3d13
        S12=10.0*csd13
        kr=(mr**2-mn**2)/(2.*mr)
        Er=(mr**2+mn**2-mp**2)/(2.*mr)
        qr=sqrt(Er**2-mn**2)
        a=sqrt((kr*mn*etta)/((2.*jr+1.)*pi*qr*mr*Gamma))/50.6
        A12=(2.*a*c)*A12
        A32=-A32*(2.*a*c)/sqrt(lr*(lr+2.))
        AM1(1,4)=(A12+lr*A32)/(2.*(lr+1.))
        AE1(1,4)=((lr+2.)*A32-A12)/(2.*(lr+1.))
        AS1(1,4)=-S12*(2.*a*c)/sqrt(2.)/(lr+1.)
        AE1(2,4)=0.
        AM1(2,4)=0.
        AS1(2,4)=0.
C      P11(1710)
        mr=1.71
        Gamma=0.1
        etta=0.15
       c=-sqrt(1./3.)
        A12=cap11
        S12=csp11
        kr=(mr**2-mn**2)/(2.*mr)
        Er=(mr**2+mn**2-mp**2)/(2.*mr)
        qr=sqrt(Er**2-mn**2)
        a=sqrt((kr*mn*etta)/(2.*pi*qr*mr*Gamma))/50.6
        AM1(1,10)=A12*(2.*a*c)/2.
        AS1(1,10)=-S12*(2.*a*c)/sqrt(2.)
        AM1(2,10)=0.
        AS1(2,10)=0.
C D15(1675)
        lr=2.
        jr=5./2.
       c=-sqrt(1./3.)
        mr=1.675
        Gamma=0.15
        etta=0.4
        A12=a1d15
        A32=a3d15
        S12=sd15
        kr=(mr**2-mn**2)/(2.*mr)
        Er=(mr**2+mn**2-mp**2)/(2.*mr)
        qr=sqrt(Er**2-mn**2)
        a=sqrt((kr*mn*etta)/((2.*jr+1.)*pi*qr*mr*Gamma))/50.6
        A12=-(2.*a*c)*A12
        A32=A32*(2.*a*c)/sqrt(lr*(lr+2.))
        AM1(1,9)=(A12-(lr+2.)*A32)/(2.*(lr+1.))
        AE1(1,9)=(lr*A32+A12)/(2.*(lr+1.))
        AS1(1,9)=-S12*(2.*a*c)/sqrt(2.)/(lr+1.)
        AE1(2,9)=0.
        AM1(2,9)=0.
        AS1(2,9)=0.
C P13(1720)
        lr=1.
        jr=3./2.
       c=-sqrt(1./3.)
        mr=1.72
        Gamma=0.2
        etta=0.15
	A12=(20.+130.*Q2)/(1.+2.8*Q2**2-0.4*Q2**2*sqrt(Q2))
        A12=A12*a1p13
	A32=-(20.+220.*Q2)/(1.+4.*Q2**2)
        A32=A32*a3p13
        S12=sp13
        kr=(mr**2-mn**2)/(2.*mr)
        Er=(mr**2+mn**2-mp**2)/(2.*mr)
        qr=sqrt(Er**2-mn**2)
        a=sqrt((kr*mn*etta)/((2.*jr+1.)*pi*qr*mr*Gamma))/50.6
        A12=-(2.*a*c)*A12
        A32=A32*(2.*a*c)/sqrt(lr*(lr+2.))
        AM1(1,7)=(A12-(lr+2.)*A32)/(2.*(lr+1.))
        AE1(1,7)=(lr*A32+A12)/(2.*(lr+1.))
        AS1(1,7)=-S12*(2.*a*c)/sqrt(2.)/(lr+1.)
        AE1(2,7)=0.
        AM1(2,7)=0.
        AS1(2,7)=0.
C P33(1600)
        lr=1.
        jr=3./2.
       c=sqrt(2./3.)
        mr=1.6
        Gamma=0.3
        etta=0.2
        A12=a1p33
        A32=a3p33
        S12=sp33
        kr=(mr**2-mn**2)/(2.*mr)
        Er=(mr**2+mn**2-mp**2)/(2.*mr)
        qr=sqrt(Er**2-mn**2)
        a=sqrt((kr*mn*etta)/((2.*jr+1.)*pi*qr*mr*Gamma))/50.6
        A12=-(2.*a*c)*A12
        A32=A32*(2.*a*c)/sqrt(lr*(lr+2.))
        AM3(4)=(A12-(lr+2.)*A32)/(2.*(lr+1.))
        AE3(4)=(lr*A32+A12)/(2.*(lr+1.))
        AS3(4)=-S12*(2.*a*c)/sqrt(2.)/(lr+1.)
C-Resonances with (****),(***),(**) are included
c-(1)   P33(1232)
c-(2)   D33(1700)  
c-(4)   P33(1600)   
c-(5)   S31(1620)
c-(21)  P11(1440)  
c-(22)  S11(1535)  
c-(23)  D13(1520)  
c-(24)  D13(1700)  
c-(25)  S11(1650) 
c-(26)  F15(1680)
c-(27)  P13(1720)  
c-(29)  D15(1675)
c-(30)  P11(1710)  
C
      MRES(1)=1.2283
      MRES(2)=1.7
      MRES(3)=1.93
      MRES(4)=1.6
      MRES(5)=1.63
      MRES(6)=1.905
      MRES(7)=1.91
      MRES(8)=1.925
      MRES(9)=1.9
      MRES(10)=1.92
      MRES(11)=1.93
      MRES(12)=1.94
      MRES(13)=2.
C      
      MRES(21)=1.44
      MRES(22)=1.535
      MRES(23)=1.52
      MRES(24)=1.7
      MRES(25)=1.655
      MRES(26)=1.685
      MRES(27)=1.72
      MRES(28)=1.97
      MRES(29)=1.675
      MRES(30)=1.71
      MRES(31)=1.9
      MRES(32)=1.99
      MRES(33)=2.
C
      GRES(1)=0.11
      GRES(2)=0.3
      GRES(3)=0.3
      GRES(4)=0.35
      GRES(5)=0.145
      GRES(6)=0.35
      GRES(7)=0.25
      GRES(8)=0.3
      GRES(9)=0.2
      GRES(10)=0.2
      GRES(11)=0.35
      GRES(12)=0.2
      GRES(13)=0.3
C
      GRES(21)=0.45
      GRES(22)=0.15
      GRES(23)=0.12
      GRES(24)=0.1
      GRES(25)=0.165
      GRES(26)=0.13
      GRES(27)=0.2
      GRES(28)=0.3
      GRES(29)=0.15
      GRES(30)=0.1
      GRES(31)=0.5
      GRES(32)=0.35
      GRES(33)=0.2
C
         XRES(1)=0.2
      do  i=2,40
         XRES(i)=0.5
      end do
C
      eta(1)=1.
      eta(2)=0.15
      eta(3)=0.42
      eta(4)=0.2  
      eta(5)=0.25
      eta(6)=0.1 
      eta(7)=0.18 
      eta(8)=0.01 
      eta(9)=0.13 
      eta(10)=0.16
      eta(11)=0.12
      eta(12)=0.01
      eta(13)=0.01

      eta(21)=0.6
      eta(22)=0.485
      eta(23)=0.5
      eta(24)=0.1
      eta(25)=0.75
      eta(26)=0.65
      eta(27)=0.15
      eta(28)=0.1
      eta(29)=0.4
      eta(30)=0.15
      eta(31)=0.2
      eta(32)=0.06
      eta(33)=0.01

c	For I=3/2 AM3=2*M(3/2)/3
c	For I=1/2 AM1(1,i)=M(1/2)/3,	AM1(2,i)=M(0)
c
C       P33(1232) 
C	G_M(Ash)/(3G_d), is given in Fig.10
        GM=exp(-0.21*Q2)/(1.+0.027334*Q2-0.008607*Q2**2)
C	G_M(Ash)
        GM=3.*GM/(1.+Q2/0.71)**2
        GM=GM*(sqrt(((2.3933+Q2)/2.46)**2-0.88)*6.568*1.013)*1.02
      AM3(1)=cm*GM/1.5
	REM=-0.02
      AE3(1)=REM*ce*AM3(1)
	RSM=-6.066-8.5639*Q2+2.3705755*Q2**2
	RSM=(RSM+5.807*sqrt(Q2)-0.75445*Q2**2*sqrt(Q2))/100.
      AS3(1)=RSM*cs*AM3(1)
C
c-(6)   F35(1905)   
c-(7)   P31(1910)  
c-(8)   P31(1925) missing resonance
c-(9)   S31(1900) 
c-(10)  P33(1920)+ missing resonance (1975)
c-(11)  D35(1930)   
c-(12)  D33(1940) 
c-(13)  F35(2000)  
C	
c-(3)   F37(1950)
        lr=3.
        jr=7./2.
       c=sqrt(2./3.)
        mr=1.95
        Gamma=0.3
        etta=0.9
        A12=0.
        A32=0.
        S12=0.
        kr=(mr**2-mn**2)/(2.*mr)
        Er=(mr**2+mn**2-mp**2)/(2.*mr)
        qr=sqrt(Er**2-mn**2)
        a=sqrt((kr*mn*etta)/((2.*jr+1.)*pi*qr*mr*Gamma))/50.6
        A12=-(2.*a*c)*A12
        A32=A32*(2.*a*c)/sqrt(lr*(lr+2.))
        AM3(3)=(A12-(lr+2.)*A32)/(2.*(lr+1.))
        AE3(3)=(lr*A32+A12)/(2.*(lr+1.))
        AS3(3)=-S12*(2.*a*c)/sqrt(2.)/(lr+1.)
c
c         ae3(3)=40.0
c         am3(3)=40.0
c         as3(3)=40.0
      DO k1=6,13
         as3(k1)=0.
         ae3(k1)=0.
         am3(k1)=0.
      ENDDO
C	
c-(32)  F17(1990)
c-(33)  F15(2000)+ missing resonance (1955)
C
c-(28)  D13(2080)   
c-(31)  P13(1900)+3 missing resonances (1870),(1980),(1955)
      DO i=1,2
            am1(i,8)=0.
            ae1(i,8)=0.
            AS1(i,8)=0.
      ENDDO
      DO i=1,2
         DO j=11,13
            am1(i,j)=0.
            ae1(i,j)=0.
            AS1(i,j)=0.
         ENDDO
      ENDDO
      RETURN
      END
c
c------------------------------------------------------------------------------




