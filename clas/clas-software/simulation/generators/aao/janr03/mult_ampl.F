      SUBROUTINE MULT_AMPL(S,Q2,kgamma,qpion)

      IMPLICIT NONE
      SAVE

#include "inpar.inc"
#include "gpar.inc"
#include "back_ampl.inc"
#include "multipoles.inc"
#include "ebac.inc"
#include "amplitudes.inc"
#include "fit_par.inc"
#include "highmult.inc"

      REAL S, Q2,kgamma,qpion

      INTEGER NWRITE
      REAL WWLAST
      DATA NWRITE/0/wWlast/0/
      REAL Br1(2,9),Br3(2,8)
      REAL EBr1(2,9),EBr3(2,9)
      REAL BM33,BE23,B3M,B3Mn,B6E,B6M,B6Mn
      REAL BM11,w1
      REAL BrM1(2,2,9),BrE1(2,2,9),BrS1(2,2,9)
      REAL BrM3(2,8),BrE3(2,8),BrS3(2,8)
      REAL RMP33,IMP33,coeff
      REAL XIN,XIN1,elab,WW,Suppress
      REAL YOUTSimr,YOUTSimi 
      REAL mpion(2),qpi(2),ratio
      INTEGER k,i,j,lun
            
      CALL breit_ampl(S,Q2,kgamma,qpion)
      CALL background(S,Q2)
      CALL back_high(S)
      
      WW = sqrt(s)
      mpion(1) = 0.13498
      mpion(2) = 0.13957  
      
      do i=1,2
        qpi(i)=sqrt(((s-mn22+mpion(i)**2)/2./WW)**2-mpion(i)**2)
      enddo
      
      do j=1,9
        do i=1,2
          EBr1(i,j)=1.0
          EBr3(i,j)=1.0
        enddo
      enddo
      
c Used below to correct pi+ multipoles for pion mass ratio
c All calculations in isospin channels use pi0 mass by default
      
      ratio = qpi(2)/qpi(1)
      
      XIN   = WW
      elab  = (s-mn**2)/2./mn
      
      if (WW.le.1.34) go to 366
      
      go to 367
      
366   CALL interp1(WSa,simr,13,3,XIN,YOUTSimr)
      CALL interp1(WSa,simi,13,3,XIN,YOUTSimi)
      
      do j=1,2
         BreitE1(2,j,2)=BreitE1(1,j,2)*YOUTSimr/(1.-YOUTSimi)
         BreitS1(2,j,2)=BreitS1(1,j,2)*YOUTSimr/(1.-YOUTSimi)
      enddo
      
367   continue

c Breit-Wigner amplitudes
c I=1/2
      
      do i=1,2 		! real(1) imaginary(2)
         do j=1,2	! pA(1/2) (j=1) nA(1/2) (j=2)
            BrM1(i,j,6)=BreitM1(i,j,6)+BreitM1(i,j,13)
            BrE1(i,j,6)=BreitE1(i,j,6)+BreitE1(i,j,13)
            BrS1(i,j,6)=BreitS1(i,j,6)+BreitS1(i,j,13)
            BrM1(i,j,7)=BreitM1(i,j,7)+BreitM1(i,j,11)    
            BrE1(i,j,7)=BreitE1(i,j,7)+BreitE1(i,j,11)    
            BrS1(i,j,7)=BreitS1(i,j,7)+BreitS1(i,j,11)    
            BrE1(i,j,2)=BreitE1(i,j,2)+BreitE1(i,j,5)
            BrS1(i,j,2)=BreitS1(i,j,2)+BreitS1(i,j,5)
            BrM1(i,j,1)=BreitM1(i,j,1)+BreitM1(i,j,10)
            BrS1(i,j,1)=BreitS1(i,j,1)+BreitS1(i,j,10)
            BrM1(i,j,3)=BreitM1(i,j,3)+BreitM1(i,j,4)+BreitM1(i,j,12)
            BrE1(i,j,3)=BreitE1(i,j,3)+BreitE1(i,j,4)+BreitE1(i,j,12)
            BrS1(i,j,3)=BreitS1(i,j,3)+BreitS1(i,j,4)+BreitS1(i,j,12)
            BrM1(i,j,9)=BreitM1(i,j,9)
            BrE1(i,j,9)=BreitE1(i,j,9)
            BrS1(i,j,9)=BreitS1(i,j,9)
            BrM1(i,j,8)=BreitM1(i,j,8)
            BrE1(i,j,8)=BreitE1(i,j,8)
            BrS1(i,j,8)=BreitS1(i,j,8)
         enddo
      enddo
      
c 1+ P33 M1+
      XIN1=XIN
      CALL interp(WSa,mp33r,51,4,XIN1,RMP33)
      CALL interp(WSa,mp33i,51,4,XIN1,IMP33)
c 1- P11 1(1) 
      CALL interp(WSa,p11r,51,3,XIN,Br1(1,1))
      CALL interp(WSa,p11i,51,3,XIN,Br1(2,1))
      if (nebac.gt.0) then
      CALL interp(EWSa,ep11r,48,3,XIN,EBr1(1,1))
      CALL interp(EWSa,ep11i,48,3,XIN,EBr1(2,1))
      endif
c 0+ S11 1(2)
      CALL interp(WSa,s11r,51,3,XIN,Br1(1,2))
      CALL interp(WSa,s11i,51,3,XIN,Br1(2,2))
c 2- D13 1(3)
      CALL interp(WSa,d13r,51,3,XIN,Br1(1,3))
      CALL interp(WSa,d13i,51,3,XIN,Br1(2,3))
      Br1(1,4)=0.
      Br1(2,4)=0.
      Br1(1,5)=0.
      Br1(2,5)=0.
      Br1(1,8)=0.
      Br1(2,8)=0.
c 3- F15 1(6)
      CALL interp(WSa,f15r,51,3,XIN,Br1(1,6))
      CALL interp(WSa,f15i,51,3,XIN,Br1(2,6))
c 1+ P13 1(7)
      CALL interp(WSa,p13r,51,3,XIN,Br1(1,7))
      CALL interp(WSa,p13i,51,3,XIN,Br1(2,7))
c 2+ D15 1(9)
      CALL interp(WSa,d15r,51,3,XIN,Br1(1,9))
      CALL interp(WSa,d15i,51,3,XIN,Br1(2,9))
c 1+ P33 3(1)
      CALL interp(WSa,p33r,51,3,XIN,Br3(1,1))
      Br3(1,1)=Br3(1,1)-0.05
      CALL interp(WSa,p33i,51,3,XIN,Br3(2,1))
      Br3(2,1)=Br3(2,1)*1.017
c 2- D33 3(2)
      CALL interp(WSa,d33r,51,3,XIN,Br3(1,2))
      CALL interp(WSa,d33i,51,3,XIN,Br3(2,2))
c 3+ F37 3(3)
      CALL interp(WSa,f37r,51,3,XIN,Br3(1,3))
      CALL interp(WSa,f37i,51,3,XIN,Br3(2,3))
      Br3(1,4)=0.
      Br3(2,4)=0.
c 0+ S31 3(5)
      CALL interp(WSa,s31r,51,3,XIN,Br3(1,5))
      CALL interp(WSa,s31i,51,3,XIN,Br3(2,5))
c 3- F35 3(6)
      Br3(1,6)=0.
      Br3(2,6)=0.
c 2+ D35 3(7)
      Br3(1,7)=0.
      Br3(2,7)=0.
c 1- P31 3(8)
      CALL interp(WSa,p31r,51,3,XIN,Br3(1,8))
      CALL interp(WSa,p31i,51,3,XIN,Br3(2,8))
      
      do j=1,9
         do i=1,2	! Real (1) Imag (2)
            MM1(1,i,j)=n1*BM1(i,j)*(1.-Br1(2,j))+n2*BrM1(1,i,j)*EBr1(1,j)
            ME1(1,i,j)=n1*BE1(i,j)*(1.-Br1(2,j))+n2*BrE1(1,i,j)*EBr1(1,j)
            MS1(1,i,j)=n1*BS1(i,j)*(1.-Br1(2,j))+n2*BrS1(1,i,j)*EBr1(1,j)
            MM1(2,i,j)=n1*BM1(i,j)*Br1(1,j)     +n2*BrM1(2,i,j)*EBr1(2,j)
            ME1(2,i,j)=n1*BE1(i,j)*Br1(1,j)     +n2*BrE1(2,i,j)*EBr1(2,j)
            MS1(2,i,j)=n1*BS1(i,j)*Br1(1,j)     +n2*BrS1(2,i,j)*EBr1(2,j)
         enddo
      enddo
      
c I=3/2

      do i=1,2
         BrM3(i,1)=BreitM3(i,1)+BreitM3(i,4)+BreitM3(i,10)
         BrE3(i,1)=BreitE3(i,1)+BreitE3(i,4)+BreitE3(i,10)
         BrS3(i,1)=BreitS3(i,1)+BreitS3(i,4)+BreitS3(i,10)
         BrM3(i,2)=BreitM3(i,2)+BreitM3(i,12)
         BrE3(i,2)=BreitE3(i,2)+BreitE3(i,12)
         BrS3(i,2)=BreitS3(i,2)+BreitS3(i,12)
         BrM3(i,3)=BreitM3(i,3)
         BrE3(i,3)=BreitE3(i,3)
         BrS3(i,3)=BreitS3(i,3)
         BrE3(i,5)=BreitE3(i,5)+BreitE3(i,9)
         BrS3(i,5)=BreitS3(i,5)+BreitS3(i,9)
         BrM3(i,6)=BreitM3(i,6)+BreitM3(i,13)      
         BrE3(i,6)=BreitE3(i,6)+BreitE3(i,13)      
         BrS3(i,6)=BreitS3(i,6)+BreitS3(i,13)      
         BrM3(i,7)=BreitM3(i,7)
         BrE3(i,7)=BreitE3(i,7)
         BrS3(i,7)=BreitS3(i,7)
         BrM3(i,8)=BreitM3(i,8)+BreitM3(i,11)     
         BrS3(i,8)=BreitS3(i,8)+BreitS3(i,11)
      enddo
           
c     - construct Re(1) Im(2) parts of (1+) multipole amplitudes for D(1232)

      MM3(1,1)=n1*BM3(1)*(1.-Br3(2,1))+n2*BrM3(1,1) 
      ME3(1,1)=n1*BE3(1)*(1.-Br3(2,1))+n2*BrE3(1,1)
      MS3(1,1)=n1*BS3(1)*(1.-Br3(2,1))+n2*BrS3(1,1)
      MM3(2,1)=n1*BM3(1)*Br3(1,1)     +n2*BrM3(2,1)  
      ME3(2,1)=n1*BE3(1)*Br3(1,1)     +n2*BrE3(2,1)
      MS3(2,1)=n1*BS3(1)*Br3(1,1)     +n2*BrS3(2,1)
      
c Note RMP33 and IMP33 are previously defined shape for M1+ which
c was determined from fits to Q2=0 VPI M1+ data in PRC 67, 015209 (2003).
c Shape follows description of modified B-W and Regge modified backgrounds
c described in Eq. 16-17 of paper.  resM is GM* parameterization defined 
c in janr_ini_point.F 

      MM3(1,1)=resM*RMP33
      MM3(2,1)=resM*IMP33 

c Empirical adjustment to shape of D(1232) away from resonance position
c These parameters are intended to be used for Q2=0.4 fits.
      
c      MM3(1,1)=resM*RMP33*(WW/1.2283)**(0.3857+0.229-0.095)
c      MM3(2,1)=resM*IMP33*(WW/1.2283)**(0.3857+0.229-0.095)
                  
c      MM3(1,1)=resM*RMP33*(WW/1.2283)**(0.1205-0.03)	! 2006
c      MM3(2,1)=resM*IMP33*(WW/1.2283)**(2.211-0.243)	! 2006 

      
c      if (WW.ge.1.2283) then
c        MM3(1,1)=resM*RMP33*(WW/1.2283)**(-0.096546+0.18467+0.0932)     ! 0.181
c        MM3(2,1)=resM*IMP33*(WW/1.2283)**(-1.6189+0.39945+0.192)        !-1.027
c        ME3(1,1)= ME3(1,1)*(WW/1.2283)**(-1.465+1.55-0.54)              !-0.455
c        ME3(2,1)= ME3(2,1)*(WW/1.2283)**(5.505-0.66997+0.29)            !5.125
c        MS3(1,1)= MS3(1,1)*(WW/1.2283)**(2.3835)                        !2.3835
c        MS3(2,1)= MS3(2,1)*(WW/1.2283)**(-16.621-2.495)                 !-19.12
c      endif
      
c      if (WW.ge.1.2283) then
c        MM3(1,1)=resM*RMP33*(WW/1.2283)**(-0.9324+0.0371)
c        MM3(2,1)=resM*IMP33*(WW/1.2283)**(-0.6683-0.1)
c        ME3(1,1)= ME3(1,1)*(WW/1.2283)**(-0.6589-0.2366)
c        ME3(2,1)= ME3(2,1)*(WW/1.2283)**(1.374+1.535)
c        MS3(1,1)= MS3(1,1)*(WW/1.2283)**( 1.5965-0.457)
c        MS3(2,1)= MS3(2,1)*(WW/1.2283)**(0.669+1.7226)
c      endif
      
c Empirical adjustment to shape of D(1232) away from resonance position
c These parameters are intended to be used for Q2>0 fits.

c      go to 77
      if (WW.le.1.2283) then
      do i=1,2
        MM3(i,1)=MM3(i,1)*(WW/1.2283)**cm2
        ME3(i,1)=ME3(i,1)*(WW/1.2283)**ce2
        MS3(i,1)=MS3(i,1)*(ww/1.2283)**cs2
      enddo
      else
      do i=1,2
        MM3(i,1)=MM3(i,1)*(WW/1.2283)**cm3
        ME3(i,1)=ME3(i,1)*(WW/1.2283)**ce3
        MS3(i,1)=MS3(i,1)*(WW/1.2283)**cs3
      enddo
      endif
c77    continue            
c     - construct Re(1) Im(2) parts of other multipole amplitudes A(3/2) 
      do j=2,8
         MM3(1,j)=n1*BM3(j)*(1.-Br3(2,j))+n2*BrM3(1,j)
         ME3(1,j)=n1*BE3(j)*(1.-Br3(2,j))+n2*BrE3(1,j)
         MS3(1,j)=n1*BS3(j)*(1.-Br3(2,j))+n2*BrS3(1,j)
         MM3(2,j)=n1*BM3(j)*Br3(1,j)     +n2*BrM3(2,j)
         ME3(2,j)=n1*BE3(j)*Br3(1,j)     +n2*BrE3(2,j)
         MS3(2,j)=n1*BS3(j)*Br3(1,j)     +n2*BrS3(2,j)
      enddo
      
c      print *, WW,MM3(2,6),ME3(2,6)
c     coeff=10.	! units mFm
      coeff=1.
	
      do j=1,9
        do i=1,2
          do k=1,2
            MM1(k,i,j)=coeff*MM1(k,i,j)
            ME1(k,i,j)=coeff*ME1(k,i,j)
            MS1(k,i,j)=coeff*MS1(k,i,j)
          enddo
        enddo
      enddo
      
      do j=1,8
        do k=1,2
          MM3(k,j)=coeff*MM3(k,j)
          ME3(k,j)=coeff*ME3(k,j)
          MS3(k,j)=coeff*MS3(k,j)
        enddo
      enddo

C     Reaction multipoles
      
c     ReacM1(i,j)=0+	i=1,2 for Re,Im;	j=1,2 for pi0,pi+	 
C     ReacM2(i,j)=1-
C     ReacM3(i,j)=1+
C     ReacM4(i,j)=2-
C     ReacM5(i,j)=2+
C     ReacM6(i,j)=3-
C     ReacM7(i,j)=3+

      do i=1,2        
        ReacE1(i,1)= ME1(i,1,2)+ME3(i,5)/1.5
        ReacE1(i,2)= sqrt(2.)*(ME1(i,1,2)-ME3(i,5)/3.)
        ReacS1(i,1)= MS1(i,1,2)+MS3(i,5)/1.5
        ReacS1(i,2)= sqrt(2.)*(MS1(i,1,2)-MS3(i,5)/3.)
        ReacM2(i,1)= MM1(i,1,1)+MM3(i,8)/1.5
        ReacM2(i,2)= sqrt(2.)*(MM1(i,1,1)-MM3(i,8)/3.)*ratio
        ReacS2(i,1)= MS1(i,1,1)+MS3(i,8)/1.5
        ReacS2(i,2)= sqrt(2.)*(MS1(i,1,1)-MS3(i,8)/3.)*ratio
        ReacM3(i,1)= MM1(i,1,7)+MM3(i,1)/1.5
        ReacM3(i,2)= sqrt(2.)*(MM1(i,1,7)-MM3(i,1)/3.)*ratio
        ReacE3(i,1)= ME1(i,1,7)+ME3(i,1)/1.5
        ReacE3(i,2)= sqrt(2.)*(ME1(i,1,7)-ME3(i,1)/3.)*ratio
        ReacS3(i,1)= MS1(i,1,7)+MS3(i,1)/1.5
        ReacS3(i,2)= sqrt(2.)*(MS1(i,1,7)-MS3(i,1)/3.)*ratio
        ReacM4(i,1)= MM1(i,1,3)+MM3(i,2)/1.5
        ReacM4(i,2)= sqrt(2.)*(MM1(i,1,3)-MM3(i,2)/3.)*ratio**2
        ReacE4(i,1)= ME1(i,1,3)+ME3(i,2)/1.5
        ReacE4(i,2)= sqrt(2.)*(ME1(i,1,3)-ME3(i,2)/3.)*ratio**2
        ReacS4(i,1)= MS1(i,1,3)+MS3(i,2)/1.5
        ReacS4(i,2)= sqrt(2.)*(MS1(i,1,3)-MS3(i,2)/3.)*ratio**2
        ReacM5(i,1)= MM1(i,1,9)+MM3(i,7)/1.5
        ReacM5(i,2)= sqrt(2.)*(MM1(i,1,9)-MM3(i,7)/3.)*ratio**2
        ReacE5(i,1)= ME1(i,1,9)+ME3(i,7)/1.5
        ReacE5(i,2)= sqrt(2.)*(ME1(i,1,9)-ME3(i,7)/3.)*ratio**2
        ReacS5(i,1)= MS1(i,1,9)+MS3(i,7)/1.5
        ReacS5(i,2)= sqrt(2.)*(MS1(i,1,9)-MS3(i,7)/3.)*ratio**2
        ReacM6(i,1)= MM1(i,1,6)+MM3(i,6)/1.5
        ReacM6(i,2)= sqrt(2.)*(MM1(i,1,6)-MM3(i,6)/3.)*ratio**3
        ReacE6(i,1)= ME1(i,1,6)+ME3(i,6)/1.5
        ReacE6(i,2)= sqrt(2.)*(ME1(i,1,6)-ME3(i,6)/3.)*ratio**3
        ReacS6(i,1)= MS1(i,1,6)+MS3(i,6)/1.5
        ReacS6(i,2)= sqrt(2.)*(MS1(i,1,6)-MS3(i,6)/3.)*ratio**3
        ReacM7(i,1)= MM1(i,1,8)+MM3(i,3)/1.5
        ReacM7(i,2)= sqrt(2.)*(MM1(i,1,8)-MM3(i,3)/3.)*ratio**3
        ReacE7(i,1)= ME1(i,1,8)+ME3(i,3)/1.5
        ReacE7(i,2)= sqrt(2.)*(ME1(i,1,8)-ME3(i,3)/3.)*ratio**3
        ReacS7(i,1)= MS1(i,1,8)+MS3(i,3)/1.5
        ReacS7(i,2)= sqrt(2.)*(MS1(i,1,8)-MS3(i,3)/3.)*ratio**3
      enddo
           
      RETURN
      END
