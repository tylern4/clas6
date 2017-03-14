      SUBROUTINE HEL_AMPL(Q2,THETA,kgamma,opt)
      
      IMPLICIT NONE
      SAVE
      
#include "gpar.inc"
#include "inpar.inc"
#include "phiampl.inc"
#include "helicity.inc"

      REAL Q2,theta,kgamma
      CHARACTER*10 opt
      INTEGER NWRITE
      INTEGER i,j
      REAL teta,qq,sx,cx,sx2
      REAL PHreac(6,3,2)

      teta = theta*pi/180.
      qq   = sqrt(Q2)/kgamma
      sx   = sin(teta)
      cx   = cos(teta)
      sx2  = sx**2
      
c- Calculate Intermediate amplitudes for different reactions. 
c- Loop by "i" counts for amplitudes. Loop by "j" stands for 
c- imaginary(j=1) and real(j=2) parts.

      do i=1,6
         do j=1,2
c  - Reaction g + p => p + pi0
            PHreac(i,1,j)=phi_amp(i,j,1)
c  - Reaction g + p => n + pi+
            PHreac(i,2,j)=phi_amp(i,j,2) 
         end do
      end do	

c- Now get the helicity amplitudes.
c- Here as before "j" stands for real and imaginary parts.
c- Loop for "i" done for the reactions:
c- i=1 - g + p => p + pi0
c- i=2 - g + p => n + pi+
c- h1,h2,...h6 are helicity amplitudes f_m2_m1_l,   
c- where m1 and m2 are helicities of initial and final nucleons, l is 
c- helicity of incomming photon.
 
      do i=1,2
      do j=1,2
      
c- h1 = f+ + - =  f- - +	

        h1(i,j)=-sx*(PHreac(3,i,j)+cx*PHreac(4,i,j))/a2

c- h2 = f- + + = -f+ - -	

	h2(i,j)=2.*PHreac(1,i,j)-2.*cx*PHreac(2,i,j)+sx2*PHreac(4,i,j)
	h2(i,j)=-h2(i,j)/a2

c- h3 = f+ - + = f- + -	

        h3(i,j)=-sx2*PHreac(4,i,j)/a2

c- h4 = f+ + + = f- - -	

	h4(i,j)=2.*PHreac(2,i,j)+PHreac(3,i,j)+cx*PHreac(4,i,j)
	h4(i,j)=sx*h4(i,j)/a2

c- h5 = f- - 0 = -f+ + 0	

        h5(i,j)=qq*(PHreac(5,i,j)+cx*PHreac(6,i,j))

c- h6 = f+ - 0 =  f- + 0	

        h6(i,j)=qq*sx*PHreac(6,i,j)

      end do
      end do
     
      RETURN
      END





