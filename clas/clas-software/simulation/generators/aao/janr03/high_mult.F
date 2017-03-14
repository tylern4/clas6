      SUBROUTINE HIGH_MULT (S,Q2,costh)
    
      IMPLICIT NONE
      SAVE

#include "multipoles.inc"
#include "highmult.inc"
#include "amplitudes.inc"
#include "fit_par.inc"

      REAL S, Q2, costh

      INTEGER NWRITE
      DATA NWRITE/0/
      REAL p1,p2,p3,p4,p5,p6,p7,p8
      INTEGER i,j

      CALL back_high(S)
      
c-P''_5
	p1=-52.5*costh+157.5*costh**3
c-P''_3
	p2=15.*costh
c-P''_4
	p3=-7.5+52.5*costh**2
c-P'_5
	p4=1.875-26.25*costh**2+39.375*costh**4
c-P'_3
	p5=-1.5+7.5*costh**2
c-P'_4
	p6=-7.5*costh+17.5*costh**3

c-P''_6
	p7=13.125-236.25*costh**2+433.125*costh**4
c-P'_6
	p8=13.125*costh-78.75*costh**3+86.625*costh**5
	
c-These are loops for p(1/2), n(1/2), 3/2

	do i=1,3
	phih(1,i)=(4.*M4p(i)+E4p(i))*p4+(5.*M5p(i)+E5p(i))*p8
	phih(1,i)=phih(1,i)+(5.*M4m(i)+E4m(i))*p5+(6.*M5m(i)+E5m(i))*p6
	phih(2,i)=(5.*M4p(i)+4.*M4m(i))*p6+(6.*M5p(i)+5.*M5m(i))*p4
	phih(3,i)=(E4p(i)-M4p(i))*p1+(E5p(i)-M5p(i))*p7
	phih(3,i)=phih(3,i)+(M4m(i)+E4m(i))*p2+(M5m(i)+E5m(i))*p3
	phih(4,i)=(M4p(i)-E4p(i)-M4m(i)-E4m(i))*p3
	phih(4,i)=phih(4,i)+(M5p(i)-E5p(i)-M5m(i)-E5m(i))*p1
	phih(5,i)=5.*S4p(i)*p4-4.*S4m(i)*p5+6.*S5p(i)*p8-5.*S5m(i)*p6
	phih(6,i)=-5.*S4p(i)*p6+4.*S4m(i)*p6-6.*S5p(i)*p4+5.*S5m(i)*p4
	enddo
	  
c Construct phih for reactions pi0,pi+

	do i=1,6
	 phih(i,1)=phih(i,1)+phih(i,3)/1.5
	 phih(i,2)=sqrt(2.)*(phih(i,1)-phih(i,3))
	enddo
	
      RETURN
      END
