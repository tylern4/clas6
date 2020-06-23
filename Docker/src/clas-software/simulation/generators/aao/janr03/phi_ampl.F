      SUBROUTINE PHI_AMPL (S,costh,Q2,kgamma,qpion)
      
      IMPLICIT NONE
      SAVE
      
#include "multipoles.inc"
#include "highmult.inc"
#include "phiampl.inc"

      REAL S, Q2, kgamma, qpion ,costh
      INTEGER NWRITE
      REAL E1(2,2),E3(2)
      REAL p1,p2,p3,p4,p5
      INTEGER i, j

      CALL mult_ampl(S,Q2,kgamma,qpion)
      CALL high_mult(S,Q2,costh)

c- Calculate derivatives of Legendre polynomials
c- p1=P2', p2=P3', p3=P4', p4=P3", p5=P4"
	      
      p1 = 3.*costh
      p2 = (15.*costh**2-3.)/2.
      p3 = 2.5*(7.*costh**3-3.*costh)
      p4 = 15.*costh
      p5 = 7.5*(7.*costh**2-1.)	

c- Calculate intermediate amplitudes for transition from 
c- multipole amplitudes to helicity amplitudes and cross sections.
c- Multipole ampl. are normalized according to CGLN Phys.Rev. 106 (1957) 1345.
c- They are equal to mult.ampl. of Devenish&Lyth/8W/pi
c- mult.ampl. (1) => (0+)
c- mult.ampl. (2) => (1-)
c- mult.ampl. (3) => (1+)
c- mult.ampl. (4) => (2-)
c- mult.ampl. (5) => (2+)
c- mult.ampl. (6) => (3-)
c- mult.ampl. (7) => (3+)

     
      do i=1,2	! Re(1) Im(2)
      do j=1,2	! pi0(1) pi+(2)
        E1(i,j)=ReacM6(i,j)+ReacE6(i,j)
        phi_amp(1,i,j)=ReacE1(i,j)+p1*(ReacM3(i,j)+ReacE3(i,j))+
     +       p3*(3.*ReacM7(i,j)+ReacE7(i,j))+
     +       3.*ReacM4(i,j)+ReacE4(i,j)+p1*(4.*ReacM6(i,j)+ReacE6(i,j))+
     +       p2*(2.*ReacM5(i,j)+ReacE5(i,j))
        phi_amp(2,i,j)=2.*ReacM3(i,j)+ReacM2(i,j)+
     +       p1*(2.*ReacM4(i,j)+3.*ReacM5(i,j))+
     +       3.*p2*ReacM6(i,j)+4.*p2*ReacM7(i,j)
        phi_amp(3,i,j)=3.*(ReacE3(i,j)-ReacM3(i,j)+E1(i,j))+
     +       p5*(ReacE7(i,j)-ReacM7(i,j))+p4*(ReacE5(i,j)-ReacM5(i,j))
        phi_amp(4,i,j)=-3.*(ReacM4(i,j)+ReacE4(i,j))+
     +       3.*(ReacM5(i,j)-ReacE5(i,j))-
     -       p4*E1(i,j)+p4*(ReacM7(i,j)-ReacE7(i,j))   
        phi_amp(5,i,j)=ReacS1(i,j)+2.*p1*ReacS3(i,j)+
     +       4.*p3*ReacS7(i,j)+3.*p2*ReacS5(i,j)-
     -       2.*ReacS4(i,j)-3.*p1*ReacS6(i,j)
        phi_amp(6,i,j)=ReacS2(i,j)+2.*p1*ReacS4(i,j)+3.*p2*ReacS6(i,j)-
     -       2.*ReacS3(i,j)-4.*p2*ReacS7(i,j)-3.*p1*ReacS5(i,j)
      end do
      end do

      do i=1,6
      do j=1,2
        phi_amp(i,1,j)=phi_amp(i,1,j)+phih(i,j)
      enddo
      enddo

      RETURN
      END


