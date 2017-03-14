      SUBROUTINE RE_PHI(S,Q2)

      IMPLICIT NONE
      SAVE

#include "gpar.inc"
#include "bornterms.inc"
#include "rephi.inc"

      REAL S, Q2 

      INTEGER i,j
      REAL on1,on2,op1,op2,om1,om2,wm,wp,xx
      REAL br1,br2,br3,br4,br5,br6
      REAL AK(6),W,qq2

c- Determination of Real parts of Intermediate Amplitudes through 
c- Real parts of Invariant Amplitudes determined by Dispersion Relations. 

      W		= sqrt(S)
      qq2       = Q2
      if (Q2.eq.0) qq2=0.00001
      
      on1	= W-Egamma
      on2	= W-Epion
      op1	= on1+mn
      om1	= on1-mn
      op2	= on2+mn
      om2	= on2-mn
      
      Wm	= W-mn
      Wp	= W+mn
      XX	= Egamma*qk-Epion*qq2
      
      AK(1)	= sqrt(op1*op2)/w2
      AK(2)	= sqrt(om1*om2)/w2
      AK(3)	= AK(2)*op2
      AK(4)	= AK(1)*om2
      AK(5)	= sqrt(om1*op2)/(qq2*w2)
      AK(6)	= sqrt(op1*om2)/(qq2*w2)
      
c     - Loop by Isospin states (+), (-) & (0)

      do i=1,3
      do j=1,2
	 
         br5	= br(j,5,i)+(E+qk)*br(j,6,i)/2.	  
	  
         PHisot(j,1,i) =  AK(1)*(Wm*br(j,1,i)-br5)     
         PHisot(j,2,i) = -AK(2)*(Wp*br(j,1,i)+br5)
     
         br1	= 2.*br(j,3,i)-br(j,2,i)
         br2	= br(j,6,i)/2.-br(j,4,i)
         br3	= qq2*br(j,1,i)
     
         PHisot(j,3,i) = AK(3)*( br1+Wp*br2)
         PHisot(j,4,i) = AK(4)*(-br1+Wm*br2)
     
         br4=op1*(br3+Wm*br5+W2*om1*(br(j,2,i)-Wp*br(j,6,i)/2.))
         PHisot(j,5,i)=AK(5)*(br4-XX*(br1+Wp*br2))
     
         br6=-om1*(br3-Wp*br5+W2*op1*(br(j,2,i)+Wm*br(j,6,i)/2.))     
         PHisot(j,6,i)=AK(6)*(br6+XX*(br1-Wm*br2))
         
      end do
      end do
      
      RETURN
      END



