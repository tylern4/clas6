      SUBROUTINE MIXING_AMPL (S,Q2)
     
      IMPLICIT NONE
      SAVE
    
#include "gpar.inc"
#include "fit_par.inc"
#include "back_ampl.inc"

      REAL S, Q2
      INTEGER i, ll, j, k
      REAL on1,on2,op1,om1,op2,om2 
      REAL W,B1(2),Lpv,Lmix 
      
c     -  here we calculate multipole amplitudes for PV-PS mixing 

      W   = sqrt(s)
      on1 = W-Egamma
      on2 = W-Epion
      op1 = on1+mn 
      om1 = on1-mn
      op2 = on2+mn
      om2 = on2-mn  
      
      Lmix  = 0.55+cspa
      Lpv   = Lmix**2/(Lmix**2+Epion**2-mp22)
      B1(1) = Lpv*3.134*(F2p-F2n)/mn/W/2.
      B1(2) = Lpv*3.134*(F2p+F2n)/mn/W/2.
      
C -this is loop for (+) and (0) amplitudes. (-)=0.

      do i=1,2
        E0(i) =  sqrt(op1*op2)*B1(i)*(W-mn)
        M1(i) = -sqrt(om1*om2)*B1(i)*(W+mn)
        S0(i) =  sqrt(om1*op2)*B1(i)*op1
        S1(i) = -sqrt(op1*om2)*B1(i)*om1
      enddo

      RETURN
      END
