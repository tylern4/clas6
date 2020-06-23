      SUBROUTINE BREIT_AMPL (S,Q2,kgamma,qpion)

      IMPLICIT NONE
      SAVE

#include "gpar.inc"
#include "resonances.inc"
#include "fit_par.inc"
#include "amplitudes.inc"
#include "delta_dat.inc"

      REAL S, Q2, kgamma, qpion

      INTEGER i,ii,j,k
      REAL rv,Breit(2)
      
c     Isospin 3/2 resonances
      
      DO i=1,6
         call getbreit(s,q2,kgamma,qpion,i,Breit,rv)         
         do j=1,2	! real (j=1) imag (j=2)
            BreitE3(j,i)=Breit(j)*AE3(i)
            BreitM3(j,i)=Breit(j)*AM3(i)
            BreitS3(j,i)=Breit(j)*AS3(i)*rv
         enddo
      ENDDO
     
      DO i=7,13
         DO j=1,2
            BreitE3(j,i)=0.
            BreitM3(j,i)=0.
            BreitS3(j,i)=0.
         ENDDO	
      ENDDO	
      
c     Isospin 1/2 resonances

      DO i=1,10
         ii=i+20
         call getbreit(s,q2,kgamma,qpion,ii,Breit,rv)  
         do k=1,2	! pA(1/2) (k=1) nA(1/2) (k=2)
            do j=1,2	! real (j=1) imag (j=2)
               BreitE1(j,k,i)=Breit(j)*AE1(k,i)
               BreitM1(j,k,i)=Breit(j)*AM1(k,i)
               BreitS1(j,k,i)=Breit(j)*AS1(k,i)*rv
            enddo
         enddo
      ENDDO
     
      DO i=11,13
         do k=1,2
            do j=1,2
               BreitE1(j,k,i)=0.
               BreitM1(j,k,i)=0.
               BreitS1(j,k,i)=0.
            enddo
         enddo
      ENDDO
      
      RETURN
      END
