      SUBROUTINE ETTA

      IMPLICIT NONE
      SAVE

#include "ettapar.inc"

      INTEGER NWRITE
      INTEGER ETampl(6)
      INTEGER ETisot(3)
      INTEGER i,k,m 

c- Definition of coefficents that denote the symmetry of Invariant 
c- Amplitudes under the interchange "s<==>u". 

      ETampl(1) = 1
      ETampl(2) = 1
      ETampl(6) = 1
      do i=3,5
         ETampl(i) = -1
      end do

c- Coefficients which determine the symmetry of Isospin Amplitudes 
c- (+),(-),(0) under the interchange "s<==>u".

      ETisot(1) = 1
      ETisot(2) = -1
      ETisot(3) = 1

c Products of two symmetry coefficients.

      DO k=1,6
        do m=1,3
           ett(k,m) = ETampl(k)*ETisot(m)
        enddo
      ENDDO
      
      RETURN
      END



