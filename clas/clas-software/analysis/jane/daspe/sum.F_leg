            Function Sum(x)
c
      IMPLICIT NONE
      SAVE
c
#include "cross_sec.inc"
c
      common/itotal/i,w,q2,epsilon,phi,ierr
        INTEGER i,ierr
        REAL Q2, W, Phi, Epsilon
        REAL Sum, x
c
              CALL DASPE_RUN(W,Q2,epsilon,x,phi,ierr)
c
                     Sum= sigma_leg(i,2)
      RETURN
      END

