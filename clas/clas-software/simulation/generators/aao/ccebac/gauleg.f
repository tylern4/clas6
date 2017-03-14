C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C---- NUMREC.f -------------------------- WEN-TAI CHIANG --- 11/08/99 --
C-----------------------------------------------------------------------
C The subroutines compiled here are from 
C Numerical Recipes in Fortran, 2nd ed., Cambridge Univ. Press, 1992.
C GAULEG: Computes the abacissas X(i) and weights W(i) of the
C         Gauss-Legendre N-point quadrature formula
C GAUSSJ: Solves linear matrix equation by Gauss-Jordan elimination
C PLGNDR: Computes the associated Legendre polynomial P_l^m(x)
C-----------------------------------------------------------------------
      SUBROUTINE GAULEG(X1,X2,X,W,N)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER N
      REAL*8 X1,X2,X(N),W(N)
      DOUBLE PRECISION EPS
      PARAMETER (EPS=3.D-14)
      INTEGER I,J,M
      DOUBLE PRECISION P1,P2,P3,PP,XL,XM,Z,Z1
      M=(N+1)/2
      XM=0.5D0*(X2+X1)
      XL=0.5D0*(X2-X1)
      DO 30 I=1,M
        Z=COS(3.141592654D0*(I-.25D0)/(N+.5D0))
   10   CONTINUE
          P1=1.D0
          P2=0.D0
          DO 20 J=1,N
            P3=P2
            P2=P1
            P1=((2.D0*J-1.D0)*Z*P2-(J-1.D0)*P3)/J
   20     CONTINUE
          PP=N*(Z*P1-P2)/(Z*Z-1.D0)
          Z1=Z
          Z=Z1-P1/PP
        IF(ABS(Z-Z1).GT.EPS)GOTO 10
        X(I)=XM-XL*Z
        X(N+1-I)=XM+XL*Z
        W(I)=2.D0*XL/((1.D0-Z*Z)*PP*PP)
        W(N+1-I)=W(I)
   30 CONTINUE
      RETURN
      END



