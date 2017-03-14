c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S.Stepanyan     Created:  Tue Mar 25 14:32:25 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     
      SUBROUTINE ROTES2(C,S,C2,S2,PR,I)
C--  CALLED FROM - GENEV
C         THIS SUBROUTINE NOW DOES TWO ROTATIONS (XY AND XZ)
      DIMENSION PR(50)
      K1 = 5*I - 4
      K2 = K1 + 1
      SA = PR(K1)
      SB = PR(K2)
      A      = SA*C - SB*S
      PR(K2) = SA*S + SB*C
      K2 = K2 + 1
      B = PR(K2)
      PR(K1) = A*C2 - B*S2
      PR(K2) = A*S2 + B*C2
      RETURN
      END
C
      FUNCTION PDK(A,B,C)
c
      real a,b,c,d
C=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
C--  CALLED FROM -  GENEV
C     PDK = SQRT(A*A+(B*B-C*C)**2/(A*A) - 2.0*(B*B+C*C))/2.0
      A2 = A*A
      B2 = B*B
      C2 = C*C
      d=A2 + (B2-C2)**2/A2 - 2.0*(B2+C2)
      if(d.lt.0)d=0.
      PDK = 0.5*SQRT(d)
      RETURN
      END
C=-=-=-
      SUBROUTINE A4(S1,AM,BM,E_1,P_1)
CC===
      DOUBLE PRECISION FLAM1,S,AM2,BM2
      S=S1
      AM2=AM
      BM2=BM**2
      E_1=(S+AM2-BM2)/DSQRT(S)/2.
      FLAM1=(S-AM2-BM2)**2-4.*AM2*BM2
      P_1=DSQRT(FLAM1/S)/2.
C===
      RETURN
      END
c ==========================
c
      FUNCTION f_lambda(xi,yi,zi)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   4-FEB-1994   Stepan Stepanyan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      real xi,yi,zi
      double precision f_lambda
      double precision x,y,z
      x=xi
      y=yi
      z=zi
      f_lambda=(x-y-z)**2-4.*y*z
C----------------------------------------------------------------------
  999 RETURN
      END
C     =-=-=-=-=-=-=-=-=-
      SUBROUTINE TOPOL(P_C,THETAC,PHIC)
      REAL P_C(4),THETAC,PHIC
C     
      PI=2.*ACOS(0.)
      THETAC=ACOS(P_C(3)/
     &     SQRT(P_C(1)**2+P_C(2)**2+P_C(3)**2))
      P_C12=P_C(1)**2+P_C(2)**2
      IF(P_C12.EQ.0.)THEN
         PHIC=0.
      ELSE
         COS_PHIC=P_C(1)/SQRT(P_C12)
         PHIC=ACOS(COS_PHIC)
         IF(P_C(2).LT.0.0)PHIC=2.*PI-PHIC
      ENDIF
C----------------------------------------------------------------------
      RETURN
      END
C
C++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE ROTA(THE,PHI,P)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-MAR-1994   Stepan Stepanyan
C-
C----------------------------------------------------------------------
      DIMENSION P(3)
      DOUBLE PRECISION ROT(3,3),PV(3)
      IF(THE**2+PHI**2.GT.1E-20) THEN
C...ROTATE (TYPICALLY FROM Z AXIS TO DIRECTION THETA,PHI)
        ROT(1,1)=COS(THE)*COS(PHI)
        ROT(1,2)=-SIN(PHI)
        ROT(1,3)=SIN(THE)*COS(PHI)
        ROT(2,1)=COS(THE)*SIN(PHI)
        ROT(2,2)=COS(PHI)
        ROT(2,3)=SIN(THE)*SIN(PHI)
        ROT(3,1)=-SIN(THE)
        ROT(3,2)=0.
        ROT(3,3)=COS(THE)
        DO 100 J=1,3
  100   PV(J)=P(J)
        DO 110 J=1,3
  110   P(J)=ROT(J,1)*PV(1)+ROT(J,2)*PV(2)+ROT(J,3)*PV(3)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
c
c----------------------------------------------------------------------
      function getrand(dummy)
c
c     calls ranmar when necessary and gets random numbers
c      Initialized in fsgen_init.F
c
      real getrand, dummy
c
      integer cnt
      integer len
      parameter (len=1)
      real vec(1)
c      dimension vec(len)

c      cnt = len+1
c      if (cnt .gt. len) then
c         cnt = 1
         call ranmar(vec,len)
c      endif

      getrand = vec(1)
c     cnt = cnt+1

      return

      END
c
      Logical function phipass(phi)
c
      real phi,x,pi
      data pi /3.14159265/
c
      x=phi*180./pi
      fr=getrand(-1.)
c
      fv=(exp(-3.136+0.087311*abs(x))+20*abs(x)+1000)/291860.5
      phipass=fr.le.fv
c
      return
      end
c(exp(-3.136+0.087311*abs(x))+20*abs(x)+1000)/291860.5
