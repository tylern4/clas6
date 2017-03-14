      SUBROUTINE CFNROT(VECIN,VECOUT)
C
C ROTATE SO THAT VECIN(1) AXIS IS PARALLEL TO VECIN
C
      REAL THETA
      REAL TWOPI
      REAL PHI
      REAL VECIN(3)
      REAL VECOUT(3)
      REAL XT
      INTEGER I
      INTEGER J
      LOGICAL DONE
      REAL M(3,3)
      REAL ZERO
      PARAMETER ( ZERO = 0. )
      DATA DONE / .FALSE./
      SAVE M,DONE
C     ----------------------------------------------------------------
C
      DO 10 I=1,3
          VECOUT(I) = 0.0
        DO 10 J=1,3
          VECOUT(I)=VECOUT(I)+M(I,J)*VECIN(J)
   10 CONTINUE
      RETURN
C
C               set up transformation matrix
C
      ENTRY EULER1(VECIN)
C- Original entry name  :  EULER1
      IF(.NOT.DONE) THEN
        TWOPI = 2.*4.*ATAN(1.)
        DONE = .TRUE.
      END IF
      XT = SQRT(VECIN(1)**2+VECIN(2)**2)
      THETA = ATAN2(XT,VECIN(3))
      PHI = ATAN2(VECIN(2),VECIN(1))
      IF(PHI.LT.0.0) PHI = PHI+TWOPI
      M(1,1)= SIN(THETA)*COS(PHI)
      M(1,2)=-SIN(PHI)
      M(1,3)= -COS(THETA)*COS(PHI)
C
      M(2,1)= SIN(THETA)*SIN(PHI)
      M(2,2)= COS(PHI)
      M(2,3)= -COS(THETA)*SIN(PHI)
C
      M(3,1)= -COS(THETA)
      M(3,2)= ZERO
      M(3,3)= SIN(THETA)
      END
