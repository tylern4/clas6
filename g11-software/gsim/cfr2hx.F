      SUBROUTINE CFR2HX( ENERGY, X0TOT, RTRNS )
C- Original subroutine name  :  RND2HX
C
      REAL ENERGY, X0TOT, RTRNS
C
      REAL A1, B1, B2, A2, X, Y, RNDM(3), NORM, F1
C
C     --------------------------------------------------------
C
      IF( X0TOT .GT. 28.) THEN
          RTRNS = 0.
      ELSE
          B2 = 0.92 - 0.023 * X0TOT
          A1 = 201.68 * EXP( -.185 * X0TOT )
          B1 = 4.1 - 0.141 * X0TOT
          A2 = 1.0
          NORM = ( A1/B1) + (A2/B2)
          F1 = (A1/B1)/NORM
          call grndm(rndm,3)
          X = -ALOG( RNDM(1) )
          Y = -ALOG( RNDM(2) )
          RTRNS = SQRT( X*X + Y*Y )
          IF( RNDM(3) .LT. F1 ) THEN
              RTRNS = RTRNS / B1
          ELSE
              RTRNS = RTRNS / B2
          ENDIF
      ENDIF
      END
