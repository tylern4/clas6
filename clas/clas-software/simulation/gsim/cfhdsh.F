      SUBROUTINE CFHDSH
CA)========================================(begin of short description)
C HaDron SHower parametrization
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods :
C
C    Called by: GTNINO
C
C    Input arguments :  None
C
C    Input/output arguments :  None
C
C    Output arguments:  None
C
C    Side effects (other variables/banks modified, etc.)
C
C    Created:   20-APR-1994  Serguei Boiarinov
C    Modified:  29-JUN-1998  David Tedeschi
C
CC)-------------------------------------------(end of long description)
      implicit none
CD)----------------------------------(declare common blocks, variables)
C  Common blocks
#include "gcvolu.inc"
#include "gcsets.inc"
#include "gckine.inc"
#include "gcflag.inc"
#include "gconst.inc"
#include "gctrak.inc"
#include "gctmed.inc"
#include "gcmate.inc"
#include "cfpara.inc"
#include "cflset.inc"
C  Variables in argument list
C  Save variables
      REAL L0TOT,FACT,ELAST,X0TOT,XSCAL,AHD,BHD,AEM,BEM
      SAVE L0TOT,FACT,ELAST,X0TOT,XSCAL,AHD,BHD,AEM,BEM
C  Local variables
      REAL RIN(7), VIN(3), VOU(3)
      REAL GEKIN0,FGEKIN
      REAL XXX,W,ECRIT,ENOW,DEJSTP,RTRNS,PHI
      INTEGER NS,JSTEP,NSPLIT,ISPLIT,I
      real rndm(1)
C  External references
      REAL CFGAM1
C  Statement functions
C  Local data statements
CE)===============================================(end of declarations)
C
*      write(IOWRIT,*) 'CFHDSH reached'
      NMEC = 0
C
      IF( GEKIN .LT. SHCUTH) THEN
          NMEC =  NMEC + 1
          LMEC(NMEC) = 30
          DESTEP = GEKIN
          GEKIN = 0.
          VECT(7) = 0.
          CALL CFEDEP(2,VECT,DESTEP)
          ISTOP = 1
          INWVOL = 1
          DESTEP = 0.
          RETURN
      ENDIF
C
C           Set up PARAMS  based on GEKIN at beginning of shower
C
      IF(SLENG.EQ.0.) THEN
         GEKIN0=GEKIN
34       call grndm(rndm,1)
         FGEKIN = 1. + rndm(1)*(RHSO/SQRT(GEKIN)+RHSOC)
         IF (FGEKIN.LE.0.) GO TO 34
         GEKIN = GEKIN*FGEKIN
         GEKIN = GEKIN/PMUTO
         FACT = GEKIN
         ELAST = 0.0
         X0TOT =0.
         L0TOT =0.
         CALL EULER1(VECT(4))
         XSCAL = 1.0
302      call grndm(rndm,1)
         xxx = rndm(1)
         W = SCALH + SIGTHH*XXX
         IF(W.LT.0. .OR. W.GT.1.0)GOTO 302
         AHD = 0.6165 + 0.3183*ALOG(GEKIN)
         BHD = 0.9099 - 0.0237*ALOG(GEKIN)
         IF (IPARAH.GT.0) THEN
          AEM = AHD
          BEM = 0.2198
         ELSE IF (IPARAH.LT.0) THEN
312       IF (IABS(IPARAE).EQ.1) THEN
           ECRIT = 0.55/Z
           BEM = 0.5
           AEM = BEM*LOG(GEKIN/ECRIT)
          ELSE IF (IABS(IPARAE).EQ.2) THEN
CBOY           BEM = 0.434
CBOY           AEM = SCALE*(2.7059 + 0.3706 * ALOG( GEKIN ) )
           BEM = 0.48
           AEM = SCALE*(2.481 + 0.5489*ALOG( GEKIN ) )
          END IF
          call grndm(rndm,1)
          AEM = AEM + rndm(1)*SIGTHE
          IF( AEM .LE. 0.)GOTO 312
         END IF
      ENDIF
C
C                      determ step size
C
      NS = NINT( STEP/(STEPL0*ABSL) )
      IF ( NS.GT.0 ) THEN
        STEP = STEP/FLOAT(NS)
      ELSE
        NS = 1
      ENDIF
C
C                STEP and find DE for each step
C
      RIN(1) = VECT(1) - VECT(4) * STEP/2.
      RIN(2) = VECT(2) - VECT(5) * STEP/2.
      RIN(3) = VECT(3) - VECT(6) * STEP/2.
      NMEC = NMEC + 1
      LMEC( NMEC) = 28
      DO 11 JSTEP = 1, NS
         X0TOT = X0TOT + STEP/RADL
         L0TOT = L0TOT + STEP/ABSL
         SLENG = SLENG + STEP
         RIN(1) = RIN(1) + VECT(4) * STEP
         RIN(2) = RIN(2) + VECT(5) * STEP
         RIN(3) = RIN(3) + VECT(6) * STEP
         ENOW = FACT* (W*CFGAM1(AEM,BEM*X0TOT*XSCAL)+
     &      (1.0-W)*CFGAM1(AHD,BHD*L0TOT*XSCAL))
         DEJSTP = ENOW - ELAST
         ELAST = ENOW
C
         IF(DEJSTP.GT.SHCUTH ) THEN
            IF(LTRNH) THEN
C want HAD transverse fluctuations
              IF(LHSPOT) THEN
C want nspot prop to energy
                NSPLIT = NINT(DEJSTP/SHSPOH)
                IF (NSPLIT.LT.3) NSPLIT=3
              ELSE
                NSPLIT =3
              END IF
            ELSE
              NSPLIT = 1
            END IF
            DESTEP = DEJSTP/NSPLIT
            DO  12 ISPLIT = 1,NSPLIT
               IF(ABSL.GT.500.) THEN
C for light materials
                 RTRNS = 0.01
               ELSE
                 CALL CFR2EX(FACT, L0TOT, RTRNS)
                 RTRNS = MIN( 10., RTRNS)
                 RTRNS = RTRNS*ABSL
               END IF
               call grndm(rndm,1)
               PHI = TWOPI*RNDM(1)
               VIN(1) = 0.0
               VIN(2) = RTRNS*COS(PHI)
               VIN(3) = RTRNS*SIN(PHI)
               IF(LTRNH) THEN
                 CALL CFNROT(VIN,VOU)
               ELSE
                 VOU(1) = 0.0
                 VOU(2) = 0.0
                 VOU(3) = 0.0
               END IF
               VECT(1) = RIN(1)+VOU(1)
               VECT(2) = RIN(2)+VOU(2)
               VECT(3) = RIN(3)+VOU(3)
               GEKIN = GEKIN - DESTEP
               CALL CFEDEP(2,VECT,DESTEP)
12             CONTINUE
C
C                   energy to low. just forget it
C
        ELSE
            IF( DEJSTP .GT. 0.)THEN
               DESTEP = DEJSTP
               GEKIN = GEKIN - DESTEP
               VECT(1) = RIN(1)
               VECT(2) = RIN(2)
               VECT(3) = RIN(3)
               CALL CFEDEP(2,VECT,DESTEP)
            ENDIF
         ENDIF
11    CONTINUE
      DO 13 I = 1,6
         VECT(I) = VOUT(I)
13    CONTINUE
      VECT(7) = GEKIN
      DESTEP = 0.
      ISTOP = 0
      INWVOL = 2
      NMEC = NMEC + 1
      LMEC(NMEC) = 1
      END
