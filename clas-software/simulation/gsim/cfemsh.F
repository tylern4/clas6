      SUBROUTINE CFEMSH
CA)========================================(begin of short description)
C EM SHower parametrization
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
C    Side effects
c     variable destep in gctrak common is filled
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
      REAL AEM,BEM,FACT,ELAST,X0TOT
      SAVE AEM,BEM,FACT,ELAST,X0TOT
C  Local variables
      REAL RIN(7), VIN(3), VOU(3)
      REAL ECRIT,FGEKIN
      REAL ENOW,DEJSTP,RTRNS,PHI
      INTEGER NS,JSTEP,NSPLIT,ISPLIT,I
      real rndm(1)
C  External references
      REAL CFGAM1
C  Statement functions
C  Local data statements
CE)===============================================(end of declarations)
C
c      call recmes('CFEMSH','I','CFEMSH reached')
c
c turn off all mechanisms
c
      NMEC = 0
c
c if energy less then cutoff, dump the energy and stop the particle
c 30=STOP
c cfedep calls step_ec (do this before zeroing energy counters)
c
c      write(6,*) 'cfemsh -i- if gekin lt shcute',gekin,shcute
      IF( GEKIN .LT. SHCUTE) THEN
          NMEC =  NMEC + 1
          LMEC(NMEC) = 30
          DESTEP = GEKIN
          CALL CFEDEP(1,VECT,DESTEP)
          GEKIN = 0.
          VECT(7) = 0.
          ISTOP = 1
          INWVOL = 1
          DESTEP = 0.
          RETURN
      ENDIF
C
C Set up PARAMS  based on Z of beginning of shower
c see /gcmate/
C
c      write(*,*) ' cfemsh: SLENG Z GEKIN=',SLENG,Z,GEKIN
      IF(SLENG.EQ.0.) THEN
   2    IF (IPARAE.EQ.1) THEN
          ECRIT = 0.55/Z
          BEM = 0.5
          AEM = BEM*LOG(GEKIN/ECRIT)
         ELSE IF (IPARAE.EQ.2) THEN
CBOY          BEM = 0.434
CBOY          AEM = SCALE*(2.7059 + 0.3706 * ALOG( GEKIN ) )
          BEM = 0.48
          AEM = SCALE*(2.481 + 0.5489*ALOG( GEKIN ) )
        END IF
        CALL grndm(rndm,1)
c        write(*,*) ' cfemsh: AEM,RNDM,SIGTHE=',AEM,RNDM(1),SIGTHE
        AEM = AEM + RNDM(1)*SIGTHE
        IF( AEM .LE. 0.) GOTO 2
   3    CALL grndm(rndm,1)
        FGEKIN = 1. + rndm(1)*(RESO/SQRT(GEKIN)+RESOC)
        IF (FGEKIN.LE.0.) GOTO 3
        GEKIN = GEKIN*FGEKIN
        GEKIN = GEKIN/EMUTO
        FACT = GEKIN
        ELAST = 0.0
        X0TOT =0.
        CALL EULER1(VECT(4))
      ENDIF
*      write(*,*) ' cfemsh: X0TOT=',X0TOT
C
C determ step size
c step - from /gctrak/
c radl - from /gcmate/ (0.56,41.9)
c stepx0 - adjustable parameter - cfblkd.F
c
c      write(*,*) ' cfemsh: STEP,STEPX0,RADL=',STEP,STEPX0,RADL
      NS = NINT( STEP/(STEPX0*RADL) )
      IF ( NS.GT.0 ) THEN
        STEP = STEP/FLOAT(NS)
      ELSE
        NS = 1
      ENDIF
c      write(*,*) ' cfemsh: NS=',NS
C
C STEP and find DE for each step
C
      RIN(1) = VECT(1) - VECT(4) * STEP/2.
      RIN(2) = VECT(2) - VECT(5) * STEP/2.
      RIN(3) = VECT(3) - VECT(6) * STEP/2.
c
c 28=LOOP
c
      NMEC = NMEC + 1
      LMEC( NMEC) = 28

      DO 11 JSTEP = 1, NS
         X0TOT = X0TOT + STEP/RADL
c         write(*,*) ' cfemsh: STEP,RADL,X0TOT=',STEP,RADL,X0TOT
         SLENG = SLENG + STEP
         RIN(1) = RIN(1) + VECT(4) * STEP
         RIN(2) = RIN(2) + VECT(5) * STEP
         RIN(3) = RIN(3) + VECT(6) * STEP
         ENOW = FACT*CFGAM1(AEM,BEM*X0TOT)
         DEJSTP = ENOW - ELAST
c         write(*,*) ' cfemsh: ENOW,ELAST,DEJSTP=',ENOW,ELAST,DEJSTP
         ELAST = ENOW
         IF(DEJSTP.GT.SHCUTE ) THEN
            IF(LTRNE) THEN
C want em transverse fluctuations
              IF(LESPOT) THEN
C want nspot prop to energy
                NSPLIT = NINT(DEJSTP/SHSPOE)
                IF (NSPLIT.LT.3) NSPLIT=3
              ELSE
                NSPLIT =3
              END IF
            ELSE
              NSPLIT = 1
            END IF
*            write(*,*) ' cfemsh: NSPLIT=',NSPLIT
            DO 12 ISPLIT = 1,NSPLIT
              DESTEP = DEJSTP/NSPLIT
*              write(*,*) ' cfemsh: DESTEP=',DESTEP
              IF(RADL.GT.500.) THEN
C for light materials
                RTRNS = 0.01
              ELSE
                CALL CFR2HX( GEKIN, X0TOT, RTRNS )
                RTRNS = RTRNS*RADL
              END IF
              call grndm(rndm,1)
              PHI = TWOPI*RNDM(1)
              VIN(1) = 0.0
              VIN(2) = RTRNS*COS(PHI)
              VIN(3) = RTRNS*SIN(PHI)
              IF(LTRNE) THEN
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
c      write(*,*) ' cfemsh: destep1=',destep
              CALL CFEDEP(1,VECT,DESTEP)
C
  12          CONTINUE

C energy to low. just forget it

        ELSE
           IF(DEJSTP .GT. 0. )THEN
              DESTEP = DEJSTP
              GEKIN = GEKIN - DESTEP
              VECT(1) = RIN(1)
              VECT(2) = RIN(2)
              VECT(3) = RIN(3)
*      write(*,*) ' cfemsh: destep2=',destep
              CALL CFEDEP(1,VECT,DESTEP)
           ENDIF
         ENDIF
11       CONTINUE
      DO 13 I = 1,6
         VECT(I) = VOUT(I)
13    CONTINUE
c
c particle continues on with whatever energy (gekin) it has left 
c istop=0 normal transport
c invol=2 exiting a volume 
c lmec =1  NEXT
c
      VECT(7) = GEKIN
      DESTEP = 0.
      ISTOP = 0
      INWVOL = 2
      NMEC = NMEC + 1
      LMEC(NMEC) = 1
C
      RETURN
      END
