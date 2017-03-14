*
* $Id: gtnino.F,v 1.1 1998/08/27 18:54:13 tedeschi Exp $
*
* $Log: gtnino.F,v $
* Revision 1.1  1998/08/27 18:54:13  tedeschi
*  patched geant routine for cf shower param
*
* Revision 1.1.1.1  1995/10/24 10:21:44  cernlib
* Geant
*
*
#include "pilot.h"
*CMZ :  3.21/04 01/11/94  16.25.59  by  S.Ravndal
*-- Author :
      SUBROUTINE GTNINO
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Tracking routine for "GEANTINO" type tracks              *
C.    *                                                                *
C.    *    ==>Called by : GTRACK                                       *
C.    *       Author    R.Brun, F.Bruyant                              *
C.    *                                                                *
C.    ******************************************************************
C.
      implicit none
#include "gconsp.inc"
#include "gctmed.inc"
#include "gctrak.inc"
cf#if defined(CERNLIB_DEBUG)
cf#include "gcunit.inc"
cf#endif
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C                CF patch
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#include "gckine.inc"
#include "cfpid.inc"
      INTEGER I, IPROC
      INTEGER IDUM
      REAL VSAV(7)
      REAL VECTMP
      REAL EPSMAC
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C             end of CF patch
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cf#if !defined(CERNLIB_SINGLE)
      PARAMETER (EPSMAC=1.E-6)
cf#endif
cf#if defined(CERNLIB_SINGLE)
cf      PARAMETER (EPSMAC=1.E-11)
cf#endif
C.
C.    ------------------------------------------------------------------
C.
      STEP = STEMAX
      IPROC = 103
      CALL GTNEXT
      IF (IGNEXT.NE.0) THEN
         STEP = SNEXT + PREC
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C                CF patch
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c        call recmes('GTNINO','I','GTNINO reached')

         IF(IPART.EQ.IDCF1.OR.IPART.EQ.IDCF2) THEN
           DO I = 1,3
             VOUT(I+3) = VECT(I+3)
             VOUT(I) = VECT(I) + STEP * VECT(I+3)
             VSAV(I) = VECT(I)
             VSAV(I+3) = VECT(I+3)
           ENDDO
           VOUT(7) = VECT(7)
           VSAV(7) = VECT(7)
C
           IF(IPART.EQ.IDCF1) THEN
             CALL CFEMSH
            ELSE IF(IPART.EQ.IDCF2) THEN
             CALL CFHDSH
           ENDIF
C restore idet iset etc.
           CALL GTMEDI(VSAV(1),IDUM)
           CALL GFINDS
C
           RETURN
         ENDIF
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C             end of CF patch
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IPROC=0
         INWVOL= 2
         NMEC =  1
         LMEC(1)=1
*
* *** Linear transport
*
         DO 10 I = 1,3
            VECTMP  = VECT(I) +STEP*VECT(I+3)
            IF(VECTMP.EQ.VECT(I)) THEN
*
* *** Correct for machine precision
*
               IF(VECT(I+3).NE.0.) THEN
                  VECTMP = VECT(I)+ABS(VECT(I))*SIGN(1.,VECT(I+3))*
     +            EPSMAC
*                 IF(NMEC.GT.0) THEN
*                    IF(LMEC(NMEC).EQ.32) NMEC=NMEC-1
*                 ENDIF
*                 NMEC=NMEC+1
*                 LMEC(NMEC)=32
#if defined(CERNLIB_DEBUG)
                  WRITE(CHMAIL, 10000)
                  CALL GMAIL(0,0)
                  WRITE(CHMAIL, 10100) GEKIN, NUMED, STEP, SNEXT
                  CALL GMAIL(0,0)
10000 FORMAT(' Boundary correction in GTNINO: ',
     +       '    GEKIN      NUMED       STEP      SNEXT')
10100 FORMAT(31X,E10.3,1X,I10,1X,E10.3,1X,E10.3,1X)
#endif
               ENDIF
            ENDIF
            VECT(I) = VECTMP
   10    CONTINUE
      ELSE
         DO 20 I = 1,3
            VECT(I)  = VECT(I) +STEP*VECT(I+3)
   20    CONTINUE
      ENDIF
      SLENG = SLENG +STEP
*
* *** Update time of flight
*
      TOFG = TOFG +STEP/CLIGHT
*
      IF(IPROC.NE.0) THEN
         NMEC=NMEC+1
         LMEC(NMEC)=IPROC
      ENDIF
      END
