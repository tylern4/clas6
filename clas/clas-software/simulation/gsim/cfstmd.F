      SUBROUTINE CFSTMD(IMED)
CA)========================================(begin of short description)
C Set or Remove a tracking medium to the list of
C    starting the parametrization
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods :
C
C    Called by:
C                  init_ec.F   (DJT 6/29/98)
C
C    Input arguments :
C      IMED : Tracking medium number
C       IMED > 0  :  Set to the list
C       IMED < 0  :  Remove from the list
C
C    Input/output arguments :  None
C
C    Output arguments:  None
C
C    Side effects (other variables/banks modified, etc.)
C
C    Created:   20-APR-1994  Serguei Boiarinov
C    Modified:  06-JUN-1998  David Tedeschi
C
CC)-------------------------------------------(end of long description)
      implicit none
CD)----------------------------------(declare common blocks, variables)
C  Common blocks
#include "cftmed.inc"
C  Variables in argument list
      INTEGER IMED
C  Save variables
C  Local variables
      INTEGER K,L
C  External references
      INTEGER IUCOMP
C  Statement functions
C  Local data statements
CE)===============================================(end of declarations)
C
      K=IUCOMP(IABS(IMED),ISHT,99)
      IF (IMED.GT.0) THEN
       IF (K.GT.0) THEN
        WRITE(*,*)'>>>>>>>>> ERROR IN CFSTMD <<<<<<<<<<'
        WRITE(*,*)'TRACKING MEDIA NUMBER',IMED,
     &                 ' IS ALREADY DEFINED.'
       ELSE
        NSHT=NSHT+1
        ISHT(NSHT)=IMED
       END IF
      ELSE IF (IMED.LT.0) THEN
       IF (K.EQ.0) THEN
        WRITE(*,*)'>>>>>>>>> ERROR IN CFSTMD <<<<<<<<<<'
        WRITE(*,*)'TRACKING MEDIA NUMBER',IMED,' IS NOT DEFINED.'
       ELSE
        DO 11 L=K+1,NSHT
        ISHT(L-1)=ISHT(L)
11      CONTINUE
        NSHT=NSHT-1
       END IF
      END IF
C
      RETURN
      END
