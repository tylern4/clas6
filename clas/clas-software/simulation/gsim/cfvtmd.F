      SUBROUTINE CFVTMD(NAM,MED)
CA)========================================(begin of short description)
C Convert from a volume name to a tracking medium number
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods :
C
C    Called by: CFCOMD
C
C    Input arguments :
C          NAM C*4 : Volume name
C    Input/output arguments :
C
C    Output arguments:
C          MED Int : Tracking medium number
C                    0 for the volume is not defined
C    Side effects (other variables/banks modified, etc.)
C
C    Created:   20-APR-1994  Serguei Boiarinov
C    Modified:  29-JUN-1998  David Tedeschi
C
CC)-------------------------------------------(end of long description)
      implicit none
CD)----------------------------------(declare common blocks, variables)
C  Common blocks
#include "gcbank.inc"
#include "gcnum.inc"
C  Variables in argument list
      CHARACTER*4 NAM
      INTEGER     MED
C  Save variables
C  Local variables
      INTEGER     IVO,JVO
C  External references
C  Statement functions
C  Local data statements
CE)===============================================(end of declarations)
C
      CALL GLOOK(NAM,IQ(JVOLUM+1),NVOLUM,IVO)
      IF (IVO.LE.0) THEN
        MED=0
        RETURN
      ENDIF
      JVO=LQ(JVOLUM-IVO)
      MED=Q(JVO+4)
      RETURN
      END
