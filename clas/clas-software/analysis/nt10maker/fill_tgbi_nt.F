      SUBROUTINE FILL_TGBI_NT
      
      IMPLICIT NONE
      SAVE
      
#include "bcs.inc"
#include "tgbi_nt.inc"

      INTEGER ITGBI
      DATA ITGBI /0/
      integer ind
      INTEGER namind
      external namind


      IF (ITGBI.eq.0) ITGBI=NAMIND('TGBI')
      
      ind	= 0
      
      IF (ITGBI.gt.0) ind = IW(ITGBI)

      IF (ind.NE.0) THEN
         latch1 = IW(ind+1)
      ENDIF
      
      END
