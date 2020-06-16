      SUBROUTINE make_dc1_digi()
C
C
C pull the digi common out of trk_event_hbt routine
C			- J. J. Manak, March 1998
C
C***********INCLUDE FILES***************
#include "sdadigi.inc"
*
****************************************
*
******************************

      call make_dc1_banks(dc_digi, dc_ndig, pt_digi)

*****************************************************
      RETURN
      END







