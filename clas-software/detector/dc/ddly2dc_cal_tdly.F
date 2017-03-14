      SUBROUTINE ddly2dc_cal_tdly()
C
C
C Fill the dc_cal_tdly common from BOS banks
C
C
      IMPLICIT NONE
C***********INCLUDE FILES***************
#include "dc_cal_tdly.inc"
*
****************************************
*
******************************
      call ddly_to_t0(t0)
      call ddly_to_status(stat)
*****************************************************
      RETURN
      END







