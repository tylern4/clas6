      SUBROUTINE run_control_set_def()
C
C
C fill the run control banks to make seb and tag run outside of 
C  recsis
C			- J. J. Manak, May 1998
C
C***********INCLUDE FILES***************
#include "run_control.inc"
*
****************************************
*
******************************

      ltrk_do = .true.
      legn_do = .true.
      lcc_do  = .true.
      ltof_do = .true.
      lec1_do = .true.
      lseb_do = .true.
      mapmanager = 1

*****************************************************
      RETURN
      END
