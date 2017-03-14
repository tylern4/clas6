      subroutine cc_bor(irun)

      implicit none
      save 

#include "cccal.inc"

      integer irun

      print *, 'Read CC pedestals from Map - Run ',irun
      call cc_read_map(irun,ccpedes,'CC_CALIB','pedestals','mean')
      
      return
      end
