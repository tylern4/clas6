      subroutine ffky_dvcs
      
      implicit none
      
#include "ffpar_dvcs.inc"

      call ffkey('DVCSPOS',dvcs_pos,3,'REAL')
      dvcs_pos(1) = 0.0
      dvcs_pos(2) = 0.0
      dvcs_pos(3) = -66.0  
c
c     for now not used anywhere
c  
      return
      end
      

