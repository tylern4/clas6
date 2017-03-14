      subroutine fill_mvrt_nt

      IMPLICIT NONE
      SAVE
      
#include "bcs.inc"
#include "mvrt_nt.inc"

      integer mamind, nami
      integer ind
      data nami/0/ 
         
      If (nami.eq.0) nami = mamind(iw,'MVRT')
      
      ind	= nami+1           

      ind 	= IW(ind-1)
      
      if(ind.eq.0) go to 99

      ntrk_mvrt = IW(ind+2)
      x_mvrt = RW(ind+3) 
      y_mvrt = RW(ind+4) 
      z_mvrt = RW(ind+5) 

99    return

      end




