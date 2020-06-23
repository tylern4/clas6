      subroutine fill_part_nt

      IMPLICIT NONE
      SAVE
      
#include "bcs.inc"
#include "PARTnt.inc"

      integer mamind, nami
      integer ind,nrow
      integer indx, j
      data nami/0/ 

      If (nami.eq.0) nami = mamind(iw,'PART')
      
      ind	= nami+1           

      ind 	= IW(ind-1)
      
      if(ind.eq.0) go to 99

      nrow 	= IW(ind-4)
      nPRT  = nrow
      do j = 1, nrow 
        indx   = ind+(j-1)*13
        pidPART(j) 	= IW(indx+1)
        xPART(j) 	= RW(indx+2) 
        yPART(j) 	= RW(indx+3) 
        zPART(j) 	= RW(indx+4) 
        ePART(j)    = RW(indx+5) 
        pxPART(j) 	= RW(indx+6) 
        pyPART(j) 	= RW(indx+7) 
        pzPART(j)	= RW(indx+8) 
        qPART(j) 	= RW(indx+9) 
        flagsPART(j) = IW(indx+13)
      enddo

99    return

      end
