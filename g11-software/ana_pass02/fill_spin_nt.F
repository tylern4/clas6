      subroutine fill_spin_nt

      IMPLICIT NONE
      SAVE
      
#include "bcs.inc"
#include "SPINnt.inc"

      integer mamind, nami
      integer ind,nrow,j,k
      data nami/0/ 

      If (nami.eq.0) nami = mamind(iw,'SPIN')
      
      ind	= nami+1           
      nSPN 	= 0

      ind 	= IW(ind-1)
      
      if(ind.eq.0) go to 99

      nrow 	= IW(ind-4)

      k = 0
      
      do j = 1, nrow 
        nSPN 		= nSPN+1
        xSPIN(nSPN) 	= IW(ind+1)
        ind 		= ind + 1
        ySPIN(nSPN) 	= IW(ind+1) 
        ind 		= ind + 1
        zSPIN(nSPN) 	= IW(ind+1) 
        ind 		= ind + 1 
      enddo

99    return

      end
