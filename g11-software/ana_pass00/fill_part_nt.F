      subroutine fill_part_nt

      IMPLICIT NONE
      SAVE
      
#include "bcs.inc"
#include "PARTnt.inc"

      integer mamind, nami
      integer ind,nrow,j,k
      data nami/0/ 

      If (nami.eq.0) nami = mamind(iw,'PART')
      
      ind	= nami+1           
      nPRT 	= 0

      ind 	= IW(ind-1)
      
      if(ind.eq.0) go to 99

      nrow 	= IW(ind-4)

      k = 0
      
      do j = 1, nrow 
        nPRT 		= nPRT+1
        pidPART(nPRT) 	= IW(ind+1)
        ind 		= ind + 1
        xPART(nPRT) 	= RW(ind+1) 
        ind 		= ind + 1
        yPART(nPRT) 	= RW(ind+1) 
        ind 		= ind + 1
        zPART(nPRT) 	= RW(ind+1) 
        ind 		= ind + 1
        ePART(nPRT)    	= RW(ind+1) 
        ind 		= ind + 1
        pxPART(nPRT) 	= RW(ind+1) 
        ind 		= ind + 1
        pyPART(nPRT) 	= RW(ind+1) 
        ind 		= ind + 1 
        pzPART(nPRT)	= RW(ind+1) 
        ind 		= ind + 1 
        qPART(nPRT) 	= RW(ind+1) 
        ind 		= ind + 1 
        trkPART(nPRT)	= IW(ind+1) 
        ind 		= ind + 1 
        qpiPART(nPRT)	= RW(ind+1) 
        ind 		= ind + 1 
        qtrPART(nPRT)	= RW(ind+1) 
        ind 		= ind + 1 
        flgPART(nPRT)	= IW(ind+1) 
        ind 		= ind + 1 
      enddo

99    return

      end
