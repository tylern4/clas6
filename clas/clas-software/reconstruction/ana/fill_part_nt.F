      subroutine fill_part_nt(ierr_part)

      IMPLICIT NONE
      SAVE
      
#include "bcs.inc"
#include "PARTnt.inc"

      integer mamind, nami,ierr_part
      integer ind,nrow,j,k,it_part
      data nami/0/ 

      If (nami.eq.0) nami = mamind(iw,'PART')
      ind	= nami+1           
      nPRT 	= 0
      it_part=1
      ind 	= IW(ind-1)
      
      if(ind.eq.0) go to 99
      ierr_part=1
      if(IW(ind-2).eq.0)ierr_part=11
      nrow 	= IW(ind-4)

      k = 0
      
      do j = 1, nrow 
        nPRT 		= nPRT+1
        pidPART(nPRT) 	= IW(ind+1)
        xPART(nPRT) 	= RW(ind+2) 
        yPART(nPRT) 	= RW(ind+3) 
        zPART(nPRT) 	= RW(ind+4) 
        ePART(nPRT)    	= RW(ind+5) 
        pxPART(nPRT) 	= RW(ind+6) 
        pyPART(nPRT) 	= RW(ind+7) 
        pzPART(nPRT)	= RW(ind+8) 
        qPART(nPRT) 	= RW(ind+9) 
        IPART10(nPRT) 	= IW(ind+10) 
        RPART11(nPRT) 	= RW(ind+11) 
        RPART12(nPRT) 	= RW(ind+12) 
        IPART13(nPRT) 	= IW(ind+13) 
        ind 		= ind + 13 
        it_part=it_part+1
      if(it_part.ge.maxPART) go to 99
      enddo

99    return

      end
