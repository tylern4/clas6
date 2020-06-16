      subroutine fill_part_nt

      IMPLICIT NONE
      SAVE
      
#include "bcs.inc"
#include "PARTnt.inc"

      integer mamind, nami
      integer ind,nrow,j,k,nd,ncol,indx
      data nami/0/ 

      nami = mamind(iw,'PART')
      ind  = nami+1           
      nPRT = 0
      ind 	= IW(ind-1)      
      if(ind.eq.0) go to 99
      nrow 	= IW(ind-4)
      if(nrow.gt.maxPART) nrow=maxPART
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
         ePART(nPRT)    = RW(ind+1) 
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

 99    continue

      nami = mamind(iw,'TBID')
      nTBID=0
      IND=IW(nami)
      IF(IND.NE.0) THEN
         ND=IW(IND)
         ncol=IW(ind-5)
         DO j = 1, ND, ncol
            INDX=IND+j-1
               nTBID=nTBID+1
               trkTBID(nTBID) = IW(INDX+1)
               secTBID(nTBID) = IW(INDX+2)
               bTBID(nTBID)   = RW(INDX+3)
               vtTBID(nTBID)  = RW(INDX+4)
               scsTBID(nTBID) = IW(INDX+5)
               sciTBID(nTBID) = IW(INDX+6)
               sctTBID(nTBID) = RW(INDX+7)
               scqTBID(nTBID) = RW(INDX+8)
               scvTBID(nTBID) = RW(INDX+9)
               scbTBID(nTBID) = RW(INDX+10)
               ecsTBID(nTBID) = IW(INDX+17)
               eciTBID(nTBID) = IW(INDX+18) 
               ectTBID(nTBID) = RW(INDX+19)
               ecqTBID(nTBID) = RW(INDX+20) 
               ecvTBID(nTBID) = RW(INDX+21)
               ecbTBID(nTBID) = RW(INDX+22)
               stsTBID(nTBID) = IW(INDX+23)
               stiTBID(nTBID) = IW(INDX+24)
               sttTBID(nTBID) = RW(INDX+25)
               stqTBID(nTBID) = RW(INDX+26)
               stvTBID(nTBID) = RW(INDX+27) 
               stbTBID(nTBID) = RW(INDX+28)
               lcsTBID(nTBID) = IW(INDX+29)
               lciTBID(nTBID) = IW(INDX+30)
               lctTBID(nTBID) = RW(INDX+31)
               lcqTBID(nTBID) = RW(INDX+32)
               lcvTBID(nTBID) = RW(INDX+33)
               lcbTBID(nTBID) = RW(INDX+34) 
         ENDDO
      ENDIF




      end
