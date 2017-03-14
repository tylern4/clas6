      subroutine get_EVNT
      IMPLICIT NONE
      SAVE
      include "bcs.inc"
      include "ntpl_com.inc"
*
*      include "EC1Rnt.inc"
      
*
      integer namind, nami, minIND, minTime
      integer namk, indk, indh, namh
      integer ind,nd,nrow,j
      INTEGER iEVNT,indEVNT,nrowEVNT
      INTEGER iMCTK,indMCTK,nrowMCTK
      data nami/0/ 
      

c  get links to EVNT banks 

         indEVNT = namind('EVNT')
         iEVNT   = indEVNT + 1
         nEVNT = 0
333       continue 
         iEVNT = IW (iEVNT-1)                        ! next index
         If (iEVNT.EQ.0) goto 999
         nrowEVNT   = IW(iEVNT -4)
c 
         DO j=1,nrowEVNT
             nEVNT = nEVNT +1
             idEVNT    (nEVNT) = iw(iEVNT+1 +18*(j-1))
             pmomEVNT  (nEVNT) = rw(iEVNT+2 +18*(j-1))
             massEVNT  (nEVNT) = rw(iEVNT+3 +18*(j-1))
             chargeEVNT(nEVNT) = iw(iEVNT+4 +18*(j-1))
             cxEVNT    (nEVNT) = rw(iEVNT+6 +18*(j-1))
             cyEVNT    (nEVNT) = rw(iEVNT+7 +18*(j-1))
             czEVNT    (nEVNT) = rw(iEVNT+8 +18*(j-1))
             xEVNT     (nEVNT) = rw(iEVNT+9 +18*(j-1))
             yEVNT     (nEVNT) = rw(iEVNT+10+18*(j-1))
             zEVNT     (nEVNT) = rw(iEVNT+11+18*(j-1))
             if(idEVNT (nEVNT) .eq.0) massEVNT(nEVNT) = -1
         ENDDO        ! next track

c         goto 333      ! next sector
         
c         END IF
999       continue                                    ! end TRKL section

c  get links to MCTK bank 

         indMCTK = namind('MCTK')
         iMCTK   = indMCTK + 1
         nMCTK = 0
         iMCTK = IW (iMCTK-1)                        ! next index
         If (iMCTK.EQ.0) goto 9999
         nrowMCTK   = IW(iMCTK -4)
c 
         DO j=1,nrowMCTK
             nMCTK = nMCTK +1
             pmomMCTK    (nMCTK) = rw(iMCTK+4 +11*(j-1))
             chargeMCTK  (nMCTK) = rw(iMCTK+6 +11*(j-1))
             idMCTK      (nMCTK) = iw(iMCTK+7 +11*(j-1))
             V_MC_CX     (nMCTK) = rw(iMCTK+1 +11*(j-1))   
             V_MC_CY     (nMCTK) = rw(iMCTK+2 +11*(j-1))  
             V_MC_CZ     (nMCTK) = rw(iMCTK+3 +11*(j-1))
         ENDDO        
9999     continue
      return
      end
