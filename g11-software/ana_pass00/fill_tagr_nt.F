      subroutine fill_tagr_nt(ierr)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c fill_tagr_nt -- fill the TAGR block of the N-tuple, if   c
c it exists.                                               c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c John Price, UCLA                                         c
c 1 September 1999                                         c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      SAVE
      
#include "bcs.inc"
#include "tagr_nt.inc"

      integer mamind, nami, ierr
      integer ind,nrow, ncol
      integer indx, j
      data nami/0/ 

      If (nami.eq.0) nami = mamind(iw,'TAGR')
      
      ind	= nami+1           

      ind 	= IW(ind-1)
      
      if(ind .gt. 0) then
         nrow = IW(ind-4)
         ncol = iw(ind-5)
         nphot_tagr  = nrow
         if(nphot_tagr .gt. Maxphot_tagr) nphot_tagr = Maxphot_tagr
         
         do j = 1, nphot_tagr
            indx   = ind+(j-1)*ncol
            tagr_E_gamma(j) = RW(indx+1)
            tagr_ttag(j) = RW(indx+2) 
            tagr_tpho(j) = RW(indx+3) 
            tagr_stat(j) = IW(indx+4)
            tagr_Tid(j) = IW(indx+5)
            tagr_Eid(j) = IW(indx+6)
 
         end do
      end if

      end



