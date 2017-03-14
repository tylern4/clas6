      subroutine fill_tgpb_nt
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c fill_tgpb_nt -- fill the TGPB block of the N-tuple, if   c
c it exists.                                               c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c John Price, UCLA                                         c
c 1 September 1999                                         c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IMPLICIT NONE
      SAVE
      
#include "bcs.inc"
#include "tgpb_nt.inc"

      integer mamind, nami
      integer ind,nrow, ncol
      integer indx, j
      data nami/0/ 

      If (nami.eq.0) nami = mamind(iw,'TGPB')
      
      ind	= nami+1           

      ind 	= IW(ind-1)
      
      if(ind .gt. 0) then
         nrow = IW(ind-4)
         ncol = iw(ind-5)
         nphotons  = nrow
         do j = 1, nrow 
            indx   = ind+(j-1)*ncol
            tgpb_pointer(j) = IW(indx+1)
            tgpb_Time(j) = RW(indx+2) 
            tgpb_Energy(j) = RW(indx+3) 
            tgpb_dt(j) = RW(indx+4) 
         end do
      end if

      end
