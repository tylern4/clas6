      subroutine ec_bor(irun)
      
      implicit none
      save
      
#include "eccal.inc"

      integer i,j,k,l,lay,irun
      
      real EcCal_Eo(36,3,3,6)
      real EcCal_Ech(36,3,3,6)

      do i = 1, 6
        do j = 1,3
          do k = 1,3
            do l = 1, 36
              EcCal_Eo(l,k,j,i) = 0.0
              EcCal_Ech(l,k,j,i) = 0.0001
            enddo
          enddo
        enddo
      enddo
      
      print *,'Read EC pedestals from Map - Run ',irun
        
      call ec_read_map(irun,EcCal_Eo,'EC_PEDESTALS',400.0)
        
      do i = 1,6
        do j = 2,3
          do k = 1,3
            lay = (j-2)*3+k
            do l = 1,36
              ecped(l,lay,i) = EcCal_Eo(l,k,j,i)
            enddo
          enddo
        enddo
      enddo
                      
      return
      end

