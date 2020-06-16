      subroutine sc_bor(irun)

      implicit none
      save
      
#include "sccal.inc"
      character*80 fname, crmess
      real left_peds(48,6)
      real right_peds(48,6)
      
      integer i,j,l,irun,first
      
      do i = 1,6
        do j = 1,2
          do l = 1,48
            scped(l,j,i) = 0.
          enddo
        enddo
      enddo
      call revinm('CLAS_PARMS','Maps/SC_CALIBRATIONS.map',fname)

      write(crmess,'(A,I7)')'Read SC pedestals from Map - Run ',irun
      call recmes('SC_BOR','I',crmess)

      call map_get_float(fname,'pedestals','left',288,left_peds,irun,first)
      call map_get_float(fname,'pedestals','right',288,right_peds,irun,first)
      
      do i = 1,6
         do l = 1,48
            scped(l,1,i) = left_peds(l,i)
            scped(l,2,i) = right_peds(l,i)
         enddo
      enddo

      return
      end
