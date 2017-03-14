      subroutine sc_bor(irun)

      implicit none
      save
      
#include "sccal.inc"
      character*80 fname, crmess
      real left_peds(342)
      real right_peds(342)
      integer get_sc_version_flag
      integer i,j,l,irun,first
      
      call vzero(scped,342)
      sc_version=get_sc_version_flag(irun)
      if(sc_version.eq.2) then
         n_scpd_sec=57
         call revinm('CLAS_PARMS','Maps/SC_CALIBRATIONS_V2.map',fname)
      else
         n_scpd_sec=48
         call revinm('CLAS_PARMS','Maps/SC_CALIBRATIONS.map',fname)
      endif
      write(crmess,'(A,I7)')'Read SC pedestals from Map - Run ',irun
      call recmes('SC_BOR','I',crmess)

      call map_get_float(fname,'pedestals','left',n_scpd_sec*6,left_peds,irun,first)
      call map_get_float(fname,'pedestals','right',n_scpd_sec*6,right_peds,irun,first)
      
      do i = 0,5,1
         do l = 1,n_scpd_sec
            scped(l,1,i+1) = left_peds(l+i*n_scpd_sec)
            scped(l,2,i+1) = right_peds(l+i*n_scpd_sec)
         enddo
      enddo

      return
      end
