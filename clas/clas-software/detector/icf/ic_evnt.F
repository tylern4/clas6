      Subroutine ic_evnt()
c Subroutine to be called for each event
      IMPLICIT NONE
      SAVE
c
#include "bcs.inc"
      integer ierr
c
      ierr=0
      CALL BDROP(IW,'ICHBICPB')
      call ic_read_bos(ierr)
      if(ierr.gt.0)then
         call ic_e_t(ierr)
         if(ierr.gt.0)then
            call ic_group(ierr)
            if(ierr.gt.0)then
               call ichb(ierr)
            endif
         endif
      endif
c
      return
      end
