      subroutine read_bank(ierr)
      implicit none
      
#include "bcs.inc"
#include "mc.inc"

      integer ind,ierr,indx,j,nami,col,row,ierr1
      real    kf(3)
      INTEGER NAMIND
      EXTERNAL NAMIND
c      
      data nami /0/

      call frbos(IW,32,'E',ierr1)
      if(ierr1.eq.-1) then
        write(6,*) 'EOF reached. Run Stopped'
        call bos_end
        STOP
      elseif(ierr1.ne.0) then
        write(6,*) 'Error reading INPUT file'
        ierr = 1
        return
      endif
        
      if(nami.eq.0) nami = NAMIND('PART')
      ind = nami + 1
c  
      if(ind.eq.0) then
        ierr = 1
        return
      endif
      ind = IW(ind-1)
      if(ind.eq.0) then
        ierr = 1
        return
      endif
      row = IW(ind-4)
      col = IW(ind-5)
c      
      do j= 1, row
         indx   = ind+(j-1)*13
         id(j)  = iw(indx+1)		    ! particle ID (GEANT)
         v(j,1) = rw(indx+2)			! x vertex position
         v(j,2) = rw(indx+3)			! y vertex position
         v(j,3)	= rw(indx+4)			! z vertex position
         p(j,4) = rw(indx+5)			! energy 
         p(j,1)	= rw(indx+6)			! px
         p(j,2) = rw(indx+7)			! py
         p(j,3) = rw(indx+8)			! pz
         q(j)   = rw(indx+9)			! charge
      enddo
      if(id(1).ne.3) then
        ierr = 1
        print *, 'the particle is not electron'
        return
      endif

      return
      END
