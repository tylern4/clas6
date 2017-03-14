      subroutine step_ic1
      
      implicit none
      
      integer nubuf,nvert,jv
      real ubuf(1),vvert(3),pvvert(3)
      
#include "gcunit.inc"
#include "gckine.inc"
#include "gcsets.inc"
#include "gctrak.inc"
#include "gcvolu.inc"
#include "gcbank.inc"
#include "icsets.inc"
#include "icgeom.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_ic.inc"

c  tdc_sens = refractive index / (speed of light * cos( average photon angle))
c    n = 1.58   c = 3. 10e8   angle = 15.2 degrees
c    unit : nano-second per centimeter

      real tdc_sens
      parameter ( tdc_sens   = 0.0546 )
      
      integer ihit
      real hit(5),att_dis
      
c      print *, 'IC1-STEP: id,inwvol,istop',idtype,inwvol,istop

c      print *, 'IC1-STEP: id_ic1',idtype_ic1
            
      if (nohits(ic)) return
      
      if (idtype.eq.idtype_ic1) then
      
c Calculate distance from track to far end of crystal
      
        att_dis = ic_z0+2*icvol1c(5)-hit(3)
              
        if (inwvol.eq.1) then
          call ucopy(vect,hit,3)
          hit(4) = tofg*1.e09+att_dis*tdc_sens
          hit(5) = 0
        elseif (inwvol.eq.2.or.istop.ne.0) then
          hit(4) = (hit(4)+tofg*1.e09+att_dis*tdc_sens)*0.5
          hit(5) = hit(5)+destep
        else
          hit(5) = hit(5)+destep
          return
        endif
        
c        print *, 'IC1-STEP: iset,idet,itra,tofg',iset,idet,itra,tofg
c        print *, 'IC1-STEP: numbv',numbv(1),numbv(2),numbv(3),numbv(4)
c        print *, 'IC1-STEP: hit',hit(1),hit(2),hit(3),hit(4),hit(5)
                
        if (inwvol.ne.2.and.istop.eq.0) return
        if (hit(5).le.10.e-9)           return
         
c        print *,'IC-STEP: Storing hit'
        call gschit(iset,idet,itra,numbv,hit,1,ihit)
        
      endif
      
      return
      end
   
