      subroutine step_sh
      
      implicit none
      
      integer nubuf,nvert,jv
      real ubuf(1),vvert(3),pvvert(3)
      
#include "gcunit.inc"
#include "gckine.inc"
#include "gcking.inc"
#include "gcsets.inc"
#include "gctrak.inc"
#include "gcvolu.inc"
#include "gcbank.inc"
#include "gcflag.inc"
#include "shsets.inc"
#include "shgeom.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_sh.inc"
#include "gctmed.inc"


c  local variables:
c  ----------------
      real hit(nhits_hd),ebxd(3)
      real vertex(3), p(4), ubuff(1)
      integer nubuff,ihit,itrk,num_vertex,ipart_num
      real hith(nhits_hdh)

      integer ig,ip,id,itypa
C     translate volume names for all levels from INTEGER to CHARACTER*4

      CHARACTER*4 volnam(15)
      EQUIVALENCE(names(1),volnam(1))
c
c  tdc_sens = refractive index / (speed of light * cos( average photon angle))
c    n = 1.58   c = 3. 10e8   angle = 15.2 degrees
c    unit : nano-second per centimeter

      real tdc_sens,att_dis
      parameter ( tdc_sens   = 0.0546 )
      
c_end_var 
c     
c  executable code:         
c         
      if (nohits(sh)) return
c
c  SH data hits      
         if (idtype.eq.idtype_hd) then
c             
      att_dis = 1.
           if ((inwvol.eq.1).and.(nstep.gt.1).and.
     1  (infrom.eq.0).and.(volnam(nlevel).eq.'SPIX')) then     ! When entering the volume.
c       
      call ucopy(vect,hit,3)  ! Copy position from hit vector to "hit".
c
      hit(4) = abs(tofg)*1.e09+att_dis*tdc_sens ! tofg from gctrak: current time of flight.
c         
      hit(5) = destep         ! destep from gctrak: total energy loss in current step.

          elseif (inwvol.eq.2.or.istop.ne.0) then ! exiting volume, or no longer tracking.

      hit(4) = (hit(4)+abs(tofg)*1.e09+att_dis*tdc_sens)*0.5   
      hit(5) = hit(5)+destep
          else
      hit(5) = hit(5)+destep

               return
         endif
c        
        if (inwvol.ne.2.and.istop.eq.0) return
        if (hit(5).le.10.e-9)           return
         
c        print *,'SH-STEP: Storing hit'

        call gschit(iset,idet,itra,numbv,hit,1,ihit)

      endif
      
      return

       end
   
c--------------------------------------------------------------------------
