      subroutine digi_ic1

      implicit none

#include "gcunit.inc"
#include "icgeom.inc"
#include "icsets.inc"
#include "lcgeom.inc"
#include "ffpar_ic.inc"

      integer nhmax
      parameter (nhmax=4000)
      real ic_tdc_to_channel
      parameter (ic_tdc_to_channel=20)

      integer numbv(nvic1,nhmax),itra(nhmax),numvs(nvic1),kdigi(ndic1)
      real hits(nhic1,nhmax)

c  -  pedes : pedestal
c  -  sign  : sigma for the gaussian noise distribution [gev]
c  -  ic_spar  : ADC sparcification threshold [channels]
c  -  gain  : ADC conversion gain [channels/GeV] 
c -   eff   : Misc. efficiency 

      real pedes,sign,gain,eff
      data pedes,sign,gain,eff/0.,0.3e-3,1.25e3,1.0/

      real em,ggauss,anode,xmean,sigma
      integer det,idig,i,j,k,ngamma,nhits
      integer ngamma_vect(1)
      real tdc,valmoy
      real nbcrystals
      
      do i=1,nvic1
         numvs(i)=0
      enddo
      
      if (ic_mod.eq.2) then
         nbcrystals = 424
      elseif (ic_mod.eq.3) then
         nbcrystals = 100
      else
      endif

      do i=1,nbcrystals
         numvs(1)=i
         call gfhits('IC  ','IC1 ',nvic1,nhic1,nhmax,0,
     1        numvs,itra,numbv,hits,nhits)
 
c  check for too many hits
         if (nhits.gt.nhmax) then
            print *, 'Too many hits'
            nhits=nhmax
         endif
         
c  sum all adc hits for this block, get earliest tdc hit if energy > 1 MeV
         if(nhits.gt.0)then
            em=0.
            ngamma = 0
            tdc = 999999.
            do k=1,nhits
               em=em+hits(5,k)
c               print *,'em= ',em
               tdc=min(tdc,hits(4,k))
c               print *, 'IC1-DIGI: i,j,k,z,em,tdc ',i,j,k,hits(3,k),em,tdc
            enddo
c            em = em * 1000
            
c     print *, 'IC1-DIGI: tdc,em ',tdc,em
            
c  recalculate adc if poisson statistics requested
            if(ic_pois.ne.0.)then
               valmoy = em * ic_pois * 1000.
               call gpoiss(valmoy,ngamma_vect,1)
               ngamma = ngamma_vect(1)
               
c  Fluctuations in PMT gain distributed using Gaussian with sigma SNR 
c  SNR = sqrt(ngamma)/sqrt(del/del-1) del = dynode gain = 3 
c  (From RCA PMT Handbook) p. 169)
               xmean = float(ngamma)
               sigma = sqrt(xmean)*1.15
               anode = ggauss(xmean,sigma)
               em = gain * anode * eff / (ic_pois * 1000.)
            else
               em = gain*eff*em       
c               em = em
            endif  
            
            
c  make hit only if there is energy deposit in block > ADC sparcification thresh

            if(em.gt.ic_spar) then
               kdigi(1) = ifix( em )
               kdigi(2) = min(65535,int(tdc * ic_tdc_to_channel) )
               
               call gsdigi(set_ic1,det_ic1,itra,nhits,
     1              numvs,kdigi,idig)
            endif         
         endif
      enddo
      
      return
      end
      
