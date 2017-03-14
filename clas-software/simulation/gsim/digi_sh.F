      subroutine digi_sh

      implicit none

#include "gcunit.inc"
#include "shgeom.inc"
#include "shsets.inc"
#include "lcgeom.inc"
#include "ffpar_sh.inc"

      integer nhmax
      parameter (nhmax=4000)


      integer numbv(nvol_hd,nhmax),itra(nhmax),numvs(nvol_hd),
     &              kdigi(ndigi_hd)
      real hits(nhits_hd,nhmax)

c  -  pedes : pedestal
c  -  sign  : sigma for the gaussian noise distribution [gev]
c  -  thre  : threshold [gev]
c  -  fact  : factor for digitisation

ccc      real pedes,sign,thre,fact
ccc      data  pedes,sign,thre,fact/0.,0.3e-3,1.0e-3,1.0e4/
      real pedes,sign,gain,eff
      data pedes,sign,gain,eff/0.,0.3e-3,2.4e5,1.0/

      real em,ggauss,anode,xmean,sigma
      integer det,idig,i,j,k,ngamma,nhits
      integer ngamma_vect(1)
      real tdc,valmoy
c
      do i=1,nvol_hd
         numvs(i)=0
      enddo
c      
      do i=1,npix
         numvs(1)=i
         call gfhits('SH  ','SPIX',nvol_hd,nhits_hd,nhmax,0,
     1        numvs,itra,numbv,hits,nhits)

c
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
               tdc=min(tdc,hits(4,k))
            enddo
c
c  recalculate adc if poisson statistics requested
            if(sh_pois.ne.0.)then
               valmoy = em * sh_pois * 1000.
               call gpoiss(valmoy,ngamma_vect,1)
               ngamma = ngamma_vect(1)
               
c  Fluctuations in PMT gain distributed using Gaussian with sigma SNR 
c  SNR = sqrt(ngamma)/sqrt(del/del-1) del = dynode gain = 3 
c  (From RCA PMT Handbook) p. 169)
               xmean = float(ngamma)
               sigma = sqrt(xmean)*1.15
               anode = ggauss(xmean,sigma)
               em = gain * anode * eff / (sh_pois * 1000.)
           else
               em = gain*eff*em

            endif            
c  make hit only if there is energy deposit            

            if(em.gt.0) then
c
c  tdc converted to number of channels (20 channels/ns)
               kdigi(1) = ifix( em )
               kdigi(2) = min(65535,int(tdc * sh_tdc_to_channel) )
c
               call gsdigi(set_hd,det_hd,itra,nhits,
     1              numvs,kdigi,idig)
c
            endif         
         endif
      enddo
      
      return
      end
      
