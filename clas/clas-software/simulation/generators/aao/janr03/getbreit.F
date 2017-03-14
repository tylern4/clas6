      subroutine getbreit(s,q2,kgamma,qpion,i,breit,rv)
      
      implicit none
      save
      
      real s,q2,kgamma,qpion
      real wthr,breta,eeta,qreta,reta
      real ww,q22,k22,kkr,x2,w2pion
      real m22,m2,en1,en2,kr22,qr22,kr,qr,rv
      real e2pion,qr2pion,r2pion,rpion,qxl,qx2l
      real gpion,geta,ginel,gtotal,qxgamma,ggamma
      real den,breitw,breit(2),f1,f2,a,jr
      
      integer i,ll,lp
      
#include "gpar.inc"
#include "resonances.inc"
#include "fit_par.inc"
#include "amplitudes.inc"
#include "delta_dat.inc"
      
      a    = 1.0
      wthr = 1.0
      breta = 0.0

      ww	= sqrt(s)
      m22	= mres(i)**2	
      m2	= 2.*mres(i)
      
      if (i.eq.22) then
        breta = 0.55
        if (ww.lt.(mn+meta)) wthr=0.0
        eeta  = (m22+meta**2-mn22)/m2
        qreta = eeta**2-meta**2
        reta  = abs(qeta/qreta)
        reta  = sqrt(reta)
      endif
      
      q22	= qpion**2
      k22	= kgamma**2
      
      ll 	= langul(i)
      lp        = lprime(i)
      jr        = jres(i)*0.5
      x2	= xres(i)**2
      
      if (ww.le.(mn+2.*mp)) q2pion = 0.
      
      kkr       = (m22-mn22)/m2      
                        
      en1	= (m22 + mn22 + q2)/m2
      en2	= (m22 + mn22 - mp22)/m2
      
      kr22	= en1**2 - mn22
      qr22	= en2**2 - mn22
      
      kr	= sqrt(kr22)
      qr	= sqrt(qr22)
      
      rv	= kgamma/kr
      rpion	= qpion/qr
      
      e2pion	= (m22 + m2pion**2 - mn22)/m2
      qr2pion	= e2pion**2 - m2pion**2
      r2pion	= (q2pion/qr2pion)**(ll+2)
           
      qxl	= ((x2 + qr22)/(x2 + q22))**ll
      qx2l	= ((x2 + qr2pion)/(x2 + q2pion))**(ll+2)
      
      gpion	= eta(i)*gres(i)*rpion**(2.*ll+1.)*qxl 
      geta      = breta*gres(i)*reta 
      ginel	= (1.-eta(i)-breta)*gres(i)*r2pion*qx2l
      
      gtotal	= gpion+geta*wthr+ginel
      if (i.eq.1) gtotal=gpion/sqrt(s/m22)+geta*wthr+ginel
      
      qxgamma	= ((x2+kr22)/(x2+k22))**lp
      ggamma	= gres(i)*rv**(2.*lp+1.)*qxgamma
      
      den	= (m22-s)**2+(mres(i)*gtotal)**2
      breitw	= mres(i)*sqrt(gpion*ggamma/(eta(i)*rv*rpion))/den
      
      f1        = kkr*mn*eta(i)
      f2        = (2.*jr+1.)*pi*qr*mres(i)*gres(i)
      if (i.ne.1) a = sqrt(f1/f2)/50.6
      
      breit(1) = a*breitw*(m22-s)
      breit(2) = a*breitw*mres(i)*gtotal

      end
      