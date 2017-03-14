      subroutine cross_sections(s,q2,phi,eps)
      implicit none
      save

#include "helicity.inc"
#include "res_func.inc"
#include "fit_par.inc"

      real s,q2,phi,eps,qkratio,sinth,mpion(2),mnuc,qpi
      real vl,vtt,vlt,vltp,sigma_tt,sigma_lt,sigma_ltp
      integer i,j
      data mnuc/0.93827/mpion/0.13498,0.13957/
          
      vl   = eps
      vtt  = eps
      vlt  = sqrt(2*eps*(1+eps))
      vltp = sqrt(2*eps*(1-eps))
      
      do i=1,2
        
        qpi = sqrt(((s-mnuc**2+mpion(i)**2)/2.)**2/s-mpion(i)**2)
        qkratio = qpi/((s-mnuc**2)/2./sqrt(s))   
        
        sigmaT(i)   = 0.
        sigmaL(i)   = 0.
        sigmaTT(i)  = 0.
        sigmaTL(i)  = 0.
        sigmaTLP(i) = 0.
        
        do j=1,2
          sigmaT(i)  = sigmaT(i) + 
     1                H1(i,j)**2.+H2(i,j)**2.+H3(i,j)**2.+H4(i,j)**2.
          sigmaL(i)  = sigmaL(i) +H5(i,j)**2.+H6(i,j)**2.
          sigmaTT(i) = sigmaTT(i)-H4(i,j)*H1(i,j)+H3(i,j)*H2(i,j)
          sigmaTL(i) = sigmaTL(i)+H5(i,j)*(H1(i,j)-H4(i,j))+ 
     1                            H6(i,j)*(H2(i,j)+H3(i,j))
        enddo
        
        sigmaTLP(i) = sigmaTLP(i)+
     1        H5(i,2)*(H1(i,1)-H4(i,1))-H5(i,1)*(H1(i,2)-H4(i,2))+
     1        H6(i,2)*(H2(i,1)+H3(i,1))-H6(i,1)*(H2(i,2)+H3(i,2))
      
      sigmaT(i)   = sigmaT(i)  *qkratio/2.
      sigmaL(i)   = sigmaL(i)  *qkratio
      sigmaTT(i)  = sigmaTT(i) *qkratio
      sigmaTL(i)  = sigmaTL(i) *qkratio/sqrt(2.)
      sigmaTLP(i) = sigmaTLP(i)*qkratio/sqrt(2.)
      
      sigma_tt      = vtt *sigmaTT(i) *cos(2*phi)
      sigma_ltp     = vltp*sigmaTLP(i)*sin(phi)
      sigma_lt      = vlt *sigmaTL(i) *cos(phi)
      
      sigma(i)    = sigmaT(i) 
     1        +  vl*sigmaL(i) 
     1        +     sigma_tt
     1        +     sigma_lt
      
      if (q2.gt.0) robs(1,i) = sigma(i)
      if (q2.eq.0) robs(1,i) = sigmaT(i)
      robs(2,i) = sigma_ltp/sigma(i)
      robs(3,i) = sigma_lt/sigma(i)
      robs(4,i) = -sigmaTT(i)/sigmaT(i)
      robs(5,i) = sigmaT(i)
      robs(6,i) = sigmaT(i)+vl*sigmaL(i)
      robs(7,i) = sigmaTT(i)
      robs(8,i) = sigmaTL(i)
      robs(9,i) = sigmaTLP(i)
      enddo
      
      
      return
      end




