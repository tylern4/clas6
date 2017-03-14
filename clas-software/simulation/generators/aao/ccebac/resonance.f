c-------------------------------------------------------------------
c
      subroutine resonance
      implicit real*8(a-h,o-z)
      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)
      common / cres / nres(2*maxl+1,0:maxl,3)
     &               ,xmres(2*maxl+1,0:maxl,3,maxres)
     &               ,cfac(2*maxl+1,0:maxl,3,maxres)


c----------------------------------------------      
      i2        = 1

C P11(1440) 
      l2        = 2
      j2        = 1
      xmr	= 1.44
      Gamma	= 0.35
      etta	= 0.6
      nres(j2,l2,i2) = 1
      xmres(j2,l2,i2,1) = xmr*1000
      cfac (j2,l2,i2,1) = aconv(j2,xmr,gamma,etta)

C S11(1535)  
      l2        = 0
      j2        = 1
      xmr	= 1.53 
      Gamma	= 0.15    
      etta	= 0.4
      nres(j2,l2,i2) = 2
      xmres(j2,l2,i2,1) = xmr*1000
      cfac (j2,l2,i2,1) = aconv(j2,xmr,gamma,etta)

C S11(1650) 
      l2        = 0
      j2        = 1
      xmr	= 1.65
      Gamma	= 0.15
      etta	= 0.7
      nres(j2,l2,i2) = 2
      xmres(j2,l2,i2,2) = xmr*1000
      cfac (j2,l2,i2,2) = aconv(j2,xmr,gamma,etta)


C D13(1520) 
      l2	= 4
      j2	= 3
      xmr	= 1.52
      Gamma	= 0.12
      etta	= 0.5
      nres(j2,l2,i2) = 1
      xmres(j2,l2,i2,1) = xmr*1000
      cfac (j2,l2,i2,1) = aconv(j2,xmr,gamma,etta)

C D15(1675) 
      l2	= 4
      j2	= 5
      xmr	= 1.675
      Gamma	= 0.15
      etta	= 0.45
      nres(j2,l2,i2) = 1
      xmres(j2,l2,i2,1) = xmr*1000
      cfac (j2,l2,i2,1) = aconv(j2,xmr,gamma,etta)

C F15(1680)  
      l2	= 6
      j2	= 5
      xmr	= 1.68
      Gamma	= 0.13
      etta	= 0.65
      nres(j2,l2,i2) = 1
      xmres(j2,l2,i2,1) = xmr*1000
      cfac (j2,l2,i2,1) = aconv(j2,xmr,gamma,etta)

C D13(1700) 
      l2	= 4
      j2	= 3
      xmr	= 1.7
      Gamma	= 0.1
      etta	= 0.1
      nres(j2,l2,i2) = 2
      xmres(j2,l2,i2,2) = xmr*1000
      cfac (j2,l2,i2,2) = aconv(j2,xmr,gamma,etta)

C P11(1710)
      l2	= 2
      j2	= 1
      xmr	= 1.71
      Gamma	= 0.11
      etta	= 0.18
      nres(j2,l2,i2) = 2
      xmres(j2,l2,i2,2) = xmr*1000
      cfac (j2,l2,i2,2) = aconv(j2,xmr,gamma,etta)


C P13(1720) 
      l2	= 2
      j2	= 3
      xmr	= 1.72
      Gamma	= 0.15
      etta	= 0.15
      nres(j2,l2,i2) = 1
      xmres(j2,l2,i2,1) = xmr*1000
      cfac (j2,l2,i2,1) = aconv(j2,xmr,gamma,etta)



      return
      end
