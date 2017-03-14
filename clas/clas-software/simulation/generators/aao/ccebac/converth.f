      subroutine converth(ie,iq,jpin,lpin,itpin
     &                   ,zanse,zansm,zanss
     &                   ,za1,za3,zss)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)
      common / cres / nres(2*maxl+1,0:maxl,3)
     &               ,xmres(2*maxl+1,0:maxl,3,maxres)
     &               ,cfac(2*maxl+1,0:maxl,3,maxres)
      common / clasam0 / wcm0(100),q20(100),zamp0(100,100,8,0:5,3)
      common / clasamp / wcmp(100),q2p(100),zampp(100,100,8,0:5,3)
c
      if(itpin.eq.1) then
        cc = -sqrt(3.d0)
      else
        cc = sqrt(2.d0/3.d0)
      end if

      if(jpin.gt.lpin) then
         im1 = 1
         im2 = 3
         il  = 7
      else
         im1 = 2
         im2 = 4
         il  = 8
      end if

      lp = lpin/2
      ix = 3  ! resonanc

      zanse=(zamp0(ie,iq,im1,lp,ix)+zampp(ie,iq,im1,lp,ix)*sqrt(2.d0))/3
      zansm=(zamp0(ie,iq,im2,lp,ix)+zampp(ie,iq,im2,lp,ix)*sqrt(2.d0))/3
      zanss=(zamp0(ie,iq,il ,lp,ix)+zampp(ie,iq,il ,lp,ix)*sqrt(2.d0))/3
c

      lxx   = lpin/2
      xxl   = lxx

      if(jpin.gt.lpin) then
         za1  = - ( dble(lxx + 2)*zanse  + dble(lxx)*zansm )/2.d0
         za3  =   (zanse -  zansm )*sqrt(xxl*(xxl+2.d0))/2.d0
         zss  = - dble(lxx+1)/sqrt(2.d0)*zanss

      else if(jpin.lt.lpin) then
         leff= lxx -1
         xeff= leff

         za1  = - ( dble(leff)*zanse  - dble(leff+2)*zansm )/2.d0
         za3  = - sqrt(xeff*(xeff+2.d0))*(zanse  + zansm )/2.d0
         zss  = - dble(leff+1)/sqrt(2.d0)*zanss  

      end if

      za1 = za1/cfac(jpin,lpin,itpin,1)*cc
      za3 = za3/cfac(jpin,lpin,itpin,1)*cc
      zss = zss/cfac(jpin,lpin,itpin,1)*cc

      return
      end
