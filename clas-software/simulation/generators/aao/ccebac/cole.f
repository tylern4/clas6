      subroutine cole(idx)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
      character c1*2,c2*4,cdum*4,cc*13
      character c0*12,cp*12,c0n*17,cpn*17,c0r*16,cpr*16
      common / clasam0 / wcm0(100),q20(100),zamp0(100,100,8,0:5,3)
      common / clasamp / wcmp(100),q2p(100),zampp(100,100,8,0:5,3)
      common / clasmax / iemax,iqmax,lmax
      dimension itr(6)
      data itr/7,8,1,2,3,4/


      lmax   = 5
      iqmax  =13
      iemax  =45

      do iq  = 1, iqmax
      do ie  = 1, iemax

      read(1,1000)c1,f1,c2,f2
      read(2,1000)c1,f1x,c2,f2x
 1000 format(a2,f6.3,a4,f6.3)

c      write(*,*)ie,iq,f1,f2
      wcm0(ie) = f1
      q20(iq)  = f2

      do im  = 1,6
      imx    = itr(im)

      read(1,1001)cdum
      read(2,1001)cdum
 1001 format(a4)

      read(1,*)x1,x2,x3,x4,x5,x6
      read(1,*)y1,y2,y3,y4,y5,y6
      zamp0(ie,iq,imx,0,idx) = x1 + (0.d0,1.d0)*x2
      zamp0(ie,iq,imx,1,idx) = x3 + (0.d0,1.d0)*x4
      zamp0(ie,iq,imx,2,idx) = x5 + (0.d0,1.d0)*x6
      zamp0(ie,iq,imx,3,idx) = y1 + (0.d0,1.d0)*y2
      zamp0(ie,iq,imx,4,idx) = y3 + (0.d0,1.d0)*y4
      zamp0(ie,iq,imx,5,idx) = y5 + (0.d0,1.d0)*y6

      read(2,*)x1,x2,x3,x4,x5,x6
      read(2,*)y1,y2,y3,y4,y5,y6
      zampp(ie,iq,imx,0,idx) = x1 + (0.d0,1.d0)*x2
      zampp(ie,iq,imx,1,idx) = x3 + (0.d0,1.d0)*x4
      zampp(ie,iq,imx,2,idx) = x5 + (0.d0,1.d0)*x6
      zampp(ie,iq,imx,3,idx) = y1 + (0.d0,1.d0)*y2
      zampp(ie,iq,imx,4,idx) = y3 + (0.d0,1.d0)*y4
      zampp(ie,iq,imx,5,idx) = y5 + (0.d0,1.d0)*y6

      end do

      end do
      end do


      return
      end

