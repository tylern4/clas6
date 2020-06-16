c  ccfrmp.inc

c  $Id: ccfrmp.inc,v 1.2 1997/04/24 15:38:08 holtrop Exp $

      character*132 ciccfrmp
      data ciccfrmp/
     1'$Id: ccfrmp.inc,v 1.2 1997/04/24 15:38:08 holtrop Exp $'
     1/


c--- Alexander Vlassov ---
c
C --- COMMON /CCFRMP/ Coordinates for frame draw in ccview.
c
c --- mirrn - number of points for mirrors
c --- nwcon - number of points for WC
c
c --- xell(j,i),yell(j,i) - elliptical mirror coordinates
c --- xhyp(j,i),yhyp(j,i) - hyperbolic mirror coordinates
c
c --- xw1(j,i),yw1(j,i) - WC surface nearest to the middle plane
c --- xw2(j,i),yw2(j,i) - WC surface far from the middle plane
c
c --- xcms(j,i,k) - X Coordinates for Magnetic Shield walls
c --- ycms(j,i,k) - Y Coordinates for Magnetic Shield walls
c --- zcms(j,i,k) - Z Coordinates for Magnetic Shield walls
c                   j - point # (1-4); i - wall # (1-4); k - CC#
      integer mirrn,nwcon
c
      parameter (mirrn=99)
      parameter (nwcon=25)
c
      real xell,yell,xhyp,yhyp,xw1,yw1,xw2,yw2,xcms,ycms,zcms
c
      COMMON /CCFRMP/ xell(mirrn,18),yell(mirrn,18),
     &                xhyp(mirrn,18),yhyp(mirrn,18),
     &  xw1(nwcon,18),yw1(nwcon,18),xw2(nwcon,18),yw2(nwcon,18),
     &  xcms(4,4,18),ycms(4,4,18),zcms(4,4,18)
c
c ===============================================================
c
