      subroutine mat_cor(the_x,phy_x,the_y,phy_y,the_z,phy_z)
c     =======================================================
c
c     Making correction for IC calorimeter :
c     Calculates correct angles the_z, phy_z
c     In case Y axis is not perpendicular to X, corrects Y axis
c     parameters as well. 
c
c     Input parameters  : Angles theta and phy for axis X and Y
c     Output parameters : Angle Theta and phy for axis Z
c
c     A. Vlassov July,2006
c
      implicit none
c
      integer i
      real the_x,the_y,the_z,phy_x,phy_y,phy_z,small
      real DEGRAD,RADDEG
      PARAMETER (DEGRAD=0.0174532925199432958)
      PARAMETER (RADDEG=57.2957795130823209)
c
      real XX(3),YY(3),ZZ(3),s,Z0(3),vmod,vdot
      data Z0 /0.,0.,1./
      data small /0.00001/
      save
c
      XX(3) = cos(degrad*the_x)
      s     = sqrt(1.-XX(3)*XX(3))
      XX(2) = s*sin(degrad*phy_x)
      XX(1) = s*cos(degrad*phy_x)
c
      YY(3) =  cos(degrad*the_y)
      s     = sqrt(1.-YY(3)*YY(3))
      YY(2) = s*sin(degrad*phy_y)
      YY(1) = s*cos(degrad*phy_y)
c
      s = vdot(XX,YY,3)
      if(abs(s) .gt. small) then
        do i = 1,3
          YY(i) = YY(i) - s*XX(i)
        end do
        s = 1./vmod(YY,3)
        call vscale(YY,s,YY,3)
        s = vdot(YY,Z0,3)
        the_y = raddeg*acos(s)
        s = vmod(YY,2)
        if(s .lt. small) then
          phy_y = 0.
        else
          phy_y = raddeg*atan2(YY(2)/s,YY(1)/s)
        end if
      end if
c
      call cross(XX,YY,ZZ)
c
      s = vdot(ZZ,Z0,3)
      the_z = raddeg*acos(s)
      s = vmod(ZZ,2)
      if(s.eq.0.) then
        phy_z = 0.
      else
        phy_z = raddeg*atan2(ZZ(2)/s,ZZ(1)/s)
      end if
c
      return
      end
