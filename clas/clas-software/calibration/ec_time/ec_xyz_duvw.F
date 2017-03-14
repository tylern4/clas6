      Subroutine ec_xyz_duvw(x,y,z,u,v,w,xi,yi,in_out)

c Stepan's originally, modified by W. Brooks to invert
c what is used in the reconstruction for inner and 
c outer.

      real x,y,z,u,v,w,xi,yi,zi
      real EC_the,ec_phi,phi,ylow(2),yhi(2),tgrho,sinrho,cosrho
      data EC_the/0.4363323/
c     I believe these numbers are not exactly what was in the
c     design drawings, however, they're in the reconstruction code.
c      data ylow,yhi/-182.974,189.956/
c      data ylow / -182.974-0.,-182.974-15.*0.43708 /
c      data yhi  /  189.956+0., 189.956+15.*0.45419 /

      data ylow / -182.974,-189.530 /
      data yhi  /  189.956, 196.956 /

      data tgrho,sinrho,cosrho/1.95325,0.8901256,0.455715/
      real rot(3,3)
      integer in_out            ! = 1 for inner, = 2 for outer
c     

      phi=atan2(y,x)*57.29578
      if(phi.lt.0.)phi=phi+360.
      phi=phi+30.
      if(phi.ge.360.)phi=phi-360.      
      Ec_phi=int(phi/60.)*1.0471975
c     
      rot(1,1)=cos(Ec_the)*cos(Ec_phi)
      rot(1,2)=-sin(Ec_phi)
      rot(1,3)=sin(Ec_the)*cos(Ec_phi)
      rot(2,1)=cos(Ec_the)*sin(Ec_phi)
      rot(2,2)=cos(Ec_phi)
      rot(2,3)=sin(Ec_the)*sin(Ec_phi)
      rot(3,1)=-sin(Ec_the)
      rot(3,2)=0.
      rot(3,3)=cos(Ec_the)
c     
      yi=x*rot(1,1)+y*rot(2,1)+z*rot(3,1)
      xi=x*rot(1,2)+y*rot(2,2)+z*rot(3,2)
      zi=x*rot(1,3)+y*rot(2,3)+z*rot(3,3)
      zi=zi-510.32
      u=(yi-ylow(in_out))/sinrho
      v=(yhi(in_out)-ylow(in_out))/tgrho-xi+(yhi(in_out)-yi)/tgrho
      w=((yhi(in_out)-ylow(in_out))/tgrho+xi+(yhi(in_out)-yi)/tgrho)/2./cosrho
      return
      end

