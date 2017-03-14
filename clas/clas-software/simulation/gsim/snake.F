      subroutine snake(x,f)

c_begin_doc

c  Documentation for subroutine snake(x,f)

c  $Id: snake.F,v 1.2 1997/04/24 15:38:54 holtrop Exp $

c  Purpose:
c  --------
c    Purpose and Methods: Performs 3-dim 2-nd order interpolation for each 
c                         component of the magnetic field using Taylor series. 
c                         All calculations done in the sector coordinate system, thus
c                         calling routine must perform appropriate transformations.


c    Inputs  : X(3) - space point (in sector coordinate system)
c    Outputs : F(3) - magnetic field at X(3) (in sector coordinate system)

c  Author:
c  -------
c    Author:     P.Vernin     (Aug. 1987)
c    Modified:   M.Guckes     (Oct. 1987)
c                B.Niczyporuk (Feb. 1991)

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 30-jan-95
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: snake.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:54 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'snake')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: snake.F,v 1.2 1997/04/24 15:38:54 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "bgrid.inc"
c_end_inc

c  input/output variables:
c  -----------------------
      real x(3),f(3)

c  local variables:
c  ----------------
      integer i,nx3,nynx3, ixm,iym,izm, bxfac
      real    rdx,rdy,rdz, axm,aym,azm, pp,qq,rr, xr,yr,zr
      real    cp,cq,cr, dp,dq,dr,e
      real    f000,f00p,f00m,f0p0,f0m0,fp00,fm00,f0pp,fp0p,fpp0,fppp

      logical lfirst
      data    lfirst /.true./
c_end_var


c  executable code:
c  ----------------

      if(lfirst) then
        lfirst = .false.
        rdx = 1./dx
        rdy = 1./dy
        rdz = 1./dz
        nx3   = nx*3
        nynx3 = ny*nx*3
      endif

c  each sector is symmetric in y-coordinate (dst system)
      if(x(2).lt.0.) then
        x(2) = -x(2)
        bxfac = -1.
      else
        bxfac =  1.
      endif

c  find intercepts.
      xr  = (x(1)-xmin)*rdx
      axm = aint(xr)
      ixm = ifix(axm)
      if(ixm.lt.1 .or. ixm+2.gt.nx) go to 100
      pp  = xr-axm

      yr  = (x(2)-ymin)*rdy
      aym = aint(yr)
      iym = ifix(aym)
      if(iym.lt.1 .or. iym+2.gt.ny) go to 100
      qq  = yr-aym

      zr  = (x(3)-zmin)*rdz
      azm = aint(zr)
      izm = ifix(azm)
      if(izm.lt.1 .or. izm+2.gt.nz) go to 100
      rr  = zr-azm

c  3-dim 2nd order interpolation inside torus for each component of the field.
c  we use 11 values of the field:
      do i=1,3
         f000 = qfield((izm  )*nynx3 + (iym  )*nx3 + (ixm  )*3 + i)
         f00p = qfield((izm+1)*nynx3 + (iym  )*nx3 + (ixm  )*3 + i)
         f00m = qfield((izm-1)*nynx3 + (iym  )*nx3 + (ixm  )*3 + i)
         f0p0 = qfield((izm  )*nynx3 + (iym+1)*nx3 + (ixm  )*3 + i)
         f0m0 = qfield((izm  )*nynx3 + (iym-1)*nx3 + (ixm  )*3 + i)
         fp00 = qfield((izm  )*nynx3 + (iym  )*nx3 + (ixm+1)*3 + i)
         fm00 = qfield((izm  )*nynx3 + (iym  )*nx3 + (ixm-1)*3 + i)
         f0pp = qfield((izm+1)*nynx3 + (iym+1)*nx3 + (ixm  )*3 + i)
         fp0p = qfield((izm+1)*nynx3 + (iym  )*nx3 + (ixm+1)*3 + i)
         fpp0 = qfield((izm  )*nynx3 + (iym+1)*nx3 + (ixm+1)*3 + i)
         fppp = qfield((izm+1)*nynx3 + (iym+1)*nx3 + (ixm+1)*3 + i)
         cp   = fp00+fm00-2.*f000
         cq   = f0p0+f0m0-2.*f000
         cr   = f00p+f00m-2.*f000
         dp   = f000-fp00+fpp0-f0p0
         dq   = f000-f0p0+f0pp-f00p
         dr   = f000-f00p+fp0p-fp00
         e    = -dp-f0pp+f00p-fp0p+fppp
         
c  compute the taylor's series:
         f(i) = f000+0.5*(pp*(fp00-fm00)+pp*pp*cp
     1         +qq*(f0p0-f0m0)+qq*qq*cq
     2         +rr*(f00p-f00m)+rr*rr*cr)
     3         +pp*qq*dp+qq*rr*dq+pp*rr*dr+pp*qq*rr*e
        
      enddo

      f(1) = bxfac*f(1)
      f(3) = bxfac*f(3)
      go to 999

c  point is outside of the grid
 100  continue
      f(1) = 0.
      f(2) = 0.
      f(3) = 0.

 999  continue

      return
      end

c-------------------------------------------------------------------------------------
