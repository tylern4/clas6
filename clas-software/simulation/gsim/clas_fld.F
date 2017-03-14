      subroutine clas_fld(xin,bfield )

c_begin_doc

c  Documentation for subroutine clas_fld(xin,bfield)

c  $Id: clas_fld.F,v 1.13 2011/04/08 20:54:51 fxgirod Exp $

c  Purpose:
c  --------
c    Returns the magnetic field at point xin, as sum of torus,mini-torus,ptg 
c    Option -nofield sets ALL fields to 0
c        magscale(2)  scales torus and mini respectively
c        ptg_scale scales ptg

c    magtype controls torus and mini, except magtype<0 calls guser_fld only
c    Ptg field included only if geom include (geom 'PTG') and ptg_field.ne.0
c    If ptg_field=3 the  Ptg field is read from lookup table 
c  magtype     type
c  -------     ----
c   < 0        call guser_fld(xin,bfield) to get complete field
c     0        no torus or mini
c     1        analytic torus, no mini
c     2        Bogdan's lookup table for torus only
c     3        Bogdan's lookup table for torus+mini
c     4        Bogdan's lookup table for mini only

c  Input:
c  ------
c    xin(3) - position vector (cm)

c  Output:
c  -------
c    bfield - field at the position (kgauss)

c  Author:
c  -------
c  I got most of this from Bogdan and Etienne

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 25-jan-95
c  Modified  by:
c  -------------
c     Angela Biselli,Dipartimento di fisica,Italy,8-Apr-97
c     biselli@infnge.ge.infn.it
c
c     Junho Yun, ODU, Aug. 1998
c     jyun@cebaf.gov
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_fld.F,v $')
      parameter (crevis= '$Revision: 1.13 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2011/04/08 20:54:51 $')
      parameter (cautho= '$Author: fxgirod $')
      parameter (crname= 'clas_fld')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_fld.F,v 1.13 2011/04/08 20:54:51 fxgirod Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "bgrid.inc"
#include "ffpar.inc"
#include "ffpar_mg.inc"
#include "ffpar_ptg.inc"
#include "ffpar_tg.inc"
#include "lcgeom.inc"
c_end_inc


c  input/output variables:
c  -----------------------
      real xin(3)
      real bfield(3)

c  local variables:
c  ----------------
      integer sector, get_sector
      real x0(3),b0(3),xyrad2,bptg(3),vmod, xxx0(3), twopi

c  for simple analytic torus only
      real   tubi2,  tubo2,  sphe2,   ztmin,   ztmax
      data   tubi2,  tubo2,  sphe2,   ztmin,   ztmax
     1    /  550.,   45000., 19000.,  -140.,   300./
c_end_var


c  executable code:
c  ----------------

      call vzero(bfield,3)
      call vzero(b0,3)

c  no b field at all
      if(nofield) then
         return

c  call user routine to get complete field
      elseif(magtype.lt.0)then
         call guser_fld(xin,bfield)
         return
      endif

c  get sector number and transform to elementary sector 
      if(ptg_ifield.LE.10 .OR. magtype.GT.1) then
         sector=get_sector(xin)
         x0(1) =  xin(1)*cossec(sector) + xin(2)*sinsec(sector)
         x0(2) = -xin(1)*sinsec(sector) + xin(2)*cossec(sector)
         x0(3) =  xin(3)
      endif

c  polarized target field if ptg included, field non-zero,
c   and radius small enough

      if(ptg_ifield.gt.0) then              ! geom PTG has nothing to do with fiel 
         xin(3)=xin(3)-mg_pos_ff(3)         ! field defined with respect to the magnet center
         if(vmod(xin,3).le.ptg_maxradius) then
c  calculate field 
            if(ptg_ifield.GT.10) then
               call ptg_field(xin,bptg)
               call vscale(bptg,ptg_scale,bptg,3)
c  read field map
            elseif(ptg_ifield.GT.0) then
               if(ptg_ifield.EQ.4) then
                  call snake_dipole(xin,bptg)
               else
                  xxx0(3) =  x0(3) - mg_pos_ff(3)
                  xxx0(1) =  x0(1)*cossec(sector) - x0(2)*sinsec(sector) - mg_pos_ff(1)
                  xxx0(2) =  x0(1)*sinsec(sector) + x0(2)*cossec(sector) - mg_pos_ff(2)

                  twopi  = ACOS(-1.)*2

                  x0(1) =  xxx0(1)*cos(twopi*ptg_angle(2)/360.) + xxx0(2)*sin(twopi*ptg_angle(2)/360.) 
                  x0(2) = -xxx0(1)*sin(twopi*ptg_angle(2)/360.) + xxx0(2)*cos(twopi*ptg_angle(2)/360.)
                  x0(3) =  xxx0(3)

                  xxx0(1) = x0(1)*cos(twopi*ptg_angle(1)/360.) - x0(3)*sin(twopi*ptg_angle(1)/360.)
                  xxx0(2) = x0(2)
                  xxx0(3) = x0(1)*sin(twopi*ptg_angle(1)/360.) + x0(3)*cos(twopi*ptg_angle(1)/360.)

                  x0(1) =  xxx0(1)*cos(twopi*ptg_angle(3)/360.) + xxx0(2)*sin(twopi*ptg_angle(3)/360.)
                  x0(2) = -xxx0(1)*sin(twopi*ptg_angle(3)/360.) + xxx0(2)*cos(twopi*ptg_angle(3)/360.)
                  x0(3) =  xxx0(3)

                  xxx0(1) =  x0(1)*cos(twopi*ptg_angle(1)/360.) + x0(3)*sin(twopi*ptg_angle(1)/360.)
                  xxx0(2) =  x0(2)
                  xxx0(3) = -x0(1)*sin(twopi*ptg_angle(1)/360.) + x0(3)*cos(twopi*ptg_angle(1)/360.)
                  
                  x0(1) = xxx0(1)*cos(twopi*ptg_angle(2)/360.) - xxx0(2)*sin(twopi*ptg_angle(2)/360.) 
                  x0(2) = xxx0(1)*sin(twopi*ptg_angle(2)/360.) + xxx0(2)*cos(twopi*ptg_angle(2)/360.)
                  x0(3) = xxx0(3)

                  xxx0(1) = x0(1)
                  xxx0(2) = x0(2)
                  xxx0(3) = x0(3)

                  call snake_ptg(xxx0,b0)
                  x0(3) = xin(3) + mg_pos_ff(3)
                  bptg(1) =  b0(1)
                  bptg(2) =  b0(2)
                  bptg(3) =  b0(3)

                  b0(1) =  bptg(1)*cos(twopi*ptg_angle(2)/360.) + bptg(2)*sin(twopi*ptg_angle(2)/360.)
                  b0(2) = -bptg(1)*sin(twopi*ptg_angle(2)/360.) + bptg(2)*cos(twopi*ptg_angle(2)/360.)
                  b0(3) =  bptg(3)

                  bptg(1) = b0(1)*cos(twopi*ptg_angle(1)/360.) - b0(3)*sin(twopi*ptg_angle(1)/360.)
                  bptg(2) = b0(2)
                  bptg(3) = b0(1)*sin(twopi*ptg_angle(1)/360.) + b0(3)*cos(twopi*ptg_angle(1)/360.)

                  b0(1) = bptg(1)*cos(twopi*ptg_angle(3)/360.) - bptg(2)*sin(twopi*ptg_angle(3)/360.)
                  b0(2) = bptg(1)*sin(twopi*ptg_angle(3)/360.) + bptg(2)*cos(twopi*ptg_angle(3)/360.)
                  b0(3) = bptg(3)

                  bptg(1) =  b0(1)*cos(twopi*ptg_angle(1)/360.) + b0(3)*sin(twopi*ptg_angle(1)/360.)
                  bptg(2) =  b0(2)
                  bptg(3) = -b0(1)*sin(twopi*ptg_angle(1)/360.) + b0(3)*cos(twopi*ptg_angle(1)/360.)

                  b0(1) = bptg(1)*cos(twopi*ptg_angle(2)/360.) - bptg(2)*sin(twopi*ptg_angle(2)/360.)
                  b0(2) = bptg(1)*sin(twopi*ptg_angle(2)/360.) + bptg(2)*cos(twopi*ptg_angle(2)/360.)
                  b0(3) = bptg(3)
                  
                  bptg(1) = b0(1)
                  bptg(2) = b0(2)
                  bptg(3) = b0(3)

               endif
            endif
         endif
         xin(3)=xin(3)+mg_pos_ff(3)
      endif

c ... RESET THE POSITION FOR TORUS
      if(ptg_ifield.LE.10 .OR. magtype.GT.1) then
         sector=get_sector(xin)
         x0(1) =  xin(1)*cossec(sector) + xin(2)*sinsec(sector)
         x0(2) = -xin(1)*sinsec(sector) + xin(2)*cossec(sector)
         x0(3) =  xin(3)
      endif
c  simple, analytic torus...from Etienne...add ptg at end
      if(magtype.eq.1)then
         if (xin(3).GE.ztmin .AND. xin(3).LE.ztmax) then
            xyrad2 = xin(1)**2 + xin(2)**2
            if (xyrad2.GE.tubi2 .AND. xyrad2.LE.tubo2 .AND.
     &                          (xyrad2+xin(3)**2) .GE. sphe2) then
               bfield(1) =  xin(2) * (torus_field/xyrad2)
               bfield(2) = -xin(1) * (torus_field/xyrad2)
               bfield(3) =  0.0
            endif
         endif
c
c  Bogdan's lookup table torus:   (MUST be initialized in 'init_mg.F')
c        magtype = 2: read torus field map only
c        magtype = 3: read torus & minitorus field map
c        magtype = 4: read minitorus field map only
c
      elseif(magtype.GE.2 .and. magtype.LE.4) then
         
c  get field in elementary sector
         call snake(x0,b0)

c  Rotate field vector back to original sector
         bfield(1) =  b0(1)*cossec(sector) - b0(2)*sinsec(sector)
         bfield(2) =  b0(1)*sinsec(sector) + b0(2)*cossec(sector)
         bfield(3) =  b0(3)
      endif

      if(ptg_ifield.gt.0) then
         call vadd(bfield,bptg,bfield,3)
      endif

      END






