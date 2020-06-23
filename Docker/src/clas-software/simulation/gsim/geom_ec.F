      subroutine geom_ec

c_begin_doc

c  Documentation for subroutine geom_ec

c  $Id: geom_ec.F,v 1.3 1998/01/22 19:19:34 lcsmith Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Defines the geometry of the electromagnetic               *
c     *      sampling calorimeter                                      *
c     *                                                                *
c     *      Author: V. Burkert   July, 1988                           *
c     *                                                                *
c     *          Modified by M. Guidal March 93                        *
c     *          Modified by K.B.Beard 5 Aug 93 [consistant geom. seed]*
c     *          Modified by E.Burtin  May 4, 1994                     *   	
c     *                      -> reading of attenuation length          *
c     *                                                                *
c     *****************************************************************

c  Major revisions:
c  ----------------
c  derived from geec.f by Elliott Wolin, College of William and Mary, 9-nov-94
c  redid volume structure for ganged strips...EJW, W&M, 20-nov-95
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: geom_ec.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/01/22 19:19:34 $')
      parameter (cautho= '$Author: lcsmith $')
      parameter (crname= 'geom_ec')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: geom_ec.F,v 1.3 1998/01/22 19:19:34 lcsmith Exp $
     1 '/
      save

c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcunit.inc"
#include "gcflag.inc"
#include "gconst.inc"
#include "gclist.inc"
#include "ectmed.inc"
#include "ecgeom.inc"
#include "lcgeom.inc"
c_end_inc


c  local variables:  *** most of this should be in BOS banks ***
c  ----------------

c  For consistancy, the CLAS forward calorimeters (Ec)
c  should be generated from a standard set of basic
c  measurements; all measurements here are to the active
c  volume.                             K.B.Beard Hampton U. 28Jun1993
 
      real X_of_corner_nearest_beam
      real Y_of_corner_nearest_beam
      real Z_of_corner_nearest_beam
      real X_shift_required
      real Y_shift_required
      real Z_shift_required
      real physical_thickness,air_gap
      
      real a1,a2,au,avw,cvx0,cvz0,cx0,cz0
      real dthi,dx,dy,dz,dzpb,dzsc,h1,h2,hu,hvw
      integer i,ipl,ivol
      integer j,k
      real r_cover,r_project
      real tana,tanh,the,ther,thet,x0,y0
      
      real parcal(11),parcov(11)
      integer ec_u,ec_v,ec_w,ec_5
      integer get_rotm
      
      real ec_parm(16)
      data ec_parm/-30., 60., 1., 4.,
     +     366., 402., 403.,
     +     420., 285., 450.,
     +     535.,  40., 207.,
     +     600.,  40.,  70./

      integer tmed
c_end_var


c  executable code:
c  ----------------

c  read ec geometry data...eventually from bos banks
      call geom_ec_init()

c  size on inner face in radial direction
      h1 = ec_basic_in_hght

c  size on outer face in radial direction
      h2 = ec_basic_out_hght

c  size on inner perpindicular to radial direction
      a1 = ec_basic_in_wdth

c  size on outer perpindicular to radial direction
      a2 = ec_basic_out_wdth

c  current temporary location of inner u corner
      x_of_corner_nearest_beam= ec_basic_r*sin(ec_basic_angle) -
     &				cos(ec_basic_angle)*ec_basic_in_hght/2.
      y_of_corner_nearest_beam= 0.
      z_of_corner_nearest_beam= ec_basic_r*cos(ec_basic_angle) +
     &				sin(ec_basic_angle)*ec_basic_in_hght/2.

c  must slide dtector in plane to get correct displacement
c   from beam axis
      x_shift_required= ec_basic_uin - x_of_corner_nearest_beam
      y_shift_required= 0.
      z_shift_required= -tan(ec_basic_angle) * x_shift_required

c  recalculated u inner active corner
      x_of_corner_nearest_beam= x_of_corner_nearest_beam +
     &						x_shift_required 
      y_of_corner_nearest_beam= y_of_corner_nearest_beam +
     &						y_shift_required 
      z_of_corner_nearest_beam= z_of_corner_nearest_beam +
     &						z_shift_required 

c  note:  The Ec_basic_R distance is along a line from
c         the target normal to the surface of scintillators in 
c 	  layer#1, not the center of that layer.  
c 		There are 39 layers, but from the first layer has no
c 	  lead; the other 38 have PBTHI cm lead.  All scintillators are
c 	  nominally SCTHI cm thick. KBB
      physical_thickness= 1*scthi*centimeter + 
     &     (nlayer-1)*(scthi+pbthi)*centimeter

c  distance from nominal target along normal to geometric middle 
c   plane of detector, NOT to the center of the detector
      r_project= ec_basic_r + physical_thickness/2
      cx0= r_project* sin(ec_basic_angle)
      cz0= r_project* cos(ec_basic_angle)
      thet = 2*atan2(a1/2,h1)/degree            !included angle at u corner

      dthi = pbthi + scthi 
      tana = (a2-a1)/2./38./dthi
      tanh = (h2-h1)/2./38./dthi
      ther = ( 180. - thet ) / 2.


c  rotm for ECAL, cover plates
      ec_5 = get_rotm()
      call gsrotm(ec_5,90.,90.,115.,0.,25.,0.)

c  rotm around x-axis ( 90 deg. angle) for U planes, lead plates
      ec_u=get_rotm()
      call gsrotm(ec_u,90.,0.,180.,0.,90.,90.)
      
c  rotm for V strips
      ec_v = get_rotm()
      call gsrotm(ec_v,90.,360.-ther,180.,0.,90.,90.-ther)

c  rotm for W strips
      ec_w = get_rotm()
      call gsrotm(ec_w,90.,ther,180.,0.,90.,90.+ther)

c  define a polygon that surrounds entire calorimeter, including cover plates
      if(helium_only)then
         tmed=ec_med_helium
      else
         tmed=ec_med_air
      endif
      call gsvolu('EC  ','PGON',tmed,ec_parm,16,ivol)
      call gspos('EC  ',1,'LSS-',0.,0.,0.,0,'ONLY')	

c  define ECAL mother parameters ??? why vacuum ???
      parcal(1)  = (float(3*nmod)*dthi-pbthi)/2. 
      parcal(2)  = 0.
      parcal(3)  = 0.
      parcal(4)  = h1/2. 
      parcal(5)  = 0.
      parcal(6)  = a1/2. 
      parcal(7)  = 0.
      parcal(8)  = h2/2. 
      parcal(9)  = 0.
      parcal(10) = a2/2. 
      parcal(11) = 0.
      call gsvolu('ECAL','TRAP',ec_med_vac,parcal,11,ivol)

c  Position ECAL in EC  
      cx0=cx0 + X_shift_required
      cz0=cz0 + Z_shift_required
      call gspos('ECAL',1,'EC  ',cx0,0.,cz0,ec_5,'ONLY')

c  Definition of volumes for scintillator (inner and outer) and lead
      call gsvolu('ECUI','TRAP',ec_med_scint,parcal,0,ivol)
      call gsvolu('ECVI','TRAP',ec_med_scint,parcal,0,ivol)
      call gsvolu('ECWI','TRAP',ec_med_scint,parcal,0,ivol)
      call gsvolu('ECUO','TRAP',ec_med_scint,parcal,0,ivol)
      call gsvolu('ECVO','TRAP',ec_med_scint,parcal,0,ivol)
      call gsvolu('ECWO','TRAP',ec_med_scint,parcal,0,ivol)
      call gsvolu('ECPB','TRAP',ec_med_lead ,parcal,0,ivol)

c  define and position various cover plate volumes
c              |  1.90 mm stain.st + |
c              |  7.62 cm "air"    + |  ===  9-jan-93.  ===
c              |  1.60 mm stain.st.  |    
      parcov(1)=(cov1th+cov2th+cov3th)/2. 
      do i=2,11
         k=i-(i/8)*4
         if(i.le.3)k=3
         parcov(i) = parcal(k)
      enddo
      call gsvolu('ECST','TRAP',ec_med_steel,parcov,0,ivol)

      if(helium_only)then
         tmed=ec_med_helium
      else
c         tmed=ec_med_cvp_air
          tmed=ec_med_foam
      endif
      call gsvolu('ECAI','TRAP',tmed,parcov,0,ivol)

c  ??? why vacuum ???
      air_gap= 0.1*centimeter   !between cover&active volume
      r_cover=r_project - physical_thickness/2 -parcov(1) - air_gap
      cvx0=r_cover* sin(ec_basic_angle)
      cvz0=r_cover* cos(ec_basic_angle)
      call gsvolu('ECC1','TRAP',ec_med_vac,parcov,11,ivol)

      cvx0=cvx0 + X_shift_required
      cvz0=cvz0 + Z_shift_required
      call gspos('ECC1',1,'EC  ',cvx0,0.,cvz0,ec_5,'ONLY')

      dz=-parcov(1) + cov1th / 2.
      parcov (1) = cov1th / 2.
      call gsposp('ECST',1,'ECC1',0.,0.,DZ,0,'ONLY',parcov,11)

      parcov (1) = cov2th / 2. 
      dz=dz + cov1th/2. + cov2th/2.
      call gsposp('ECAI',1,'ECC1',0.,0.,DZ,0,'ONLY',parcov,11)

      parcov (1) = cov3th / 2. 
      dz=dz + cov2th/2. + cov3th/2.
      call gsposp('ECST',2,'ECC1',0.,0.,DZ,0,'ONLY',parcov,11)

      parcov (1) = ( cov1th +cov2th +cov3th ) / 2. 
      parcov (4) = h2 / 2. 
      parcov (6) = a2 / 2. 
      parcov (8) = h2 / 2. 
      parcov(10) = a2 / 2. 

c  ??? why vacuum ???
      air_gap= 0.1*centimeter   !between cover&active volume
      r_cover=r_project + physical_thickness/2 + parcov(1) + air_gap
      cvx0=r_cover* sin(ec_basic_angle)
      cvz0=r_cover* cos(ec_basic_angle)
      call gsvolu('ECC2','TRAP',ec_med_vac,parcov,11,ivol)

      cvx0=cvx0 + X_shift_required
      cvz0=cvz0 + Z_shift_required
      call gspos('ECC2',1,'EC  ',CVX0,0.,CVZ0,ec_5,'ONLY')

      dz=-parcov(1) + cov1th / 2.
      parcov (1) = cov1th / 2. 
      call gsposp('ECST',3,'ECC2',0.,0.,DZ,0,'ONLY',parcov,11)

      parcov (1) = cov2th / 2. 
      dz=dz + cov1th/2. + cov2th/2.
      call gsposp('ECAI',2,'ECC2',0.,0.,DZ,0,'ONLY',parcov,11)

      parcov (1) = cov3th / 2. 
      dz=dz + cov2th/2. + cov3th/2.
      call gsposp('ECST',4,'ECC2',0.,0.,DZ,0,'ONLY',parcov,11)


c  position lead and scintillator planes
c   loop over modules (planes), then 3 views per module (U,V,W)
      do j=1,nmod
         do i=1,nviews

            ipl  = i + (j-1) * nviews
            hu   = h1/2. + float(ipl-1)*dthi*tanh
            au   = a1/2. + float(ipl-1)*dthi*tana
            hvw  = au * sin(ther*pi/180.)
            avw  = hu / sin(ther*pi/180.)
            dzsc = -(39.*dthi-pbthi)/2. + float(ipl-1)*dthi + 0.5*scthi

            parcal(1)  = hu
            parcal(2)  = 0.
            parcal(3)  = 0.
            parcal(5)  = 0.
            parcal(6)  = 0.
            parcal(4)  = pbthi/2.
            parcal(8)  = pbthi/2.
            parcal(9)  = au
            parcal(10) = au

c  place lead planes
            if (ipl.ne.1)then
               dzpb = dzsc - dthi/2.
               call gsposp('ECPB',IPL-1,'ECAL',0.,0.,DZPB,ec_u,'ONLY',
     1              parcal,11)
               dzsc = dzpb + dthi/2.
            endif

            parcal(4) = 0.5 * scthi
            parcal(8) = 0.5 * scthi

c  place U planes
            if (i.eq.1) then
               if(j.le.5)then
                  call gsposp('ECUI',j,'ECAL',0.,0.,dzsc,ec_u,'ONLY',
     1                 parcal,11)
               else
                  call gsposp('ECUO',j-5,'ECAL',0.,0.,dzsc,ec_u,'ONLY',
     1                 parcal,11)
               endif

            else
               the = atan((avw-hvw*2./tan(ther*pi/180.))/2./hvw)*180./pi
               parcal(1)  = hvw
               parcal(2)  = the
               parcal(5)  = avw
               parcal(6)  = avw
               parcal(9)  = 0.001
               parcal(10) = 0.001

c  place V planes
               if (i.eq.2) then
                  dx = hvw * tan(the*pi/180.) * cos(ther*pi/180.)
                  dy = hvw * tan(the*pi/180.) * sin(ther*pi/180.)
                  x0 =  au - hvw * sin(ther*pi/180.) + dx
                  y0 =  hu - hvw * cos(ther*pi/180.) - dy
                  parcal(3) = 180.
                  if(j.le.5)then
                     call gsposp('ECVI',j,'ECAL',x0,y0,dzsc,ec_v,
     1                    'ONLY',parcal,11)
                  else
                     call gsposp('ECVO',j-5,'ECAL',x0,y0,dzsc,ec_v,
     1                    'ONLY',parcal,11)
                  endif
                  
c  place W planes
               else
                  x0 = - au + hvw * sin(ther*pi/180.) - dx
                  y0 =   hu - hvw * cos(ther*pi/180.) - dy
                  parcal(3) = 0.
                  if(j.le.5)then
                     call gsposp('ECWI',j,'ECAL',x0,y0,dzsc,ec_w,
     1                    'ONLY',parcal,11)
                  else
                     call gsposp('ECWO',j-5,'ECAL',x0,y0,dzsc,ec_w,
     1                    'ONLY',parcal,11)
                  endif
               endif

            endif

         enddo  !views
      enddo   !modules


c  divide U,V,W scintillator planes into parallel strips
      call gsdvn('CSUI','ECUI',nstrip,3)
      call gsdvn('CSVI','ECVI',nstrip,3)
      call gsdvn('CSWI','ECWI',nstrip,3)
      call gsdvn('CSUO','ECUO',nstrip,3)
      call gsdvn('CSVO','ECVO',nstrip,3)
      call gsdvn('CSWO','ECWO',nstrip,3)
      
      return
      end

c-------------------------------------------------------------------------------
