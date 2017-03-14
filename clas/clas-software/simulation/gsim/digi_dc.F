      subroutine digi_dc

c_begin_doc

c  Documentation for subroutine digi_dc

c  $Id: digi_dc.F,v 1.9 2009/03/14 20:27:52 fklein Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *     Store DIGI for drift chambers                              *
c     *      KDIGI(1): WIRE number                                     *
c     *      KDIGI(2): ADC  left                                       *
c     *      KDIGI(3): DRIFT time                                      *
c     *      KDIGI(4): DOCA                                            *
c     *                                                                *
c     *                                                                *
c     *        Author   : Bogdan Niczyporuk          May 1988          *
c     *       modified  : M.Guckes       July  1988                    *
c     *       modified  : K. Joo         April 1998                    *
c     *                                                                *
c     *                                                                *
c     *          Mostly rewritten by M. Guidal March 93                *
c     *          ======================================                *
c     ******************************************************************

c  Notes:
c  ------
c   ejw did first pass at cleaning up this code.  I am doing 2nd pass.
c   It still needs a 3rd.  It looked bad enough when I got it.  I would
c   hate to see what it looked like when Elliott got it.  When I ask him
c   about it, he just cries. (jam)
c
c   Although lab x,y, and z coordinates are used, phi refers to
c   bogdan-coordinate-system.  In lab coordinates it would be theta.
c
c   How it is done:
c   ---------------
c	Get "hits" in each DC layer.  (where track passes sense wire plane)
c	Using geometry, calculate which DC "cell" this "hit" is in.
c	Since glancing tracks can leave hits in many cells for each "hit"
c	  in the layer also check "cells" on either side of found one.
c	Oh, check to make sure there actually are wires at the spot where
c	  your "cell" is.
c	For each sense wire candidate, calculate distance-of-closest-approach
c	  (dca) of track to wire.
c	If dca is smaller than the cell radius, smear the hit, and calculate
c	  the TDC and ADC(which is later thrown away).
c	Since this proceedure could generate multiple hits on wires, scan
c	  through the list of already saved hits.  If there are none, save
c	  this one.  If you find one, keep the one with the smallest time
c	  (largest TDC).  Add the ADC's together (to have more to throw away).
c	After going through all the "hits" in a layer, store them for future
c	  use.
c     
c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 2-dec-94
c     James A. Mueller, University of Pittsburgh, 25-jul-96
c     Burin Asavapibhop, U. of Massachusetts, 23-mar-98
c	- set Ttrig = 0(This is the start time) so we don't have to worry
c	  about the trigger time related to the TOF. There are also some
c	  parameters(td1,t0) which we can set to zero as well currently 
c	  these parameters are included in t0(iw,il,isec) in the map database
c	  for RECSIS. It's set to 1750 for run number 1(GSIM data). 
c 
c     Kyungseon Joo, 14-Apr-98
c       no dca smearing by gaussian function any more. 
c       It will be done in GSIMKO.
c       Since doca bank is defined as B16,
c       dca is multipled by 10000 and stored in idca.
c       idca -> doca bank. So idca has micron unit whereas 
c       dca has cm unit.
c
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: digi_dc.F,v $')
      parameter (crevis= '$Revision: 1.9 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/03/14 20:27:52 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'DIGI_DC')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: digi_dc.F,v 1.9 2009/03/14 20:27:52 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcunit.inc"
#include "gcflag.inc"
#include "gconst.inc"

#include "lcgeom.inc"
#include "dcpar.inc"
#include "dcsets.inc"
#include "dcgeom.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_dc.inc"

#include "gsim_trigger.inc"

#include "wcs.inc"
#include "clas_offsets.inc"

c_end_inc

c  local variables:
c  ----------------
      integer nhmax
      parameter   (nhmax=1000)

      integer numbv(nvdc,nhmax),itra(nhmax),numvs(nvdc)
      integer kdigi(nddc,nhmax)
      real hits(nhdc,nhmax)

      real vdot, ggauss
      real diff(3),dca,a(3)
      real xscs(3),dscs(3)
      integer itdc,adc,ihit(nhmax),ndigi,idi,idca
      real wpos(3), wdir(3)
      real tdrift,tflight,tprop,tfull,tdc,wire_len, signal_dist
      real wirehv_len
C The dreaded parameters!  We must get rid of these in the near future.
c Make sure that they are consistent with RECSIS for run number 1.
      real Tcnt,t0,td1,vprop
cfk     data vprop /16.0000/            ! cm/ns
cfk dc_v_signal=23cm/s according to include/dc_cal_tdly.inc (Mac)
      data vprop /23.0000/              ! cm/ns
      data Tcnt /2.0/			! counts/ns
      data t0 /100.0/			! cable length
      data td1 /1850.0/			! delay for common TDC stop
c  the following was automatically generated...ejw, 2-dec-94
c  deleted some that are no longer needed. probably missed some...jam 25-jul-96
c      real aaa(3),ast,dis,diss,disw
c      real phi,philay,phis,phiw,pnw,x0,xh,yh,z0,zh,zhp
      integer idig,ih,ik,il,ils,ir,is,isect,iwir,iwire
      integer nhits,nwmin,nwmax,nhita,nhitb
      real theta, th_norm, alfa, sixty_deg
      data sixty_deg/1.0471976/
c
c
c function to get wire number
      integer dc_getwire

c  for accessing the Bos banks
      INTEGER ind_dcgm(6),ncol_dcgm,LayInd
      INTEGER ind_dcgw(6),ncol_dcgw,WireInd      !,gwfirst,gwmid,gwlast
      integer nami_dcgw, nami_dcgm, ind, check
      data nami_dcgm, nami_dcgw/0,0/
c 
c  FUNCTIONS for the indicies of DCGM and DCGW BosBanks
      integer MAMIND, GWind, GMind, sector, layer, wire, baseind
      REAL MidX, MidY, MidZ, DirX, DirY, DirZ, WireHalfL, WireHVHalfL

      integer MAX_WIRE
      parameter (MAX_WIRE=192)

      GWind(sector,layer,wire) = ind_dcgw(sector) + 
     &     ((layer-1)*MAX_WIRE+wire-1)*ncol_dcgw
      GMind(sector,layer) = ind_dcgm(sector) + (layer-1)*ncol_dcgm
      MidX(baseind) = RJW(baseind+DCGW_x_mid)
      MidY(baseind) = RJW(baseind+DCGW_y_mid)
      MidZ(baseind) = RJW(baseind+DCGW_z_mid)
      DirX(baseind) = RJW(baseind+DCGW_x_dir)
      DirY(baseind) = RJW(baseind+DCGW_y_dir)
      DirZ(baseind) = RJW(baseind+DCGW_z_dir)

      WireHalfL(baseind) = RJW(baseind+DCGW_w_len)
      WireHVHalfL(baseind) = RJW(baseind+DCGW_w_len_hv)
c_end_var


c  executable code:
c  ----------------

c  find DCGM, DCGW banks
      if (nami_dcgm .le. 0) nami_dcgm = mamind(jw,'DCGM')
      if (nami_dcgw .le. 0) nami_dcgw = mamind(jw,'DCGW')

      check = 0
      ind = nami_dcgm+1
      do while (jw(ind-1).gt.0)
         ind = jw(ind-1)
         ind_dcgm(jw(ind-2)) = ind
         check = check+1
      enddo
      if (check.ne.6) call recmes(crname,'W','Error getting DCGM bank')
      ncol_dcgm = jw(ind_dcgm(check)-5)

      check = 0
      ind = nami_dcgw+1
      do while (jw(ind-1).gt.0)
         ind = jw(ind-1)
         ind_dcgw(jw(ind-2)) = ind
         check = check+1
      enddo
      if (check.ne.6) call recmes(crname,'W','Error getting DCGW bank')
      ncol_dcgw = jw(ind_dcgw(check)-5)
      
c  loop over the sectors
      do isect=1,nsect
         numvs(1) = isect
         
c  loop over the super layers
         do is=1,ndc_sl
            numvs(2)=is
            
c  loop over wire layers
            do 30 il=1,6
               numvs(3) = il

c  get CTUB section hits
               call gfhits('DC  ',cdet_dcs(is),nvdc,nhdc,nhmax,
     1              0,numvs,itra,numbv,hits,nhita)

c  add straight section hits
               nhitb=0
               if(is.le.2)then
                  call gfhits('DC  ',cdet_sts(is),nvdc,nhdc,
     1                 nhmax-nhita,0,numvs,
     1                 itra(nhita+1),numbv(1,nhita+1),
     1                 hits(1,nhita+1),nhitb)
               endif
               
               nhits=nhita+nhitb
               
c  skip this wire layer if there are no hits
               if (nhits.le.0) go to 30
               
               if (nhits.gt.nhmax) then
                  write(chmail,1100)il,is,isect,nhmax,nhits
                  call recmes(crname,'I',chmail)
                  nhits = nhmax
               end if
               
               ils=(is-1)*6+il
               ir=(is-1)/2+1

c  Initialize number of digitizations to zero for this layer
               ndigi = 0
               do ih=1,nhits
                  kdigi(1,ih) = 0
                  kdigi(2,ih) = 0
                  kdigi(3,ih) = 0
               enddo

c  Loop over the hits in one layer ( do digitalization )
               do 50 ih=1,nhits
c  ignore hit if energy loss too small
c  temporary kludge...allow hits if "nomate" specified
                  if((hits(5,ih).lt.1.e-6).and..not.nomate(dc))
     1                 goto 50

c  Change to Sector Coordinate System
                  xscs(1) = hits(1,ih)*cossec(isect)+hits(2,ih)
     $                 *sinsec(isect)
                  xscs(2) =-hits(1,ih)*sinsec(isect)+hits(2,ih)
     $                 *cossec(isect)
                  xscs(3) = hits(3,ih)

                  dscs(1) = hits(8,ih)*cossec(isect)+hits(9,ih)*
     1                 sinsec(isect)
                  dscs(2) =-hits(8,ih)*sinsec(isect)+hits(9,ih)*
     1                 cossec(isect)
                  dscs(3) = hits(10,ih)

                  iwir = dc_getwire(xscs,isect,ils)

c  To Check wire number range
                  LayInd = GMind(isect,ils)
                  nwmax = JW(LayInd+DCGM_MAX_WIRE)
                  nwmin = JW(LayInd+DCGM_MIN_WIRE)

c Calculate the angle of the track at the wire
                  if(dscs(1).eq.0.0 .and. dscs(3).eq.0.0) go to 50
                  theta = ATAN2(dscs(1),dscs(3))
           
C  Find TDC and ADC for this wire and those on either side.  
C    Should modify to account for curlers hitting more wires than this
C    in a layer, but it's probably a small effect.

                  DO 90 ik = 1,3
                     if(ik.eq.1) iwire = iwir       
                     if(ik.eq.2) iwire = iwir - 1
                     if(ik.eq.3) iwire = iwir + 1
 81                  continue
                     if(iwire.lt.nwmin .or. iwire.gt.nwmax) go to 90

c Calculate the angle of the normal to the layer at the wire
                     IF(iwire.GT.JW(layind+DCGM_max_cylw)) THEN !Plane
                        th_norm = ATAN2(RJW(layind+DCGM_x_norm),
     &                       RJW(layind+DCGM_z_norm))
                     ELSE       !Cylinder
                        th_norm = RJW(layind+DCGM_theta_start)+
     &                      (iwire-1) * RJW(layind+DCGM_d_theta)
                     ENDIF
C Calculate entrance angle and fold into 30 degree range
                     alfa = abs(theta - th_norm) ! alfa
                     alfa = mod(alfa,sixty_deg)
                     if(alfa.gt.sixty_deg/2.0) alfa = sixty_deg - alfa
              
                     WireInd = GWind(isect,ils,iwire)
                     wpos(1) = MidX(WireInd)
                     wpos(2) = MidY(WireInd)
                     wpos(3) = MidZ(WireInd)
                     wdir(1) = DirX(WireInd)
                     wdir(2) = DirY(WireInd)
                     wdir(3) = DirZ(WireInd)

c  Calculate a distance of closest approach to the wire
                     call cross(wdir(1),dscs(1),a)
                     call vunit(a,a,3)
                     
                     diff(1)=xscs(1)-wpos(1)
                     diff(2)=xscs(2)-wpos(2)
                     diff(3)=xscs(3)-wpos(3)
                     
                     dca = vdot(diff,a,3)
c dca in um for DOCA bank
                     idca = NINT(dca*10000)
c smear doca by resolution:
                     dca=ggauss(dca,dc_resolution(is))

c  Check cell radius
c    Approximates hexagonal cell with cylinder.
c    slightly underestimates hit probability in neighboring cells

                     if(iwire .le. JW(LayInd+DCGM_max_cylw)) then
                        if(abs(dca)*cos(alfa).gt.
     +                       RJW(LayInd+DCGM_cell_size)) go to 90
                     else
                        if(abs(dca)*cos(alfa).gt.
     +                       0.5*RJW(LayInd+DCGM_p_sep)) go to 90
                     endif

c  Calculate drift-time (Should be a call to DC function for this here. 
c     function should be inverse of function used in reconstruction)
c     default dc_dist_to_time is in tdc_counts/cm. YUCH! 
                     tdrift = abs(dca*dc_dist_to_time)/Tcnt

c  Get flight-time to layer from hits array in ns
                     tflight = hits(7,ih)
c  Calculate propagation time along wire in ns
                     wire_len = WireHalfL(WireInd)
                     wireHV_len = WireHVHalfL(WireInd)

c  region 1 has STB's on opposite side (-y) than R2 and R3 (+y)
cfk  it's the opposite in dc/dc_nominal_geom.c
                     if (ils.gt.12) then
                        signal_dist = wire_len - xscs(2)/wdir(2)
                     else
                        signal_dist = wire_len + xscs(2)/wdir(2)
                     endif

c make sure the hit is inside the chamber, where the wires are
                     if (signal_dist .LT. 0. .OR.
     $                    signal_dist .GT. wire_len+wireHV_len) go to 90
                     
                     tprop = signal_dist / vprop

                     tfull = tdrift + tflight + tprop

c  Find TDC, ADC  (Again should be pair of DC routines that use calibrations
c     to do this and inverse in reconstruction.)
c Set Ttrig = 0
		     Ttrig = 0.	
                     tdc = Ttrig + TD1 - tfull - t0   

                     itdc = NINT(Tcnt*tdc)
                     adc = int(hits(5,ih)*1.e7)
c
c  Check to see if we already have a hit on this wire.
c    If so determine which one would have fired TDC and save it.
                     if (ndigi.gt.0) then
                        do idi=1,ndigi
                           if (iwire.eq.kdigi(1,idi)) then
                              if (itdc.gt.kdigi(3,idi)) then
                                 kdigi(2,idi) = kdigi(2,idi) + adc
                                 kdigi(3,idi) = itdc
                                 kdigi(4,idi) = idca
                                 ihit(idi) = ih
                              else
                                 kdigi(2,idi) = kdigi(2,idi) + adc
                              endif
                              go to 80
                           endif
                        enddo
                     endif
c  New hit wire
                     ndigi = ndigi + 1
                     kdigi(1,ndigi) = iwire
                     kdigi(2,ndigi) = adc
                     kdigi(3,ndigi) = itdc
                     kdigi(4,ndigi) = idca
                     ihit(ndigi) = ih
 80                  CONTINUE
                     if(ik.eq.2) iwire = iwire - 1
                     if(ik.eq.3) iwire = iwire + 1
                     if(ik.gt.1) go to 81

 90               continue
 50            continue
               

c  Store digitizations
               do idi=1,ndigi
                  if(kdigi(1,idi).ne.0) then
                     ih = ihit(idi)
                     call gsdigi(set_dc,det_dc(is),itra(ih),1,numvs,
     1                       kdigi(1,idi),idig)
                  endif 
                     
                  if (idig.eq.0) then
                     write (chmail,1200) il,is,isect
                     call recmes(crname,'I',chmail)
                  endif
               enddo

         
 30         continue            !wire layer loop
         enddo                  !superlayer loop
      enddo                     !sector loop
      
      return



 1000 FORMAT ('?CHDIGI ERROR   : User parameters of set ',A4,
     1        ' could not be returned; no digitisation.')
 1100 FORMAT ('?CHDIGI ERROR   : Hits overflow for LA=',I3
     1       ,' SL=',I3,' in sector ',I2,'. Only ',I4,' of ',I4,' hits '
     2       ,'could not be processed.')
 1200 FORMAT ('?CHDIGI ERROR   : Digitization for LA=',I3
     1       ,' SL=',I3,' in sector ',I2,' could not be stored.')
 101  FORMAT(/1X,'Sect,SLayer,Layer=',3I8/
     1       1X,'Wire,ADCL,ADCR,DT=',4I8,' <',I4,'    Cell=',F5.3)

      end

c---------------------------------------------------------------------------------

