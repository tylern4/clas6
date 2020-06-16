      Subroutine trk_dtime_retbla(sect,lay,inwire,hitp,alpha,tdc,Tlen,beta
     $     ,start_time,driftt,tdoca,tsigma,wlen,B)
*_begin_doc
*  RCS ID string
*  $Id: trk_dtime_retbla.F,v 1.4 1999/03/02 18:24:01 claschef Exp $
*
*  Documentation for subroutine TRK_DTIME_RETBLA -- a hack of trk_dtime
*
*  Purpose:  Derive the drift time from raw TDC. Then get the
*  -------   drift distance and sigma.
*              
*  Purpose:  Derive Drift Time from raw TDC
*  -------   The cable lenght are now provided
*            by the DC processor t0(wire,layer,sector)
*           
*
*  Input Parameters: sect        : sector number
*  ----------------  lay         : layer number
*                    inwire      : wire number of the hit
*                    hitp(3)     : location of the hit (SCS)
*                    alpha       : local track angle at the plane in degrees
*                    tdc         : raw TDC value
*                    Tlen        : track length from start to this hit.
*                    beta        : particle velocity
*                    start_time  : event star_time
*
*  Output Parameters: driftt      : drift time to the hit wire
*  -----------------  tdoca       : measured distance Dmeas [cm]
*                     tsigma      : Sigma of Dmeas [cm]
*                     wlen        : distance from hit to STB's [cm]
*                     B(3)        : magnetic field at hitp (SCS)
*
*
*  Other routines: call by ana_dcam
*  --------------
*
*  Notes:  Old ana_dtime.F
*  -----
*
*  Author:         Francois H. Roudot
*  ------        
*
*  Creation Date:  21 Nov, 1996
*  -------------
*
*  Major revisions: Sept.97  F.Klein:   Sigma of Dmeas,
*  --------------- 			using Bogdan's parameters
*                   Oct. 1997 Gilfoyle: Removed switch to use
*                                       Bogdan's spline fit to 
*                                       June, 1997 data and made
*                                       this fit available as a
*                                       choice in the dc library.
*
*                   Dec 1998 R. Feuerbach: Single hit version
*                                          for simple recreation
*                                          of the TBLA bank.
*                                          No tracks in common blocks.
*_end_doc
*
      Implicit None
      Save
*
*
*_begin_inc
*  include files:
*  -------------
#include "dc_cal_tdly.inc"
#include "dc_tcl.inc"
#include "bcs.inc"
#include "wcs.inc"
#include "clas_offsets.inc"

*
*_end_inc
*
*_begin_var
*  input/output variables:
*  ----------------------
      INTEGER sect, lay, inwire
      REAL hitp(3), alpha, tdc, Tlen, beta, start_time, driftt, tdoca,
     $     tsigma, wlen, B(3)
*
*  Local pre-defined variables:
*  --------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_dtime_retbla.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.4 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/03/02 18:24:01 $')
      PARAMETER ( CAUTHO = '$Author: claschef $')
      DATA CRCSID/
     1'$Id: trk_dtime_retbla.F,v 1.4 1999/03/02 18:24:01 claschef Exp $
     3roudot'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_DTIME')
      PARAMETER (CRAUTH='Francois H. ROUDOT')
*
*  Local User defined variables:
*  ----------------------------
      integer GWind, GMind, MAMIND, sector, layer, wir, baseind
      real WireHalfL
      integer ind_dcgw(6), ind_dcgm(6), ind, check, ncol_dcgm, ncol_dcgw
      integer nami_dcgm, nami_dcgw
      INTEGER nlay, ireg, ip, is, wir_ind, wire
      REAL tFLd,tPRd, B_abs, B_para, xyz(3)
      REAL V_light
      data V_light/29.97925/                ![cm/nsec]
      DATA nlay /6/

c  STATEMENT Functions to get indices for DCGM and DCGW wanted row
      GWind(sector,layer,wir) = ind_dcgw(sector) + 
     &     ((layer-1)*MAX_WIRE+wir-1)*ncol_dcgw
      GMind(sector,layer) = ind_dcgm(sector) + (layer-1)*ncol_dcgm
      WireHalfL(baseind) = RJW(baseind+DCGW_w_len)
c ------------------------------------
      
      if(inwire.EQ.0. .OR. tdc.eq.0.)  then   ! no information, exit
         driftt = 0.
         tdoca = 0.
         tsigma = 0.
         wlen = 0.
         B(1) = 0.
         B(2) = 0.
         B(3) = 0.
         goto 999
      endif

c INITIALIZATION
c Get the indices for DCGM and DCGW banks
      if (nami_dcgm.le.0) nami_dcgm = mamind(jw,'DCGM')
      if (nami_dcgw.le.0) nami_dcgw = mamind(jw,'DCGW')
      
c each sector has its own DCGW and DCGM bank
      check = 0
      ind = nami_dcgm+1
      do while (jw(ind-1).gt.0)
         ind = jw(ind-1)
         ind_dcgm(jw(ind-2)) = ind
         check = check+1
      enddo
      if (check.ne.6) call recmes(crname,'W','Error getting DCGM bank')
      ncol_dcgm = jw(ind_dcgm(1)-5)

      check = 0
      ind = nami_dcgw+1
      do while (jw(ind-1).gt.0)
         ind = jw(ind-1)
         ind_dcgw(jw(ind-2)) = ind
         check = check+1
      enddo
      if (check.ne.6) call recmes(crname,'W','Error getting DCGW bank')
      ncol_dcgw = jw(ind_dcgw(1)-5)

c END setup DCGM, DCGW banks

C END INITIALIZATION

      is = (lay-1)/6 + 1                    ! superlayer number

      wire = INT(ABS(inwire)+0.5)
      wir_ind = GWind(sect,lay,wire)

      tfld = 0
      if (beta.EQ.0.0) then
c if beta = 0 the correction on time of flight is lost
c  tentatively set beta=0.7 as a rough estimation 
c  (at least better than nothing!)
         tFLd = Tlen/(0.7*V_light)
      else
         tFLd = Tlen/(beta*V_light)
      endif

c Calculate the distance the signal propagated along the wire
c     remember that R1 had the STB's on the -y side, while R2 and R3
c     have then on the +y side.
c     should be a small stereo angle correction, but that is at the .5%
c     level

      if (lay.le.12) then         ! Region 1
         wlen = WireHalfL(wir_ind) + hitp(2)
      else
         wlen = WireHalfL(wir_ind) - hitp(2)
      endif

      tPRd = wlen/SNGL(dc_vel_prop)
      driftt = t0(wire,lay,sect)-
     &     (start_time + tPRd + tFLd) - tdc/DC_Tslope
      ireg = (is-1)/2+1
      driftt = driftt - dc_reg_pulser_time(ireg)
      tdoca = -9.999
      tsigma = 0.150 
c     
c     remove switch to use bogdan's spline fit after putting that
c     choice into the dc library. gpg 10/97
c     
      
c sda_snake does sometimes change the value of the point passed in,
c so make a dummy copy.
      xyz(1) = hitp(1)
      xyz(2) = hitp(2)
      xyz(3) = hitp(3)
      call sda_snake(xyz,B)
      if (ireg.eq.2) then
         B_abs=SQRT(B(1)*B(1)+B(2)*B(2)+B(3)*B(3))
         B_para = B(2)
      else
         b_abs=0.0
         B_para = 0.
      endif   
      call dc_time_to_dist(sect,lay,wire,beta,alpha,
     &     driftt,B_abs,B_para,tdoca,tsigma)
      
c      endif

 999  RETURN
      END

