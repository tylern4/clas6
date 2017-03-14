      Subroutine trk_dtime(is,itr,itr_el,start_time,beta,xdigi)
*_begin_doc
*  RCS ID string
*  $Id: trk_dtime.F,v 1.1 2001/08/30 16:51:46 feuerbac Exp $
*
*  Documentation for subroutine TRK_DTIME
*
*  Purpose:  Derive Drift Time from raw TDC
*  -------   The calble lenght are now provided
*            by the DC processor t0(wire,layer,sector)
*
*  Input Parameters: is          : superlayer number
*  ----------------  itr         : track candidate number
*                    itr_el      : electron track candidate
*                    beta        : particle velocity
*                    start_time  : event star_time
*                    xdigi(1,la)  : hit wire number
*                    xdigi(2,la)  : raw TDC for the hit wire "xdigi(1,la)"
*
*  Output Parameters: xdigi(3,la) : drift time to the hit wire "xdigi(1,la)"
*  -----------------  xdigi(4,la) : measured distance Dmeas [cm]
*                     xdigi(5,la) : Sigma of Dmeas [cm]

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
*_end_doc
*
      Implicit None
      Save
*
*
*_begin_inc
*  include files:
*  -------------
#include "sdaswim.inc"
#include "anapatt.inc"
#include "dc_cal_tdly.inc"
#include "dc_tcl.inc"
#include "trktcl_var.inc"
*
*_end_inc
*
*_begin_var
*  input/output variables:
*  ----------------------
      INTEGER is, itr, itr_el
      REAL xdigi(5,6), beta, start_time
*
*  Local pre-defined variables:
*  --------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_dtime.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.1 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2001/08/30 16:51:46 $')
      PARAMETER ( CAUTHO = '$Author: feuerbac $')
      DATA CRCSID/
     1'$Id: trk_dtime.F,v 1.1 2001/08/30 16:51:46 feuerbac Exp $
     3roudot'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_DTIME')
      PARAMETER (CRAUTH='Francois H. ROUDOT')
*
*  Local User defined variables:
*  ----------------------------
      INTEGER la, il, iw, nlay, ireg, ip
      INTEGER isec
      REAL tFLd,tPRd, B(3), B_abs, B_para, xyz(3)
      REAL V_light
      data V_light/29.97925/                ![cm/nsec]
      DATA nlay /6/

      real alpha                            ! local track angle in degre (absolut value)

      isec = itr_sect(itr)

      DO 10 la = 1,nlay
        if(xdigi(1,la).EQ.0. .OR. xdigi(2,la).eq.0.) goto 10 
        il = (is-1)*nlay + la
        iw = IFIX(ABS(xdigi(1,la)))
        tfld = 0
        if (beta.EQ.0.0) then
c if beta = 0 the correction on time of flight is lost
c  tentatively set beta=0.7 as a rough estimation 
c  (at least better than nothing!)
           tFLd = trk(8,il,itr)/(0.7*V_light)
        else
           tFLd = trk(8,il,itr)/(beta*V_light)
        endif
        tPRd = trk(9,il,itr)/SNGL(dc_vel_prop)
        xdigi(3,la) = t0(iw,il,isec)-
     &	  (start_time + tPRd + tFLd) - xdigi(2,la)/DC_Tslope
        ireg = (is-1)/2+1
        xdigi(3,la) = xdigi(3,la) - dc_reg_pulser_time(ireg)
        xdigi(4,la) = -9.999
	xdigi(5,la) = 0.150 
c alpha is extracted from hit based tracking but until now it is 
c set by default to 15 degres in the dc_time_to_distance function

        alpha = trk(10,il,itr)
	if(ipar_trk_print(4).EQ.2) then
	  write(6,'(A,7F9.3)')' dtime:',xdigi(3,la),
     &    t0(iw,il,isec),start_time,tprd,tfld,xdigi(2,la)/dc_Tslope,alpha
	endif
c
c remove switch to use bogdan's spline fit after putting that
c choice into the dc library. gpg 10/97
c
        if (xdigi(3,la).GT.-100.and.xdigi(3,la).lt.3000) then

           if (ipar_trk_magtyp.EQ.0) then
              if (ireg.eq.2) then
                 ip = il + 3 
                 xyz(1) = trkp(2,ip,itr)
                 xyz(2) = trkp(3,ip,itr)
                 xyz(3) = trkp(1,ip,itr)
                 call sda_snake(xyz,B)
                 B_abs=SQRT(B(1)*B(1)+B(2)*B(2)+B(3)*B(3))
                 B_para = B(2)
              else
                 b_abs=0.0
              endif   
           else
              b_abs=0.0
           endif
           call dc_time_to_dist(isec,il,iw,beta,alpha,
     &          xdigi(3,la),B_abs,B_para,xdigi(4,la),xdigi(5,la))

c For doing the DC alignment, we want the option of manually modifying
c the Sigma's for the different SuperLayers.
           xdigi(5,la) = xdigi(5,la)*dc_mult_Sigma(is)

        endif
 10   enddo

 999  RETURN
      END

