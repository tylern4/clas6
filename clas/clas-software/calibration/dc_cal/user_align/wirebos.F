      SUBROUTINE WIREBOS
c
c_begin_doc
c
c  Documentation for subroutine WIREBOS
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Robert Feuerbach      Created:  Thu Aug 14 14:09:29 EDT 1997
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
c#include "bcs.inc"
c#include "../trk/sdaswim.inc"
#include "wcs.inc"
#include "clas_offsets.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: wirebos.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2001/08/30 16:51:50 $')
      PARAMETER (CAUTHO = '$Author: feuerbac $')
      DATA CRCSID/ 
     1'$Id: wirebos.F,v 1.1 2001/08/30 16:51:50 feuerbac Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='wirepos')                                   
      PARAMETER (CRAUTH='Robert Feuerbach')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c  Module information:
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE/0/
      integer MAX_WIRES
      parameter (max_wires = 192)
      integer lun, layer, wire, minwire, maxwire,ipl, isec
      integer namind, wireind, layind, nami_dcgw, nami_dcgm
      integer ind_dcgw, ind_dcgm, ncol_dcgw, ncol_dcgm
      integer i
      character *20 filename
      integer baseind, Max_Wire, Min_Wire, GMind, GWind
c_end_var
      
      REAL MidX, MidY, MidZ, DirX, DirY, DirZ,WLen,WLenHV
      GWind(layer,wire) = ind_dcgw + ((layer-1)*MAX_WIRES+wire-1)*ncol_dcgw
      GMind(layer) = ind_dcgm + (layer-1)*ncol_dcgm
      Max_Wire(baseind) = JW(baseind+DCGM_max_wire)
      Min_Wire(baseind) = JW(baseind+DCGM_min_wire)
      MidX(baseind) = RJW(baseind+DCGW_x_mid)
      MidY(baseind) = RJW(baseind+DCGW_y_mid)
      MidZ(baseind) = RJW(baseind+DCGW_z_mid)
      DirX(baseind) = RJW(baseind+DCGW_x_dir)
      DirY(baseind) = RJW(baseind+DCGW_y_dir)
      DirZ(baseind) = RJW(baseind+DCGW_z_dir)
      WLen(baseind) = RJW(baseind+DCGW_w_len)
      WLenHV(baseind) = RJW(baseind+DCGW_w_len_hv)

c
c  executable code for routine WIREPOS:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      else
        goto 999
      ENDIF

      isec = 3

      lun = 11

      if (nami_dcgw.le.0) nami_dcgw = namind('DCGW')

      ind_dcgw = jw(nami_dcgw)
      
      do while (jw(ind_dcgw-2).lt.isec.AND.jw(ind_dcgw-1).gt.0)
         ind_dcgw = jw(ind_dcgw-1)
      enddo

      if (jw(ind_dcgw-2).ne.isec) then     !Send out warning
         write(crmess,401)isec
         call recmes(crname,'W',crmess)
      else
         ncol_dcgw = JW(ind_dcgw-5)
      endif

      if (nami_dcgm.le.0) nami_dcgm = namind('DCGM')

      ind_dcgm = jw(nami_dcgm)

      do while (jw(ind_dcgm-2).lt.isec.AND.jw(ind_dcgm-1).gt.0)
         ind_dcgm = jw(ind_dcgm-1)
      enddo

      if (jw(ind_dcgm-2).ne.isec) then     !Send out warning
         write(crmess,401)isec
         call recmes(crname,'W',crmess)
      else
         ncol_dcgm = JW(ind_dcgm-5)
      endif
 401  format(1x,'Cannot get bank for sector ',i5)
      
      
c     Now, loop over all the layers and print out the layer, wire,
c     wire location, and wire direction.
      
      write(lun,3)jw(ind_dcgm-2)
      write(lun,1)
      do 100 layer = 1,36
         if(layer.eq.5.or.layer.eq.6) goto 100
         layind = GMind(layer)
         minwire = Min_Wire(layind)
         maxwire = Max_Wire(layind)
         write(lun,5)minwire,maxwire,JW(layind+DCGM_max_cylw)
         write(lun,4)layer,RJW(layind+DCGM_x_curve),RJW(layind+DCGM_y_curve),
     $        RJW(layind+DCGM_z_curve),RJW(layind+DCGM_r_curve),
     $        RJW(layind+DCGM_theta_start),RJW(layind+DCGM_d_theta),
     $        RJW(layind+DCGM_x_nmid),RJW(layind+DCGM_y_nmid),
     $        RJW(layind+DCGM_z_nmid),RJW(layind+DCGM_stereo),
     $        RJW(layind+DCGM_cell_size),RJW(layind+DCGM_x_norm),
     $        RJW(layind+DCGM_y_norm),RJW(layind+DCGM_z_norm),
     $        RJW(layind+DCGM_p_dist),RJW(layind+DCGM_p_sep)
         do wire = minwire,maxwire
            wireind = GWind(layer,wire)
            write(lun,2)layer,wire,MidX(wireind),MidY(wireind),MidZ(wireind),
     $           DirX(wireind),DirY(wireind),DirZ(wireind),
     $           WLen(wireind),WLenHV(wireind)
         enddo
 100  continue
 999  continue      
      return
      
 1    format('#',1x,'Layer',1x,'Wire',4x,'Midx',6x,'Midy',6x,'Midz',6x
     $     ,'Dirx',6x,'Diry',6x,'Dirz',3x,'+Wlen',4x,'-Wlen')
 2    format(3x,i3,1x,i4,3(1x,F9.4),3(1x,F9.6),2(1x,F9.4))
 3    format(1x,'Sector is: ',i3)
 5    format(3x,'Min wire: ',i3,2x,'Max wire: ',i3,2x,'Cyl wire: ',i3)
 4    format(3x,'layer ',i3,2x,'C: (',3(1x,f9.4),')  R: ',f10.4,/
     $     ,3x,'Th1: ',f10.6,2x,'dTh: ',f9.6,2x,'MidN: (',3(1x,f8.5),' )',/
     $     ,3x,'Ster: ',f9.6,2x,'CellS: ',f6.4,2x,'PlN: (',3(1x,f8.5),' )',/
     $     ,3x,'PlDist: ',f8.4,2x,'PlSep: ',f8.4)
      END
c
c------------------------------------------------------------------------------




