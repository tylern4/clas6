      SUBROUTINE TRK_CALC_WIRE_MOD(isec,ilnk,beta,trk_wire,trk_doca,mcin)
c
c_begin_doc
c  RCS ID string
c  $Id: trk_calc_wire_mod.F,v 1.3 1999/03/02 18:23:04 claschef Exp $
c
c  Documentation for subroutine TRK_CALC_WIRE
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
c  Author:   James Mueller      Created:  Thu Apr  2 08:23:20 EST 1998
c  -------
c
c  Major revisions:
c  ----------------
c  John McNabb: modified trk_calc_wire to return docas and wire from an 
c     array mcin(9), instead of from a bosbank, where 
c         mcin(1-3) is the vertex location
c         mcin(4-6) is the direction cosines 
c         mcin(7) is the momentum of the particle
c         mcin(8) is beta of the particle
c         mcin(9) is the charge of the particle
c     it still is dependent on the existance of several tracking common blocks
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "sdaswim.inc"
#include "anapatt.inc"
#include "bcs.inc"
#include "wcs.inc"
#include "clas_offsets.inc"
c_end_inc
c
c_begin_var
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: trk_calc_wire_mod.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/03/02 18:23:04 $')
      PARAMETER (CAUTHO = '$Author: claschef $')
      DATA CRCSID/   
     1'$Id: trk_calc_wire_mod.F,v 1.3 1999/03/02 18:23:04 claschef Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TRK_CALC_WIRE')
      PARAMETER (CRAUTH='James Mueller')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER nlay, ndc, nsuper
      PARAMETER (nlay = 6, nsuper = 6, ndc = nlay*nsuper)
      integer irun,ievnt,irec
      INTEGER i,il,is,ilmin,la,iwire,ipl
      REAL cell_size,doca,trk_point(3),trk_dir(3),svin(9),dvin(9)
      integer trk_rehit,get_xxtr,ind_xxtr
      real trk_redoca
      real mcin(9)

      real momentum, px, py, pz
      real theta, theta_norm
      integer sda_sec

c  input/output variables:
c  -----------------------
c
      INTEGER ilnk,isec
      REAL beta
      INTEGER trk_wire(ndc)
      REAL trk_doca(ndc),trk_sigma(ndc)
c
c_end_var
c  for accessing the Bos banks
      integer MAX_WIRE
      parameter(MAX_WIRE = 192)

      INTEGER ind_dcgm(6),ncol_dcgm,LayInd
      INTEGER ind_dcgw(6),ncol_dcgw,WireInd,gwfirst,gwmid,gwlast
      integer nami_dcgw, nami_dcgm, ind, check
      data nami_dcgm, nami_dcgw/0,0/

c  FUNCTIONS for the indicies of DCGM and DCGW BosBanks
      integer MAMIND, GWind, GMind, sector, layer, baseind, wire
      REAL MidX, MidY, MidZ, WireHalfL

      GWind(sector,layer,wire) = ind_dcgw(sector) + 
     &     ((layer-1)*MAX_WIRE+wire-1)*ncol_dcgw
      GMind(sector,layer) = ind_dcgm(sector) + (layer-1)*ncol_dcgm
      MidX(baseind) = RJW(baseind+DCGW_x_mid)
      MidY(baseind) = RJW(baseind+DCGW_y_mid)
      MidZ(baseind) = RJW(baseind+DCGW_z_mid)
      WireHalfL(baseind) = RJW(baseind+DCGW_w_len)
c
c  executable code for routine TRK_CALC_WIRE:
c----6----------------------------------------------------------------72
c
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
      
    
C  Get a pointer to the appropriate bank and make it if necessary

      ind_xxtr = get_xxtr(ilnk)

C  Fill svin with initial track values and call sda_swim to propagate it
C  through the detector.
      IF(mcin(9).eq.0) then  
         do i=1,36 
            trk_wire(i)=-1
            trk_doca(i)=0
         enddo
         RETURN
      ELSE
        dvin(1)=mcin(1)
        dvin(2)=mcin(2)
        dvin(3)=mcin(3)
        dvin(4)=mcin(4)
        dvin(5)=mcin(5)
        dvin(6)=mcin(6)
        dvin(7)=mcin(7)
        dvin(8)=mcin(8)
        dvin(9)=mcin(9)
      ENDIF
      call sda_rotds(mcin,svin,sda_sec)

C  Fill the initial entry in trkp
      
      DO i=1,9
         trkp(i,1,ilnk) = svin(i)
      ENDDO
      trkp(10,1,ilnk) = 0.0
      trkp(11,1,ilnk) = IW(ind_xxtr+9)
      trkp(12,1,ilnk) = RW(ind_xxtr+8)

C  Swim the track through the rest of the detector
      
      call sda_swim(isec,1,ndc_max,1,svin)

C  Now use the results from sda_swim to fill the dc planes of the trkp array
      DO ipl=ndc_min,ndc_max
         DO i=1,6
            trkp(i,ipl,ilnk) = sda_trp(i,ipl)
         ENDDO
         trkp(7,ipl,ilnk)  = momentum
         trkp(8,ipl,ilnk)  = beta
         trkp(9,ipl,ilnk)  = RW(ind_xxtr+7)
         trkp(10,ipl,ilnk) = sda_trp(7,ipl)
         trkp(11,ipl,ilnk) = IW(ind_xxtr+9)
         trkp(12,ipl,ilnk) = RW(ind_xxtr+8)
      ENDDO

C  OK, now we have to find the cell this track passes through and fill the
C  relevant quantities in the trk array

      DO is = 1,nsuper
         ilmin = (is-1)*nlay
         DO la = 1,nlay
            il= la + ilmin
            ipl = il + nst_max
            layind = GMind(isec,il)            
            trk_point(1) = trkp(2,ipl,ilnk)
            trk_point(2) = trkp(3,ipl,ilnk)
            trk_point(3) = trkp(1,ipl,ilnk)
            trk_dir(1) = trkp(5,ipl,ilnk)
            trk_dir(2) = trkp(6,ipl,ilnk)
            trk_dir(3) = trkp(4,ipl,ilnk)
            trk_wire(il) = trk_rehit(trk_point,il)
            if (trk_wire(il) .ne. 0) then
               trk_doca(il) = trk_redoca(trk_point,trk_dir,il,trk_wire(il))
               IF (trk_wire(il).GT.JW(layind+DCGM_max_cylw)) THEN ! planar section
                  cell_size = RJW(layind+DCGM_p_sep)
               ELSE
                  cell_size = RJW(layind+DCGM_cell_size)*2
               ENDIF
               trk_sigma(il) = cell_size/2/1.732
               
               trk(5,il,ilnk) = trk_sigma(il)
               trk(6,il,ilnk) = beta
               trk(7,il,ilnk) = trk_doca(il)
               trk(8,il,ilnk) = sda_trp(7,ipl) ! Tlen
               
c     in figuring the length of wire the signal propagates along,
c     we must use the fact that Region 1 has the STB's on the -y (SCS) side,
c     while Regions 2 and 3 have the STB's on the +y side.
               
               if (il.le.12) then ! Region 1
                  trk(9,il,ilnk) = WireHalfL(GWind(isec,il,trk_wire(il))) + 
     &                 sda_trp(3,ipl)
               else             ! Regions 2 and 3
                  trk(9,il,ilnk) = WireHalfL(GWind(isec,il,trk_wire(il))) - 
     &                 sda_trp(3,ipl)
               endif
               
c     Calculate the angle of the normal to the layer at the wire
               if(sda_trp(5,ipl).eq.0.0 .and. sda_trp(4,ipl).eq.0.0) then
                  theta=0.0
               else
                  theta = ATAN2(sda_trp(5,ipl),sda_trp(4,ipl))
               endif
c     
               IF(trk_wire(il).GT.JW(layind+DCGM_max_cylw)) THEN !Plane
                  theta_norm = ATAN2(RJW(layind+DCGM_x_norm),
     &                 RJW(layind+DCGM_z_norm))
               ELSE             !Cylinder
                  theta_norm = RJW(layind+DCGM_theta_start)+(trk_wire(il)-1)*
     &                 RJW(layind+DCGM_d_theta)
               ENDIF
               trk(10,il,ilnk) = theta - theta_norm ! alfa
               
               trk(11,il,ilnk) = RW(ind_xxtr+9)
            ENDIF
         Enddo
      Enddo

      RETURN
      END
c
c------------------------------------------------------------------------------





