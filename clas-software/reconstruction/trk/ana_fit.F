
      SUBROUTINE ana_fit(ntracks)
C----------------------------------------------------------------------
*_begin_doc
*  RCS ID string
*  $Id: ana_fit.F,v 1.26 2005/11/15 23:10:54 fklein Exp $
*
*  Documentation for subroutine ANA_FIT
C-
C-   Purpose and Methods : Final fitting with corrected drift times.
C-
C-   Calls              : ana_trfit
C-
C-   Created   JUNE-15-1992  Bogdan Niczyporuk
C-   Modified  JUNE-16-1992  Billy Leung
C-
C-   Modified  April-21-1997 G.P.Gilfoyle
C-                           Added calls to TRK_FILL_TBLA and
C-                           TRK_FILL_TBTR that fill time-based
C-                           tracking results banks.
C-             Nov.1997      F.Klein
C-                           remove obsolete common block 'sdakeys'
C-                           changes to 6-fold geometry
C-
C-   Modified  Jan-7-1998    R.J.Feuerbach
C-                           Changed to get DC geometry info from DCGM
C-                           and DCGW bosbanks.
C-                           Also to correct for STB's of Region1 on the -y
C-                           side while Regions 2 and 3 have them on the +y
C-                           side.
C-   Called by TRK_EVNT_TBT
C-
C----------------------------------------------------------------------
*_begin_doc

      IMPLICIT NONE
      SAVE
*
*  I/O variables
*  -------------
      INTEGER  ntracks
*
*  Include Files
*  -------------
#include "anapatt.inc"
#include "trktcl_var.inc"
#include "sdaswim.inc"
#include "clas_offsets.inc"
#include "bcs.inc"
#include "wcs.inc"
#include "dc_tcl.inc"
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: ana_fit.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.26 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2005/11/15 23:10:54 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: ana_fit.F,v 1.26 2005/11/15 23:10:54 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='ANA_FIT')
      PARAMETER (CRAUTH='Franz Klein')
C
C Local variables
      LOGICAL BTEST
      INTEGER nsuper, nlay, ndc
      PARAMETER (nsuper = 6, nlay = 6, ndc = 36)
      INTEGER MAX_WIRE
      PARAMETER (MAX_WIRE = 192)

C
      INTEGER i, j, il, it, ip, ipl, iwr, ilnk, isec, Level
      INTEGER ilmin, is, la, nd
      LOGICAL lsum, lsum2, lshare
c variables used for BOS or setting up bos info
      INTEGER ind_dcgw(6), ncol_dcgw, ind_dcgm(6), ncol_dcgm
      integer nami_dcgm, nami_dcgw, ind, check, indFBPM
      data nami_dcgm,nami_dcgw,indFBPM/0,0,0/
      INTEGER ipt_gm, ipt_gw
      integer*4 i32(8)
      integer*2 i16(16)
      equivalence (i32(1),i16(1))

      integer counting

      INTEGER k1, k2, isum, k, vxchanged, ipmin,id,ishare
      REAL my_chi2(MAX_TRACKS), pln1dist(6), pln1pos(3,6)
      INTEGER ntrk_per_sect(6), trklist_per_sect(MAX_TRACKS,6)
      INTEGER trkno_in_sect(MAX_TRACKS), ntrk
      REAL    svin(9), chifit, phi, phinorm, temp(3), svin1(7)

      REAL  adx,ady,adx0,ady0
CC
CC LRAMB test mods
CC
      integer lrflag,ifirst
      real lrdigi(4,36),tempdigi(4,36),chibest_track,temp_svin(9)
      real piby3, sinsec(6),cossec(6)
      data lrflag,ifirst/1,1/
CC
      
      integer GWind, GMind, MAMIND, sector, layer, wire, baseind
      real WireHalfL

c  STATEMENT Functions to get indices for DCGM and DCGW wanted row
      GWind(sector,layer,wire) = ind_dcgw(sector) + 
     &     ((layer-1)*MAX_WIRE+wire-1)*ncol_dcgw
      GMind(sector,layer) = ind_dcgm(sector) + (layer-1)*ncol_dcgm
      WireHalfL(baseind) = RJW(baseind+DCGW_w_len)
c ------------------------------------

C Save the initial parameters and replace them with the new ones
      IF(ifirst.EQ.1) THEN
        piby3 = ACOS(-1.)/3.
C Initialize array for rotation to sectors
        DO isec=1,6
          sinsec(isec) = SIN((isec-1) * piby3)
          cossec(isec) = COS((isec-1) * piby3)
        ENDDO
        ifirst = 0
      ENDIF
         
      Level = TRK_LEVEL_tbt
      ntracks = 0
      counting = 0
c
c  variable 'vertex' plane ?
c
      vxchanged=0
      ipmin=1                 !default: fit starts with tracking plane 1

      if(ipar_trk_vxconstr.EQ.4 .OR. ipar_trk_fitregion.GT.0
     &       .OR. ipar_poltarget_current.NE.0) then
         ipmin = 1 +3
         if(BTEST(ipar_trk_fitregion,2)) ipmin=24 +3
         if(BTEST(ipar_trk_fitregion,1)) ipmin=12 +3
         if(BTEST(ipar_trk_fitregion,0)) ipmin= 1 +3
         if(BTEST(ipar_trk_fitregion,3)) ipmin= 7 +3
         if(ipar_trk_vxconstr.EQ.4) then
           do isec=1,6
             do j=1,3
               pln1pos(j,isec)=sda_pln(j,1,isec)
             enddo
             pln1dist(isec) =sda_pln(8,1,isec)
             sda_pln(8,1,isec)=0.001
           enddo
           if(ipar_poltarget_current.NE.0) then
             vxchanged=2
             temp(1)=0.
             temp(2)=0.
             temp(3)=0.
             ind=0
             if(indFBPM.EQ.0) indFBPM=MAMIND(iw,'FBPM')
             if(indFBPM.GT.0) ind=IW(indFBPM)
             if(ind.GT.0) then
               nd=iw(ind)
               do j=1,nd
                  i32(j)=IW(ind+j)
               enddo
c  has to be checked!!
c  mean_x=2612 (2160-3063), mean_y=2414 (1810-3022)
               adx0=2612
               ady0=2414
               adx =adx0
               ady =ady0
               do j=1,nd*2,3
                  id=i16(j)
                  if(id.EQ.7) then
                     adx=i16(j+2)
                  elseif(id.EQ.8) then 
                     ady=i16(j+2)
                  endif
               enddo
c       xval=offset(=0.4) +3.33*delx(for norm=1)  *1/Ebeam         
c       yval=offset(=-.15)+2.05*dely(for norm=0.8)*1/Ebeam         
c  example:
               temp(1) =dpar_BeamOff(1) +3.33*(adx-2612.)/902. *1. /4.045
               temp(2) =dpar_BeamOff(2) +2.05*(ady-2414.)/1212.*0.8/4.045
             endif
           else
             vxchanged=1
             temp(1)=dpar_BeamOff(1)
             temp(2)=dpar_BeamOff(2)
             temp(3)=0.
           endif
           do isec=1,6
             sda_pln(1,1,isec) = temp(3)
             sda_pln(2,1,isec) = temp(1)*cossec(isec) + temp(2)*sinsec(isec)
             sda_pln(3,1,isec) =-temp(1)*sinsec(isec) + temp(2)*cossec(isec)
           enddo
         endif
      endif

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


C Loop over all track candidates prepared by Pattern Recognition routines
      IF(ntr_link.EQ.0) GO TO 999

      call vzero(ntrk_per_sect,6)
      call vzero(trklist_per_sect,6*MAX_TRACKS)
      call vzero(trkno_in_sect,MAX_TRACKS)

      DO 100 ilnk = 1,ntr_link
        my_chi2(ilnk)=10000.
C Check if fit is successful
        IF(itr_level(ilnk).LT.TRK_LEVEL_lramb) GO TO 100
        isec = itr_sect(ilnk)
        counting = counting + 1
CC
CC  Mods for lramb test
CC

        chibest_track = dpar_trk_tbtfit_chi2 + 1.0
c
C Loop (with goto's yuch) over all combinations of L-R ambiguities that
C gave decent sengle superlayer fits.
C Try the global fit and save the best combination for later use.
c
 28     CONTINUE
        DO i = 1,9
          svin(i) = trkp(i,ipmin,ilnk)
        ENDDO
        if(ipar_trk_lramb.LT.0) goto 25

        call sort_fits(ilnk,lrdigi,lrflag)
        do il=1,ndc
           is=(il+5)/6
           trk(1,il,ilnk) = lrdigi(1,il)
           trk(3,il,ilnk) = lrdigi(2,il)
           trk(4,il,ilnk) = lrdigi(3,il)
c           trk(5,il,ilnk) = sqrt(lrdigi(4,il)**2+dc_sigma_doca(is)**2)
           trk(5,il,ilnk) = lrdigi(4,il)
           if(trk(1,il,ilnk).le.0)then
              trk(12,il,ilnk) = 1.0
           else
              trk(12,il,ilnk) = 0.0
           endif
        enddo
        IF(ipar_trk_maxiter.GT.0) CALL ana_trfit(isec,ilnk,Level,ipmin,svin,chifit)
        if(chifit.le.chibest_track)then
           chibest_track = chifit
           do il=1,ndc
              do i=1,4
                 tempdigi(i,il) = lrdigi(i,il)
              enddo
           enddo
           DO i = 1,9
              temp_svin(i) = svin(i)
           ENDDO
        endif
        if(lrflag.eq.1)goto 29
        go to 28
 29     continue
        if (chibest_track .gt. dpar_trk_tbtfit_chi2) go to 100
C
C set everything to the values for the best global fit, and then let
c the rest of the routine proceed as if we weren't here. (What a mess.)
        DO i = 1,9
          svin(i) = temp_svin(i)
        ENDDO
        do il=1,36
           trk(1,il,ilnk) = tempdigi(1,il)
           trk(3,il,ilnk) = tempdigi(2,il)
           trk(4,il,ilnk) = tempdigi(3,il)
c           trk(5,il,ilnk) = sqrt(tempdigi(4,il)**2+dc_sigma_doca(is)**2)
           trk(5,il,ilnk) = tempdigi(4,il)
           if(trk(1,il,ilnk).le.0)then
              trk(12,il,ilnk) = 1.0
           else
              trk(12,il,ilnk) = 0.0
           endif
        enddo
CC
CC End of LRamb test
CC
 25     continue
C Fit using the reconstructed drift times
        IF(ipar_trk_maxiter.GT.0) CALL ana_trfit(isec,ilnk,Level,ipmin,svin,chifit)

C Check if fit was successful
        IF(chifit.GT.dpar_trk_tbtfit_chi2) GO TO 100
        call ana_statb(8,isec,trkp(9,1,ilnk))
        ntracks = ntracks + 1
c
c  if fit didn't start with 'vertex' plane: swim back to vertex
c
        if(ipmin.NE.1) then
           do i=1,6
              svin1(i)=svin(i)
           enddo
           svin1(7)=sda_trp(7,ipmin)
           call trk_p2p(isec,ipmin,1,svin,ilnk)
        endif
                  
C Store the results of FF fit, which was not rejected from the above cuts
        itr_level(ilnk) = Level
        my_chi2(ilnk)=chifit
        ntrk_per_sect(isec)=ntrk_per_sect(isec)+1
        trklist_per_sect(ntrk_per_sect(isec),isec)=ilnk

        DO il = 1,ndc
          ipl=il+nst_max			!detector plane in 'sda_swim'
          iwr = ABS(trk(1,il,ilnk))
c fr 05-23-97: wire number should be non 0 fr 05-23-97
          if (iwr .ne. 0) then
             ipt_gw = GWind(isec,il,iwr)           !wire index in DCGW
             ipt_gm = GMind(isec,il)              !layer index in DCGM
             trk(8,il,ilnk)  = sda_trp(7,ipl) ! Tlen
c in figuring the length of wire the signal propagates along,
c we must use the fact that Region 1 has the STB's on the -y (SCS) side,
c while Regions 2 and 3 have the STB's on the +y side.

             if (il.le.12) then             ! Region 1
                trk(9,il,ilnk) = WireHalfL(ipt_gw) + sda_trp(3,ipl)
             else                           ! Regions 2 and 3
                trk(9,il,ilnk) = WireHalfL(ipt_gw) - sda_trp(3,ipl)
             endif

            phi = ATAN2(sda_trp(5,ipl),sda_trp(4,ipl))
            IF(iwr.GT.JW(ipt_gm + DCGM_max_cylw)) THEN     ! phi (wrt plane)
               phinorm = ATAN2(RJW(ipt_gm+DCGM_x_norm),RJW(ipt_gm+DCGM_z_norm))
            ELSE                                          ! phi (wrt wire norm)
               phinorm = RJW(ipt_gm+DCGM_theta_start) +
     &              (iwr-1)*RJW(ipt_gm+DCGM_d_theta)
            ENDIF
            trk(10,il,ilnk) = phi - phinorm
            trk(11,il,ilnk)  = 10*isec + Level ! Level & Sect
          endif
        ENDDO
        DO i = 1,9
          trkp(i,1,ilnk) = svin(i)
        ENDDO
        trkp(10,1,ilnk) = 0.                  ! s (track length)
        trkp(11,1,ilnk) = 10*isec + Level     ! Analysis Level & Sector No
        trkp(12,1,ilnk) = chifit              ! Reduced Chi-Square
        DO ip = 2,npln
          DO i = 1,6
            trkp(i,ip,ilnk) = sda_trp(i,ip)
          ENDDO
          do i=8,10
            trkp(i+5,ip,ilnk)=sda_trp(i,ip)
          enddo
          trkp(7,ip,ilnk)  = svin(7)           ! p 
          trkp(8,ip,ilnk)  = svin(8)           ! m 
          trkp(9,ip,ilnk)  = svin(9)           ! Q 
          trkp(10,ip,ilnk) = sda_trp(7,ip)     ! s (track length)
          trkp(11,ip,ilnk) = 10*isec + Level   ! Analysis Level & Sector No
          trkp(12,ip,ilnk) = chifit            ! Reduced Chi-Square
        ENDDO

        if(ipmin.NE.1) then
           do i=1,6
              trkp(i,ipmin,ilnk)=svin1(i)
           enddo
           trkp(10,ipmin,ilnk)=sda_trp(7,ipmin)-svin1(7)
c  track length corrected for distance to intersect with layer 1
        endif

 100  CONTINUE                  ! End of loop over linked clusters
C  still 'pseudo-tracks' in Time-Based Tracking?
      do isec=1,6
	if(ntrk_per_sect(isec).GT.1) then
	  do i=1,ntrk_per_sect(isec)-1
	    k1=trklist_per_sect(i,isec)
	    do j=i+1,ntrk_per_sect(isec)
	      k2=trklist_per_sect(j,isec)
	      isum=0
              DO is=1,nsuper
                 ilmin=(is-1)*nlay
                 lsum = .FALSE.
                 lsum2 = .FALSE.
                 lshare = .FALSE.
                 ishare = 0
                 do la=1,nlay
                    il = la + ilmin
                    if(trk(1,il,k1).GT.0)then
                       lsum = .TRUE.
                       if(trk(1,il,k2).GT.0)then
                          lsum2 = .TRUE.
                          if(abs(trk(1,il,k1)-trk(1,il,k2)).GT.1.0)then
                             ishare = ishare+1
                          else
                             lshare = .TRUE.
                          endif
                       endif
                    endif
                    if(trk(1,il,k2).GT.0)lsum2 = .TRUE.
                 enddo         
                 lshare = (ishare.lt.4).and.lshare
                 if(lshare .OR. ((.NOT.lsum) .AND. (.NOT.lsum2)))isum = isum +1
              ENDDO
              
	      if(isum.GE.3) then   			!or: .EQ.6
	        if(my_chi2(k1).LT.my_chi2(k2))then
	          it=k2
	        else
	          it=k1
                endif
                itr_level(it)=0
                ntracks=ntracks-1
	        if(ipar_trk_print(5).GT.1) write(6,*)' truncated track#,chi2:',it,my_chi2(k1),my_chi2(k2)
	      endif
	    enddo
	  enddo
	endif
      enddo

C
C Count number of good tracks remaining for return.
C Let's make this simple.  All the more complicated attempts have been wrong.
C
      ntracks = 0
      do k=1,ntr_link
         if(itr_level(k).ge.Level) then
          ntracks = ntracks + 1
          isec = itr_sect(k)
          call ana_statb(9,isec,trkp(9,1,k))
         endif
      enddo    

C Fill the TBT bos banks.

      if(ntracks.GT.0) then
        do isec=1,6
          if(ntrk_per_sect(isec).GT.0) then
            ntrk = 0
            do i=1,ntrk_per_sect(isec)
              ilnk=trklist_per_sect(i,isec)
              if(itr_level(ilnk).GE.Level) then
                ntrk = ntrk +1
                trkno_in_sect(ilnk)=ntrk
                if(ipar_trk_print(5).EQ.1) then
                  CALL sda_ptrk(ilnk,isec)
                endif
              endif
            enddo
            call trk_fill_banks_per_sect(TRK_LEVEL_tbt,ntr_link,isec)
          endif
        enddo
        call trk_fill_banks(TRK_LEVEL_tbt,ntr_link,trkno_in_sect)
      endif
 999  CONTINUE

      if(vxchanged.NE.0) then 
         do isec=1,6
            do j=1,3
               sda_pln(j,1,isec)=pln1pos(j,isec)
            enddo
            sda_pln(8,1,isec)=pln1dist(isec)
         enddo
      endif
      RETURN
      END
