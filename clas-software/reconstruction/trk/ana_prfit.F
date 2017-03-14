C======================================================================
      SUBROUTINE ana_prfit(ntracks)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Performs track fitting using the stored wire
C-                         positions and momentum from the pattern
C-                         recognition templates.
C-
C-   Calls              : ana_trfit, ana_statb, sda_ptrk, trk_fill_trk, trk_fill_hbtr
C-
C-   Created   JUN-10-1992  Bogdan Niczyporuk
C-   Modified  JUN-11-1992  Billy Leung
C-   Modified  JAN-16-1996  Billy Leung
C-   Modified  Nov-XX-1996  Francois Roudot
C-                          Now fill the Hit Based Tracking Bos Banks : HBTR, TRKS, TRKL, and DTRK
C-                          I add also a new variable ok_hbf_trk(10) to fill the bos banks with the
C                           good reconstructed tracks
C-   Modified  Mar-24-1997  C.A.Meyer
C-                          Keep track of the row number of the hit in the
C-                          DC0 banks as trk(13,....). 
C-   Aug.1997  F.Klein  In case that less than 6 superlayers are required
C-			for the linking only take that track with the best
C-			chi^2  (wrt. SEB decisions at this stage of tracking,
C-			       might be done at the end of time based tracking)
C-			[comment: unfortunately the DC planes are the same for
C-			          all sectors - to be changed! ]
C-   Nov.1997  F.Klein  remove obsolete common block 'sdakeys.inc'
C-                      change to 6-fold geometry
C-   Modified Jan-7-1998    R.J.Feuerbach
C-                          Correct to use the DCGM and DCGW bosbanks.
C-                          Also to correct for STB's of Region1 on the -y
C-                          side while Regions 2 and 3 have them on the +y
C-                          side.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
*  I/O variables
*  -------------
      INTEGER ntracks
*
*  Include Files
*  -------------
#include "sdaswim.inc"
#include "anapatt.inc"
#include "dc_cal_tdly.inc"
#include "trktcl_var.inc"
#include "bcs.inc"
#include "wcs.inc"
#include "clas_offsets.inc"
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: ana_prfit.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.43 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2005/11/15 23:10:54 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: ana_prfit.F,v 1.43 2005/11/15 23:10:54 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='ANA_PRFIT')
      PARAMETER (CRAUTH='Franz Klein')

C Local variables
      LOGICAL BTEST
      INTEGER ifirst/1/
      real cossec(6), sinsec(6)
      real piby3
      INTEGER nlay, ndc, isec, Level
      PARAMETER (nlay = 6, ndc = 36)
c      integer MAX_WIRE
c      PARAMETER (MAX_WIRE = 192)      ! defined in dc_cal_tdly.inc
      INTEGER nsuper /6/
C
      INTEGER i,il,is,isg,ilnk,ilmin,la,iwire,ip,ipl,icl
      INTEGER iw9,iw15,iw27,iw3,iw21,iw33        
      INTEGER k, k1, k2, j, it, isum1,isum2,ishare, wire
      REAL cell_size,rwire,dwire,sighb
C
      REAL dx21, dx32, dy21, dy32, defl
      REAL pmean, dtime, pln1dist(6)
      REAL svin(9), chifit, sphi, phi, svin1(7)
      REAL my_chi2(MAX_TRACKS),wire_temp(36)
      REAL phinorm
      REAL small_step
      INTEGER ntrk_per_sect(6), trklist_per_sect(MAX_TRACKS,6)
      INTEGER trkno_in_sect(MAX_TRACKS), ntrk, il1, iw1
      INTEGER ok_flag(MAX_TRACKS), ipmin
      BYTE  bpatt

c  for accessing the Bos banks
      INTEGER MLINK,ind_dhcl
      INTEGER ind_dcgm(6),ncol_dcgm,LayInd, W1Ind
      INTEGER ind_dcgw(6),ncol_dcgw,WireInd,gwfirst,gwmid,gwlast
      integer nami_dcgw, nami_dcgm, ind, check
      data nami_dcgm, nami_dcgw/0,0/

c  FUNCTIONS for the indicies of DCGM and DCGW BosBanks
      integer MAMIND, GWind, GMind, sector, layer, baseind
      REAL MidX, MidY, MidZ, WireHalfL

      GWind(sector,layer,wire) = ind_dcgw(sector) + 
     &     ((layer-1)*MAX_WIRE+wire-1)*ncol_dcgw
      GMind(sector,layer) = ind_dcgm(sector) + (layer-1)*ncol_dcgm
      MidX(baseind) = RJW(baseind+DCGW_x_mid)
      MidY(baseind) = RJW(baseind+DCGW_y_mid)
      MidZ(baseind) = RJW(baseind+DCGW_z_mid)
      WireHalfL(baseind) = RJW(baseind+DCGW_w_len)
      
c
      if (ifirst.eq.1) then
         piby3 = acos(-1.)/3.
         do i=1,6
            cossec(i) = cos((i-1)*piby3)
            sinsec(i) = sin((i-1)*piby3)
         enddo
         ifirst = 0
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


C Loop over linked track candidates

      ntracks = 0
      IF(ntr_link.EQ.0) GO TO 999

      Level = TRK_LEVEL_hbt
C Fit to wire positon only (drift times not used)
      ipmin=1                   !default: fit starts with tracking plane 1

      if(ipar_trk_fitregion.GT.0) then
         ipmin = 1 +3
         if(BTEST(ipar_trk_fitregion,2)) ipmin=24 +3
         if(BTEST(ipar_trk_fitregion,1)) ipmin=12 +3
         if(BTEST(ipar_trk_fitregion,0)) ipmin= 1 +3
         if(BTEST(ipar_trk_fitregion,3)) ipmin= 7 +3
         do isec=1,6
            pln1dist(isec)=sda_pln(8,1,isec)
            sda_pln(8,1,isec)= 0.001
         enddo
      endif
      call vzero(ntrk_per_sect,6)
      call vzero(trklist_per_sect,6*MAX_TRACKS)
      call vzero(trkno_in_sect,MAX_TRACKS)

      DO 100 ilnk = 1,ntr_link
        my_chi2(ilnk)=10000.
        IF(itr_level(ilnk).LT.TRK_LEVEL_linking) GO TO 100
        isec = itr_sect(ilnk)


C Store wire numbers for fitting without drift times
        DO is = 1,nsuper
          ilmin = (is-1)*nlay
          isg = lnk_segm(is,ilnk)
          icl = lnk_clust(is,ilnk)
          if (icl.le.0) then   !is this ever true??
             DO la = 1,nlay
                il= la + ilmin
c                layind = GMind(isec,il)
c                IF ((-icl).GT.JW(layind+DCGM_max_cylw)) THEN ! planar section
c                   cell_size = RJW(layind+DCGM_p_sep)/2.
c                ELSE
c                   cell_size = RJW(layind+DCGM_cell_size)
c                ENDIF
                trk(1,il,ilnk)  = icl                   ! Wire number
                trk(2,il,ilnk)  = 0                     ! Raw TDC [ns]
                trk(3,il,ilnk)  = 0.                    ! Tdrift [ns]
                trk(4,il,ilnk)  = 0.                    ! Dmeas. [cm]
                trk(5,il,ilnk)  = 100.                  ! SIGm [cm]
                trk(6,il,ilnk)  = 0.                    ! Velocity [c]
                trk(11,il,ilnk) = 10*isec + 1           ! Analysis Level & Sect
                trk(12,il,ilnk) = 10.                   ! Status notOK
                trk_pt(il,ilnk) = 0
                wire_temp(il) = 0
             Enddo
          ELSE      
             k = 1
             DO la = 1,nlay
                il = la + ilmin
                layind = GMind(isec,il)
                rwire = 0.0
                dwire = 0.0
                wire = segm(k,isg,is,isec)
                if(wire.NE.0) then
                   dtime = t0(iabs(wire),il,isec)-
     &                  float(segm(k+1,isg,is,isec))/DC_Tslope
                else
                   dtime = 9000.
                endif
                IF (wire.GT.JW(layind+DCGM_max_cylw)) THEN ! planar section
                   cell_size = RJW(layind+DCGM_p_sep)/2.
                ELSE
                   cell_size = RJW(layind+DCGM_cell_size)
                ENDIF
C
C     De-weight the hit if this is part of a multihit cluster.
C     This is to protect against the linking (which is not a fine grid)
C     picking the wrong segment in a high occupancy event.
C
                if(clwmax(la,icl,is,isec,2).ge.clwmin(la,icl,is,isec,2))then
                  rwire=(clwmin(la,icl,is,isec,1)+clwmax(la,icl,is,isec,1))/2.0
                  dwire = 1.0+clwmax(la,icl,is,isec,1)-clwmin(la,icl,is,isec,1)
                  sighb = dwire*cell_size/1.732
                else
                  sighb = cell_size/1.732
                endif
C
                trk(1,il,ilnk)  = wire                     ! Wire number
                trk(2,il,ilnk)  = segm(k+1,isg,is,isec)    ! Raw TDC [ns]
                trk(3,il,ilnk)  = dtime                    ! Tdrift [ns]
                trk(4,il,ilnk)  = 0.                       ! Dmeas. [cm]
                trk(5,il,ilnk)  = sighb                    ! SIGm [cm]
                trk(6,il,ilnk)  = 0.                       ! Velocity [c]
                trk(11,il,ilnk) = 10*isec + 1              ! Analysis Level & Sect
                trk(12,il,ilnk) = 0.                       ! Status OK
*
*--c.a.meyer march 1997: keep track of row number of DC0 hit.
*
                IF(trk(1,il,ilnk).LE.0.) then
                   trk(12,il,ilnk) = 9.                    ! Status notOK
                   trk_pt(il,ilnk) = 0
                   if(trk(1,il,ilnk).EQ.0) trk(1,il,ilnk)=-1 !DC1 not checked
                Else
                   trk_pt(il,ilnk) = segm_rp(((k+1)/2),isg,is,isec)
                Endif
*--
                wire_temp(il)=wire
                if(rwire.gt.0)then
                  wire = int(rwire)
                  trk(1,il,ilnk) = wire
                  trk(4,il,ilnk) = (rwire-wire)*cell_size
                  trk(12,il,ilnk) = 0.
                endif
                k = k + 2
             ENDDO
          ENDIF
        ENDDO
C
        trk(12,5,ilnk)=11
        trk(12,6,ilnk)=11
        svin(9)=0.                             ! charge not determined
        if(lnk_segm(2,ilnk).GT.0) then
          il = 9                               ! Wire # of track in layer 9 (SL2)
        elseif(lnk_segm(1,ilnk).GT.0) then
          il = 3                               ! Wire # of track in layer 3 (SL1)
        else
          il=15
        endif
        iwire = ABS(trk(1,il,ilnk))
        WireInd = GWind(isec,il,iwire)
C initialize the track parameters with a first guess
        if(ipmin.GE.4) then
           il1=ipmin-3
           iw1 = ABS(trk(1,il1,ilnk))
           W1Ind = GWind(isec,il1,iw1)
           sphi = ATAN2(MidX(WireInd)-MidX(W1Ind),MidZ(WireInd)-MidZ(W1Ind))
           svin(4)=COS(sphi)
           svin(5)=SIN(sphi)
           svin(6)=0.
           svin(1)=MidZ(W1Ind)-svin(4)
           svin(2)=MidX(W1Ind)-svin(5)
           svin(3)=0.
        else
           if(trkp(4,1,ilnk).ne.0) then
             do i=1,9
               svin(i)=trkp(i,1,ilnk)
             enddo
           else
             svin(1) = dpar_targetPos(3)
             svin(2) = 0.
             svin(3) = 0.
             sphi = ATAN2(MidX(WireInd),MidZ(WireInd)-svin(1))
cc        sphi = ATAN2(dc_wpmid(2,iwire,il,isec),dc_wpmid(1,iwire,il,isec)-svin(1))
             svin(4) = COS(sphi)
             svin(5) = SIN(sphi)
             svin(6) = 0.

c     c rotate the target into this SCS frame, and take step back to y=0
c     (SCS x=0)  plane.
             svin(1) = dpar_targetPos(3)
             svin(2) = dpar_targetPos(1)*cossec(isec)+dpar_targetPos(2)*sinsec(isec)
             svin(3) =-dpar_targetPos(1)*sinsec(isec)+dpar_targetPos(2)*cossec(isec)
             small_step = 0.
             if (svin(5).ne.0) small_step = svin(2)/svin(5)
             svin(1) = svin(1) - svin(4)*small_step
             svin(2) = svin(2) - svin(5)*small_step
             svin(3) = svin(3) - svin(6)*small_step
           endif 
        endif
C
        IF (ipar_trk_magtyp.EQ.5) THEN       ! B = 0 run, no mag field
           pmean = 1.
        ELSE
           pmean = trkp(7,1,ilnk)
        ENDIF
        IF(pmean.LT.0.02) THEN
          svin(7) = 0.02
        ELSE
          svin(7) = pmean
        ENDIF
        svin(8)=trkp(8,1,ilnk)/100.
C
C Guess charge of a particle
        IF(ipar_trk_magtyp.EQ.5) THEN        ! B = 0, no charge sensitivity
           svin(9) = -1.0       ! q=-1 so will be looked at by TBT
        ELSE
         if(svin(9).ne.0) then
           svin(9)=svin(9)*ipar_Bsign_torus
         else
           iw9  = 0
           iw15 = 0
           iw27 = 0
           if(trk(12,9,ilnk).ne.10)  iw9  = ABS(trk(1,9,ilnk))
           if(trk(12,15,ilnk).ne.10) iw15 = ABS(trk(1,15,ilnk))
           if(trk(12,27,ilnk).ne.10) iw27 = ABS(trk(1,27,ilnk))
           if(iw9.EQ.0 .OR. iw15.EQ.0 .OR. iw27.EQ.0) then
              iw3  = 0          !take stereo SL
              iw21 = 0
              iw33 = 0
              if(trk(12,3,ilnk).ne.10)  iw3  = ABS(trk(1,3,ilnk))
              if(trk(12,21,ilnk).ne.10) iw21 = ABS(trk(1,21,ilnk))
              if(trk(12,33,ilnk).ne.10) iw33 = ABS(trk(1,33,ilnk))
              if(iw3.EQ.0 .OR. iw21.EQ.0 .OR. iw33.EQ.0) then
                 goto 100
              endif
c     cleaned this up a little bit (feuerbac)
              gwfirst = GWind(isec,3,iw3)
              gwmid = GWind(isec,21,iw21)
              gwlast = GWind(isec,33,iw33)
           else
              gwfirst = GWind(isec,9,iw9)
              gwmid = GWind(isec,15,iw15)
              gwlast = GWind(isec,27,iw27)
           endif
           dx21 = MidZ(gwmid) - MidZ(gwfirst)
           dy21 = MidX(gwmid) - MidX(gwfirst)
           dx32 = MidZ(gwlast) - MidZ(gwmid)
           dy32 = MidX(gwlast) - MidX(gwmid)
           
           defl = ATAN2(dy32,dx32) - ATAN2(dy21,dx21)
           IF(defl.LT.0.) then
              svin(9) = -ipar_Bsign_torus
           ELSE
              svin(9) = ipar_Bsign_torus
           ENDIF
         endif
        ENDIF

        IF(ipar_trk_maxIter.GT.0) then
           CALL ana_trfit(isec,ilnk,Level,ipmin,svin,chifit)
        endif
        
C Chi-square (for Fit to wire position) cut

        IF(chifit.GT.dpar_trk_prfit_chi2 .OR. chifit.LE.0.) GOTO 100

        ntracks = ntracks + 1
        
c	do il=1,nsuper*nlay
c	  trk(1,il,ilnk) = wire_temp(il)
c         trk(4,il,ilnk) = 0.0
c       enddo

C Store the results of PR fit, which was not rejected from the above cuts
        itr_level(ilnk) = Level
        my_chi2(ilnk)=chifit
        ntrk_per_sect(isec)=ntrk_per_sect(isec)+1
        trklist_per_sect(ntrk_per_sect(isec),isec)=ilnk

c  if fit didn't start with 'vertex' plane: swim back to vertex
c
        if(ipmin.GE.4) then
           do i=1,6
              svin1(i)=svin(i)
           enddo
           svin1(7)=sda_trp(7,ipmin)
           call trk_p2p(isec,ipmin,1,svin,ilnk)
        endif

        DO il = 1,ndc
           ipl=il+nst_max       !detector plane in 'sda_swim'
           iwire = ABS(trk(1,il,ilnk))
           if (iwire .ne. 0) then
              trk( 8,il,ilnk) = sda_trp(7,ipl)             ! Tlen
c in figuring the length of wire the signal propagates along,
c we must use the fact that Region 1 has the STB's on the -y (SCS) side,
c while Regions 2 and 3 have the STB's on the +y side.

             if (il.le.12) then             ! Region 1
                trk(9,il,ilnk) = WireHalfL(GWind(isec,il,iwire)) + sda_trp(3,ipl)
             else                           ! Regions 2 and 3
                trk(9,il,ilnk) = WireHalfL(GWind(isec,il,iwire)) - sda_trp(3,ipl)
             endif

c lmq 6-24-07: add a test to prevent from crashing
              if(sda_trp(5,ipl).eq.0.0 .and. sda_trp(4,ipl).eq.0.0) then
                 phi=0.0
              else
                 phi = ATAN2(sda_trp(5,ipl),sda_trp(4,ipl))
              endif
           
              layind = GMind(isec,il)
c Calculate the angle of the normal to the layer at the wire
              IF(iwire.GT.JW(layind+DCGM_max_cylw)) THEN    !Plane
                 phinorm = ATAN2(RJW(layind+DCGM_x_norm),
     &                RJW(layind+DCGM_z_norm))
              ELSE                                       !Cylinder
                 phinorm = RJW(layind+DCGM_theta_start)+(iwire-1)*
     &                RJW(layind+DCGM_d_theta)
              ENDIF
              trk(10,il,ilnk) = phi - phinorm            ! alfa
              trk(11,il,ilnk) = 10*isec + Level          ! sec/L
           endif
        ENDDO

        DO i = 1,9
           trkp(i,1,ilnk) = svin(i)
        ENDDO
        trkp(10,1,ilnk) = 0.                   ! s (track length at target)
        trkp(11,1,ilnk) = 10*isec + Level      ! Analysis Level & Sector
        trkp(12,1,ilnk) = chifit               ! Reduced Chi-Square

        DO ip = 2,npln
          DO i = 1,6
            trkp(i,ip,ilnk) = sda_trp(i,ip)    ! x,y,z,Cx,Cy,Cz
          ENDDO
          do i=8,10
             trkp(i+5,ip,ilnk) = sda_trp(i,ip)
          enddo
          trkp( 7,ip,ilnk) = svin(7)           ! p 
          trkp( 8,ip,ilnk) = svin(8)           ! m 
          trkp( 9,ip,ilnk) = svin(9)           ! Q 
          trkp(10,ip,ilnk) = sda_trp(7,ip)     ! s (track length)
          trkp(11,ip,ilnk) = 10*isec + Level   ! Analysis Level & Sector
          trkp(12,ip,ilnk) = chifit            ! Reduced Chi-Square
        ENDDO
        if(ipmin.GE.4) then
           do i=1,6
              trkp(i,ipmin,ilnk)=svin1(i)
           enddo
           trkp(10,ipmin,ilnk)=sda_trp(7,ipmin)-svin1(7)  
! corrected Tlen for fit value w.r.t. intersection with layer 1
        endif

  100 CONTINUE                  ! End of loop over linked clusters

C  get rid of pseudo-Tracks (depending on Linking algorithm)

      do isec=1,6
	if(ntrk_per_sect(isec).GT.1) then
	  do i=1,ntrk_per_sect(isec)-1
	    k1=trklist_per_sect(i,isec)
	    do j=i+1,ntrk_per_sect(isec)
	      k2=trklist_per_sect(j,isec)
              if(ipar_trk_print(3).GT.1) then
	        write(6,'(A,12I4)') ' clust1&2',
     &		   (lnk_clust(ip,k1),ip=1,6),
     &		   (lnk_clust(ip,k2),ip=1,6)
	      endif
	      isum1=0
	      isum2=0
	      ishare=0
	      do ip=1,6
	        if(lnk_clust(ip,k1).GT.0) isum1 = isum1 + 1
                if(lnk_clust(ip,k2).GT.0) isum2 = isum2 + 1
                if(lnk_clust(ip,k1).EQ.lnk_clust(ip,k2)) ishare = ishare + 1
              enddo
              it=0
              if(ishare.gt.0) then
                 if((isum2-ishare.LT.3) .AND. 
     &              (my_chi2(k1)+(2.0*isum2/ishare).LT.my_chi2(k2)))then
                    it=k2
                 elseif((isum1-ishare.LT.3) .AND.
     &                  (my_chi2(k2)+(2.0*isum1/ishare).LT.my_chi2(k1)))then
                    it=k1
                 endif
                 if(it.GT.0) then
                    itr_level(it)=0
                    if(ipar_trk_print(3).GT.1) write(6,*)' truncated track#',it
                 endif
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
         endif
      enddo    

C Fill the HBT bos banks and mark 'DHCL' for found track
      do i=1,MAX_TRACKS
        ok_flag(i)=1
      enddo

      if(ntracks.gt.0) then
        do isec=1,6
          if(ntrk_per_sect(isec).GT.0) then
            ntrk = 0
            ind_dhcl = MLINK(iw,'DHCL',isec)
            call ana_statb(4,isec,0.)
            do i=1,ntrk_per_sect(isec)
              ilnk=trklist_per_sect(i,isec)
              if(itr_level(ilnk).GE.Level) then
                do is=1,6
                   icl = lnk_clust(is,ilnk)
                   if(icl.GT.0) then
                   bpatt=IW08(4*ind_dhcl+pt_clust(icl,is,isec)*16-14)
                   bpatt=bpatt+2**ntrk          !ntrk=(trk_in_sector-1)
                   IW08(4*ind_dhcl+pt_clust(icl,is,isec)*16-14)=bpatt
                   endif
                enddo
                ntrk = ntrk +1
                trkno_in_sect(ilnk)=ntrk
                ok_flag(ilnk)=0
                if(ipar_trk_print(3).EQ.1) then
                  CALL sda_ptrk(ilnk,isec)
                endif
              endif
            enddo
            call trk_fill_banks_per_sect(TRK_LEVEL_hbt,ntr_link,isec)
          endif
        enddo
        call trk_fill_banks(TRK_LEVEL_hbt,ntr_link,trkno_in_sect)
      endif

 999  CONTINUE
      if(ipmin.GT.1) then
         do isec=1,6
            sda_pln(8,1,isec)=pln1dist(isec)
         enddo
      endif

 33   format('Layer : ',i2,' Wire : ',i3,' doesn"t exist : need GSIM geometry modification') 

      RETURN
      END











