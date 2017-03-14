C======================================================================
      SUBROUTINE trk_dcam(ntr_fit_ok,ntracks)
*_begin_doc
*  RCS ID string
*  $Id: trk_dcam.F,v 1.35 2008/03/16 04:33:26 fklein Exp $
*
*  Documentation for subroutine TRK_DCAM
*
*  Purpose:  This routine calulate the left-right ambiguity and determine the 
*  -------   distance of closest aproach
*
*  Input Parameters: NONE
*  ----------------
*
*  Output Parameters: ntr_fit_ok = #tracks which passed the LeftRightAmb.Fit
*                                  in at least 1 Segment
*                     ntracks    = #tracks which passed the LeftRightAmb.Fit
*                                  in at least 'ipar_trk_minlramb' Segments
*  -----------------
*
*  Other routines:
*  ---------------
*
*  Note:          Replace bogdan's ana_dcam
*  -----
*
*  Author:         Francois H. Roudot 
*  ------          CEA Saclay
*
*  Creation Date:  25 April, 1997
*  -------------
*
*  Major revisions:  Sept.1997  F.Klein : allowing less than hits for
*  ---------------			  all layers in SuperLayer,
*					  using a distance depending sigma   
*  Minor revisions: Dec-2-97   F.Klein :  use TRKS Scint_Id for further
*                                         time correction.
*                   Dec-11-97  C.A.Meyer: MAMIND was implemented incorrectly.
*                                                    ^^^^^^^^^^^^^^^^^^^^^^^
*                                        was correct but time-consuming (fk),
*                   Dec-13-97  F.Klein:  the ind_pointer wasn't correct 
*_end_doc
*
      Implicit None
      Save
*
*
*_begin_inc
*  include files :
*  ---------------------
*
#include "bcs.inc"
#include "anapatt.inc"
#include "dc_tcl.inc"
#include "trktcl_var.inc"
*
*_end_inc
*
*_begin_var
*  input/output variables:
*  -----------------------
      INTEGER  ntr_fit_ok, ntracks
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_dcam.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.35 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/03/16 04:33:26 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: trk_dcam.F,v 1.35 2008/03/16 04:33:26 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_DCAM')
      PARAMETER (CRAUTH='Francois H. ROUDOT')
*
*  Local User defined variables:
*  -----------------------------
C-working array:  xdigi(1,la)  - hit wire# "iw"
C-                xdigi(2,la)  - Raw TDC [counts]
C-                xdigi(3,la)  - Drift Time Td [ns] to the hit wire#
C-                xdigi(4,la)  - Drift Distance (measured) Dm [cm]
C-                xdigi(5,la)  - Sigma of Drift Distance [cm]
*
      INTEGER nlay
      PARAMETER (nlay = 6)
      INTEGER i,k, il, ilmin,is,icl,isg,ilnk,isgc, la,lab,lamin
      INTEGER improve, nsgc, n_seg, isec,Level, itr_el, itr_new,isgb
      INTEGER ilnk2,isec2,nshare,nsum,nsum2,icl2
      INTEGER time_hits(NSGMX),time_hits_max,ihit
      REAL chibest, alfa, beta, slopD, slopDbest, chib, sigm, cut, t_start
      REAL rltrk,rl2trk,sl2,r2bys2,r1bys2,invsl2,weight_slope 
      REAL xdigi(5,nlay,NSGMX), xdigib(5,nlay)
      INTEGER mamind,ind,nd,itrk_tbt,j
      REAL start_tbt(MAX_TRACKS),beta_tbt(MAX_TRACKS)
      LOGICAL lsum,lsum2,lshare

      integer trk_wire(36),trk_doca(36),road_width,ntbt_seg(6),tbt_seg(12,30,6)
      INTEGER nsuper 
      INTEGER nLAYinSL(6)
c      INTEGER sc_id2plane(48), scid, scpl
      INTEGER ind_trks, ncol
      SAVE    ind_trks
      DATA    ind_trks /0/
      DATA    road_width /4/
      DATA nsuper /6/
      DATA nLAYinSL /4,6,6,6,6,6/
c      DATA sc_id2plane /23*1,11*2,8*3,6*4/           

      INTEGER*4 LRflag
      INTEGER min_hit_seg
      LRflag = 0
* SC_id in SC_plane (combined 3&4=>#3, 5&6=>#4)
*
*
      Level = TRK_LEVEL_lramb
      ntr_fit_ok = 0
      ntracks = 0
      itr_el  = 0
c      IF(ntr_link.EQ.0) GO TO 999 
      if(ipar_trk_lramb.GT.0) then
        call lramb_init
      endif
      call vzero(start_tbt,20)
      call vzero(beta_tbt,20)
      cut = dpar_trk_LRambfit_chi2 + 10. 

* Retrieve electron from TRKS       
*
c If no electron is present, the 1st track's start time is used for
c all tracks
c
      itr_el=1
c
      If(ind_trks.LE.0) ind_trks = MAMIND(iw,'TRKS')
      IF(IW(ind_trks).LE.0) goto 999
      ind = ind_trks +1
      do while( IW(ind-1).GT.0 )
        ind  = IW(ind-1)
        isec = IW(ind-2)
        ncol= IW(ind-5)
        nd  = IW(ind)-1
        if(nd.GT.0) then
          do j = 0, nd, ncol
            itrk_tbt = MOD(IW(ind+1+j),100)
            ntr_link = max(ntr_link,itrk_tbt)
            if(itrk_tbt.GT.0) then
              beta_tbt(itrk_tbt) = RW(ind+2+j)
              start_tbt(itrk_tbt)= RW(ind+3+j)
              itr_level(itrk_tbt)= IW(ind+1+j)/100
              itr_sect(itrk_tbt) = isec
              start_tbt(itrk_tbt)= start_tbt(itrk_tbt)+dc_trig_time
            endif
          enddo
        endif
      enddo

      do i = 1, ntr_link
        trkp(8,1,i)=beta_tbt(i)
        if(beta_tbt(i).GT.0.9999. AND. beta_tbt(i).LT.1.0001) itr_el = i
      enddo

        t_start=start_tbt(itr_el)

      DO 100 ilnk = 1,ntr_link                   ! loop over the linked tracks
        IF(itr_level(ilnk).LT.TRK_LEVEL_hbt) GO TO 100       ! Check if HBT fit was successful 
        isec = itr_sect(ilnk)
        IF(ipar_trk_print(4).GT.0) WRITE(6,101) Level,isec,ilnk,beta_tbt(ilnk)
        beta = beta_tbt(ilnk)             ! assign the velocity of the particle
cfk        IF(beta.le.0) GO TO 100           ! no good beta found. Bail for now
        if(beta.le.0)   beta=0.3
        if(beta.gt.1.2) beta=1.2

C find possible segments for this track
 
        if(ipar_trk_level.LT.trk_level_scl) then
           call trk_calc_wire(isec,ilnk,beta,trk_wire,trk_doca)
           if(mod(road_width,2).eq.0)then
              do i=1,36
                 if(trk_doca(i).LT.0)trk_wire(i)=trk_wire(i)-1 
              enddo
           endif
           call tbt_segm(isec,road_width,trk_wire,ntbt_seg,tbt_seg)
        endif
        
        call ana_statb(5,isec,trkp(9,1,ilnk))		  ! Count tracks with good beta
CCC   
C Resolve the L-R ambiguity
        n_seg   = 0
        itr_new = 1
        IF(ipar_trk_lramb*ipar_trk_print(4).LT.0) WRITE(6,102)
        DO 60 is = 1,nsuper
          chibest = 1.E+8
          isgb    = 0
          improve = 0
          ilmin = (is-1)*nlay
          alfa = trk(10,ilmin+3,ilnk)
C          icl = lnk_clust(is,ilnk)
C
C If we require < 6 seg per link, we must avoid this loop.
C NOTE. we must also have trk_minlramb<6 or we will lose those tracks in dcam
C
C          IF (icl.le.0) GOTO 60
C
C
C
          r2bys2 = 0.0
          r1bys2 = 0.0
          invsl2 = 0.0
          DO la = 1,nlay
            il = ilmin+la
            IF(is.NE.1 .OR. la.LE.4 )THEN
              rltrk = trk(8,il,ilnk)-trk(8,ilmin+3,ilnk)
              rl2trk = rltrk**2
              sl2 = trk(5,il,ilnk)**2
              IF(sl2.GT.0)THEN
                r2bys2 = r2bys2 + rl2trk/sl2
                r1bys2 = r1bys2 + rltrk/sl2
                invsl2 = invsl2 + 1.0/sl2
              ENDIF
            ENDIF
          ENDDO
          IF(invsl2.GT.0)THEN
            weight_slope = r2bys2 - r1bys2**2/invsl2
            if(ipar_trk_lramb.eq.-2) weight_slope=1
          ELSE
            weight_slope = 1.0
          ENDIF
C          
          nsgc = min(ntbt_seg(is),NSGMX)
          time_hits_max = 0

C Loop over segments in a linked cluster
          DO 40 isg = 1,nsgc
            time_hits(isg) = 0

C Store {iw,tdc} in "xdigi" array
            k = 1
            DO la = 1,nlay
              xdigi(1,la,isg) = tbt_seg(k,  isg,is)           ! wire id         
              xdigi(2,la,isg) = tbt_seg(k+1,isg,is)           ! raw tdc
              k = k + 2
	      xdigi(3,la,isg)=0.                              !drift time
	      xdigi(4,la,isg)=0.			       !drift distance
	      xdigi(5,la,isg)=0.			       !sigma(drift distance)
            ENDDO
C Get drift time and the derived drift distance (using a funcion or a straight line (cf dc tcl variables)
            CALL trk_dtime(is,ilnk,itr_el,t_start,beta, xdigi(1,1,isg))
C
C How many hits pass t-to-d conversion
C
            DO la = 1,nlay
              if(xdigi(1,la,isg).gt.0)time_hits(isg)=time_hits(isg)+1
            ENDDO
            time_hits_max = max(time_hits(isg),time_hits_max)

 40	  CONTINUE
c          print *, 'trk_dcam: ',isec,is,time_hits_max,lamin,chibest,nsgc

          min_hit_seg = ipar_trk_minhits(is)
          
c lcs - force 2-hit segments for sector 6/SL1 - SL1 has only 2 good layers 
c          if (isec.eq.6.and.is.eq.1) min_hit_seg=2
c          if                (is.eq.1) min_hit_seg=2
          
          IF(time_hits_max.LT.min_hit_seg)GO TO 52
C
C The segments were sorted before we called trk_dtime, but since we may have
C reduced the number of hits on some segment, we better resort.
C Also, don't bother checking smaller segments if we have a good enough one
C
          DO 49 ihit = time_hits_max,min_hit_seg,-1
CO	   IF(ihit.LT.lamin.AND.chibest.LT.dpar_trk_lrambfit_chi2) GO TO 49
cStep	   if(ipar_trk_lramb.LT.0 .AND. ihit.LT.lamin.AND.chibest.LT.lamin) LRflag=1
	   if(ipar_trk_lramb.LT.0 .AND. ihit.LT.lamin.AND.chibest.LT.dpar_trk_lrambfit_chi2) LRflag=1
           IF(LRflag.EQ.1) GO TO 49
           DO 50 isgc = 1,nsgc

C Resolve L-R ambiguity
            if(ipar_trk_lramb.GT.0) then
c               print *, time_hits(isgc),ihit
               IF(time_hits(isgc).EQ.ihit) THEN
                 CALL ana_lramb_c(isec,is,xdigi(1,1,isgc),alfa,weight_slope,ilnk,LRflag,ipar_trk_print(4))
               ELSE IF(time_hits(isgc).EQ.ihit+1.and.ihit.GE.min_hit_seg) THEN
                 DO la = 1,nlay
                   IF(xdigi(1,la,isgc).GT.0.) THEN
                     xdigi(1,la,isgc) = - xdigi(1,la,isgc) ! disable the layer temp
                     CALL ana_lramb_c(isec,is,xdigi(1,1,isgc),alfa,weight_slope,ilnk,LRflag,ipar_trk_print(4))
                     xdigi(1,la,isgc) = ABS(xdigi(1,la,isgc)) ! re-enable the layer
                   ENDIF
                 ENDDO
               ELSE
                 GO TO 50
               ENDIF
c               Level = TRK_LEVEL_lramb
               IF(LRflag.NE.0) THEN
                DO la = 1,nlay
                 xdigib(1,la) = xdigi(1,la,isgc)
                 xdigib(2,la) = xdigi(2,la,isgc)
                 xdigib(3,la) = xdigi(3,la,isgc)
                 xdigib(4,la) = xdigi(4,la,isgc)
                 xdigib(5,la) = xdigi(5,la,isgc)
                ENDDO
               ENDIF

            else
               IF(time_hits(isgc).NE.ihit.OR.ihit.LT.3) GO TO 50
               CALL ana_lramb(isec,is,xdigi(1,1,isgc),alfa,weight_slope, chib,slopD)
CO              IF(chib.GT.chibest) goto 50
               IF(chib-ihit.GT.chibest-lamin) goto 50
               chibest   = chib
               slopDbest = slopD
               lamin = ihit
               DO la = 1,nlay
                 xdigib(1,la) = xdigi(1,la,isgc)
                 xdigib(2,la) = xdigi(2,la,isgc)
                 xdigib(3,la) = xdigi(3,la,isgc)
                 xdigib(4,la) = xdigi(4,la,isgc)
                 xdigib(5,la) = xdigi(5,la,isgc)
C               IF(xdigib(1,la).GT.0.) lamin = lamin + 1
              ENDDO
              isgb = isg
            endif
 50        CONTINUE              ! loop over segments in a SuperLayer
 49       CONTINUE 
         dca_segm(is,ilnk) = isgb
         call weed_known_amb(ilnk,is)
C Improve LR_amb by allowing to disable one layer in a track segment
         if(ipar_trk_lramb.LT.0) then
          IF(LRflag.EQ.0 .OR. (chibest.GT.dpar_trk_lrambfit_chi2 .AND. lamin.GT.3)) THEN
            lab = 0
            DO 51 la = 1,nlay
               IF(xdigib(1,la).LT.0.) GO TO 51
               xdigib(1,la) = - xdigib(1,la) ! disable the layer temp
                  CALL ana_lramb(isec,is,xdigib,alfa,weight_slope, chib,slopD)       
                  IF(chib.LT.chibest) THEN
                     LRflag=1
                     chibest   = chib
                     slopDbest = slopD
                     lab = la
                  ENDIF
               xdigib(1,la) = ABS(xdigib(1,la)) ! re-enable the layer
 51         CONTINUE
            IF(lab.GT.0) xdigib(1,lab) = -xdigib(1,lab)   
            improve = lab
          ENDIF                  ! IF(chibest.GT.dpar_trk_lrambfit_chi2)
         endif
 52      CONTINUE
          IF(ipar_trk_lramb*ipar_trk_print(4).LT.0) THEN
            WRITE(6,103) is,isgb,(xdigib(1,i),i=1,6),improve,slopDbest,(xdigib(4,i),i=1,6),chibest
          ENDIF

C Check if L-R Ambiguity was successfully resolved
C If not, we have a crap cluster.  Trash it.
C (We might want to come back and deal with 2-hit segments in SL1 later.)

C          IF(chibest.GT.cut) THEN
         IF(LRflag.EQ.0) THEN
            lnk_clust(is,ilnk) = 0
            DO la = 1,nlay
              il = la + ilmin
              trk(12,il,ilnk)= 1.                          ! Status notOK
            ENDDO
         ELSE  
            LRflag = 0
            n_seg = n_seg + 1

C Store info in "trk( , , )" array needed for final fit (with DT)
C            sigm = dc_sigma_doca(is)	
C            IF(sigm.LT.0.002) sigm = 0.002
            DO la = 1,nlay
              il = la + ilmin
              if(ipar_trk_lramb.LT.0) then
                trk(1,il,ilnk) = xdigib(1,la)               ! iw
                trk(2,il,ilnk) = xdigib(2,la)               ! Raw tdc [counts]
                trk(4,il,ilnk) = xdigib(4,la)               ! Dm [cm]
                trk(5,il,ilnk) = xdigib(5,la)               ! SIGd [cm]
              endif
              trk(3,il,ilnk) = xdigib(3,la)               ! Td [ns]
              trk(6,il,ilnk) = beta_tbt(ilnk)             ! Velocity = beta [c]
              trk(11,il,ilnk) = 10*isec + Level           ! Analysis Level & Sect
              trk(12,il,ilnk)= 0.                         ! Status OK
              IF(trk(1,il,ilnk).LE.0.) trk(12,il,ilnk) =1.! Status notOK
            ENDDO
          ENDIF
 60     CONTINUE                      ! End of loop over the superlayers

C Count tracks with resolved L-R Ambiguity in six SuperLayers

        IF(n_seg.GE.ipar_trk_minlramb-1) then
          ntr_fit_ok=ntr_fit_ok+1          
          call ana_statb(6,isec,trkp(9,1,ilnk))
        endif
        
        IF(n_seg.GE.ipar_trk_minlramb) THEN
          ntracks = ntracks + 1
          itr_level(ilnk) = Level
          itr_sect(ilnk)  = isec
          call ana_statb(7,isec,trkp(9,1,ilnk))		   
        ENDIF
        
 100  CONTINUE                  ! End of loop over linked clusters
C
C Now loop over all the still good tracks and delete those which
C Have become subsets of others due to losing SL during ambiguity resolution
C (reset the level passed to HBT)
C
      IF(ntracks.GT.1)THEN
       DO ilnk=1,ntr_link-1
        IF(itr_level(ilnk).EQ.Level)THEN
	  isec = itr_sect(ilnk)
          DO ilnk2=ilnk+1,ntr_link
	    isec2 = itr_sect(ilnk2)
            IF(isec.EQ.isec2.AND.itr_level(ilnk2).EQ.Level)THEN
              nshare = 0
              nsum   = 0
              nsum2  = 0
              DO is=1,nsuper
c                icl  = lnk_clust(is,ilnk)
c                icl2 = lnk_clust(is,ilnk2)
c                IF(icl.GT.0.AND.icl.EQ.icl2)nshare = nshare+1
c                IF(icl.GT.0) nsum = nsum+1
c                IF(icl2.GT.0)nsum2 = nsum2+1
                 ilmin=(is-1)*nlay
                 lsum = .FALSE.
                 lsum2 = .FALSE.
                 lshare = .FALSE.
                 do la=1,nlay
                    il = la + ilmin
                    if(trk(1,il,ilnk).GT.0)then
                       lsum = .TRUE.
                       if(abs(trk(1,il,ilnk)-trk(1,il,ilnk2)).LE.1.0)then
                          lshare = .TRUE.
                          lsum2 = .TRUE.
                       endif
                    endif
                    if(trk(1,il,ilnk2).GT.0)lsum2 = .TRUE.
                 enddo                       
                 if(lsum)nsum = nsum +1
                 if(lsum2)nsum2 = nsum2 +1
                 if(lshare)nshare = nshare +1
              ENDDO
              IF(nsum.GT.nsum2.AND.nsum2.eq.nshare)THEN
                itr_level(ilnk2) = TRK_LEVEL_hbt
              ELSEIF(nsum.LT.nsum2.AND.nsum.eq.nshare)THEN
                itr_level(ilnk)  = TRK_LEVEL_hbt
              ENDIF
            ENDIF
          ENDDO
        ENDIF
       ENDDO
      ENDIF
C
C Now count the tracks again.
C
      ntracks = 0
      DO ilnk = 1,ntr_link
        IF(itr_level(ilnk).EQ.Level)  ntracks = ntracks + 1
      ENDDO 

 999  CONTINUE

 101  FORMAT(//' LEVEL=',I2,'  Sector=',I2,'  itrk=',I2,'Beta = ',F5.3,'  (L/R Ambig)')        
 102  FORMAT(' SL isg    La1    La2    La3    La4    La5    La6  Chi2      impr  SlopD')    
 103  FORMAT(/2I3,6F7.0,12X,I5,F7.3/10X,6F7.3,E12.5)

      RETURN
      END
