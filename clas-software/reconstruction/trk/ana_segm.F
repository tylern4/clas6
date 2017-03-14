C======================================================================
      SUBROUTINE ana_segm(isec,ifail)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Classify hits into segments and clusters
C-
C-
C-   Inputs  : isec  - the sector to be analyzed
C-             ifail - control flag 
C-                    (>0: segments found in 'ipar_trk_minlinks' Superlayers)
C-
C-    Controls: ipar_trk_make_prlink   - must be on for writing
C-                              new templates and no background must
C-                              be ensured (see the data-card).
C-              ipar_trk_minhits(isuperlayer): min.hits per segment in 'isuperlayer'
C-              ipar_trk_minlinks: min.#Superlayers containing track segments
C-
C-   Calls              : none
C-
C-   Created   JUNE-8-1992  Bogdan Niczyporuk
C-   Modified  JUNE-9-1992  Billy Leung
C-   Modified  March 1997   Curtis A. Meyer
C-                          Added and filled the pt_row array. The data
C-                          contained herin point back to a row in the 
C-                          corresponding DC0 bank. In addition, this
C-                          information is then loaded into the segm_rp
C-                          array in the /anapatt/ common block.
C-
C-   Modified  June-25-1997 Curtis A. Meyer
C-                          loosened the segment initators so that with
C-                          the 4/6 and 3/4 requirements, we will be
C-                          100% efficient in this routine.
C-
C-                          Tried to clean up some of the initialization.
C- 
C-                          Included additional, but necessary documentation. 
C-
C-                          In looking at this, I have noted quite a 
C-                          number of "error condifition" where too
C-                          many of something is found. In its current
C-                          state, the code simply stops looking for more, 
C-                          and continues. No error or warning is
C-                          reported. THis needs to be fixed!
C-
C-   Modified July-24-1997  Richard A Thompson
C-                          Changed segment finding initiator condition.  Use
C-                          a mask that is the .or. of all segments to scan
C-                          down layer three and count the number of layers 
C-                          that had hits within the mask.  The "mask" is just
C-                          the width of the the segment roads in each layer.
C-                          The segment road mask is Width(la,isg) and is 
C-                          initialized in sda_dinit and is in the 'analink' 
C-                          common block.
C-   Aug.1997  F.Klein  -- Option: trktcl_minhits(1...6): min.#hits per segment
C-   Nov.1997  F.Klein  -- remove obsolete common bloack 'sdakeys.inc'
C-
C-   Modified Dec-11-1997   R.Feuerbach
C-                          Replaced dc_pln array with information from DCGM
C-                          BOS bank.
C-
C-   Definitions: A segment is a collection of hits within a superlayer
C-                that matches a pattern mask.
C-
C-                A cluster is a group of segments that share hits.
C-                  ^^^^that have neighbouring hits.
C- 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
*  I/O variables
*  -------------
      INTEGER isec, ifail
*
*  Include Files
*  -------------
#include "sdadigi.inc"
#include "sdaswim.inc"
#include "analink.inc"
#include "anapatt.inc"
#include "trktcl_var.inc"
#include "clas_offsets.inc"
#include "wcs.inc"
#include "bcs.inc"
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: ana_segm.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.23 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/03/16 04:33:25 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: ana_segm.F,v 1.23 2008/03/16 04:33:25 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='ANA_SEGM')
      PARAMETER (CRAUTH='Francois Roudot')
*
C Local variables
      INTEGER no_hit, nlay, nwmx, nsgmx_tmp
      PARAMETER (no_hit = 0, nlay = 6, nwmx = 192, nsgmx_tmp = nsgmx*2)
      INTEGER i, j, k, m, n, Level
      INTEGER icl, ifound, iw_master, mx_w, mn_w,iclmerge, iw2, mxwire
      INTEGER ih, il, ilmin, is, iwx, iw3, iwc, its, its2, isg, isgc
      INTEGER isum,isumm,isumSL, iwmin(6),iwmax(6),ifound_temp
      BYTE  bpatt(16)
      INTEGER la, nd, nd3, nsum, max_hit_seg, min_hit_seg, ihit_seg
      INTEGER nw(nlay)
      INTEGER nwall(nlay,nsgmx_tmp),nwhit(nsgmx_tmp),seg_hits(nsgmx)
      INTEGER nwflag(nsgmx_tmp), idtime(nlay,nwmx), pt_row(nlay,nwmx)
      INTEGER nsuper /6/
      INTEGER NLAYinSL(6)
      DATA NLAYinSL /4,6,6,6,6,6/
      logical nohit

C BOS manipulation
      integer MBANK,ind_dhcl,irow
      integer nami_dcgm, ind_dcgm(6), ncol_dcgm, check, ind

      integer MAMIND, GMind, MaxWire, sector, layer, baseind
c Functions to get indices for a given layer
      GMind(sector,layer) = ind_dcgm(sector) + (layer-1)*ncol_dcgm
      MaxWire(baseind) = JW(baseind+DCGM_max_wire)
C____________________________________________________________________
C
C ANA_SEGM begins
C
C

      call ana_statb(1,isec,0.)
      
C get BOS indices for DCGM banks.
      if (nami_dcgm.le.0) nami_dcgm = MAMIND(jw,'DCGM')

C Each sector has its own bank
      check = 0
      ind = nami_dcgm+1
      do while (jw(ind-1).gt.0)
         ind = jw(ind-1)
         ind_dcgm(jw(ind-2)) = ind
         check = check+1
      enddo
      if (check.ne.6) call recmes(crname,'W','Error getting DCGM bank')
      ncol_dcgm = jw(ind_dcgm(check)-5)

C Loop over the superlayers in a Sector
C
      ifail = 0
*

      DO 6000 is = 1,nsuper
*
*           ilmin:  number of layer just before SL begins
*           maximum hits per segment is normally 6, except in superlayer
*           two of region one, where itis four.
*
         ilmin       = (is-1)*nlay
*
         IF(ipar_trk_make_prlink.GT.0) THEN
*
*              Generating the prlink file:
*
            min_hit_seg = nlay
            max_hit_seg = NLAYinSL(is)
            nd3 = dc_ndig(ilmin+3,isec)
            IF(nd3.LE.0) GO TO 6000
            nd3 = 1

         ELSE
*
*              Analyzing data: Set the minimum hits per segment 
*              to six, except in superlayer 2 of region 1 where
*              we set this to 3.           ^^^ SL1
*
	       min_hit_seg=ipar_trk_minhits(is)
               max_hit_seg = NLAYinSL(is)
	       
c lcs - force 2-hit segments for sector 6/SL1 - SL1 has only 2 good layers 
c	       if (isec.eq.6.and.is.eq.1) min_hit_seg=2
c	       if               (is.eq.1) min_hit_seg=2

c fk           min_hit_seg = 4     
c fk            if(is.eq.1) min_hit_seg = 3
crat        IF(max_hit_seg .LT. nlay) min_hit_seg = 3 
*
*              set nd3 to the maximum number of wires in this plane.
*
               nd3 = MaxWire(GMind(isec,ilmin+3))
               
         ENDIF
*
C Initialization of idtime(6,192) for given SuperLayer
*
         Call Vzero(Idtime(1,1),(nwmx*nlay))
*
c fk         nsegm(is,isec)  = 0
c fk         nclust(is,isec) = 0
c fk         Do 900 j = 1,nclmx
c fk            nsegmc(j,is,isec) = 0
c fk 900     Continue
*
C Fill "idtime" array with hits for a given superlayer "is"
*
*          The idtime array is 6 layers by 192 wires. It is
*          zero except for where we load the hits. It is
*          then used for reasonabaly quick comparisons 
*          with the pattern masks. To try and speed up
*          a later search, we are going to find the
*          smallest (mn_w) and largest (mx_w) wire
*          number hit in this sector.
*
        mn_w = nd3
        mx_w = 0
*
        DO 1100 la = 1,nlay
*
*             Convert the layer within this super layer to 
*             an absolute layer: (il) Then get the number
*             of hits (nd) in this layer and sector.
*
           il = ilmin + la
           nd = dc_ndig(il,isec)
*
*             If we are beyond layer 4 in superlayer 2, set 
*             the amount of data to be zero. There really
*             shouldn't be any, but ....
*
           IF (la .GT. max_hit_seg) nd = 0
*
*             If there is data in this layer, then loop
*             over those hits, and store the information 
*             in the idtime array. We also keep track of
*             the pointer back to the raw data via the
*             pt_row array.
*
           IF(nd.GT.0) THEN
*     
              mxwire = MaxWire(GMind(isec,il)) !last physical sense wire
              DO 1000 ih = 1,nd
                 iwx = dc_digi(1,ih,il,isec)
                 IF(iwx.GE.1 .AND. iwx.LE.mxwire) THEN
*
                    idtime(la,iwx) = dc_digi(2,ih,il,isec)
*
*--c.a.meyer march 1997: keep track of row number in DC0.
*
                    pt_row(la,iwx) = pt_digi(ih,il,isec)
*--
                    if ( iwx .lt. mn_w ) mn_w = iwx
                    if ( iwx .gt. mx_w ) mx_w = iwx
*
                 ENDIF
 1000         CONTINUE
           ENDIF
 1100   CONTINUE
*
C Loop over the hits(training) or all wires(analysis) in layers #2 & #3
*
        ifound = 0
*
*          Note the following loop was over all 192 wires. We now
*          try and speed up the search by only looking over the
*          range where data exist. However, for generating the
*          patterns, leave this alone:
*
        If (ipar_trk_make_prlink.eq.0) Then
           mn_w = MAX(1,(mn_w-2))
           mx_w = MIN(nd3,(mx_w+2))
        Else
           mn_w = 1
           mx_w = nd3
        Endif
*
        DO 2400 ih = mn_w,mx_w
*
*             Segment generation:
*
           IF(ipar_trk_make_prlink.GT.0) THEN
              iw3 = dc_digi(1,ih,ilmin+3,isec)
              IF(idtime(3,iw3).EQ.no_hit) GO TO 2400
           ELSE
*
*             Data handling:
*
*             Entry test condition for pattern matching:
*             The idea here is to count up the number of layers that have wires
*             hit lying within the segment road width.  Width(la,1) is the 
*             downstream road width and Width(la,2) is the upstream road width
*             for layer la.  So the segment roads go from ih-Width(la,1) to 
*             ih+Width(la,2) in layer la.  If there are at least min_hit_segm 
*             layers with wires hit that lie within the width of the roads, 
*             then go ahead with pattern matching.

              iw3 = ih
              isum = 0
              do la = 1,nlay
                 il = ilmin+la
                 nohit = .true.
                 iwx = max(1,ih-Width(la,1)) 
                 mxwire = min(MaxWire(GMind(isec,il)),ih+Width(la,2))
                 do while(nohit .and. (iwx.le.mxwire))
                    if(idtime(la,iwx).gt.no_hit) then
                       isum=isum+1
                       nohit = .false.
                    end if
                    if (isum.ge.min_hit_seg) goto 1600 !enough hits --> move on to pattern matching
                    iwx = iwx+1
                 end do
              end do
              GOTO 2400     ! not enough hits --> go to next wire
           ENDIF

C Loop over templatets, with "nw" storing the wire#'s for all 6 layers 
C of a given segment
*
*             We get to here if we have an initiator from the data.
*             We now search the masks based on the wire number in
*             layer three from above. Our masks are offsets for
*             each layer based upon this layer.
*
*             Start by looping over all the pattern masks:
*
 1600      DO 2300 isg=1,npat
*
              IF(max_hit_seg.EQ.3 .AND. ipat_flg3(isg).EQ.1) GO TO 2300
              IF(max_hit_seg.EQ.4 .AND. ipat_flg4(isg).EQ.1) GO TO 2300
              IF(is .LT. minsl(isg)) GO TO 2300
*
*                Set the number of matched hits to zero.
*
              isum = 0
*
*                Loop over the nlay (6) layers in this superlayer.
*
              DO 1700 la = 1,nlay
*
*                   il is the actual layer number 1 to 36, while la
*                   is the layer number within a superlayer.
*
                 il = ilmin + la
*
*                   ipat contains an offset from layer 3 for the wire
*                   in this mask. This leads to iwx being the expected
*                   wire number in the current layer for the current
*                   mask. We then initially tag the nw array as the
*                   negative of this wire number to indicate the is
*                   no hit -- i.e. generate a pseudo-hit.
*
                 iwx = iw3 + ipat(la,isg)
*
*                   Make sure that iwx is within the range of allowed
*                   wire numbers (1 to 192) for this layer.
*
                 IF(iwx.LE.0) THEN
                    nw(la) = -1
                 ELSE IF (iwx.GT.MaxWire(GMind(isec,il))) THEN
                    nw(la) = -ABS(MaxWire(GMind(isec,il)))
                 ELSE
                    nw(la) = -IABS(iwx)
*
*                      Check to see if there is a hit on this wire.
*                      If there is, then set nw to contain the wire number,
*                      and add one to the total number of matched hits
*                      in this mask.
*
                    IF(idtime(la,iwx).GT.no_hit) THEN
                       nw(la) = iwx
                       isum = isum + 1
                    ENDIF
                    
*
                 ENDIF
*
 1700         CONTINUE
*
C Check if track segment was already found
*
*                In order for a segment to be "found", isum must be
*                at least as big as min_hit_seg. If this is not true,
*                then go to the next mask.
*
              IF(isum .LT. min_hit_seg) GO TO 2300 
*
C Check IF hits exist in layer 2 and 3
*
*                This is rather odd, we now force there to be a hit
*                in layer 2 or layer 3?? This needs to be loosened.
*                if we are going to include layer 4 as above.
*
crat          IF(nw(2).LT.0 .AND. nw(3).LT.0) GO TO 2300
*
***>          IF((nw(2).LT.0).AND.(nw(3).LT.0).AND.(nw(4).LT.0)) 
***>     &        GO TO 2300
*
*
cfr 10-31-1996            IF(ifound .GT. 0) THEN
*
*                If we have found previous segments based on this
*                reference hit, then we want to compare with previous
*                found segments. We loop over all previous segments,
*                and see how many hits are shared with those. This
*                loop keeps a new segment as long as "all" its hits are
*                not shared with a previous segment. It would seem that
*                we should also discard previous segments where all
*                their hits are shared with this segment as well. It
*                would be fairly trivial to add here:
*
              IF((ifound.GT.0).and.(ifound.le.nsgmx_tmp)) THEN
*
                 DO 2100 i = 1,ifound
*
                    IF(nwflag(i).ge.0)THEN
                       its = 0
                       its2= 0
*     
                       DO 1900 la = 1,nlay
                          IF(nw(la).GT.0.AND.nw(la).EQ.nwall(la,i)) its = its+1
                          IF(nw(la).GT.0.AND.iabs(nw(la)-nwall(la,i)).LE.1)
     &                       its2 = its2+1
 1900                  CONTINUE
*     
*                "its" is the number of shared hits between these
*                two segments. If all the hits on the current
*                segment are shared, and that number is no
*                bigger than the previous segment, then 
*                discard the current segment.
*                its2 is used to only keep 2/3 hit segments in SL 1/rest
*                if they are isolated.
* 
                       IF((its.EQ.isum).AND.(isum.LE.nwhit(i))) GO TO 2300
C
                       IF(isum.LT.nwhit(i))THEN
                          IF(its.EQ.isum) GO TO 2300
                          IF(its2.GE.1)THEN
                             IF(is.EQ.1)THEN
                                IF(isum.LE.2) GO TO 2300
                             ELSE
                                IF(isum.LE.3) GO TO 2300
                             ENDIF
                          ENDIF
C     
C     Flag the previous segment as bad if the current is a superset of it. 
C
                       ELSE IF(isum.GT.nwhit(i))THEN
                          IF(its.EQ.nwhit(i)) nwflag(i) = -1
                          IF(its2.GE.1)THEN
                             IF(is.EQ.1)THEN
                                IF(nwhit(i).LE.2) nwflag(i) = -1
                             ELSE
                                IF(nwhit(i).LE.3) nwflag(i) = -1
                             ENDIF
                          ENDIF
                       ENDIF
C                      IF((its.EQ.nwhit(i)).AND.(isum.GT.nwhit(i)))nwflag(i)=-1
*     
                    ENDIF
 2100            CONTINUE
*
              ENDIF
*
*                Indicate that we have found a match, and store the 
*                information on that match in the nwall array. We
*                then put the number of matched hits into the nwhit
*                array, and tag the nwflag array as "ok"
*
*
              ifound = ifound + 1
              IF(ifound .LE. nsgmx) THEN
                 DO 2200 la =1,nlay
                    nwall(la,ifound) = nw(la)
 2200            CONTINUE
                 nwhit(ifound) = isum
                 nwflag(ifound)= 0
              ENDIF
c              do i=1,ifound+1
c                 if(i.le.ifound)then
c                    if(nwhit(i).lt.isum)go to 2199
c                 endif
c              enddo
c 2199         continue
c              ifound = min(ifound+1,nsgmx_tmp)
c              if(i.lt.ifound)then
c                 do j=ifound,i+1,-1
c                    do la=1,nlay
c                       nwall(la,j) = nwall(la,j-1)
c                    enddo
c                    nwhit(j) = nwhit(j-1)
c                    nwflag(j) = nwflag(j-1)
c                 enddo
c              endif
c              IF(i.LE. nsgmx_tmp) THEN
c                 DO 2200 la =1,nlay
c                    nwall(la,i) = nw(la)
c 2200            CONTINUE
c                 nwhit(i) = isum
c                 nwflag(i)= 0
c              ENDIF
*
* If at limit, try to compress arrays by throwing out rejected segments
* before bailing out of loop
*
              IF(ifound .eq. nsgmx_tmp) THEN
                 ifound_temp = ifound
                 ifound = 0
                 DO i=1,ifound_temp
                    IF(nwflag(i).GE.0)then
                       ifound = ifound + 1
                       DO la =1,nlay
                          nwall(la,ifound) = nwall(la,i)
                       ENDDO
                       nwhit(ifound) = nwhit(i)
                       nwflag(ifound)= nwflag(i)
                    ENDIF
                 ENDDO
              ENDIF
*
cfr 10-31-1996
*
              if(ifound.ge.nsgmx_tmp) go to 2401
*
 2300      CONTINUE             ! End of loop over templatets
 2400   CONTINUE                ! End of loop over the hits in layer 2 & 3
 2401   Continue
*
*
*
C Cleaning "nwall(ifound)" by setting flag "nwflag(ifound)=1 for good segment and Search for Segment Clusters
cfr 10-31-96        IF(ifound .GT. 0) THEN
*
*          The following is rather convoluted logic, and I suspect
*          it could be significantly cleaned up. Initially, we make
*          sure that there are found segments based on this hit.
*
        IF((ifound.GT.0).and.(ifound.le.nsgmx_tmp)) THEN
*
*             Now loop over all allowed number of hits in a valid
*             segment from the minimum to the maximum. 
*             ih =       min, min+1, ... max  We then set 
*             ihit_seg = max, max-1, ... min. 
*             It is not at all obvious why we don't just run the
*             do-loop backwards???
*
           DO 3300 ih = min_hit_seg, max_hit_seg
*
              ihit_seg = max_hit_seg - ih + min_hit_seg
*
*                For all segments with the maximum number of hits, we
*                flag them with a one. This could have more sensibly
*                been performed earlier in the routine -- I.e. when
*                the segment was found.
*
              IF(ihit_seg .EQ. max_hit_seg) THEN
*
                 DO 2500 i = 1, ifound
                    IF(nwhit(i).EQ.ihit_seg.AND.nwflag(i).GE.0) nwflag(i) = 1
 2500            CONTINUE
*
              ELSE
C*
C*                   This now gets convoluted! We are looking all segments
C*                   that have ihit_seg hits. We then want to perform
C*                   a hit by hit comparison bewteen them. Here I
C*                   believe that we want to remove all segments where
C*                   all the found hits are shared with a segment which
C*                   has more hits than this one. However, it also
C*                   appears that it two segemnts with exactly the
C*                   same number of hits share all their hits, then
C*                   only the first one is retained. Does this make
C*                   sense???
C* 
C                 DO 3000 i = 1, ifound
C*
C*                      Check for the correct number of hits:
C*
C                    IF(nwhit(i) .EQ. ihit_seg .AND. nwflag(i).GE.0) THEN
C*
C*                         Now loop over all segments, and accept those
C*                         whose nwflag variable is non-zero. Those 
C*                         will only be segments with at least as many
C*                         hits as this one.
C*
C                       DO 2900 j = 1, ifound
C*
C                          IF(nwflag(j) .GT. 0) THEN
C*
C*                              Now we want to count the number of shared
C*                              hits between the "i" and "j" segment.
C*
C                             its = 0
C                             DO 2800 la = 1,nlay
C                                IF((nwall(la,i).GT.0).AND.
C     &                             (nwall(la,i).EQ.nwall(la,j)))
C     &                             its = its+1
C 2800                        CONTINUE
C*
C*                               Are all the hits on segment "i" shared
C*                               with segment "j"? If so then we will
C*                               not mark "i" as good. We will then 
C*                               move onto the next "i" segment.
C*
C                             IF(its .GE. ihit_seg) GO TO 3000
C*
C                          ENDIF
C 2900                  CONTINUE
C*
C*                         If we get to here, we consider the "i" segment
C*                         good, and mark it as such in nwflag.
C* 
C                       nwflag(i) = 1
C*
C                    ENDIF
C*
C 3000            CONTINUE
*
                 DO 3000 i = 1, ifound
*
*                      Check for the correct number of hits:
*
                    IF(nwhit(i) .EQ. ihit_seg .AND. nwflag(i).GE.0) THEN
*
C
C     In an attempt to reduce the combinatorics generated by requiring only
C     three hits per segment, discard any segments whose hits are all contained
C     in segments with greater number of hits.
C
C     Loop over layers in this segment.  As soon as you find the hit used
C     previously, move on to next layer.  As soon as you find a layer with
C     a hit previously unused, flag the segment good and move on.
C
                       DO 2800 la = 1,nlay
                          DO 2900 j = 1, ifound
                             IF(nwhit(j).GT.ihit_seg.and.nwflag(j).GT.0)THEN
                                IF((nwall(la,i).LE.0).OR.
     &                          (nwall(la,i).EQ.nwall(la,j)))GO TO 2800
                             ENDIF
 2900                     CONTINUE
                          nwflag(i)=1
                          GO TO 3000
 2800                  CONTINUE
                    ENDIF
 3000            CONTINUE

              ENDIF
*
*                AT this point, all segments that do not share all their
*                hits with a segment with more hits, as well as some
*                peculiar fraction of those with the same number are
*                marked with nwflag=1. These are then retained for
*                further analysis.
*
C Store results in "segm" and "clust" arrays
*
*                We now want to store these into the segment array:
*
              DO 3200 i = 1, ifound
*
*                   Find the good ones with ihit_seg hits:
*
                 IF(nwflag(i).GT.0 .AND. nwhit(i).EQ.ihit_seg) THEN
*
*                      Make sure that we have not already found too
*                      many segments in this superlayer and sector.
*
                    IF(nsegm(is,isec).GE.nsgmx) GO TO 3200
*
*                      Add one to the number of found sectors. Then
*                      set the nwflag equal to the segment number.
*
                    nsegm(is,isec) = nsegm(is,isec) + 1
                    its = nsegm(is,isec)
                    nwflag(i) = its
                    seg_hits(its) = ihit_seg
*
*                      Now loop over all the hits on this segment, and
*                      retain thier wire number and time in the segm array.
*                      We also retain their pointer back to dc0.
*
                    k = 1
*  set  segm(..) array  for iwx=0 on reasonable value: 
                    j = 1
                    do while (j.LT.nlay .AND. nwall(j,i).EQ.0) 
                      j = j+1
                    enddo
                    iw2 = iabs(nwall(j,i))

                    DO 3100 la = 1,nlay
*
*                         Get wire number and make sure it is valid:
*
                       iwx = nwall(la,i)
                       if(IABS(iwx).gt.0.and.IABS(iwx).le.192) then
                         iw2 = iabs(iwx)
*
                          segm(k  ,its,is,isec) = iwx
                          segm(k+1,its,is,isec) = idtime(la,iw2)
*     
*--c.a.meyer march 1997: keep track of row number in DC0.
*
                          if(pt_row(la,iw2).GT.0) then
                           segm_rp(((k+1)/2),its,is,isec)  = pt_row(la,iw2)
                          else
                           segm_rp(((k+1)/2),its,is,isec)  = -iw2
                          endif
*--
                          k = k + 2
                       else
*
*                            An error condition was encountered:
*
                             write(CRMESS,35) iwx,isec
                             call RECMES(crname,'I',crmess)
                             segm(k,its,is,isec) = -iw2
                             segm(k+1,its,is,isec) = -8888
                             segm_rp(((k+1)/2),its,is,isec) = -iw2
                       endif
*
 3100               CONTINUE
*
                 ENDIF
*
 3200         CONTINUE
*
 3300      CONTINUE
 
C Cluster finding. Store results cluster arrays (1-st cluster).
*
*             A cluster is a set of segments that share hits. We
*             start by looping over all found segments within
*             this superlayer. Bt definition, the very first
*             segment belongs to the very first cluster.
*
*
           DO 4000 isg = 1,nsegm(is,isec)
*
*                Put the first segment in the first cluster:
*
              IF(isg.EQ.1) THEN
                 nclust(is,isec)    = 1
                 nsegmc(1,is,isec)  = 1
                 clust(1,1,is,isec) = isg
                 DO la = 1,nlay
                    k = 2*la - 1
                    iwx  = segm(k,isg,is,isec)
                    clwmin(la,1,is,isec,1) = iabs(iwx)
                    clwmax(la,1,is,isec,1) = iabs(iwx)
                    clwmin(la,1,is,isec,2) = 255
                    clwmax(la,1,is,isec,2) = 0
                    IF(iwx.gt.0)THEN
                       clwmin(la,1,is,isec,2)=iwx
                       clwmax(la,1,is,isec,2)=iwx
                    ENDIF
                 ENDDO
              ELSE

C Loop over existing clusters
*
*                   For all later segments, we want to loop over
*                   all existing clusters, and see if it is a 
*                   member. nsum will be the number of clusters
*                   to which this segment belongs, but actually
*                   appears nincomputery. As soon as it is 
*                   incremented, we jump to the next segment!
*
                 nsum = 0
                 DO 3900 icl = 1,nclust(is,isec)
*
C     Loop over segments in each found cluster
*
*                      Loop over all segments in this cluster:
*
cc                    DO 3800 isgc = 1,nsegmc(icl,is,isec)

                       isum = 0
                       its = 0
cc                       m = clust(isgc,icl,is,isec)
                       k = 1
*
*                         Loop over the layers, and deterine if the
*                         two segments have common hits. Isum is the
*                         number of common hits.
*
                       DO 3700 la = 1,nlay
c                          iwc = segm(k,m,is,isec)
                          iwx  = segm(k,isg,is,isec)
c                          IF(iwx.GT.0 .AND. iabs(iwx-iwc).le.1) isum = isum + 1
c                          IF(iwx.GT.0 .AND. iabs(iwx-iwc).gt.1) its = its + 1
                          IF(iwx.GT.0)then
                             IF(iwx.GE.(clwmin(la,icl,is,isec,2)-1). AND.
     &                          iwx.LE.(clwmax(la,icl,is,isec,2)+1)) THEN
                                isum = isum + 1
                             ELSE
                                its = its + 1
                             ENDIF
                          ENDIF
                          k = k + 2
 3700                  CONTINUE
*
C Add new segment to existing cluster
*
*                         If they have common hits, and we do not have too
*                         many segments in this cluster, then add this
*                         segment to the current cluster. 
*
* lcs - Remove hardwired limit on hits per segment
*                      IF(isum.GE.0.and.its.LT.3) THEN

                      IF(isum.GE.0.and.its.LT.min_hit_seg) THEN
                          nsum = nsum + 1
                          IF(nsum.gt.1)THEN
*
* This segment belongs to two clusters. Merge them.
*
                             DO la = 1,nlay
                                iwx  = clwmin(la,icl,is,isec,2)
                                iwc = clwmin(la,iclmerge,is,isec,2)
                                clwmin(la,iclmerge,is,isec,2)=min(iwx,iwc)
                                iwx  = clwmax(la,icl,is,isec,2)
                                iwc = clwmax(la,iclmerge,is,isec,2)
                                clwmax(la,iclmerge,is,isec,2)=max(iwx,iwc)
                                iwx  = clwmin(la,icl,is,isec,1)
                                iwc = clwmin(la,iclmerge,is,isec,1)
                                clwmin(la,iclmerge,is,isec,1)=min(iwx,iwc)
                                iwx  = clwmax(la,icl,is,isec,1)
                                iwc = clwmax(la,iclmerge,is,isec,1)
                                clwmax(la,iclmerge,is,isec,1)=max(iwx,iwc)

                                clwmin(la,icl,is,isec,2) = 255
                                clwmax(la,icl,is,isec,2) = 0
                                clwmin(la,icl,is,isec,1) = 255
                                clwmax(la,icl,is,isec,1) = 0
                             ENDDO
                             call trk_clmerge(iclmerge,icl,seg_hits,is,isec)
                          ELSE
*
* We have found a cluster to put this in (if we have room)
*
                             iclmerge = icl
                             DO la = 1,nlay
                                k = 2*la - 1
                                iwx  = segm(k,isg,is,isec)
                                IF(iwx .GT. 0)THEN
                                   clwmin(la,icl,is,isec,2)=min(clwmin(la,icl,is,isec,2),iwx)
                                   clwmax(la,icl,is,isec,2)=max(clwmax(la,icl,is,isec,2),iwx)
                                   clwmin(la,icl,is,isec,1)=min(clwmin(la,icl,is,isec,1),iwx)
                                   clwmax(la,icl,is,isec,1)=max(clwmax(la,icl,is,isec,1),iwx)
                                ELSEIF(iwx .NE. 0)THEN
                                   iwx = iabs(iwx)
                                   clwmin(la,icl,is,isec,1)=min(clwmin(la,icl,is,isec,1),iwx)
                                   clwmax(la,icl,is,isec,1)=max(clwmax(la,icl,is,isec,1),iwx)
                                ENDIF
                             ENDDO
                             IF(nsegmc(icl,is,isec).LT.nsgcmx) THEN        
                                nsegmc(icl,is,isec) = nsegmc(icl,is,isec) + 1
                                i = nsegmc(icl,is,isec)
                                clust(i,icl,is,isec) = isg
                             ENDIF
                          ENDIF
*
                          GO TO 3900
                       ENDIF
*
 3800               CONTINUE
*
 3900            CONTINUE
*
C Create a new cluster
*
*                   If we got to this point, then the current segment
*                   belongs to no cluster. Asuuming that we do not
*                   have too many clusters, then create a new one
*                   and put this segment in it.
*
                 IF(nsum.EQ.0 .AND. nclust(is,isec).LT.nclmx) THEN
*
                    nclust(is,isec) = nclust(is,isec) + 1
                    n = nclust(is,isec)
                    nsegmc(n,is,isec) = 1
                    clust(1,n,is,isec) = isg
                    DO la = 1,nlay
                       k = 2*la - 1
                       iwx  = segm(k,isg,is,isec)
                       clwmin(la,n,is,isec,1) = iabs(iwx)
                       clwmax(la,n,is,isec,1) = iabs(iwx)
                       clwmin(la,n,is,isec,2) = 255
                       clwmax(la,n,is,isec,2) = 0
                       IF(iwx.gt.0)THEN
                          clwmin(la,n,is,isec,2)=iwx
                          clwmax(la,n,is,isec,2)=iwx
                       ENDIF
                    ENDDO
*
                 ENDIF
*
              ENDIF             ! End of IF (isg.EQ.1)
*
 4000      CONTINUE             ! End of DO over nsegm(is,isec)
*
c           DO icl = 1,nclust(is,isec)
c              DO la = 1,nlay
c                 k = 2*la - 1
c                 clwmin(la,icl,is,isec,1) = 999
c                 clwmax(la,icl,is,isec,1) = 0
c                 clwmin(la,icl,is,isec,2) = 999
c                 clwmax(la,icl,is,isec,2) = 0
c                 DO isgc = 1,nsegmc(icl,is,isec)
c                    isg = clust(isgc,icl,is,isec)
c                    iwx  = segm(k,isg,is,isec)
c                    IF(iwx .GT. 0)THEN
c                      clwmin(la,icl,is,isec,2)=min(clwmin(la,icl,is,isec,2),iwx)
c                      clwmax(la,icl,is,isec,2)=max(clwmax(la,icl,is,isec,2),iwx)
c                      clwmin(la,icl,is,isec,1)=min(clwmin(la,icl,is,isec,1),iwx)
c                      clwmax(la,icl,is,isec,1)=max(clwmax(la,icl,is,isec,1),iwx)
c                    ELSEIF(iw .NE. 0)THEN
c                      iwx = iabs(iwx)
c                      clwmin(la,icl,is,isec,1)=min(clwmin(la,icl,is,isec,1),iwx)
c                      clwmax(la,icl,is,isec,1)=max(clwmax(la,icl,is,isec,1),iwx)
c                    ENDIF
c                 ENDDO
c                 write(6,*)"la,icl,is,isec,min,max ",la,icl,is,isec,
c     &                     clwmin(la,icl,is,isec,1),clwmax(la,icl,is,isec,1),
c     &                     clwmin(la,icl,is,isec,2),clwmax(la,icl,is,isec,2) 
c              ENDDO
c           ENDDO
*
        ENDIF                   ! End of IF (ifound.GT.0)

C Store new segment template when training selected 
*
        IF(ifound.EQ.0 .AND. ipar_trk_make_prlink.GT.0) THEN
*
C First check if there are hits in all layers of SL
*
           DO 4100 la = 1,nlay
              il = ilmin + la
              IF(dc_ndig(il,isec).EQ.0) GO TO 6000
 4100      CONTINUE
*
           IF(npat.GE.max_npat) GO TO 6000
           npat = npat + 1
           DO 4200 la = 1,nlay
              il = ilmin + la
C     Assuming no background when training, therefore use the 1st hit
              iw_master = dc_digi(1,1,ilmin+3,isec)
              ipat(la,npat) = dc_digi(1,1,il,isec) - iw_master
 4200      CONTINUE
           GO TO 6000           ! an early exit point
        ENDIF
 6000   CONTINUE                ! End of loop over superlayers
      
C Require clusters in all 6 super layers, if not set ifail = 1
*
*        This is a point where we probably want to modify things.
*        essentially, we are forcing clusters in all six superlayers
*        here, which is not real desireable.
*
      isumSL = 0
      isum = 0
*
      do i=1,nsuper
        if( nclust(i,isec).GT.0) then
           isumSL = isumSL + 1
           isum = isum + nclust(i,isec)
        endif
      enddo
*
*        If we have clusters in 'trk_minlinks' superlayers, then
*        add one to ifail.
*
      IF(isumSL.GE.ipar_trk_minlinks) ifail = ifail + 1
*
*  write cluster info into BOS bank and keep row# in pt_clust(icl,isly,isec)
*
      ind_dhcl=0

      if (isum.GT.0) then

         ind_dhcl = MBANK(iw,'DHCL',isec,16,isum)
         call vzero(pt_clust(1,1,isec),nclmx*6)
         irow=0
*
         do is = 1,nsuper
            do icl = 1,nclust(is,isec)
               ind = ind_dhcl + 4*irow
               irow = irow +1
               pt_clust(icl,is,isec) = irow
               do il=1,6
                  iwmin(il)=193
                  iwmax(il)=0
               enddo
               do j=1,16
                  bpatt(j) = 0
               enddo
               bpatt(1) = is
*     bpatt(2:4) are filled later
               do isg = 1,nsegmc(icl,is,isec)
                  k = clust(isg,icl,is,isec)
                  do il=1,6
                     if (IABS(segm(il*2-1,k,is,isec)).LT.iwmin(il)) 
     &                    iwmin(il) = IABS(segm(il*2-1,k,is,isec))
                     if (IABS(segm(il*2-1,k,is,isec)).GT.iwmax(il)) 
     &                    iwmax(il) = IABS(segm(il*2-1,k,is,isec))
                  enddo
               enddo
               do il=1,6
                  isumm=0
                  do isg = 1,nsegmc(icl,is,isec)
                     k = clust(isg,icl,is,isec)
                     iwx = segm(il*2-1,k,is,isec)-iwmin(il)
                     if(iwx.GE.0 .AND. iwx.LT.8) then
                       i = mod(isumm/(2**iwx),2)
                       if (i.eq.0) isumm=isumm+2**iwx
                     endif
                  enddo
                  bpatt(1+il*2) = iwmin(il)
                  bpatt(2+il*2) = isumm
               enddo
               do j=1,16
                  IW08(4*ind+j) = bpatt(j)
               enddo
            enddo               !icl=1,nclust(is,isec)
         enddo                  !is=1,nsuper
      endif                     !isumm>0
*
*
C Report clusters/segments information for each Superlayer
*
      IF(ipar_trk_print(2).GE.1) THEN
         Level = 1
         IF(isum.GT.0) WRITE(6,31) Level,isec        

         DO 7000 is = 1,nsuper
*
            IF(nsegm(is,isec).EQ.0) GO TO 7000
            WRITE(6,32) is,nsegm(is,isec),nclust(is,isec)
            WRITE(6,33)
*
            DO 6900 isg = 1,nsegm(is,isec)
*
               DO 6700 icl = 1,nclust(is,isec)
*
                  k = 0
                  DO 6600 i = 1,nsegmc(icl,is,isec)
                     IF(isg.EQ.clust(i,icl,is,isec)) THEN
                        k = icl
                        GO TO 6800
                     ENDIF
 6600             CONTINUE                 
*
 6700          CONTINUE
*
 6800          continue
               WRITE(6,34) isg,(segm(i,isg,is,isec),i=1,12,2),k

 6900       CONTINUE
*
            do n=1,nclust(is,isec)
               write(6,*)"fianl min",(clwmin(la,n,is,isec,1),la=1,6)
               write(6,*)"final max",(clwmax(la,n,is,isec,1),la=1,6)
            enddo
 7000    CONTINUE
         if(ind_dhcl.GT.0) then
            write(6,*)' sector',isec,' : DHCL #data=',IW(ind_dhcl)/4
            do j=0,IW(ind_dhcl)/4-1,1
               write(6,'(4(2X,Z8))')(IW(ind_dhcl+j*4+k),k=1,4)
            enddo
         endif
      ENDIF
*
 31   FORMAT(//' LEVEL=',I2,'  Sector=',I2,'  (DC Segments & Clusters)')   
 32   FORMAT(/' SLayer=',I2,'  Nsegm=',I2,'  Nclus=',I2)
 33   FORMAT(' isgm    La1  La2  La3  La4  La5  La6   iclus')
 34   FORMAT(I5,2X,6I5,4X,I4)
 35   FORMAT('Wire ',i4,' out of range in sector ',i1,' : Check ana_segm !')  

 9999 if (ifail.ne.0) call ana_statb(2,isec,0.) 
 
      RETURN
 
      END



