      SUBROUTINE TBT_SEGM(isec,road_width,trk_wire,nsegm,segm)
c
c_begin_doc
c  RCS ID string
c  $Id: tbt_segm.F,v 1.4 2000/01/06 15:19:18 mueller Exp $
c
c  Documentation for subroutine TBT_SEGM
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
c  Author:   James Mueller      Created:  Thu Apr  2 08:23:04 EST 1998
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
#include "sdadigi.inc"
#include "sdaswim.inc"
#include "analink.inc"
#include "clas_offsets.inc"
#include "wcs.inc"
c_end_inc
c
c_begin_var
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: tbt_segm.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2000/01/06 15:19:18 $')
      PARAMETER (CAUTHO = '$Author: mueller $')
      DATA CRCSID/   
     1'$Id: tbt_segm.F,v 1.4 2000/01/06 15:19:18 mueller Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TBT_SEGM')
      PARAMETER (CRAUTH='James Mueller')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER no_hit, nlay, nwmx, nsuper, nsgmx
      PARAMETER (no_hit = 0, nlay = 6, nwmx = 192, nsuper = 6, nsgmx = 30)
      INTEGER i, k, ifail
      INTEGER ifound, mx_w, mn_w, iw2, mxwire
      INTEGER ih, il, ilmin, is, iw, iw3, its, isg, isum
      INTEGER la, nd, nd3, max_hit_seg, min_hit_seg, ihit_seg
      INTEGER nw(nlay),nwall(nlay,nsgmx),nwhit(nsgmx)
      INTEGER nwflag(nsgmx), idtime(nlay,nwmx), pt_row(nlay,nwmx)
      INTEGER NLAYinSL(nsuper)
      DATA NLAYinSL /4,6,6,6,6,6/
c
c  input/output variables:
c  -----------------------
c
      INTEGER isec,trk_wire(nlay*nsuper),road_width
      integer segm(2*nlay,nsgmx,nsuper),nsegm(nsuper)
c
c_end_var
C BOS manipulation
      integer nami_dcgm, ind_dcgm(6), ncol_dcgm, check, ind

      integer MAMIND, GMind, MaxWire, sector, layer, baseind
c Functions to get indices for a given layer
      GMind(sector,layer) = ind_dcgm(sector) + (layer-1)*ncol_dcgm
      MaxWire(baseind) = JW(baseind+DCGM_max_wire)
c
c  executable code for routine TBT_SEGM:
c----6----------------------------------------------------------------72
c
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
*
         nsegm(is) = 0
         min_hit_seg = 1
         max_hit_seg = NLAYinSL(is)

*
*              set nd3 to the maximum number of wires in this plane.
*
         nd3 = MaxWire(GMind(isec,ilmin+3))

*
C Initialization of idtime(6,192) for given SuperLayer
*
         Call Vzero(Idtime(1,1),(nwmx*nlay))
*
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
                  iw = dc_digi(1,ih,il,isec)
                  IF(iw.GE.1 .AND. iw.LE.mxwire) THEN
*
                     idtime(la,iw) = dc_digi(2,ih,il,isec)
*
*--c.a.meyer march 1997: keep track of row number in DC0.
*
                     pt_row(la,iw) = pt_digi(ih,il,isec)
*--
*
                  ENDIF
 1000          CONTINUE
            ENDIF
 1100    CONTINUE
*
C Loop over the layers
*
         ifound = 0
*
*          Note the following loop was over all 192 wires. We now
*          try and speed up the search by only looking over the
*          range where data exist. However, for generating the
*          patterns, leave this alone:
*
            mn_w = trk_wire(ilmin+3)-int((road_width-1)/2)
            mx_w = trk_wire(ilmin+3)+int(road_width/2)
*     
         DO 2400 iw3 = mn_w,mx_w
*
*             Segment generation:
*
*
*             Data handling:
*
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
 1600       DO 2300 isg=1,npat
*
              IF(max_hit_seg.EQ.3 .AND. ipat_flg3(isg).EQ.1) GO TO 2300
              IF(max_hit_seg.EQ.4 .AND. ipat_flg4(isg).EQ.1) GO TO 2300
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
*                   in this mask. This leads to iw being the expected
*                   wire number in the current layer for the current
*                   mask. We then initially tag the nw array as the
*                   negative of this wire number to indicate the is
*                   no hit -- i.e. generate a pseudo-hit.
*
                 iw = iw3 + ipat(la,isg)
*
*                   Make sure that iw is within the range of allowed
*                   wire numbers (1 to 192) for this layer.
*
                 IF(iw.LT.(trk_wire(il)-int((road_width-1)/2))) go to 2300
                 IF(iw.GT.(trk_wire(il)+int(road_width/2))) go to 2300
                 
                 IF(iw.LE.0) THEN
                    nw(la) = -1
                 ELSE IF (iw.GT.MaxWire(GMind(isec,il))) THEN
                    nw(la) = -ABS(MaxWire(GMind(isec,il)))
                 ELSE
                    nw(la) = -IABS(iw)
*
*                      Check to see if there is a hit on this wire.
*                      If there is, then set nw to contain the wire number,
*                      and add one to the total number of matched hits
*                      in this mask.
*
                    IF(idtime(la,iw).GT.no_hit) THEN
                       nw(la) = iw
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
**
*                If we have found previous segments based on this
*                reference hit, then we want to compare with previous
*                found segments. We loop over all previous segments,
*                and see how many hits are shared with those. This
*                loop keeps a new segment as long as "all" its hits are
*                not shared with a previous segment.
*
              IF((ifound.GT.0).and.(ifound.le.nsgmx)) THEN
*     
                 DO 2100 i = 1,ifound
*     
                    IF(nwflag(i).ge.0)THEN
                       its = 0
*     
                       DO 1900 la = 1,nlay
                          IF(nw(la).GT.0.AND.nw(la).EQ.nwall(la,i)) its = its+1
 1900                  CONTINUE
*     
*                "its" is the number of shared hits between these
*                two segments. If all the hits on the current
*                segment are shared, and that number is no
*                bigger than the previous segment, then 
*                discard the current segment.
* 
                       IF((its.EQ.isum).AND.(isum.LE.nwhit(i))) GO TO 2300
C     
C     Flag the previous segment as bad if the current is a superset of it. 
C
                       IF((its.EQ.nwhit(i)).AND.(isum.GT.nwhit(i)))nwflag(i)=-1
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
              ifound = ifound + 1
*
              IF(ifound .LT. nsgmx) THEN
                 DO 2200 la =1,nlay
                    nwall(la,ifound) = nw(la)
 2200            CONTINUE
                 nwhit(ifound) = isum
                 nwflag(ifound)= 0
              ENDIF
*     
c     fr 10-31-1996
*     
              if(ifound.ge.nsgmx) go to 2401
*     
 2300      CONTINUE             ! End of loop over templatets
 2400   CONTINUE                ! End of loop over the hits in layer 2 & 3
 2401   Continue
*
*
*
C Cleaning "nwall(ifound)" by setting flag "nwflag(ifound)=1 for good segment and Search for Segment Clusters
        IF((ifound.GT.0).and.(ifound.le.nsgmx)) THEN
*
*
           DO 3300 ihit_seg = max_hit_seg, min_hit_seg, -1
*
              DO 2500 i = 1, ifound
                 IF(nwhit(i).EQ.ihit_seg.AND.nwflag(i).GE.0) THEN
                    nsegm(is) = nsegm(is) + 1
                    k=1
                    DO 3100 la = 1,nlay
*     
*     Get wire number and make sure it is valid:
*     
                       iw = nwall(la,i)
                       if(IABS(iw).gt.0.and.IABS(iw).le.192) then
                          iw2 = iabs(iw)
*     
                          segm(k  ,nsegm(is),is) = iw
                          segm(k+1,nsegm(is),is) = idtime(la,iw2)
*     
*--   c.a.meyer march 1997: keep track of row number in DC0.
*     
c     if(pt_row(la,iw2).GT.0) then
c     segm_rp(((k+1)/2),its,is,isec)  = pt_row(la,iw2)
c     else
c     segm_rp(((k+1)/2),its,is,isec)  = -iw2
c     endif
*--   
                       else
c     *
c     *                            An error condition was encountered:
c     *
                          write(CRMESS,35) iw,isec
                          call RECMES(crname,'I',crmess)
                          segm(k,its,is) = -iw2
                          segm(k+1,its,is) = -8888
c     segm_rp(((k+1)/2),its,is,isec) = -iw2
                       endif
*     
                          k = k + 2
 3100               CONTINUE
*                    
                 ENDIF
                 
 2500         CONTINUE
*     
 3300      CONTINUE
        ENDIF
 6000 CONTINUE                  ! End of loop over superlayers
        
*     
C     Report clusters/segments information for each Superlayer
*     
c      IF(ipar_trk_print(2).GE.1) THEN
c         Level = 1
c         WRITE(6,31) isec        
c         DO 7000 is = 1,nsuper
c*
c            WRITE(6,32) is,nsegm(is)
c            IF(nsegm(is).EQ.0) GO TO 7000
c            WRITE(6,33)
c*
c            DO 6900 isg = 1,nsegm(is)
c*
c 6800          WRITE(6,34) isg,(segm(i,isg,is),i=1,11,2)
c*
c 6900       CONTINUE
c*
c 7000    CONTINUE
c      ENDIF

 31   FORMAT(//' Sector=',I2,'  TBT Segments')   
 32   FORMAT(/' SLayer=',I2,'  Nsegm=',I2)
 33   FORMAT(' isgm    La1  La2  La3  La4  La5  La6')
 34   FORMAT(I5,2X,6I5)
 35   FORMAT('Wire ',i4,' out of range in sector ',i1,' : Check tbt_segm !')

 9999 RETURN
      END
c
c------------------------------------------------------------------------------




