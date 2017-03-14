      SUBROUTINE TRK_CLMERGE(icl1,icl2,seg_hits,isl,isec)
c
c_begin_doc
c  RCS ID string
c  $Id: trk_clmerge.F,v 1.2 1998/01/29 16:39:20 marki Exp $
c
c  Documentation for subroutine TRK_CLMERGE
c
c  Purpose:
c  --------
c  merge two tracking clusters into one, keeping output cluster sorted by
c  decreasing number of hits/segment.
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c  icl1,icl2 : index of cluster1 and 2, 2 gets merged into 1
c  isl, isec : superlayer and sector of clusters
c  seg_hits  : array of number of hits/segment for all segments in this
c              superlayer and sector
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c  clust and nsgmc arrays in the include file anapatt.inc get altered
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   James Mueller      Created:  Mon Nov 10 15:26:17 EST 1997
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
#include "anapatt.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      integer icl1,icl2,isl,isec
      integer seg_hits(nsgmx)
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: trk_clmerge.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/01/29 16:39:20 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: trk_clmerge.F,v 1.2 1998/01/29 16:39:20 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='TRK_CLMERGE')
      PARAMETER (CRAUTH='James Mueller')
c
c  Local User defined variables:
c  -----------------------------
      integer iseg1,iseg2,pseg1,pseg2,nhit1,nhit2
      integer ntemp,tempcl(nsgcmx)
c_end_var
c
c  executable code for routine TRK_CLMERGE:
c----6----------------------------------------------------------------72
c
c initialize
c set pointer to the first segments in cluster 1 and 2,
c get pointer to the seg_hits(array),
c get the number of hits in these segments, and
c set the number of segments in the merged cluster to 0
c
      iseg1 = 1
      iseg2 = 1
      pseg1 = clust(iseg1,icl1,isl,isec)
      pseg2 = clust(iseg2,icl2,isl,isec)
      nhit1 = seg_hits(pseg1)
      nhit2 = seg_hits(pseg2)
      ntemp = 0

 100  continue
c
c Do we still have segments to merge in
c and is there still room for more in the merged cluster?
c 
      if((nhit1.gt.0.or.nhit2.gt.0).and.ntemp.lt.nsgcmx)then
         ntemp = ntemp + 1
c
c Choose the current segment with the most hits
c add it to the temporary array and
c get the info for the next segment in that cluster.
c If we are at the end of this cluster set nhit# to zero as end-of-clust flag
c
         if(nhit1.ge.nhit2)then
            tempcl(ntemp) = pseg1
            if(iseg1.lt.nsegmc(icl1,isl,isec))then
               iseg1 = iseg1 + 1
               pseg1 = clust(iseg1,icl1,isl,isec)
               nhit1 = seg_hits(pseg1)
            else
               nhit1 = 0
            endif
         else
            tempcl(ntemp) = pseg2
            if(iseg2.lt.nsegmc(icl2,isl,isec))then
               iseg2 = iseg2 + 1
               pseg2 = clust(iseg2,icl2,isl,isec)
               nhit2 = seg_hits(pseg2)
            else
               nhit2 = 0
            endif
         endif
c
c Go back to the top and check the new pair of segments
c
         goto 100
      else
c
c No more segments to merge in, or
c the temporary array is already full, so
c copy it back into cluster1 and empty cluster2
c
c Should add a warning message here if we have maxed out on output segments
c
         do iseg1=1,ntemp
            clust(iseg1,icl1,isl,isec) = tempcl(iseg1)
         enddo
         nsegmc(icl1,isl,isec) = ntemp
         nsegmc(icl2,isl,isec) = 0
      endif
      RETURN
      END
c
c------------------------------------------------------------------------------




