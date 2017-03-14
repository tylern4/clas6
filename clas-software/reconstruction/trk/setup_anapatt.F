      SUBROUTINE setup_anapatt()
C
C
C zero out the anapatt common block - J. J. Manak, March 1998
C
C
C***********INCLUDE FILES***************
#include "anapatt.inc"
*
****************************************
*
******************************
*  Set to zero the number of the track candidates, reconstructed tracks
*  a bosdump shows several not_a_numbers (for security initialize all arrays)

      call vzero(trk,12*36*MAX_TRACKS)
      call vzero(trkp,12*nplane*MAX_TRACKS)
      call vzero(trk_pt,36*MAX_TRACKS)
      call vzero(trk_err,20*MAX_TRACKS)
      call vzero(segm_rp,6*36*nsgmx)
      call vzero(itr_sect,MAX_TRACKS)
      call vzero(itr_level,MAX_TRACKS)
      call vzero(lnk_clust,6*MAX_TRACKS)
      call vzero(lnk_segm,6*MAX_TRACKS)
      call vzero(dca_segm,6*MAX_TRACKS)
      call vzero(segm,12*36*nsgmx)
      call vzero(nsegmc,36*nclmx)
      call vzero(nsegm,36)
      call vzero(nclust,36)
      call vzero(clwmin,432*nclmx)
      call vzero(clwmax,432*nclmx)
      ntr_link = 0

*****************************************************
      RETURN
      END







