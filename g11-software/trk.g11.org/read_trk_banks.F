      SUBROUTINE READ_TRK_BANKS(Ierr)
C
C
C
C
C
C
C
C
C
C
C
C***********INCLUDE FILES***************
#include "bcs.inc"
#include "analink.inc"
#include "anapatt.inc"
#include "trktcl_var.inc"
#include "sdadigi.inc"
#include "clasmdl.inc"
*
****************************************
*
      INTEGER SCT
*
*
      INTEGER IERR, Nread
*
*
*
*
******************************
          IERR=0
          call read_mtrk_bank(lnk_segm,lnk_clust,itr_sect,ntr_link,nread)
          IF(nread.LE.0) IERR=IERR-1
          call read_clst_bank(clust,nsegmc,nclust,nread)
          IF(nread.LE.0) IERR=IERR-1          
          call read_sgmp_bank(segm,nsegm,nread)
          IF(nread.LE.0) IERR=IERR-1
          do Linkj=1,ntr_link
             itr_level(Linkj)=1
          Enddo
*****************************************************
      RETURN
      END







