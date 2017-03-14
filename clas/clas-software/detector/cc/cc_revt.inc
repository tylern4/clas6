c      include file CC_REVT
c
c_begin_doc
c  RCS ID string
c  $Id: cc_revt.inc,v 1.4 2005/11/23 21:04:04 vlassov Exp $
c
c  Documentation for include file CC_REVT
c
c  Purpose:
c  --------
C   Unpacked EVENT for RECONSTRACTION procedure.
C       Alexander Vlassov 04/15/98
C=======================================================================
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:   Alexander Vlassov    Created:  Fri Jun 21 18:41:47 EDT 1996
c  -------
c_end_doc
c
c_begin_var
c  variables for include file CC_REVT:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CICC_REVT
      DATA CICC_REVT/
     1'$Id: cc_revt.inc,v 1.4 2005/11/23 21:04:04 vlassov Exp $'
     2/
c----6----------------------------------------------------------------72
c  
C===COMMON/cc_revt/======================================================
C
C
C -- Arrays in the COMMON/cc_revt/
C
C  max_n_hit_CC - Maximun number of hitted tracks in event (100)
C  n_hits_CC    - number of hits
C  n_SECT_CC () - SECTOR #
C  n_CC_hit  () - PMT # (1 - 36)
C  I_TDC_CC  () - Total time of flight from target to CC_PMT in 50 ps/ch
C  I_ADC_CC  () - NUMBER of registered photo-electrons in PMT (1 ph_e/ch)
C  CM_TO_CH     - Factor for calculating TOF in channels (50 ps)
C=======================================================================
C
      INTEGER max_n_hit_CC, n_hits_CC, n_SECT_CC, n_CC_hit
      INTEGER I_TDC_CC, I_ADC_CC
      REAL CM_TO_CH
C
      PARAMETER (max_n_hit_CC = 20)
      PARAMETER (CM_TO_CH = 0.667128190396)
C
      COMMON /cc_revt/   n_hits_CC,
     *                   n_SECT_CC(max_n_hit_CC),
     2                   n_CC_hit (max_n_hit_CC),
     3                   I_TDC_CC (max_n_hit_CC),
     4                   I_ADC_CC (max_n_hit_CC)
C
C--- End of cc_revt common
c_end_var
c




