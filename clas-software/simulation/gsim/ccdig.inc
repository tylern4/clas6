c  ccdig.inc

c  $Id: ccdig.inc,v 1.2 1997/04/24 15:38:06 holtrop Exp $

      character*132 ciccdig
      data ciccdig/
     1'$Id: ccdig.inc,v 1.2 1997/04/24 15:38:06 holtrop Exp $'
     1/


C===COMMON/CCdig/======================================================
C
C Results of the Cerenkov Counter digitalization (simulation).
C       A. Vlassov 05/03/94
C=======================================================================
C
C -- Arrays in the COMMON/CCdig/
C
C  max_n_hit_CC - Maximun number of hitted tracks in event
C  n_hits_CC    - number of hits
C  n_SECT_CC () - SECTOR #
C  n_CC_hit  () - CC_section #
C                 (+ rigth; - left from MIDDLE plane, view from target)
C  I_TDC_CC  () - Total time of flight from target to CC_PMT in 50 ps/ch
C  I_ADC_CC  () - NUMBER of registered photo-electrons in PMT (1 ph_e/ch)
C  I_TRACK_N () - Track # in the event
C
C  CM_TO_CH     - Factor for calculating TOF in channels (50 ps)
C=======================================================================
C
      INTEGER max_n_hit_CC, n_hits_CC, n_SECT_CC, n_CC_hit
      INTEGER I_TDC_CC, I_ADC_CC, I_TRACK_N
      REAL CM_TO_CH
C
      PARAMETER (max_n_hit_CC = 100)
      PARAMETER (CM_TO_CH = 0.667128190396)
C
      COMMON/CCdig/   n_hits_CC,
     *                n_SECT_CC(max_n_hit_CC),
     2                n_CC_hit (max_n_hit_CC),
     3                I_TDC_CC (max_n_hit_CC),
     4                I_ADC_CC (max_n_hit_CC),
     5                I_TRACK_N(max_n_hit_CC)
C
C--- End of ccdig common
C
