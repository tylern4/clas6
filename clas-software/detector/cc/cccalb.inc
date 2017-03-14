c      include file CCCALB
c
c_begin_doc
c  RCS ID string
c  $Id: cccalb.inc,v 1.2 2005/11/23 21:01:43 vlassov Exp $
c
c  Documentation for include file CCCALB
c
c  Purpose:  Stored calibration constants   
c  --------
c
c  Author:   Alexander Vlassov    Created:  Thu Oct 10 16:40:14 EDT 1996
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c_begin_var
c  variables for include file CCCALB:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CICCCALB
      DATA CICCCALB/
     1'$Id: cccalb.inc,v 1.2 2005/11/23 21:01:43 vlassov Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in CCCALB common block
c  --------------------------------------------
c
c
      integer mccsegm,   ! Max number of CC segments
     &        kcciden,   ! 1000*Sector + Segment (1-36)
     &        kccpmtn,   ! Unique PMT number
     &        kccrunn    ! Calibration run number
      integer itdc_min, itdc_max ! TDC valid range ( in channels)
      integer iadc_min, iadc_max ! ADC valid range ( in channels)
      real    xcct2ch,   ! Time to chann. coeff. ( ~ 0.05 )
     &        opheamp,   ! One ph.e. amplitude
     &        dpheamp,   ! One ph.e. amplitude dispersion
     &        ccpedes,   ! Amplitude pedestal
     &        acctime,   ! Additive time constant
     &        dcctime,   ! Additive time constant dispersion
     &        rccsens    ! Normalized PMT sensib. for Cerenkov photons
c
      parameter ( mccsegm = 36 )   !  Number of CC segments
c
c_end_var
c
      common /CCCALB/ kcciden(6,mccsegm),xcct2ch(6,mccsegm),
     & opheamp(6,mccsegm), dpheamp(6,mccsegm), ccpedes(6,mccsegm),
     & acctime(6,mccsegm), dcctime(6,mccsegm), kccpmtn(6,mccsegm),
     & rccsens(6,mccsegm), kccrunn(6,mccsegm)
c
      common /CCCONS/ itdc_min, itdc_max, iadc_min, iadc_max
c    
c ============================================================
c



