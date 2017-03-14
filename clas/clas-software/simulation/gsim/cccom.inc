c  cccom.inc

c  $Id: cccom.inc,v 1.2 1997/04/24 15:38:05 holtrop Exp $

      character*132 cicccom
      data cicccom/
     1'$Id: cccom.inc,v 1.2 1997/04/24 15:38:05 holtrop Exp $'
     1/


c  -----------------------------
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL CCDRAW
c
      integer mat_cc_gas,mat_mirr_cc,mat_sh1,mat_sh2
      integer med_cc_gas,med_mirr_cc,med_sh1,med_sh2
      common /CCNUMB/  mat_cc_gas,mat_mirr_cc,mat_sh1,mat_sh2,
     &                 med_cc_gas,med_mirr_cc,med_sh1,med_sh2
c
      integer mhtrk,   ! Max number of tracks
     &        jhtrk,   ! Number of tracks in the event
     &     itrk_num,   ! # track in the event (1000*istak + itrack ) 
     &      idh_prt,   ! Particle ID
     &      ihitrk,    ! Number of photoelectrons produced by track
     &      itrsect,   ! Sector #
     &      itrsegm    ! Segment #
c
      real xenter,     ! Track entering point coordinates in Sector system
     &      xexit,     ! Track exit point coordinates in Sector system
     &     ph_tot      ! Particle momentum

      parameter ( mhtrk  = 50)       !  Number of tracks

      integer  mcchit, ! Max number of hits
     &       mbos_cch  ! Max size of the buffer for BOS - output 
      parameter ( mcchit = 400)      !  Number of hits
      parameter (mbos_cch = 5000 )   !  Max number of words in hits in CC
c
      integer icchit,  ! Number of hits in event
     &        itrnum,  ! Number if track for this hit
     &       iccsect,  ! Sector #
     &       iccsegm,  ! Segment #
     &        icctdc   ! ITDC
c
      integer jhit_cc
      real  xhit_cc(mbos_cch)
      equivalence(xhit_cc,jhit_cc)
c
      integer mccsegm,   ! Max number of CC segments
     &        kcciden,   ! 1000*Sector + Segment (1-36)
     &        kccpmtn,   ! Unique PMT number
     &        kccrunn    ! Calibration run number
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
      common /CCHIT1/ jhtrk, itrk_num(mhtrk), idh_prt(mhtrk),
     &                ihitrk(mhtrk), itrsect(mhtrk), itrsegm(mhtrk),
     &                ph_tot(mhtrk), xenter(3,mhtrk), xexit(3,mhtrk)
      common /CCHIT2/ icchit, itrnum(mcchit), iccsect(mcchit),
     & iccsegm(mcchit), icctdc(mcchit)
c
      common /CCJHIT/jhit_cc(mbos_cch)
c
      common /CCCALB/ kcciden(6,mccsegm),xcct2ch(6,mccsegm),
     & opheamp(6,mccsegm), dpheamp(6,mccsegm), ccpedes(6,mccsegm),
     & acctime(6,mccsegm), dcctime(6,mccsegm), kccpmtn(6,mccsegm),
     & rccsens(6,mccsegm), kccrunn(6,mccsegm)
c    
c =========================================================
