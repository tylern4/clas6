c      include file EVNT_PAR
c
c_begin_doc
c  RCS ID string
c  $Id: evnt_par.inc,v 1.18 2008/03/16 04:33:23 fklein Exp $
c
c  Documentation for include file EVNT_PAR
c
c  Purpose:
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:         Created:  Tue Mar 25 00:41:53 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file EVNT_PAR:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIEVNT_PAR
      DATA CIEVNT_PAR/
     1'$Id: evnt_par.inc,v 1.18 2008/03/16 04:33:23 fklein Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in EVNT_PAR common block
c  --------------------------------------------
c
c_end_var
c
      REAL vlight
      parameter (vlight= 29.9792458)   !cm/nS
c                            ! to output EVNT bank. 
      REAL CUT_T_SCCC      ! Cut on CC-SC time for coincidence.
      REAL CUT_T_SCEC      ! Cut on EC-SC time for coincidence.
      REAL CUT_T_ECCC      ! Cut on CC-EC time for coincidence.
      REAL CUT_B_SC        ! Cut on betta for SC electron hit.
      REAL CUT_B_EC        ! Cut on betta for EC electron(photon) hit.
      REAL sampl_frac      ! Sampling fraction for showering particles in EC.
      REAL sigma_etrk      ! Momentum resolution of tracking for electron ID 
c                            ! in EC (needs for correct cuts).
      REAL m2_ecel_cut     ! Cut on second moments of shower transfer 
c                            ! distribution in EC.
      REAL m3_ecel_cut     ! Cut on third moments of shower transfer 
c                          ! distribution in EC.
      REAL etot_ecel_cut   ! Cut on total energy of showers 
c                          ! in EC (1.-1sigma, 2.-2sigma...).
      REAL eio_ecel_cut    ! Cut on longitudinal energy distribution 
c                            ! of showers in EC (1.-1sigma, 2.-2sigma...).
      REAL trkcc_match     ! Minimal allowed diviation for track-CC matching.
      REAL trksc_match     ! Minimal allowed diviation for track-SC matching.
      REAL trkec_match     ! Minimal allowed diviation for track-EC matching.
      REAL trklec_match     ! Minimal allowed diviation for track-LAC matching.
      REAL eccc_match      ! Minimal allowed diviation for EC-CC matching.
      REAL ecsc_match      ! Minimal allowed diviation for EC-SC matching.
      REAL ecec_match      ! Minimal allowed diviation for EC layers matching.
      REAL leccc_match      ! Minimal allowed diviation for LAC-CC matching.
      REAL lecsc_match      ! Minimal allowed diviation for LAC-SC matching.
      REAL lecec_match      ! Minimal allowed diviation for LAC layers matching.
      REAL mtrigg,qtrigg    ! mass and charge of the first trigger particle
      REAL mtrigg_s,qtrigg_s ! mass and charge of the second trigger particle
      REAL bt_Z_center       ! Center of the beam-target interaction in the beam direction
      REAL st_tagger_match   ! maximum allowed deviation for ST and TAGGER

      INTEGER trigger_particle    ! trigger particle ID (Def. =11 electron) 
      INTEGER trigger_particle_s  ! second trigger particle (Def. =0 No second
c - particle, if set then .OR. with first) 
      INTEGER MaxEl          ! Allowed Max number of found electrons (or 
c                            ! triggered particles) for tigger to chose from.
      INTEGER photon_trig_type  ! electron beam=0, photon beam=1 
      INTEGER neut_trigg_photon ! number of neutral hits 
c     for triggering (photon beam runs), if =0 no neutral trigger
c     Default always start from HB tracking results
      INTEGER iRFpcorr          ! RF pulse correction method, =0 4 functions,
c     =1 100 nummbers, means of Gaussian fits for each 1ns bin.
c
      COMMON/EVNT_PAR/ MaxEl,CUT_T_SCCC,CUT_T_SCEC,CUT_B_SC,CUT_B_EC
     &,CUT_T_ECCC,sampl_frac, sigma_etrk, m2_ecel_cut, m3_ecel_cut,
     &etot_ecel_cut,eio_ecel_cut,trkcc_match,trksc_match,trkec_match,
     &trklec_match,eccc_match,ecsc_match,ecec_match,photon_trig_type,bt_Z_center,
     &leccc_match,lecsc_match,lecec_match,trigger_particle,neut_trigg_photon,
     &trigger_particle_s,mtrigg,qtrigg,mtrigg_s,qtrigg_s,st_tagger_match,iRFpcorr
      
c----6----------------------------------------------------------------72




