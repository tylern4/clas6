c      include file DTCL_VAR
c
c_begin_doc
c  RCS ID string
c  $Id: dtcl_var.inc,v 1.6 2001/08/14 20:54:08 stepanya Exp $
c
c  Documentation for include file DTCL_VAR
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
c  Author:  S.Stepanyan       Created:  Tue Apr  8 20:29:53 EDT 1997
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
c  variables for include file DTCL_VAR:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIDTCL_VAR
      DATA CIDTCL_VAR/
     1'$Id: dtcl_var.inc,v 1.6 2001/08/14 20:54:08 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in DTCL_VAR common block
c  --------------------------------------------
c
      REAL*8 dCUT_T_SCCC      ! Cut on CC-SC time for coincidence.
      REAL*8 dCUT_T_SCEC      ! Cut on EC-SC time for coincidence.
      REAL*8 dCUT_T_ECCC      ! Cut on CC-EC time for coincidence.
      REAL*8 dCUT_B_SC        ! Cut on betta for SC electron hit.
      REAL*8 dCUT_B_EC        ! Cut on betta for EC electron(photon) hit.
      REAL*8 dsampl_frac      ! Sampling fraction for showering particles in EC.
      REAL*8 dsigma_etrk      ! Momentum resolution of tracking for electron ID 
c                            ! in EC (needs for correct cuts).
      REAL*8 dm2_ecel_cut     ! Cut on second moments of shower transfer 
c                            ! distribution in EC.
      REAL*8 dm3_ecel_cut     ! Cut on third moments of shower transfer 
c                          ! distribution in EC.
      REAL*8 detot_ecel_cut   ! Cut on total energy of showers 
c                          ! in EC (1.-1sigma, 2.-2sigma...).
      REAL*8 deio_ecel_cut    ! Cut on longitudinal energy distribution 
c                            ! of showers in EC (1.-1sigma, 2.-2sigma...).
      REAL*8 dtrkcc_match     ! Minimal allowed diviation for track-CC matching.
      REAL*8 dtrksc_match     ! Minimal allowed diviation for track-SC matching.
      REAL*8 dtrkec_match     ! Minimal allowed diviation for track-EC matching.
      REAL*8 dtrklec_match     ! Minimal allowed diviation for track-EC matching.
      REAL*8 deccc_match      ! Minimal allowed diviation for EC-CC matching.
      REAL*8 decsc_match      ! Minimal allowed diviation for EC-SC matching.
      REAL*8 decec_match      ! Minimal allowed diviation for EC layers matching.
      REAL*8 dleccc_match      ! Minimal allowed diviation for EC-CC matching.
      REAL*8 dlecsc_match      ! Minimal allowed diviation for EC-SC matching.
      REAL*8 dlecec_match      ! Minimal allowed diviation for EC layers matching.
      REAL*8 dst_tagger_match   ! maximum allowed deviation for ST and TAGGER
      REAL*8 dTargetPos(3)     ! target postion
c_end_var
c
       COMMON/DTCL_VAR/dCUT_T_SCCC,dCUT_T_SCEC,dCUT_B_SC,dCUT_B_EC
     &,dCUT_T_ECCC,dsampl_frac, dsigma_etrk,dm2_ecel_cut,dm3_ecel_cut,
     &detot_ecel_cut,deio_ecel_cut,dtrkcc_match,dtrksc_match,
     &dtrkec_match,deccc_match,decsc_match,decec_match, dTargetPos,
     &dtrklec_match,dleccc_match,dlecsc_match,dlecec_match,dst_tagger_match
c----6----------------------------------------------------------------72




