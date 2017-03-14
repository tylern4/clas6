      SUBROUTINE EVNT_SET_DEF
c
c_begin_doc
c  RCS ID string
c  $Id: evnt_set_def.F,v 1.24 2008/03/22 20:57:22 fklein Exp $
c
c  Documentation for subroutine EVNT_SET_DEF
c
c  Purpose: To set the defaults for the seb package
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c  NONE
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c  evnt_par.inc, dtcl_var.inc common blocks filled
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author: Manak        Created:  Mon Mar 31 13:31:41 EST 1997
c  -------
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "evnt_par.inc"
#include "dtcl_var.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: evnt_set_def.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.24 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/22 20:57:22 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: evnt_set_def.F,v 1.24 2008/03/22 20:57:22 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EVNT_SET_DEF')
      PARAMETER (CRAUTH='MANAK')
c
c  Local User defined variables:
c  -----------------------------
      Integer i
c_end_var
c
      iRFpcorr = 0
c
      photon_trig_type = -1
c
      maxel = 5
c
      trigger_particle = 11
c
      trigger_particle_s = -11
c
      neut_trigg_photon = 0
c
      dCUT_T_SCCC = 50.
c
      dCUT_T_SCEC = 50.
c
      dCUT_T_ECCC = 50.
c
      dCUT_B_SC = 1.
c
      dCUT_B_EC = 1.
c
      dsampl_frac = 0.272
c
      dsigma_etrk = 0.005
c
      dm2_ecel_cut = 1200.
c
      dm3_ecel_cut = 5000.
c
      detot_ecel_cut = 5.
c
      deio_ecel_cut = 5.
c
      dtrkcc_match = 5.
c
      dtrksc_match = 30.
c
      dtrkec_match = 30.
c
      deccc_match = 5.
c
      decsc_match = 30.
c
      decec_match = 10.
c
c
      dtrklec_match = 30.
c
      dleccc_match = 5.
c
      dlecsc_match = 30.
c
      dlecec_match = 10.
c
      dst_tagger_match = 15.
c
      do i=1,3
         dTargetPos(i)=-9999.
      enddo
c
      RETURN
      END
c
c------------------------------------------------------------------------------




