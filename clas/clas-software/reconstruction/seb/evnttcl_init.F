      SUBROUTINE EVNTTCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: evnttcl_init.F,v 1.22 2002/06/27 20:53:43 stepanya Exp $
c
c  Documentation for subroutine EVNTTCL_INIT
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:         Created:  Mon Mar 31 13:31:41 EST 1997
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
      PARAMETER (CFILE = '$RCSfile: evnttcl_init.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.22 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2002/06/27 20:53:43 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: evnttcl_init.F,v 1.22 2002/06/27 20:53:43 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EVNTTCL_INIT')
      PARAMETER (CRAUTH=' ')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      Character*32  my_string
      INTEGER ITCL_INT, ITCL_STRING, ITCL_DOUBLE, i
      DATA ITCL_INT, ITCL_DOUBLE, ITCL_STRING /1,2,4/
c_end_var
c
c  executable code for routine EVNTTCL_INIT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Initialization of tcl for event builder.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c set seb defaults
      call evnt_set_def()
c
      call tcl_linkvar('RFcorrection',iRFpcorr,itcl_int)
c
      call tcl_linkvar('photon_trig_type',photon_trig_type,itcl_int)
c
      call tcl_linkvar('neut_trigg_photon',neut_trigg_photon,itcl_int)
c
      call tcl_linkvar('maxel',maxel,itcl_int)
c
      call tcl_linkvar('trigger_particle',trigger_particle,itcl_int)
c
      call tcl_linkvar('trigger_particle_s',trigger_particle_s,itcl_int)
c
      call tcl_linkvar('CUT_T_SCCC',dCUT_T_SCCC,itcl_double)
c
      call tcl_linkvar('CUT_T_SCEC',dCUT_T_SCEC,itcl_double)
c
      call tcl_linkvar('CUT_T_ECCC',dCUT_T_ECCC,itcl_double)
c
      call tcl_linkvar('CUT_B_SC',dCUT_B_SC,itcl_double)
c
      call tcl_linkvar('CUT_B_EC',dCUT_B_EC,itcl_double)
c
      call tcl_linkvar('sampl_frac',dsampl_frac,itcl_double)
c
      call tcl_linkvar('sigma_etrk',dsigma_etrk,itcl_double)
c
      call tcl_linkvar('m2_ecel_cut',dm2_ecel_cut,itcl_double)
c
      call tcl_linkvar('m3_ecel_cut',dm3_ecel_cut,itcl_double)
c
      call tcl_linkvar('etot_ecel_cut',detot_ecel_cut,itcl_double)
c
      call tcl_linkvar('eio_ecel_cut',deio_ecel_cut,itcl_double)
c
      call tcl_linkvar('trkcc_match',dtrkcc_match,itcl_double)
c
      call tcl_linkvar('trksc_match',dtrksc_match,itcl_double)
c
      call tcl_linkvar('trkec_match',dtrkec_match,itcl_double)
c
      call tcl_linkvar('eccc_match',deccc_match,itcl_double)
c
      call tcl_linkvar('ecsc_match',decsc_match,itcl_double)
c
      call tcl_linkvar('ecec_match',decec_match,itcl_double)
c
c
      call tcl_linkvar('trklec_match',dtrklec_match,itcl_double)
c
      call tcl_linkvar('leccc_match',dleccc_match,itcl_double)
c
      call tcl_linkvar('lecsc_match',dlecsc_match,itcl_double)
c
      call tcl_linkvar('lecec_match',dlecec_match,itcl_double)
c
      call tcl_linkvar('st_tagger_match',dst_tagger_match,itcl_double)
c
      do i=1,3
        write(my_string,'(A,I1,A)')'TargetPos(',i,')'
        call Tcl_Linkvar(my_string(1:12),dTargetPos(i),itcl_DOUBLE)
      enddo
c
c
      RETURN
      END
c
c------------------------------------------------------------------------------




