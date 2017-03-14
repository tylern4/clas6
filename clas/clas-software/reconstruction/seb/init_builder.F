      SUBROUTINE INIT_BUILDER
c
c_begin_doc
c  RCS ID string
c  $Id: init_builder.F,v 1.12 2001/02/13 20:15:08 fklein Exp $
c
c  Documentation for subroutine INIT_BUILDER
c
c  Purpose: Initialization of event builder. 
c  --------
c
c  Input Parameters:  Non
c  ----------------
c
c  Output Parameters:  Non
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   S. Stepanyan      Created:  Mon Mar 24 21:11:09 EST 1997
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
#include "bcs.inc"
#include "evnt_general.inc"
#include "evnt_par.inc"
#include "hbt_planes.inc"
#include "dtcl_var.inc"
#include "pdgid.inc"
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
      PARAMETER (CFILE = '$RCSfile: init_builder.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.12 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2001/02/13 20:15:08 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: init_builder.F,v 1.12 2001/02/13 20:15:08 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='INIT_BUILDER')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ITCL_INT, ITCL_STRING, ITCL_DOUBLE
      DATA ITCL_INT, ITCL_DOUBLE, ITCL_STRING /1,2,4/
c_end_var
c
c  executable code for routine INIT_BUILDER:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Initialization of event builder.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      CALL RECMES(CRNAME,'I','Event Builder is initialized-----')
      CALL RECMES(CRNAME,'N','|')
      WRITE(CRMESS,20)maxel
20    FORMAT('| Allowed Maximum number of electrons=',I5)
      CALL RECMES(CRNAME,'N',CRMESS)
      CUT_T_SCCC=sngl(dCUT_T_SCCC)
      WRITE(CRMESS,21)CUT_T_SCCC
21    FORMAT('| Cut on SC-CC time concidence =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      CUT_T_SCEC=sngl(dCUT_T_SCEC)
      WRITE(CRMESS,22)CUT_T_SCEC
22    FORMAT('| Cut on SC-EC time concidence =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      CUT_T_ECCC=sngl(dCUT_T_ECCC)
      WRITE(CRMESS,23)CUT_T_ECCC
23    FORMAT('| Cut on EC-CC time concidence =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      CUT_B_SC=sngl(dCUT_B_SC)
      WRITE(CRMESS,24)CUT_B_SC
24    FORMAT('| Cut on betta for SC electron hit =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      CUT_B_EC=sngl(dCUT_B_EC)
      WRITE(CRMESS,25)CUT_B_EC
25    FORMAT('| Cut on betta for EC electron(photon) =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      sampl_frac=sngl(dsampl_frac)
      WRITE(CRMESS,26)sampl_frac
26    FORMAT('| EC sampling fraction of electromagnic shower =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      sigma_etrk=sngl(dsigma_etrk)
      WRITE(CRMESS,27)sigma_etrk
27    FORMAT('| Tracking momentum resolution for electron selection in EC =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      m2_ecel_cut=sngl(dm2_ecel_cut)
      WRITE(CRMESS,28)m2_ecel_cut
28    FORMAT('| Cut on second moments of shower transvers distribution in EC =',F9.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      m3_ecel_cut=sngl(dm3_ecel_cut)
      WRITE(CRMESS,29)m3_ecel_cut
29    FORMAT('| Cut on third moments of shower transvers distribution in EC =',F12.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      etot_ecel_cut=sngl(detot_ecel_cut)
      WRITE(CRMESS,30)etot_ecel_cut
30    FORMAT('| Cut on showering particle total energy in EC =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      eio_ecel_cut=sngl(deio_ecel_cut)
      WRITE(CRMESS,31)eio_ecel_cut
31    FORMAT('| Cut on longitudinal energy distribution in EC =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      trkcc_match=sngl(dtrkcc_match)
      WRITE(CRMESS,32)trkcc_match
32    FORMAT('| Minimal allowed diviation for track-CC hit matching =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      trksc_match=sngl(dtrksc_match)
      WRITE(CRMESS,33)trksc_match
33    FORMAT('| Minimal allowed diviation for track-SC hit matching =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      trkec_match=sngl(dtrkec_match)
      WRITE(CRMESS,34)trkec_match
34    FORMAT('| Minimal allowed diviation for track-EC hit matching =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      eccc_match=sngl(deccc_match)
      WRITE(CRMESS,35)eccc_match
35    FORMAT('| Minimal allowed diviation for EC-CC hits matching =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      ecsc_match=sngl(decsc_match)
      WRITE(CRMESS,36)ecsc_match
36    FORMAT('| Minimal allowed diviation for EC-SC hits matching =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      ecec_match=sngl(decec_match)
      WRITE(CRMESS,37)ecec_match
37    FORMAT('| Minimal allowed diviation for EC layers hit matching =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
c
      trklec_match=sngl(dtrklec_match)
      WRITE(CRMESS,38)trklec_match
38    FORMAT('| Minimal allowed diviation for track-LAC hit matching =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      leccc_match=sngl(dleccc_match)
      WRITE(CRMESS,39)leccc_match
39    FORMAT('| Minimal allowed diviation for LAC-CC hits matching =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      lecsc_match=sngl(dlecsc_match)
      WRITE(CRMESS,40)ecsc_match
40    FORMAT('| Minimal allowed diviation for LAC-SC hits matching =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      lecec_match=sngl(dlecec_match)
      WRITE(CRMESS,41)lecec_match
41    FORMAT('| Minimal allowed diviation for LAC layers hit matching =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      st_tagger_match=SNGL(dst_tagger_match)
      if (photon_trig_type.ne.-1) then
          call set_beam_type(photon_trig_type)
      endif
c
      if(trigger_particle.eq.11)then
        mtrigg=melectron
        qtrigg=-1.
      elseif(trigger_particle.eq.2212)then
        mtrigg=mproton
        qtrigg=1.
      elseif(trigger_particle.eq.45)then
        mtrigg=mdeuteron
        qtrigg=1.
      elseif(abs(trigger_particle).eq.211)then
        mtrigg=mpion
        qtrigg=trigger_particle/211.
      elseif(abs(trigger_particle).eq.321)then
        mtrigg=mkaon
        qtrigg=trigger_particle/321.
      elseif(trigger_particle.eq.22)then
        mtrigg=mphoton
        qtrigg=0.
      elseif(trigger_particle.eq.2112)then
        mtrigg=mneutron
        qtrigg=0.
      else
        mtrigg=melectron
        qtrigg=-1.
      endif
c
      if(abs(trigger_particle_s).eq.11)then
        mtrigg_s=melectron
        qtrigg_s=-1.*trigger_particle_s/abs(trigger_particle_s)
      elseif(trigger_particle_s.eq.2212)then
        mtrigg_s=mproton
        qtrigg_s=1.
      elseif(trigger_particle_s.eq.45)then
        mtrigg_s=mdeuteron
        qtrigg_s=1.
      elseif(abs(trigger_particle_s).eq.211)then
        mtrigg_s=mpion
        qtrigg_s=trigger_particle_s/211.
      elseif(abs(trigger_particle_s).eq.321)then
        mtrigg_s=mkaon
        qtrigg_s=trigger_particle_s/321.
      elseif(trigger_particle_s.eq.22)then
        mtrigg_s=mphoton
        qtrigg_s=0.
      elseif(trigger_particle_s.eq.2112)then
        mtrigg_s=mneutron
        qtrigg_s=0.
      else
        mtrigg_s=1000.
        qtrigg_s=0.
      endif
c
      RETURN
      END
c
c------------------------------------------------------------------------------




