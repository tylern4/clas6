      SUBROUTINE ALL_NT_FILL(ntid)
c
c_begin_doc
c  RCS ID string
c  $Id: all_nt_fill.F,v 1.25 2009/03/31 19:02:22 myurov Exp $
c
c  Documentation for subroutine ALL_NT_FILL
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
c  Author:         Created:  Tue Jun 10 23:41:44 EDT 1997
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
#include "run_control.inc"
#include "histtcl.inc"
c#include "evnt_general.inc"
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
      PARAMETER (CFILE = '$RCSfile: all_nt_fill.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.25 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2009/03/31 19:02:22 $')
      PARAMETER (CAUTHO = '$Author: myurov $')
      DATA CRCSID/   
     1'$Id: all_nt_fill.F,v 1.25 2009/03/31 19:02:22 myurov Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ALL_NT_FILL')
      PARAMETER (CRAUTH='whoistheauthor')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER ierr_part,ierr_trk,ierr_ccrc,ierr_scrw,ierr_echb,ierr_icpb,ierr_shhb,ierr_shpb
      INTEGER ierr_lac,ierr_tbtk,ierr_tagr,ierr_seb,ierr_MC,ierr_ichb,ierr_icr,ierr_gcpb
      INTEGER NWRITE, NTID
c_end_var
c
c  executable code for routine ALL_NT_FILL:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
      call hcdir('//PAWC',' ')
      call hcdir('//LUN1',' ')
      
      ierr_seb=0
      ierr_MC=0
      ierr_trk=0
      ierr_ccrc=0
      ierr_scrw=0
      ierr_icr=0
      ierr_echb=0
      ierr_ichb=0
      ierr_icpb=0
      ierr_shhb=0
      ierr_shpb=0
      ierr_tbtk=0
      ierr_part=0
      ierr_tagr=0

      NTID=0
      
      if (legn_do.and.lec_nt_do) 	call fill_ec_nt
      if (legn_do.and.lecpo_nt_do) 	call fill_ecpo_nt
      if (ltof_do.and.lsc_nt_do) 	call fill_sc_nt
      if (lcc_do.and.lcc_nt_do) 	call fill_cc_nt
      if (ltrk_do.and.ldc_nt_do) 	call fill_dc_nt
      if (lmvrt_nt_do)                  call fill_mvrt_nt

      IF(LMCTK_NT_DO)			CALL FILL_MC_NT(ierr_MC)
      IF(LPART_NT_DO)			CALL FILL_PART_NT(ierr_part)
      if(ierr_part.gt.10)ierr_MC=1
      IF(LSEB_NT_DO)then
c         CALL FILL_SEB_NTN(ierr_seb)
         CALL FILL_SEB_NT(ierr_seb)
      ELSEIF(LSEB_NTN_DO)then
         CALL FILL_SEB_NTN(ierr_seb)
      endif
      IF(LTRK_DO.AND.LTRK_NT_DO)	CALL FILL_TRK_NT(ierr_trk)
      IF(LCC_DO.AND.LCCR_NT_DO)		CALL FILL_CCRC_NT(ierr_ccrc)
cTong&step
      IF(LIC_DO.and.LICR_NT_DO)	    CALL FILL_ICR_NT(ierr_icr)
      IF(LTOF_DO.AND.LSCR_NT_DO)	CALL FILL_SCRW_NT(ierr_scrw)
      IF(LEGN_DO.AND.LECHB_NT_DO)	CALL FILL_ECHB_NT(ierr_echb)
      IF(LIC_DO.AND.LICHB_NT_DO)	CALL FILL_ICHB_NT(ierr_ichb)
      IF(LIC_DO.AND.LICPB_NT_DO)	CALL FILL_ICPB_NT(ierr_icpb)
      IF(LSH_DO.AND.LSHHB_NT_DO)	CALL FILL_SHHB_NT(ierr_shhb)
      IF(LSH_DO.AND.LSHPB_NT_DO)	CALL FILL_SHPB_NT(ierr_shpb)
      IF(Lgem_DO.AND.LgCPB_NT_DO)	CALL FILL_gCPB_NT(ierr_gcpb)
      IF(LTRK_DO.AND.LTBTK_NT_DO)	CALL FILL_TBTK_NT(ierr_tbtk)
      IF(LTAGGER_DO.AND.LTAGR_NT_DO)	CALL FILL_TAGR_NT(ierr_tagr)
      IF(ierr_seb.gt.0.or.ierr_MC.gt.0.or.ierr_ichb.gt.0) ntid=10
      
      RETURN
      END




