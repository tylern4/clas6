      SUBROUTINE HISTTCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: histtcl_init.F,v 1.20 2009/03/31 19:03:01 myurov Exp $
c
c  Documentation for subroutine HISTTCL_INIT
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
c  Author:   Francois Roudot      Created:  Thu May 29 10:00:00 EDT 1997
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
#include "histtcl.inc"
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
      PARAMETER (CFILE = '$RCSfile: histtcl_init.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.20 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2009/03/31 19:03:01 $')
      PARAMETER (CAUTHO = '$Author: myurov $')
      DATA CRCSID/   
     1'$Id: histtcl_init.F,v 1.20 2009/03/31 19:03:01 myurov Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='HISTTCL_INIT')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      integer itcl_int_readonly, itcl_int, itcl_string, itcl_double
      DATA itcl_int,itcl_string,itcl_int_readonly,itcl_double
     1       /1,        4,             129,           2/

c_end_var
c
c  executable code for routine HISTTCL_INIT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Histo and Analysis Tcl variable loaded'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

      LTRK_H_DO    = .TRUE.
      LEGN_H_DO    = .TRUE.
      LCC_H_DO     = .FALSE.
      LIC_H_DO     = .FALSE.
      LTOF_H_DO    = .TRUE.
      LST_H_DO     = .FALSE.
      LUSR0_H_DO   = .FALSE.
      LUSR1_H_DO   = .FALSE.
      LUSR2_H_DO   = .FALSE.
      LTIME_H_DO   = .FALSE.
      LTAGGER_H_DO = .FALSE.
      LEC1_H_DO    = .TRUE.
      LSEB_H_DO    = .TRUE.
      LFEC_HIST    = .TRUE.
      LIC_HIST     = .FALSE.
      LSEB_HIST    = .TRUE.
      LMON_HIST    = .TRUE.
      
      call tcl_linkvar('ltrk_h_do',ltrk_h_do,itcl_int)
      call tcl_linkvar('legn_h_do',ltrk_h_do,itcl_int)
      call tcl_linkvar('lcc_h_do',lcc_h_do,itcl_int)
      call tcl_linkvar('lic_h_do',lic_h_do,itcl_int)
      call tcl_linkvar('ltof_h_do',ltof_h_do,itcl_int)
      call tcl_linkvar('lst_h_do',lst_h_do,itcl_int)
      call tcl_linkvar('lusr0_h_do',lusr0_h_do,itcl_int)
      call tcl_linkvar('lusr1_h_do',lusr1_h_do,itcl_int)
      call tcl_linkvar('lusr2_h_do',lusr2_h_do,itcl_int)
      call tcl_linkvar('ltime_h_do',ltime_h_do,itcl_int)
      call tcl_linkvar('ltagger_h_do',ltagger_h_do,itcl_int)
      call tcl_linkvar('lec1_h_do',lec1_h_do,itcl_int)
      call tcl_linkvar('lseb_h_do',lseb_h_do,itcl_int)
      call tcl_linkvar('lfec_hist',lfec_hist,itcl_int)
      call tcl_linkvar('lic_hist',lic_hist,itcl_int)
      call tcl_linkvar('lseb_hist',lseb_hist,itcl_int)
      call tcl_linkvar('lmon_hist',lmon_hist,itcl_int)

      lechb_nt_do	= .FALSE.
      lichb_nt_do	= .FALSE.
      licpb_nt_do	= .FALSE.
      lshhb_nt_do	= .FALSE.
      lshpb_nt_do	= .FALSE.
      lgcpb_nt_do	= .FALSE.
      lseb_nt_do	= .FALSE.
      lseb_ntn_do	= .FALSE.
      ltrk_nt_do	= .FALSE.
      ltbtk_nt_do	= .FALSE.
      lccr_nt_do	= .FALSE.
      licr_nt_do	= .FALSE.
      lscr_nt_do	= .FALSE.
      llac_nt_do	= .FALSE.
      ltagr_nt_do	= .FALSE.
      lall_nt_do	= .FALSE.
      lmctk_nt_do 	= .FALSE.
      lpart_nt_do	= .FALSE.
      lmvrt_nt_do       = .FALSE.
     
      lec_nt_do		= .FALSE.
      lecpo_nt_do	= .FALSE.
      lsc_nt_do		= .FALSE.
      lcc_nt_do		= .FALSE.
      ldc_nt_do		= .FALSE.
      nt_id_cut         = 11
      call tcl_linkvar('nt_id_cut',nt_id_cut,itcl_int)
      call tcl_linkvar('lechb_nt_do',lechb_nt_do,itcl_int)
      call tcl_linkvar('lichb_nt_do',lichb_nt_do,itcl_int)
      call tcl_linkvar('licpb_nt_do',licpb_nt_do,itcl_int)
      call tcl_linkvar('lshhb_nt_do',lshhb_nt_do,itcl_int)
      call tcl_linkvar('lshpb_nt_do',lshpb_nt_do,itcl_int)
      call tcl_linkvar('lgcpb_nt_do',lgcpb_nt_do,itcl_int)
      call tcl_linkvar('lseb_nt_do',lseb_nt_do,itcl_int)
      call tcl_linkvar('lseb_ntn_do',lseb_ntn_do,itcl_int)
      call tcl_linkvar('ltrk_nt_do',ltrk_nt_do,itcl_int)
      call tcl_linkvar('ltbtk_nt_do',ltbtk_nt_do,itcl_int)
      call tcl_linkvar('lccr_nt_do',lccr_nt_do,itcl_int)
      call tcl_linkvar('licr_nt_do',licr_nt_do,itcl_int)
      call tcl_linkvar('lscr_nt_do',lscr_nt_do,itcl_int)
      call tcl_linkvar('llac_nt_do',llac_nt_do,itcl_int)
      call tcl_linkvar('ltagr_nt_do',ltagr_nt_do,itcl_int)
      call tcl_linkvar('lall_nt_do',lall_nt_do,itcl_int)
      call tcl_linkvar('lmctk_nt_do',lmctk_nt_do,itcl_int)
      call tcl_linkvar('lpart_nt_do',lpart_nt_do,itcl_int)
      
      call tcl_linkvar('lec_nt_do',lec_nt_do,itcl_int)
      call tcl_linkvar('lecpo_nt_do',lecpo_nt_do,itcl_int)
      call tcl_linkvar('lsc_nt_do',lsc_nt_do,itcl_int)
      call tcl_linkvar('lcc_nt_do',lcc_nt_do,itcl_int)
      call tcl_linkvar('ldc_nt_do',ldc_nt_do,itcl_int)
      call tcl_linkvar('lmvrt_nt_do',lmvrt_nt_do,itcl_int)

      RETURN
      END


