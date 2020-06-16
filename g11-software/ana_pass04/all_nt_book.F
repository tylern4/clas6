      SUBROUTINE ALL_NT_BOOK
c
c_begin_doc
c  RCS ID string
c  $Id: all_nt_book.F,v 1.14 2004/02/11 19:22:08 clasdvcs Exp $
c
c  Documentation for subroutine ALL_NT_BOOK
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
c  Author:         Created:  Sun Jun  8 15:31:02 EDT 1997
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
      PARAMETER (CFILE = '$RCSfile: all_nt_book.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.14 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/02/11 19:22:08 $')
      PARAMETER (CAUTHO = '$Author: clasdvcs $')
      DATA CRCSID/   
     1'$Id: all_nt_book.F,v 1.14 2004/02/11 19:22:08 clasdvcs Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ALL_NT_BOOK')
      PARAMETER (CRAUTH='whoistheauthor')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, NTID
      integer irun,ievnt,irec
c_end_var
c
c  executable code for routine ALL_NT_BOOK:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine books N tuples for working packages'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
      NTID=10
      
      call rernev(irun,ievnt,irec)
      
      if (.not.(lmctk_nt_do.or.lpart_nt_do)) then
        call ec_bor(irun)
        call cc_bor(irun)
        call sc_bor(irun)
      endif
      
      call hcdir( '//PAWC',' ')
      call hcdir('//LUN1',' ')
      CALL hbnt(NTID,'All_out',' ')
      CALL hbname(NTID,' ',0,'$clear')

      IF(LSEB_NT_DO)            	CALL BOOK_SEB_NT(NTID)
      IF(LSEB_NTN_DO)            	CALL BOOK_SEB_NTN(NTID)
      if (legn_do.and.lec_nt_do) 	call book_ec_nt(ntid)
      if (lic_do.and.lichb_nt_do) 	call book_ichb_nt(ntid)
      if (legn_do.and.lecpo_nt_do) 	call book_ecpo_nt(ntid)
      if (ltof_do.and.lsc_nt_do) 	call book_sc_nt(ntid)
      if (lcc_do.and.lcc_nt_do)  	call book_cc_nt(ntid)
      if (ltrk_do.and.ldc_nt_do) 	call book_dc_nt(ntid)
      if (lst_do.and.lst_nt_do)         call book_st_nt(ntid)
      if (ltbt_nt_do)                   call book_tbt_nt(ntid)
      if (lmvrt_nt_do)                  call book_mvrt_nt(ntid)

      IF(LMCTK_NT_DO)            	CALL BOOK_MC_NT(NTID)
      IF(LPART_NT_DO)            	CALL BOOK_PART_NT(NTID)
      
      IF(LTRK_DO.AND.LTRK_NT_DO) 	CALL BOOK_TRK_NT(NTID)
      IF(LCC_DO.AND.LCCR_NT_DO) 	CALL BOOK_CCRC_NT(NTID)
ctong&step
      IF(LIC_DO.AND.LICR_NT_DO) 	CALL BOOK_ICR_NT(NTID)
      IF(LTOF_DO.AND.LSCR_NT_DO) 	CALL BOOK_SCRW_NT(NTID)
      IF(LEGN_DO.AND.LECHB_NT_DO)	CALL BOOK_ECHB_NT(NTID)
      IF(LTRK_DO.AND.LTBTK_NT_DO)	CALL BOOK_TBTK_NT(NTID)
      IF(LTAGGER_DO.AND.LTAGR_NT_DO)	CALL BOOK_TAGR_NT(NTID)
      RETURN
      END



