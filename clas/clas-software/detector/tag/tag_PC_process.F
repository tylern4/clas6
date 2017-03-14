      SUBROUTINE TAG_PC_PROCESS(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_PC_process.F,v 1.2 1999/03/12 14:40:14 anciant Exp $
c
c  Documentation for subroutine TAG_PC_PROCESS
c
c  Purpose:  PC analysis routine, called once per event
c  --------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters: NONE
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Francois Roudot      Created:  Thu Jun 19 12:00:00 EST 1997
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
#include "tag_general.PAR"
#include "event.CMN"
#include "tagcal.CMN"     
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
	logical OK
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: tag_PC_process.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.2 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/03/12 14:40:14 $')
      PARAMETER ( CAUTHO = '$Author: anciant $')
      DATA CRCSID/
     1'$Id: tag_PC_process.F,v 1.2 1999/03/12 14:40:14 anciant Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='TAG_PC_PROCESS')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local Tag defined variables:
c  ---------------------------
       integer nwrite
       character*132 crmess
       integer i
       
c_end_var
c
c  executable code for routine TAG_DSD_PROCESS
c  -------------------------------------
c
	crmess='  '
       
	IF (NWRITE .LT. 1) THEN
          NWRITE = NWRITE + 1
          crmess='analysis of Pair counter data'
          CALL RECMES(CRNAME,'I',CRMESS)
        ENDIF

	pc_ene_ve = pc_adc_ve - pc_ped_ve
	pc_ene_mn = pc_adc_mn - pc_ped_mn
	pc_ene_lt = pc_adc_lt - pc_ped_lt
	pc_ene_rb = pc_adc_rb - pc_ped_rb
	pc_ene_lb = pc_adc_lb - pc_ped_lb
	pc_ene_rt = pc_adc_rt - pc_ped_rt
	pc_time = (pc_TDC_w/1000.) * float(pc_tdc) - TagTCiMean + tag2tof - posPCpeak

c  apply same time offset to DSD than to tagger T counter, i.e tagTCiMean, and tag2tof
c  for consistency.... E.A. 3/99
c
	
		
	RETURN
      END
c
c----------------------------------------------------------------------
