      SUBROUTINE TAG_TAC_PROCESS(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_TAC_process.F,v 1.3 1999/03/12 14:40:15 anciant Exp $
c
c  Documentation for subroutine TAG_TAC_process
c
c  Purpose:  Total absorption shower counter analysis routine, called once per event
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
      PARAMETER ( CFILE = '$RCSfile: tag_TAC_process.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.3 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/03/12 14:40:15 $')
      PARAMETER ( CAUTHO = '$Author: anciant $')
      DATA CRCSID/
     1'$Id: tag_TAC_process.F,v 1.3 1999/03/12 14:40:15 anciant Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='TAG_TAC_PROCESS')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local Tag defined variables:
c  ---------------------------
       integer nwrite
       character*132 crmess
       integer i
       
c_end_var
c
c  executable code for routine TAG_TAC_PROCESS
c  -------------------------------------
c
	crmess='  '
       
	IF (NWRITE .LT. 1) THEN
          NWRITE = NWRITE + 1
          crmess='analysis of Total absorption Counter data'
          CALL RECMES(CRNAME,'I',CRMESS)
        ENDIF

        if (tac_Id.eq.1) then
          tac_ene_lt = tac_sca_lt * (tac_adc_lt - tac_ped_lt)
          tac_ene_rt = tac_sca_rt * (tac_adc_rt - tac_ped_rt)
          tac_ene_lb = tac_sca_lb * (tac_adc_lb - tac_ped_lb)
          tac_ene_rb = tac_sca_rb * (tac_adc_rb - tac_ped_rb)
          tac_ene_sum1 = tac_sca_sum1 * (tac_adc_sum1 - tac_ped_sum1)
          tac_ene_sum2 = tac_sca_sum2 * (tac_adc_sum2 - tac_ped_sum2)
          tac_ene_sum3 = tac_sca_sum3 * (tac_adc_sum3 - tac_ped_sum3)
          tac_time = (tac_TDC_w/1000.) * float(tac_tdc) - posTACpeak
          tac_time = tac_time - TagTCiMean + tag2tof 
c  apply same time offsets to DSD than to tagger T counters, i.e tagTCiMean, and tag2tof
c  for consistency.... E.A. 3/99
c

        else
          tac_ene_lt = 0.
          tac_ene_rt = 0.
          tac_ene_lb = 0.
          tac_ene_rb = 0.
          tac_ene_sum1 = 1. * tac_adc_sum1
          tac_ene_sum2 = 0.
          tac_ene_sum3 = 0.
          tac_time = (tac_TDC_w/1000.) * float(tac_tdc)
        endif

	RETURN
      END
c
c----------------------------------------------------------------------
