      SUBROUTINE TAG_DSD_DEBUG
c
c_begin_doc
c  RCS ID string
c  $Id: tag_DSD_debug.F,v 1.1 1998/03/17 01:59:34 anciant Exp $
c
c  Documentation for subroutine TAG_DSD_debug
c
c  Purpose: routine that dump the DSD common
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
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
c  Author:   Eric Anciant    Created:  march 98
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_DSD_debug.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/03/17 01:59:34 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAG_BRUN')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      integer i,j
c_end_var
c
c  executable code for routine TAG_DEBUG:
c  -------------------------------------
c

	
	print *,'=================================================='
	print *,'=   DSPC bank readout                             '
	print *,'=================================================='
	print *,'TDC :',pc_tdc
        print *,'ADC main :',pc_adc_mn,' / ADC veto :',pc_adc_ve
        print *,'ADC LT :',pc_adc_lt,' / RB :',pc_adc_rb,' / RT :',pc_adc_rt,' / LB :',pc_adc_lb

	print *,'=================================================='
	print *,'=   processed PC data                             '
	print *,'=================================================='
	print *,'Time :',pc_time
        print *,'Energy deposit main :',pc_ene_mn,' / Energy deposit veto :',pc_ene_ve
        print *,'Energy deposit LT :',pc_ene_lt,' / RB :',pc_ene_rb,' / RT :',pc_ene_rt,' / LB :',pc_ene_lb
        print *,'T id : ',pcTid

	print *,'=================================================='
	print *,'=   DSTC bank readout                             '
	print *,'=================================================='
	print *,'TDC :',tac_tdc
        print *,'ADC LT :',tac_adc_lt,' / RB :',tac_adc_rb,' / RT :',tac_adc_rt,' / LB :',tac_adc_lb
        print *,'ADC sum 1 :',tac_adc_sum1,' / sum 2 :',tac_adc_sum2,' / sum 3 :',tac_adc_sum3

	print *,'=================================================='
	print *,'=   TAC processed datas                           '
	print *,'=================================================='
	print *,'time :',tac_time
        print *,'Energy deposit LT :',tac_ene_lt,' / RB :',tac_ene_rb,' / RT :',tac_ene_rt,' / LB :',tac_ene_lb
        print *,'Energy deposit sum 1 :',tac_ene_sum1,' / sum 2 :',tac_ene_sum2,' / sum 3 :',tac_ene_sum3
        print *,'T id : ',tacTid

	print *,'=================================================='
	print *,'=   DSPS bank readout                             '
	print *,'=================================================='
        print *,' Number of paddle hit : ',Nb_Hits_PS
        do i=1,Nb_Hits_PS
	   print *,'ID :',ps_id(i),' / TDC :',ps_tdc(i),' / ADC :',ps_adc(i)
        enddo

	print *,'=================================================='
	print *,'=  pair spectrometer processed datas              '
	print *,'=================================================='
        print *,' Number of paddle hit : ',Nb_Hits_PS
        do i=1,Nb_Hits_PS
	   print *,'ID :',ps_id(i),' / time :',ps_scint_time(i),' / Energy deposit :',ps_scint_ene(i),' / T id :',psTid(i)
        enddo

	print *,'=================================================='
	print *,'=  pair spectrometer event analysis               '
	print *,'=================================================='
        print *,'Number of event: ',Nb_evnt_PS
        do i=1,Nb_evnt_PS
           print *,' Bit pattern : ',ps_pattern(i),' / time ',ps_time(i)
        enddo

	RETURN
	END
c
c------------------------------------------------------------------------------




