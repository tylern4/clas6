      SUBROUTINE tag_TAC_fetch_to_BOS(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_TAC_fetch_to_BOS.F,v 1.3 1999/03/09 18:48:47 boiarino Exp $
c
c  Documentation for subroutine tag_TAC_fetch_to_BOS
c
c  Purpose:  Put in TACO BOS bank the TAC output information
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
c  Author:   Francois ROUDOT      Created:  Thu Jun 19 12:00:00 EST 1997
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
#include "tag_general.PAR"
#include "event.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c      
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_TAC_fetch_to_BOS.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.3 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/03/09 18:48:47 $')
      PARAMETER (CAUTHO= '$Author: boiarino $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_int_fetch_to_BOS')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local st defined variables:
c  ----------------------------
      logical ok
      integer MBANK
      external MBANK
      integer ind
      
c_end_var
c
c  executable code for routine tag_TAC_fetch_to_BOS:
c  ------------------------------------
c
	crmess = ' '
c
	
	ind = MBANK(iw,TACOData_BOS_name,1,TACOData_BOS_size,1)

          IW(ind + 1) = tac_id
	  RW(ind + 2) = tac_time
	  RW(ind + 3) = tac_ene_lt
	  RW(ind + 4) = tac_ene_rt
	  RW(ind + 5) = tac_ene_lb
	  RW(ind + 6) = tac_ene_rb
	  RW(ind + 7) = tac_ene_sum1
	  RW(ind + 8) = tac_ene_sum2
	  RW(ind + 9) = tac_ene_sum3
          IW(ind + 10) = tacTid

	OK = .true.

      RETURN
      END
c
c------------------------------------------------------------------------------




