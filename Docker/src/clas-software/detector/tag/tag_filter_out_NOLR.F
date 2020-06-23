	subroutine tag_filter_out_NOLR(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_filter_out_NOLR.F,v 1.1 2000/02/18 16:16:36 anciant Exp $
c
c  Documentation for subroutine tag_filter_out
c
c  Purpose: TAGGER supplied routined for filtering hits /  T TDC range, E counter time window
c  --------  No L/R matching.
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
c  Author:   E. Anciant
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
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_filter_out_NOLR.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2000/02/18 16:16:36 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_filter_out')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      logical ok
	integer i
	integer GetFromRight
	
c_end_var
c
c  executable code for routine tag_filter_out:
c  -------------------------------------
	
	crmess = ' '

      ok=.true.
c
c ----- Find the number of E counters in a reasonable time window
c 
	NbGoodE = 0

	do i=1,Tag_E_NbHits
 	  if (ec_val(i).gt.E_TDC_MIN.and.ec_val(i).lt.E_TDC_MAX.and.
     +    ec_id(i).ge.1.and.ec_id(i).le.384) then ! we filter
 		NbGoodE = NbGoodE + 1 ! We count the number of good E
		Good_E_Id(NbGoodE) = ec_id(i)
		Good_E_val(NbGoodE) = -ECchWidth * float(ec_val(i)) /1000.- posEpeak(ec_id(i)) ! and convert 
	  endif
	enddo
	
	if(NbGoodE.lt.MAXEC) Good_E_Id(NbGoodE + 1) = 0

c
c ----- Find left and right hits.
c		
	NbGoodT = 0
	do i=1,Tag_T_NbHits
	  if (((tc_val_left(i).gt.1.and.tc_val_left(i).lt.4094).or.
     +       (tc_val_right(i).gt.1.and.tc_val_right(i).lt.4094)).and.
     +       tc_id(i).ge.1.and.tc_id(i).le.61) then
	    NbGoodT = NbGoodT + 1
	    Good_T_Id(NbGoodT) = tc_id(i)
	    if (tc_val_left(i).gt.1.and.tc_val_left(i).lt.4094) then
	    Good_T_L_val(NbGoodT) = TCchWidthL(tc_id(i)) * float(tc_val_left(i)) /1000.
     +					 - posTpeakL(tc_id(i))  ! width given in ps
	    GetFromRight = 0
	    else
	       GetFromRight =1 
	    endif
	    if (tc_val_right(i).gt.1.and.tc_val_right(i).lt.4094) then
	    Good_T_R_val(NbGoodT) = TCchWidthR(tc_id(i)) * float(tc_val_right(i)) /1000.
     +					- posTpeakR(tc_id(i))   ! width given in ps
	    else 
	       Good_T_R_val(NbGoodT) = Good_T_L_val(NbGoodT) 
	    endif

	    if (GetFromRight.eq.1) then
	       Good_T_L_val(NbGoodT) = Good_T_R_val(NbGoodT)
	    endif
	 endif
	enddo
	
	if(NbGoodT.lt.MAXTC) Good_T_Id(NbGoodT + 1) = 0
			
      RETURN
      END
c
c------------------------------------------------------------------------------



 
