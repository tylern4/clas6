	subroutine tag_filter_out(OK)
c       
c       _begin_doc
c       RCS ID string
c       $Id: tag_filter_out.F,v 1.6 2001/12/19 22:54:52 pasyuk Exp $
c       
c       Documentation for subroutine tag_filter_out
c       
c       Purpose: TAGGER supplied routined for filtering hits 
c       L/R T coincidence, E counter time window
c       --------
c       
c       Calling Sequence:
c       ----------------
c       
c       Input Parameters:  NONE
c       ----------------
c       
c       Output Parameters:  NONE
c       -----------------
c       
c       Called from:
c       ------------
c       
c       Other routines:
c       ---------------
c       
c       Notes:
c       ------
c       
c       Author:   Francois ROUDOT      Created:  Thu Jun 19 12:00:00 EST 1997
c       -------
c       
c       Major revisions:
c       ----------------
c       
c       
c       _end_doc
c       
	IMPLICIT NONE
	SAVE
c       
c       _begin_inc
c       include files :
c       ---------------------
c       BOS common block  uncomment the next line for BOS include file
c       #include "bcs.inc"
#include "tag_general.PAR"
#include "event.CMN"
#include "tagcal.CMN"
#include "tagtcl.CMN"
c       _end_inc
c       
c       _begin_var
c       input/output variables:
c       -----------------------
c       
c       Local pre-defined variables:
c       ---------------------------
c       RCS information: 
	CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
	PARAMETER (CFILE=  '$RCSfile: tag_filter_out.F,v $')
	PARAMETER (CREVIS= '$Revision: 1.6 $')
	PARAMETER (CSTATE= '$State: Exp $')
	PARAMETER (CDATE=  '$Date: 2001/12/19 22:54:52 $')
	PARAMETER (CAUTHO= '$Author: pasyuk $')
c       Module information:
	CHARACTER*(*)  CRNAME, CRAUTH
	CHARACTER*100  CRMESS
	PARAMETER (CRNAME='tag_filter_out')
	PARAMETER (CRAUTH='Francois Roudot')
c       
c       Local tag defined variables:
c       ----------------------------
	logical ok
	integer i
	
c       _end_var
c       
c       executable code for routine tag_filter_out:
c       -------------------------------------
	
	crmess = ' '
	
	ok=.true.
c       
c       ----- Find the number of E counters in a reasonable time window
c       
	NbGoodE = 0
	
	do i=1,Tag_E_NbHits
	   if (ec_val(i) .gt. E_TDC_MIN .and.
     1  	ec_val(i) .lt. E_TDC_MAX .and.
     1  	ec_id(i) .ge. 1 .and. ec_id(i) .le. 384 .and.
     1  	E_hw_status(ec_id(i)) .eq. 0) then ! we filter
	      
	      NbGoodE = NbGoodE + 1 ! We count the number of good E
	      Good_E_Id(NbGoodE) = ec_id(i)
	      Good_E_val(NbGoodE) = -ECchWidth * float(ec_val(i))
     1  	   /1000.- posEpeak(ec_id(i)) ! and convert 
	   endif
	enddo
	
	if(NbGoodE.lt.MAXEC) Good_E_Id(NbGoodE + 1) = 0
	
c       
c       ----- Find the number of coincidence between T right and T left
c       
	NbGoodT = 0
	do i=1,Tag_T_NbHits
	   if (tc_val_left(i).gt.1.and.tc_val_left(i).lt.4094.and.
     1  	tc_val_right(i).gt.1.and.tc_val_right(i).lt.4094.and.
     1  	tc_id(i).ge.1.and.tc_id(i).le.61.and.
     1          TL_hw_status(tc_id(i)).eq.0.and. 
     1  	TR_hw_status(tc_id(i))	.eq.0) then
	      NbGoodT = NbGoodT + 1
	      Good_T_Id(NbGoodT) = tc_id(i)
	      
	      if(Tagger_convert .eq.0) then
c---    old scheme	---	 
		 Good_T_L_val(NbGoodT) = TCchWidthL(tc_id(i))* 
     1  	      float(tc_val_left(i)) /1000.
     1  	      - posTpeakL(tc_id(i)) 
		 Good_T_R_val(NbGoodT) = TCchWidthR(tc_id(i))*
     1  	      float(tc_val_right(i)) /1000.
     1  	      - posTpeakR(tc_id(i)) 
	      else 
		 Good_T_L_val(NbGoodT) = TCchWidthL(tc_id(i))/1000.* 
     1  	      (float(tc_val_left(i)) - posTpeakL(tc_id(i)))     
		 Good_T_R_val(NbGoodT) = TCchWidthR(tc_id(i))/1000.* 
     1  	      (float(tc_val_right(i)) - posTpeakR(tc_id(i)))     
	      endif
	   endif
	enddo
	
	if(NbGoodT.lt.MAXTC) Good_T_Id(NbGoodT + 1) = 0
	
	RETURN
	END
c
c------------------------------------------------------------------------------



 
