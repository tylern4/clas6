	subroutine tag_filter_out(OK)
c       
c       _begin_doc
c       RCS ID string
c       $Id: tag_filter_out.F,v 1.13 2007/05/28 19:17:27 hovanes Exp $
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
#include "tagRunControl.CMN"
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
	PARAMETER (CREVIS= '$Revision: 1.13 $')
	PARAMETER (CSTATE= '$State: Exp $')
	PARAMETER (CDATE=  '$Date: 2007/05/28 19:17:27 $')
	PARAMETER (CAUTHO= '$Author: hovanes $')
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
	integer il
	integer ir
	integer best_hit

	real diff
	real bestdiff
	

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
	
c------------------------------------------------------------------------------
	if(use_tgtlr .eq. 0) then
c---------------------use TGT bank -----------------

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
c----------------------------- end TAGT --------------------------------------
	else ! <---- use_tgtlr.eq.1
c------------------ use TGTL/TGTR -----------------------------------------------
	   do i=1, maxhits ! reset arrays for used hits
	      tcl_used(i) = 0
	      tcr_used(i) = 0
	   enddo
c----   convert TGTL to ns
	   NbGoodTL = 0
	   do i=1,Tgtl_Nbhits
	      if(TL_hw_status(itc_id_left(i)).eq.0) then
		 NbGoodTL =  NbGoodTL +1
		 Good_TL_Id(NbGoodTL) = itc_id_left(i)
		 if(Tagger_convert .eq.0) then
c---    old scheme	---	 
		    Good_TL_val(NbGoodTL) = -TCchWidthL(itc_id_left(i))* 
     1		    float(itc_val_left(i)) /1000.
     2		    - posTpeakL(itc_id_left(i)) 
		 else		!<---- new scheme 
		    Good_TL_val(NbGoodTL) = -TCchWidthL(itc_id_left(i))/1000.* 
     1		    (float(itc_val_left(i)) - posTpeakL(itc_id_left(i))) 
		 endif
	      endif
	   enddo

c---- convert TGTR to ns
	   NbGoodTR = 0
	   do i=1,Tgtr_Nbhits
	      if(TR_hw_status(itc_id_right(i)).eq.0) then
		 NbGoodTR =  NbGoodTR +1
		 Good_TR_Id(NbGoodTR) = itc_id_right(i)
		 if(Tagger_convert .eq.0) then
c---    old scheme	---	 
		    Good_TR_val(NbGoodTR) = -TCchWidthR(itc_id_right(i))* 
     1  		 float(itc_val_right(i)) /1000.
     2	        	 - posTpeakR(itc_id_right(i)) 
		 else		!<---- new scheme 
		    Good_TR_val(NbGoodTR) = -TCchWidthR(itc_id_right(i))/1000.* 
     1	  	   (float(itc_val_right(i)) - posTpeakR(itc_id_right(i))) 
		 endif
	      endif
	   enddo
c------ done with conversion ----
	   NbGoodT = 0  
c------- now will do left/right time matching
	   if(NbGoodTL .gt. 0) then
	      do il = 1, NbGoodTL
		 bestdiff = TLR_window
		 best_hit = 0
		 if(NbGoodTL.gt.0) then
		    do ir=1, NbGoodTR
		       if(Good_TR_Id(ir) .eq. Good_TL_Id(il) )then
			  if (tcr_used(ir) .eq.0) then
			     diff = abs(Good_TL_val(il) - Good_TR_val(ir))
			     if (diff .le. bestdiff) then
				best_hit = ir ! candidate
				bestdiff = diff
			     endif
			  endif
		       endif
		    enddo
		 endif
		 if(best_hit .ne.0) then
		    NbGoodT = NbGoodT +1
		    Good_T_Id(NbGoodT) = Good_TL_Id(il)
		    Good_T_L_val(NbGoodT) = Good_TL_val(il)
		    Good_T_R_val(NbGoodT) = Good_TR_val(best_hit)
		    tcl_used(il) = 1
		    tcr_used(best_hit) = 1			  
		 endif
	      enddo		!do il = 1, NbGoodTL
	   endif		!if(NbGoodTL.gt.0 .and. NbGoddTR .gt.0)
	   
c---    here goes peice for unused hits 
c---    depending on the HW status for L/R PMTs we will recover those with one good PMT
c--     by assigning the same time for its partner.

c--- lefts first
	   if(NbGoodTL.gt.0) then
	      do il = 1, NbGoodTL
		 if (tcl_used(il) .eq. 0 .and. TR_hw_status(Good_TL_Id(il)) .eq. 1) then
		    if ( NbGoodT .LT. MAXTC ) then
		       NbGoodT = NbGoodT +1
		       Good_T_Id(NbGoodT) = Good_TL_Id(il)
		       Good_T_L_val(NbGoodT) = Good_TL_val(il)
		       Good_T_R_val(NbGoodT) = Good_TL_val(il)
		       tcl_used(il) = 1
		    else
		       CRMESS='Number of T-hits exceeds MAXTC'
		       CALL RECMES(CRNAME,'W',CRMESS)
		    endif
		 endif
	      enddo
	   endif
c--- now do rights
	   if(NbGoodTR.gt.0) then
	      do ir = 1, NbGoodTR
		 if (tcr_used(ir) .eq. 0 .and. TL_hw_status(Good_TR_Id(ir)) .eq. 1) then
		    if ( NbGoodT .LT. MAXTC ) then
		       NbGoodT = NbGoodT +1
		       Good_T_Id(NbGoodT) = Good_TR_Id(ir)
		       Good_T_L_val(NbGoodT) = Good_TR_val(ir)
		       Good_T_R_val(NbGoodT) = Good_TR_val(ir)
		       tcr_used(ir) = 1
		    else
		       CRMESS='Number of T-hits exceeds MAXTC'
		       CALL RECMES(CRNAME,'W',CRMESS)
		    endif
		 endif
	      enddo
	   endif

c--- sort by Tid
	   if (NbGoodT.gt.0) call tag_sort_array2(Good_T_Id,Good_T_L_val,Good_T_R_val,NbGoodT)

	   if(NbGoodT.lt.MAXTC) Good_T_Id(NbGoodT + 1) = 0
	
c--------------------------end TGTL/TGTR -----------------------------------------
	endif
	RETURN
	END
c
c------------------------------------------------------------------------------



 
