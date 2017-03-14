	subroutine tag_T_norm_analysis(OK)
c       
c       _begin_doc
c       RCS ID string
c       $Id: tag_T_norm_analysis.F,v 1.3 2007/05/28 19:17:27 hovanes Exp $
c       
c       Documentation for subroutine tag_T_norm_analysis.F
c       
c       Purpose: TAGGER routine for T alone analysis regardless of anything else (for gflux)
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
c       Author:   Eric ANCIANT      Created:  Thu Jun 19 12:00:00 EST 1997
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
c       _end_inc
c       
c       _begin_var
c       input/output variables:
c       -----------------------
	logical ok
c       
c       Local pre-defined variables:
c       ---------------------------
c       RCS information: 
	CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
	PARAMETER (CFILE=  '$RCSfile: tag_T_norm_analysis.F,v $')
	PARAMETER (CREVIS= '$Revision: 1.3 $')
	PARAMETER (CSTATE= '$State: Exp $')
	PARAMETER (CDATE=  '$Date: 2007/05/28 19:17:27 $')
	PARAMETER (CAUTHO= '$Author: hovanes $')
c       Module information:
	CHARACTER*(*)  CRNAME, CRAUTH
	CHARACTER*100  CRMESS
	PARAMETER (CRNAME='tag_T_norm')
	PARAMETER (CRAUTH='Eric ANCIANT')
c       
c       Local tag defined variables:
c       ----------------------------
	logical flag
	integer i,j
	integer nbhits, nbtbin
	integer tag_bin, doubleThit
	real meanTval, meanTvalNext

	real rf_corr_time
	integer tag_nbc_bin

c       _end_var
c       
c       executable code for routine tag_T_alone_analysis
c       -------------------------------------
        crmess = ' '
        flag = .FALSE.
        ok=.true.
	
        nbhits = 0
        nbtbin = 0
	doubleThit = 0

c
c ---- first reset all arrays
c
	do i=1,MAXTC
	   bin_Tn_ID(i) = 0
	   do j=1,2
	      bin_Tn_L_val(i,j) = 0.
	      bin_Tn_R_val(i,j) = 0.
	      Mean_Tn_val(i,j) = 0.
	      Diff_Tn_val(i,j) = 0.
	      Tn_time(i,j) = 0.
	   enddo
	enddo
	
        do i=1,NbGoodT
	   
	   if (doubleThit.eq.0) then ! doubleThit=1 means the last T analyse was double
				! so this T has already been registered has the second counter
				! of the previous double hit
				! we therefore need to skip it and go to the next one
c       
c       ----- first take care of T's ----------------------
c       
	      nbtbin = nbtbin + 1
c       
c       -------------- check for double hit
c       
	      doubleThit = 0
	      if (i.lt.MAXTC) then ! to be within array definition
		 if ((Good_t_Id(i)+1).eq.Good_T_Id(i+1)) then ! geometric matching
		    meanTval = ( Good_T_L_val(i) + Good_T_R_val(i) )/2.
     1		     - TagTCi(tag_bin(Good_T_Id(i),1))
		    meanTvalNext = ( Good_T_L_val(i+1) + Good_T_R_val(i+1) )/2.
     2	  	     - TagTCi(tag_bin(Good_T_Id(i)+1,1))
		    if (abs(meanTval-meanTvalNext ).lt.(ADJ_T_COINC/2.)) doubleThit = 1 ! time matching
		 endif
	      endif
	      
	      if (doubleThit.gt.0) then
c       
c       ----- Coincidence with next T
c       
		 bin_Tn_Id(nbtbin) = tag_bin(Good_T_Id(i),2) ! bin & 
		 bin_Tn_L_val(nbtbin,2) = Good_T_L_val(i+1) ! TDC valuesfor 
		 bin_Tn_R_Val(nbtbin,2) = Good_T_R_val(i+1) ! the second hit T
	      else
c       
c       ----- No coincidence with next T
c       
		 bin_Tn_Id(nbtbin) = tag_bin(Good_T_Id(i),1) ! bin
	      endif
c       
c       --- in both case we record TDC values for the 1st counter
c       
	      bin_Tn_L_val(nbtbin,1) = Good_T_L_val(i)
	      bin_Tn_R_Val(nbtbin,1) = Good_T_R_val(i)
	      
	   else			! T hit not analysed because part of the previous double-hit record. We reset:
	      doubleThit = 0
	   endif
	enddo
	
	NB_bins_Tn = nbtbin
	
c       
c       --- now we will do RF correction
	
	do i=1,Nb_bins_Tn
	   Mean_Tn_val(i,1) = (bin_Tn_L_val(i,1) + bin_Tn_R_Val(i,1))/2.
     1	    - TagTCi(bin_Tn_Id(i))
	   Diff_Tn_val(i,1) = (bin_Tn_R_Val(i,1) - bin_Tn_L_val(i,1))/2.
c       
c       make RF timing correction using CL01 bank and corresponding routines 9/24/98 Elton.
c       
	   Tn_time(i,1) = rf_corr_time(Mean_Tn_val(i,1)) + tag2tof
	   
	   if (tag_nbc_bin(bin_Tn_Id(i)).eq.2) then ! if this is a double counter hit bin we store data for the 2nd counter
	      Mean_Tn_val(i,2) = (bin_Tn_L_val(i,2) + bin_Tn_R_Val(i,2))/2. - TagTCi(bin_Tn_Id(i) + 1)
				! this is an approximation, we know it is not true within ~ 800 ps
				! to have the correct value, this would require an extra set of constant 
				! for peak position when PREVIOUS counter is also hit.
	      Diff_Tn_val(i,2) = (bin_Tn_R_Val(i,2) - bin_Tn_L_val(i,2))/2.
	      
	      Tn_time(i,2) = rf_corr_time(Mean_Tn_val(i,2)) +tag2tof
	      
	   endif
	enddo

	RETURN
	END
	
c       
c------------------------------------------------------------------------------

 
