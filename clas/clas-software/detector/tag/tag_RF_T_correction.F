	subroutine tag_RF_T_correction(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_RF_T_correction.F,v 1.9 1998/10/21 15:13:01 anciant Exp $
c
c  Documentation for subroutine tag_RF_T_correction
c
c  Purpose: TAGGER supplied routined for T - RF correction
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
c  Author:   Eric ANCIANT      Created:  jan 1998
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
      PARAMETER (CFILE=  '$RCSfile: tag_RF_T_correction.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.9 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/10/21 15:13:01 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_RF_T_correction')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local tag defined variables:
c  ----------------------------
      logical ok
      integer decrement,i,j
      integer tag_nbc_bin
      real rf_corr_time

c_end_var
c
c  executable code for routine tag_RF_T_correction:
c  -------------------------------------
c
      crmess = ' '
      ok=.true.

	do i=1,Nb_bin_Ts
	 Mean_T_val(i,1) = (bin_T_L_val(i,1) + bin_T_R_Val(i,1))/2. - TagTCi(bin_T_Id(i))
	 Diff_T_val(i,1) = (bin_T_R_Val(i,1) - bin_T_L_val(i,1))/2.
c
c        make RF timing correction using CL01 bank and corresponding routines 9/24/98 Elton.
c  
         T_time(i,1) = rf_corr_time(Mean_T_val(i,1))
	 	 
	 if (tag_nbc_bin(bin_T_Id(i)).eq.2) then ! if this is a double counter hit bin we store data for the 2nd counter
	   Mean_T_val(i,2) = (bin_T_L_val(i,2) + bin_T_R_Val(i,2))/2. - TagTCi(bin_T_Id(i) + 1)
	   					! this is an approximation, we know it is not true within ~ 800 ps
						! to have the correct value, this would require an extra set of constant 
						! for peak position when PREVIOUS counter is also hit.
	   Diff_T_val(i,2) = (bin_T_R_Val(i,2) - bin_T_L_val(i,2))/2.

           T_time(i,2) = rf_corr_time(Mean_T_val(i,2))
           
	 endif

	enddo


	
      RETURN
      END
c
c------------------------------------------------------------------------------



 
