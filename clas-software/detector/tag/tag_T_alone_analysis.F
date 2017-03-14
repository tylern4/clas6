	subroutine tag_T_alone_analysis(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_T_alone_analysis.F,v 1.3 1998/04/01 18:16:19 anciant Exp $
c
c  Documentation for subroutine tag_T_alone_analysis.F
c
c  Purpose: TAGGER supplied routined for T alone analysis
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
c  Author:   Eric ANCIANT      Created:  Thu Jun 19 12:00:00 EST 1997
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
      logical ok
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_T_alone_analysis.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.3 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/04/01 18:16:19 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_E_T_geom_match')
      PARAMETER (CRAUTH='Eric ANCIANT')
c
c  Local tag defined variables:
c  ----------------------------
      logical flag
      integer i
      integer nbhits, nbtbin
      integer tag_bin, doubleThit
      real meanTval, meanTvalNext
c_end_var
c
c  executable code for routine tag_T_alone_analysis
c  -------------------------------------
        crmess = ' '
        flag = .FALSE.
        ok=.true.
             
        nbhits = 0
        nbtbin = 0
	doubleThit = 0
	
        do i=1,NbGoodT

	if (doubleThit.eq.0) then ! doubleThit=1 means the last T analyse was double
				! so this T has already been registered has the second counter
				! of the previous double hit
				! we therefore need to skip it and go to the next one
c
c ----- first take care of T's ----------------------
c
	nbtbin = nbtbin + 1
	
c
c -------------- check for double hit
c
	doubleThit = 0
	  if (i.lt.61) then ! to be within array definition
	   if ((Good_t_Id(i)+1).eq.Good_T_Id(i+1)) then ! geometric matching
	      meanTval = ( Good_T_L_val(i) + Good_T_R_val(i) )/2. - TagTCi(tag_bin(Good_T_Id(i),1))
	      meanTvalNext = ( Good_T_L_val(i+1) + Good_T_R_val(i+1) )/2. - TagTCi(tag_bin(Good_T_Id(i)+1,1))
	      if (abs(meanTval-meanTvalNext ).lt.(ADJ_T_COINC/2.)) doubleThit = 1 ! time matching
	   endif
	  endif
	  
	  if (doubleThit.gt.0) then
c
c ----- Coincidence with next T
c
	   bin_T_Id(nbtbin) = tag_bin(Good_T_Id(i),2) ! bin & 
	   bin_T_L_val(nbtbin,2) = Good_T_L_val(i+1)   ! TDC valuesfor 
	   bin_T_R_Val(nbtbin,2) = Good_T_R_val(i+1)   ! the second hit T
	  else
c
c ----- No coincidence with next T
c
	   bin_T_Id(nbtbin) = tag_bin(Good_T_Id(i),1) ! bin
	  endif
c
c  --- in both case we record TDC values for the 1st counter
c
	bin_T_L_val(nbtbin,1) = Good_T_L_val(i)
	bin_T_R_Val(nbtbin,1) = Good_T_R_val(i)
	  
c
c ----- Now for each T-bin, NO E's - WE force one E channel with dummy values:
c		

        bin_E_Id(nbtbin,1) = 0
        bin_E_val(nbtbin,1,1) = 0
        bin_E_val(nbtbin,1,2) = 0
	 	 
	NB_bin_Es(nbtbin) = 1

c
c  ---------end of E analysis
c
	
	else ! T hit not analysed because part of the previous double-hit record. We reset:
	  doubleThit = 0
	endif
	enddo
	
	NB_bin_Ts = nbtbin
	Nb_bin_Hits = nbtbin	
	
c	if (flag) call tag_debug(1,4)
		
      RETURN
      END
      
c
c------------------------------------------------------------------------------

 
