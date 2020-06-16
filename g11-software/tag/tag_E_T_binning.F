	subroutine tag_E_T_binning(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_E_T_binning.F,v 1.8 2001/07/27 18:40:19 pasyuk Exp $
c
c  Documentation for subroutine tag_E_T_binning
c
c  Purpose: TAGGER supplied routined for E & T binning (for adjacent counter double hit)
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
      PARAMETER (CFILE=  '$RCSfile: tag_E_T_binning.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.8 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2001/07/27 18:40:19 $')
      PARAMETER (CAUTHO= '$Author: pasyuk $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_E_T_binning')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local tag defined variables:
c  ----------------------------
      logical flag
      integer skipj
      integer i,j,k
      integer nbhits, nbtbin, nbebin
      integer tag_bin, doubleThit, doubleEhit
      real meanTval, meanTvalNext
c_end_var
c
c  executable code for routine tag_E_T_binning:
c  -------------------------------------
        crmess = ' '
        flag = .FALSE.
        ok=.true.
                             
        nbhits = 0
        nbtbin = 0
	doubleThit = 0
	
        do i=1,nb_coinc_Ts

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
	  if (i.lt.nb_coinc_ts) then ! let's be within array definition
	   if ((coinc_T_Id(i)+1).eq.coinc_T_Id(i+1)) then ! "geometric matching"
	      meanTval = ( coinc_T_L_val(i) + coinc_T_R_val(i) )/2. - TagTCi(tag_bin(coinc_T_Id(i),1))
	      meanTvalNext = ( coinc_T_L_val(i+1) + coinc_T_R_val(i+1) )/2. - TagTCi(tag_bin(coinc_T_Id(i)+1,1))
	      if (abs(meanTval-meanTvalNext ).lt.(ADJ_T_COINC/2.).and.coinc_T_Id(i).ne.19) doubleThit = 1 ! "time matching"
	   endif
	  endif
	  
	  if (doubleThit.gt.0) then
c
c ----- Coincidence with next T / bin + TDC values for the second counter of the double hit
c
	   bin_T_Id(nbtbin) = tag_bin(coinc_T_Id(i),2) 
	   bin_T_L_val(nbtbin,2) = coinc_T_L_val(i+1)   ! values of TDC for 
	   bin_T_R_Val(nbtbin,2) = coinc_T_R_val(i+1)   ! the second hit T
	  else
c
c ----- No coincidence with next T / bin only
c
	   bin_T_Id(nbtbin) = tag_bin(coinc_T_Id(i),1)
	  endif

c
c ------- in both cases, TDC values of the 1st counter
c
	  
	bin_T_L_val(nbtbin,1) = coinc_T_L_val(i)
	bin_T_R_Val(nbtbin,1) = coinc_T_R_val(i)
	  
c
c ----- Now for each T-bin, (several) corresponding E-bin(s) 
c		
c
	  nbebin = 0
	  skipj = 0

          if (NB_coinc_Es(i).eq.0) nbhits = nbhits + 1 ! this was a hit with no E

c else:
	  do j=1,NB_coinc_Es(i)
	  
	  if (j.ne.skipj) then ! if we had a double hit with that E before we skip it
	  
	    nbebin = nbebin + 1
	    nbhits = nbhits + 1
	    doubleEhit = 0
	    if (j.lt.MAXECTC) then
	      k = j + 1
	      do while (k.lt.NB_coinc_Es(i).and.coinc_E_Id(i,k).eq.coinc_E_Id(i,j)) 
	        k = k + 1 ! this is to skip all hits in the same E
	      enddo
	      do while (doubleEhit.eq.0.and.k.le.MAXECTC.and.coinc_E_Id(i,k).eq.(coinc_E_Id(i,j)+1))
c  for all hits in the next E counter, we try to find if one was in time:

	        if (abs(coinc_E_val(i,k) - coinc_E_val(i,j) ).lt.(ADJ_E_COINC/2.)) then
	      		doubleEhit = 1 ! in that case this as a double E counter hit
	      		skipj = k ! we flag the next counter hit, not to analyse it again.
	        else
	      		k = k +1
	        endif
	      enddo
	    endif

	    if (doubleEhit.eq.1) then
c
c	Coincidence with next E
c
	        bin_E_Id(nbtbin,nbebin)  = tag_bin(coinc_E_Id(i,j),2)
	        bin_E_val(nbtbin,nbebin,2) = coinc_E_val(i,skipj)
	    else
c
c	No coincidence with next E
c
	        bin_E_Id(nbtbin,nbebin) = tag_bin(coinc_E_Id(i,j),1)
	    endif
	    
	  bin_E_val(nbtbin,nbebin,1) = coinc_E_val(i,j)
	  
c	if (bin_T_Id(nbtbin).gt.100.and.bin_E_Id(nbtbin,nbebin).lt.600) flag=.true.
	  	    
	  endif
	  enddo
	 
	NB_bin_Es(nbtbin) = nbebin
c
c  ---------end of E analysis
c
	
	else ! T hit not analysed because part of the previous double-hit record. We reset:
	  doubleThit = 0
	endif
	enddo
	
	NB_bin_Ts = nbtbin
	Nb_bin_Hits = nbhits	
	
c	if (flag) call tag_debug(1,4)
		
      RETURN
      END
      
c
c------------------------------------------------------------------------------



 
