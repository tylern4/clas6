	subroutine stcal_default(ok)
c
c_begin_doc
c  RCS ID string
c  $Id: stcal_default.F,v 1.4 1997/10/13 13:50:41 staylor Exp $
c
c  Documentation for subroutine stcal_default
c
c  Purpose: 
c  --------
c	Creates a default calibration bank if one does not already
c	exist.
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
#include "st_general.PAR"
#include "stcal.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c 
      logical ok
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: stcal_default.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.4 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1997/10/13 13:50:41 $')
      PARAMETER (CAUTHO= '$Author: staylor $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='stcal_default')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local st defined variables:
c  ----------------------------
c
	
	integer pair,side
c_end_var
c
c  executable code for routine stcal_default:
c  ------------------------------------
c      
	
	do pair=1,3		
	   do side =1,2
c---Assume TDC calibration is linear w/50 psec/channel resolution. 
		 StCal_values(C0,side,pair)=0.	
		 StCal_values(C1,side,pair)=0.047
		 StCal_values(C2,side,pair)=0.
		 StCal_uncertainties(C0,side,pair)=0.	
		 StCal_uncertainties(C1,side,pair)=0.
		 StCal_uncertainties(C2,side,pair)=0.
c
c	the following default set of value insures that no correction
c	will be applied with the standard formula 
c
		 StCal_values(M0,side,pair)=1.
		 StCal_values(W0,side,pair)=0.
		 StCal_values(W1,side,pair)=1.
		 StCal_values(W2,side,pair)=-1. 
c                
		 StCal_uncertainties(M0,side,pair)=0.
		 StCal_uncertainties(W0,side,pair)=0.
		 StCal_uncertainties(W1,side,pair)=0.
		 StCal_uncertainties(W2,side,pair)=0.
c
		 StCal_values(ped,side,pair)=600
		 StCal_uncertainties(ped,side,pair)=0.
	      enddo
	   enddo

	ok = .true.
	return
	end

c
c-----------------------------------------------------------------------------


