      SUBROUTINE tag_PS_fetch_to_BOS(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_PS_fetch_to_BOS.F,v 1.2 1999/03/09 18:48:47 boiarino Exp $
c
c  Documentation for subroutine tag_PS_fetch_to_BOS
c
c  Purpose:  Put in PSO BOS bank the PS output information
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
      PARAMETER (CFILE=  '$RCSfile: tag_PS_fetch_to_BOS.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.2 $')
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
      integer ind,i,j
      
c_end_var
c
c  executable code for routine tag_dsd_fetch_to_BOS:
c  ------------------------------------
c
	crmess = ' '
c
	
	ind = MBANK(iw,PSOData_BOS_name,1,PSOData_BOS_size,Nb_Hits_PS)

	do i=1,Nb_Hits_PS
            j = (i - 1) * PSOData_BOS_size
	    IW(ind+ j + 1 ) = ps_id(i) 
	    RW(ind+ j + 2 ) = ps_scint_time(i)
	    RW(ind+ j + 3 ) = ps_scint_ene(i)
	    IW(ind+ j + 4 ) = psTid(i)
	enddo
	
	OK = .true.

      RETURN
      END
c
c------------------------------------------------------------------------------




