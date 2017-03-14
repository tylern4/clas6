	subroutine tagcal_read_from_bos(ok)
c
c_begin_doc
c  RCS ID string
c  $Id: tagcal_read_from_BOS.F,v 1.4 1999/03/09 18:48:53 boiarino Exp $
c
c  Documentation for subroutine tagcal_read_from_bos
c
c  Purpose: 
c  --------
c	fills calibration common from the TAGC calibration bos bank.
c	Executed once before reconstruction begins.
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
#include "tagcal.CMN"
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
      PARAMETER (CFILE=  '$RCSfile: tagcal_read_from_BOS.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.4 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/03/09 18:48:53 $')
      PARAMETER (CAUTHO= '$Author: boiarino $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tagcal_read_from_bos')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local st defined variables:
c  ----------------------------
	integer MLINK	! BOS function
c_end_var
c
c  executable code for routine tagcal_read_from_bos:
c  ------------------------------------
c
	crmess = '  '
	ok=.true.
	

	
	return
	end
	
c
c------------------------------------------------------------------------------

