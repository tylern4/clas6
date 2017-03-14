	subroutine tag_reset_arrays
c
c_begin_doc
c  RCS ID string
c  $Id: tag_reset_arrays.F,v 1.6 1998/04/22 00:45:33 anciant Exp $
c
c  Documentation for subroutine tag_reset_arrays
c
c  Purpose: 
c  --------
c	Initializes all elements of tagger internal arrays to zero.
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
c
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
      PARAMETER (CFILE=  '$RCSfile: tag_reset_arrays.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.6 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/04/22 00:45:33 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_reset_arrays')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local st defined variables:
c  ----------------------------
      integer i,j
c_end_var
c
c  executable code for routine tag_reset_arrays:
c  ------------------------------------
c
      crmess = '  '

      do i=1,61
	tc_id(i) = 0
	tc_val_left(i) = 0
	tc_val_right(i) = 0
      enddo

      do i=1,MAXEC
	ec_id(i) = 0
	ec_val(i) = 0
      enddo

      MOR_tdc = -9999
      
      return
      end
c
c
c------------------------------------------------------------------------------
