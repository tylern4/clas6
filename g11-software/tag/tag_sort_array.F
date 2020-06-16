	subroutine tag_sort_array(array,table,size)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_sort_array.F,v 1.1 1998/01/22 20:48:34 anciant Exp $
c
c  Documentation for subroutine TAG_SORT_ARRAY
c
c  Purpose:
c  --------
c
c	this routine sort in increasing order the value of the array
c	of size size
c	the second array "table" is transform in the same way
c
c	for exemple:
c	if the input is : array: 23 65  4
c			  table:  1  2  3
c	
c	the ouput will be: array: 4 23 65
c			   table: 3  1  2
c
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Francois Roudot      Created:  Thu May 29 10:00:00 EDT 1997
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
	integer size,array(size),table(size)
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: tag_sort_array.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/01/22 20:48:34 $')
      PARAMETER (CAUTHO = '$Author: anciant $')
      DATA CRCSID/   
     1'$Id: tag_sort_array.F,v 1.1 1998/01/22 20:48:34 anciant Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAG_SORT_ARRAY')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
c
	integer pick, buffer,i,j
c
c_end_var
c
c  executable code for routine TAG_SORT_ARRAY:
c----6----------------------------------------------------------------72

c

	
	do i=1,size-1
	pick=i
	do j=i+1,size
	if (array(j).lt.array(pick)) pick=j
	enddo
	if (pick.ne.i) then
	  buffer=array(i)
	  array(i)=array(pick)
	  array(pick)=buffer
	  buffer=table(i)
	  table(i)=table(pick)
	  table(pick)=buffer
	endif
	enddo
		
		
 	return
 	end
 
c
c------------------------------------------------------------------------------
