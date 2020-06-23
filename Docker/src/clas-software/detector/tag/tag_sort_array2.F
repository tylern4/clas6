	subroutine tag_sort_array2(array,table,table2,size)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_sort_array2.F,v 1.2 2008/03/16 04:33:24 fklein Exp $
c
c  Documentation for subroutine TAG_SORT_ARRAY
c
c  Purpose:
c  --------
c
c	this routine sort in increasing order the value of the integer "array"
c	of size "size"
c       This is derivativ from tag_sort_array
c
c	the secondary real arrays "table" and "table2" are transformid 
c       in the same way
c
c	for exemple:
c	if the input is : array : 23  65   4
c			  table :  1.  2.  3.
c                         table2:  7.  4.  9.
c	
c	the ouput will be: array : 4  23  65
c			   table : 3.  1.  2.
c                          table2: 9.  7.  4.
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
c  Author:   Eugene Pasyuk      Created:  Thu May 4 14:21:00 EDT 2006
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
	integer size, array(size)
	real table(size), table2(size)
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: tag_sort_array2.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:24 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: tag_sort_array2.F,v 1.2 2008/03/16 04:33:24 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAG_SORT_ARRAY2')
      PARAMETER (CRAUTH='Eugene Pasyuk')
c
c  Local User defined variables:
c  -----------------------------
c
	integer pick,i,j,itmp
	real tmp1, tmp2
c
c_end_var
c
c  executable code for routine TAG_SORT_ARRAY2:
c----6----------------------------------------------------------------72

c

	
	do i=1,size-1
	   pick=i
	   do j=i+1,size
	      if (array(j).lt.array(pick)) pick=j
	   enddo
	   if (pick.ne.i) then
	      itmp=array(i)
	      array(i)=array(pick)
	      array(pick)=itmp
	      tmp1=table(i)
	      tmp2=table2(i)
	      table(i)=table(pick)
	      table(pick)=tmp1
	      table2(i)=table2(pick)
	      table2(pick)=tmp2
	   endif
	enddo
		
 	return
 	end
 
c
c------------------------------------------------------------------------------
