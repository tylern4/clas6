	subroutine tag_coincidence(size,set1,set2,coinc,only1,only2)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_coincidence.F,v 1.1 1998/01/22 20:48:25 anciant Exp $
c
c  Documentation for subroutine TAG_ANALYSIS
c
c  Purpose:  TAGGER utility routine
c  --------
c
c
c	inputs of this routine are two sets of numbers set1 & set2 of sizes size
c	Yet only the leading non-zero's element will be processed
c	(i.e. the size of each set is really determined vy the occurence of the first 0)
c	
c	this routine find values matching between the values of set 1 and set 2
c	and sort the indices (not the values) in coinc, only1, only2
c	having as a result only the indices, allows to retrieve later whatever 
c	value that was stored with these indices.
c
c	exemple : size = 24
c	set 1: 1 4 6 8 9  0 NR...
c	set 2: 2 4 5 9 11 0 NR...
c gives:
c	coinc (1,x) : 2 5 0 NR...
c	coinc (2,x) : 2 4 0 NR...
c	only1 : 1 3 4 0 NR...
c	only2 : 1 3 5 0 NR...
c    
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters: NONE
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
c  Author:   Francois Roudot      Created:  Thu Jun 19 12:00:00 EST 1997
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
	integer size,set1(size),set2(size),coinc(2,size),only1(size),only2(size)
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: tag_coincidence.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.1 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/01/22 20:48:25 $')
      PARAMETER ( CAUTHO = '$Author: anciant $')
      DATA CRCSID/
     1'$Id: tag_coincidence.F,v 1.1 1998/01/22 20:48:25 anciant Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='TAG_COINCIDENCE')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local Tag defined variables:
c  ---------------------------
       character*132 crmess
	integer id1,id2,nbcoinc,nbonly1,nbonly2,maxint,value1,value2
	parameter (MAXINT = 65535)
c_end_var
c
c  executable code for routine TAG_COINCIDENCE:
c  -------------------------------------
c
	crmess='  '
       
c	IF (NWRITE .LT. 1) THEN
c          NWRITE = NWRITE + 1
c          crmess='analysis of tagger data'
c         CALL RECMES(CRNAME,'I',CRMESS)
c        ENDIF
       	
	id1=1
	id2=1
	nbcoinc=0
	nbonly1=0
	nbonly2=0
	
	do while((id1.le.size.and.set1(id1).ne.0).or.(id2.le.size.and.set2(id2).ne.0))
	if (id1.le.size.and.set1(id1).ne.0) then
		value1=set1(id1)
			else
		value1=MAXINT
	endif
	if (id2.le.size.and.set2(id2).ne.0) then
		value2=set2(id2)
			else
		value2=MAXINT
	endif
	if (value1.eq.value2) then
		nbcoinc=nbcoinc+1
		coinc(1,nbcoinc) = id1
		coinc(2,nbcoinc) = id2
		id1 = id1 + 1
		id2 = id2 + 1
	else if (value1.lt.value2) then
		nbonly1 = nbonly1 + 1
		only1(nbonly1) = id1
		id1 = id1 + 1
	else
		nbonly2 = nbonly2 + 1
		only2(nbonly2) = id2
		id2 = id2 + 1
	endif
	enddo

	if (nbcoinc.lt.size)then
		coinc(1,nbcoinc + 1) = 0
		coinc(2,nbcoinc + 1) = 0
	endif
	if (nbonly1.lt.size) only1(nbonly1 + 1) = 0
	if (nbonly2.lt.size) only2(nbonly2 + 1) = 0
	
	return
 	end
  
