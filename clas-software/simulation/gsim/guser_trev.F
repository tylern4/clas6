	subroutine guser_trev

c_begin_doc

c  Documentation for subroutine guser_trev

c  $Id: guser_trev.F,v 1.3 1998/09/03 15:49:38 tree Exp $

c  Purpose:
c  --------
c       main steering for event tracking

c  Methods:
c  --------

c  Input:
c  ------

c  Output:
c  -------

c  Controls:
c  ---------

c  Other routines:
c  ---------------
c       called by gutreve

c  Notes:
c  ------

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 9-nov-94

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: guser_trev.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/09/03 15:49:38 $')
      parameter (cautho= '$Author: tree $')
      parameter (crname= 'GUSER_TREV')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: guser_trev.F,v 1.3 1998/09/03 15:49:38 tree Exp $
     1 '/
      save


c  include files:
c  --------------
#include "processevt.inc"
c_end_inc

c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
	logical first_time/.true./
c_end_var


c  executable code:
c  ----------------

	if(first_time)then
		first_time=.false.
		call recmes(crname,'I',
     1  	     ' *** Default version of guser_trev called ***')
	endif

	if(process_evt) then
	   call gtreve
	endif
	return
	end
	
c------------------------------------------------------------------------------

