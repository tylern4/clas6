	subroutine guser_digi

c_begin_doc

c  Documentation for subroutine guser_digi

c  $Id: guser_digi.F,v 1.2 1997/04/24 15:38:39 holtrop Exp $

c  Purpose:
c  --------
c       user hook for event digitization

c  Other routines:
c  ---------------
c       called by gudigi

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
      parameter (cfile=  '$RCSfile: guser_digi.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:39 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'GUSER_DIGI')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: guser_digi.F,v 1.2 1997/04/24 15:38:39 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
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
     1               ' *** Default version of guser_digi called ***')
	endif

	return
	end

c-------------------------------------------------------------------------------------

