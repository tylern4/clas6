	subroutine guser_last

c_begin_doc

c  Documentation for subroutine guser_last

c  $Id: guser_last.F,v 1.2 1997/04/24 15:38:40 holtrop Exp $

c  Purpose:
c  --------
c       called once after geant run completed

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
c	called by uglast

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
      parameter (cfile=  '$RCSfile: guser_last.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:40 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'GUSER_LAST')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: guser_last.F,v 1.2 1997/04/24 15:38:40 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

	call recmes(crname,'I',
     1  ' *** Default version of guser_last called ***')

	return
	end

c------------------------------------------------------------------------
