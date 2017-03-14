	subroutine guser_init

c_begin_doc

c  Documentation for subroutine guser_init

c  $Id: guser_init.F,v 1.2 1997/04/24 15:38:40 holtrop Exp $

c  Purpose:
c  --------
c       user initilization called after ffread but before geometry defined

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
c       called by uginit

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
      parameter (cfile=  '$RCSfile: guser_init.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:40 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'GUSER_INIT')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: guser_init.F,v 1.2 1997/04/24 15:38:40 holtrop Exp $
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
     1  ' *** Default version of guser_init called ***')

	return
	end
	
c------------------------------------------------------------------------------
