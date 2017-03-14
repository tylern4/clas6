	subroutine guser_beg

c_begin_doc

c  Documentation for subroutine guser_beg

c  $Id: guser_beg.F,v 1.2 1997/04/24 15:38:38 holtrop Exp $

c  Purpose:
c  --------
c       user initilization called after all geometry defined, before run begins

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 1-may-95

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: guser_beg.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:38 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'GUSER_BEG')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: guser_beg.F,v 1.2 1997/04/24 15:38:38 holtrop Exp $
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
     1  ' *** Default version of guser_beg called ***')

	return
	end
	
c------------------------------------------------------------------------------
