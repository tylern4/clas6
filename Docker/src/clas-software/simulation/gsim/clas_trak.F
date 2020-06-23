	subroutine clas_trak

c_begin_doc

c  Documentation for subroutine clas_trak

c  $Id: clas_trak.F,v 1.2 1997/04/24 15:38:20 holtrop Exp $

c  Purpose:
c  --------
c       called before tracking each track

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 9-nov-94

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_trak.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:20 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'clas_trak')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_trak.F,v 1.2 1997/04/24 15:38:20 holtrop Exp $
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

	return
	end
	
c------------------------------------------------------------------------------
