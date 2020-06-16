	subroutine guser_fld(xin,bfield)

c_begin_doc

c  Documentation for subroutine guser_fld(xin,bfield)

c  $Id: guser_fld.F,v 1.2 1997/04/24 15:38:39 holtrop Exp $

c  Purpose:
c  --------
c       user-supplied bfield

c  Methods:
c  --------

c  Input:
c  ------
c	xin(3)

c  Output:
c  -------
c       bfield(3)

c  Notes:
c  ------
c       called when magfield < 0

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 3-apr-95

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: guser_fld.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:39 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'GUSER_FLD')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: guser_fld.F,v 1.2 1997/04/24 15:38:39 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------
	real xin(3),bfield(3)

c  local variables:
c  ----------------
	logical first_time/.true./
c_end_var


c  executable code:
c  ----------------

	if(first_time)then
           first_time=.false.
           call recmes(crname,'I',
     1          ' *** Default version of guser_fld called...')
           call recmes(crname,'I','         ...bfield set to zero ***')
	endif

	bfield(1)=0.
	bfield(2)=0.
	bfield(3)=0.

	return
	end

c------------------------------------------------------------------------
