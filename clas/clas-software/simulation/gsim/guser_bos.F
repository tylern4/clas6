	subroutine guser_bos

c_begin_doc

c  Documentation for subroutine guser_bos

c  $Id: guser_bos.F,v 1.2 1997/04/24 15:38:38 holtrop Exp $

c  Purpose:
c  --------
c       fill user BOS banks

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 28-sep-95

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: guser_bos.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:38 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'GUSER_BOS')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: guser_bos.F,v 1.2 1997/04/24 15:38:38 holtrop Exp $
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
     1        ' *** Default version of guser_bos called ***')
      endif

      return
      end
	
c------------------------------------------------------------------------------
