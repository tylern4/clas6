      subroutine out_sh

c_begin_doc

c  Documentation for subroutine out_ec

c  $Id: out_sh.F,v 1.1 2009/12/22 20:00:00 natasha Exp $

c  Purpose:
c  --------
c	ec end of event processing

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 30-jun-95

c  Major revisions:
c  ----------------

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: out_sh.F,v $')
      parameter (crevis= '$Revision: 1.1 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/12/22 20:00:00 $')
      parameter (cautho= '$Author: natasha $')
      parameter (crname= 'out_EC')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: out_sh.F,v 1.1 2009/12/22 20:00:00 natasha Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  local variables:
c   ----------------
	integer i
		i=123
c_end_var


c  executable code:
c  ----------------

      return
      end

c------------------------------------------------------------------------

