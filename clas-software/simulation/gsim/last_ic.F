      subroutine last_ic

c_begin_doc

c  Documentation for subroutine last_ec

c  $Id: last_ic.F,v 1.1 2009/12/22 20:00:01 natasha Exp $

c  Purpose:
c  --------
c	ec end of run processing

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 7-jul-95

c  Major revisions:
c  ----------------

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: last_ic.F,v $')
      parameter (crevis= '$Revision: 1.1 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/12/22 20:00:01 $')
      parameter (cautho= '$Author: natasha $')
      parameter (crname= 'LAST_ec')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: last_ic.F,v 1.1 2009/12/22 20:00:01 natasha Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  local variables:
c   ----------------
c_end_var


c  executable code:
c  ----------------

      return
      end

c------------------------------------------------------------------------

