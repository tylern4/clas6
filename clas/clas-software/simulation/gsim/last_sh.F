      subroutine last_sh

c_begin_doc

c  Documentation for subroutine last_ec

c  $Id: last_sh.F,v 1.1 2009/12/22 20:00:00 natasha Exp $

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
      parameter (cfile=  '$RCSfile: last_sh.F,v $')
      parameter (crevis= '$Revision: 1.1 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/12/22 20:00:00 $')
      parameter (cautho= '$Author: natasha $')
      parameter (crname= 'LAST_ec')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: last_sh.F,v 1.1 2009/12/22 20:00:00 natasha Exp $
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

