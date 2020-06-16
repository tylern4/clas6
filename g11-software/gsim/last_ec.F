      subroutine last_ec

c_begin_doc

c  Documentation for subroutine last_ec

c  $Id: last_ec.F,v 1.2 1997/04/24 15:38:47 holtrop Exp $

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
      parameter (cfile=  '$RCSfile: last_ec.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:47 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'LAST_ec')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: last_ec.F,v 1.2 1997/04/24 15:38:47 holtrop Exp $
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

