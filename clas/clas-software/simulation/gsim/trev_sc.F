      subroutine trev_sc

c_begin_doc

c  Documentation for subroutine trev_sc

c  $Id: trev_sc.F,v 1.2 1997/04/24 15:38:57 holtrop Exp $

c  Purpose:
c  --------
c	sc end of event processing

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 30-jun-95

c  Major revisions:
c  ----------------

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: trev_sc.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:57 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'TREV_SC')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: trev_sc.F,v 1.2 1997/04/24 15:38:57 holtrop Exp $
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

