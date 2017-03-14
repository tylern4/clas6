      subroutine out_st

c_begin_doc

c  Documentation for subroutine out_st

c  $Id: out_st.F,v 1.2 1997/04/24 15:38:50 holtrop Exp $

c  Purpose:
c  --------
c	st end of event processing

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 30-jun-95

c  Major revisions:
c  ----------------

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: out_st.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:50 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'out_ST')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: out_st.F,v 1.2 1997/04/24 15:38:50 holtrop Exp $
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

