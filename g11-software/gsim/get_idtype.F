      integer function get_idtype()

c_begin_doc

c  Documentation for integer function get_idtype()

c  Purpose:
c  --------
c     Returns unused idtype...simply counts upwards from 1

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 26-sep-95

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: get_idtype.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:34 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'GET_IDTYPE')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: get_idtype.F,v 1.2 1997/04/24 15:38:34 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
      integer last_idtype/0/
c_end_var


c  executable code:
c  ----------------

      last_idtype=last_idtype+1
      get_idtype=last_idtype

      return
      end

c---------------------------------------------------------------------------------
