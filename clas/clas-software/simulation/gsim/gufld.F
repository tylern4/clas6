      subroutine gufld(x,b)

c_begin_doc

c  Documentation for subroutine gufld(x,b)

c  $Id: gufld.F,v 1.2 1997/04/24 15:38:37 holtrop Exp $

c  Purpose:
c  --------
c     dummy simply calls user_fld(x,b) to get B field at x

c  Methods:
c  --------

c  Input:
c  ------
c     x(3)  location where b field needed

c  Output:
c  -------
c     b(3)  b field at x

c  Controls:
c  ---------

c  Other routines:
c  ---------------

c  Notes:
c  ------

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 9-nov-94

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: gufld.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:37 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'gufld')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: gufld.F,v 1.2 1997/04/24 15:38:37 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc


c  input/output variables:
c  -----------------------
      real x(3),b(3)

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

      call clas_fld(x,b)

      return
      end

c---------------------------------------------------------------------------------
