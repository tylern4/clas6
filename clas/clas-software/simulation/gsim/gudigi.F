      subroutine gudigi

c_begin_doc

c  Documentation for subroutine gudigi

c  $Id: gudigi.F,v 1.2 1997/04/24 15:38:37 holtrop Exp $

c  Purpose:
c  --------
c     dummy routine controlling event digitization simply calls guser_digi

c  Methods:
c  --------

c  Input:
c  ------

c  Output:
c  -------

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
      parameter (cfile=  '$RCSfile: gudigi.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:37 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'gudigi')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: gudigi.F,v 1.2 1997/04/24 15:38:37 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc


c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

      call clas_digi
      call guser_digi

      return
      end

c---------------------------------------------------------------------------------
