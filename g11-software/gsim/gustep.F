      subroutine gustep

c_begin_doc

c  Documentation for subroutine gustep

c  $Id: gustep.F,v 1.2 1997/04/24 15:38:42 holtrop Exp $

c  Purpose:
c  --------
c     dummy routine called after each tracking step simply calls guser_step

c  Input:
c  ------

c  Output:
c  -------

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
      parameter (cfile=  '$RCSfile: gustep.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:42 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'gustep')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: gustep.F,v 1.2 1997/04/24 15:38:42 holtrop Exp $
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

      call clas_step
      call guser_step

      return
      end

c---------------------------------------------------------------------------------
