      subroutine gustep

c_begin_doc

c  Documentation for subroutine gustep

c  $Id: gustep.F,v 1.3 2006/08/03 20:13:55 hayk Exp $

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
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2006/08/03 20:13:55 $')
      parameter (cautho= '$Author: hayk $')
      parameter (crname= 'gustep')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: gustep.F,v 1.3 2006/08/03 20:13:55 hayk Exp $
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
      call gsxyz
      call gdcxyz(0)

      return
      end

c---------------------------------------------------------------------------------
