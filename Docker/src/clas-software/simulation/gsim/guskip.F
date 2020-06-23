      subroutine guskip(iskip)

c_begin_doc

c  Documentation for subroutine guskip(iskip)

c  $Id: guskip.F,v 1.3 1998/05/30 15:34:43 holtrop Exp $

c  Purpose:
c  --------
c       dummy calls guser_skip

c  Methods:
c  --------

c  Input:
c  ------

c  Output:
c  -------
c     iskip      0   continue tracking this track
c            .ne.0   skip this track  

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
      parameter (cfile=  '$RCSfile: guskip.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/05/30 15:34:43 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'guskip')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: guskip.F,v 1.3 1998/05/30 15:34:43 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------
      integer iskip

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

c  default is to track all tracks
      iskip=0

      return
      end

c---------------------------------------------------------------------------------
