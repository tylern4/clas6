	subroutine set_tpar(medium,tpar)

c_begin_doc

c  Documentation for subroutine set_tpar(medium,tpar)

c  $Id: set_tpar.F,v 1.2 1997/04/24 15:38:52 holtrop Exp $

c  Purpose:
c  --------
c       sets cutgam,cutele,cutneu,cuthad,cutmuo tpar for medium

c  Input variables:
c  ----------------
c     medium          tracking medium number
c     tpar(5)         array of:  cutgam,cutele,cutneu,cuthad,cutmuo

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 20-oct-95

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: set_tpar.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:52 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'SET_TPAR')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: set_tpar.F,v 1.2 1997/04/24 15:38:52 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------
      integer medium
      real tpar(5)

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

      call gstpar(medium,'CUTGAM',tpar(1))
      call gstpar(medium,'CUTELE',tpar(2))
      call gstpar(medium,'CUTNEU',tpar(3))
      call gstpar(medium,'CUTHAD',tpar(4))
      call gstpar(medium,'CUTMUO',tpar(5))

      return
      end
	
c------------------------------------------------------------------------------
