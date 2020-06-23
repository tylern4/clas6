	subroutine clas_trev

c_begin_doc

c  Documentation for subroutine clas_trev

c  $Id: clas_trev.F,v 1.2 1997/04/24 15:38:20 holtrop Exp $

c  Purpose:
c  --------
c       preparation for event tracking

c  Notes:
c  ------
c     just does initialization...guser_trev actually calls gtreve

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 9-nov-94

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_trev.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:20 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'CLAS_TREV')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_trev.F,v 1.2 1997/04/24 15:38:20 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
c_end_inc

c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

      if(.not.nogeom(cc))  call trev_cc
      if(.not.nogeom(dc))  call trev_dc
      if(.not.nogeom(ec))  call trev_ec
      if(.not.nogeom(ec1)) call trev_ec1
      if(.not.nogeom(sc))  call trev_sc
      if(.not.nogeom(st))  call trev_st


      return
      end
	
c------------------------------------------------------------------------------

