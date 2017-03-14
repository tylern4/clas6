      subroutine clas_out

c_begin_doc

c  Documentation for subroutine clas_out

c  $Id: clas_out.F,v 1.3 2009/12/22 14:56:46 natasha Exp $

c  Purpose:
c  --------
c	package end of event processing

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 13-apr-95

c  Major revisions:
c  ----------------

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_out.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/12/22 14:56:46 $')
      parameter (cautho= '$Author: natasha $')
      parameter (crname= 'CLAS_OUT')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_out.F,v 1.3 2009/12/22 14:56:46 natasha Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
c_end_inc

c  local variables:
c   ----------------
c_end_var


c  executable code:
c  ----------------

      if(.not.nogeom(cc))  call out_cc
      if(.not.nogeom(dc))  call out_dc
      if(.not.nogeom(ec))  call out_ec
      if(.not.nogeom(ec1)) call out_ec1
      if(.not.nogeom(sc))  call out_sc
      if(.not.nogeom(st))  call out_st
      if(.not.nogeom(ic))  call out_ic
      if(.not.nogeom(sh))  call out_sh

      return
      end

c------------------------------------------------------------------------

