	subroutine clas_last

c_begin_doc

c  Documentation for subroutine clas_last

c  $Id: clas_last.F,v 1.3 2009/12/22 14:56:40 natasha Exp $

c  Purpose:
c  --------
c     called once after geant run completed
c     closes bos output file, calls all last_xxxx routines

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 27-jun-95

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_last.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/12/22 14:56:40 $')
      parameter (cautho= '$Author: natasha $')
      parameter (crname= 'CLAS_LAST')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_last.F,v 1.3 2009/12/22 14:56:40 natasha Exp $
     1 '/
      save


c  include files:
c  --------------
#include "bcs.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
c_end_inc

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

c  call all package cleanup routines
      if(.not.nogeom(cc))  call last_cc
      if(.not.nogeom(dc))  call last_dc
      if(.not.nogeom(ec))  call last_ec
      if(.not.nogeom(ec1)) call last_ec1
      if(.not.nogeom(sc))  call last_sc
      if(.not.nogeom(st))  call last_st
      if(.not.nogeom(ic))  call last_ic
      if(.not.nogeom(sh))  call last_sh

      return
      end

c------------------------------------------------------------------------
