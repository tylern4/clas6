      subroutine clas_init

c_begin_doc

c  Documentation for subroutine clas_init

c  $Id: clas_init.F,v 1.9 2005/09/26 18:16:41 avakian Exp $

c  Purpose:
c  --------
c       general initilization...called after ffread but before geometry

c  Notes:
c  ------
c     initializes all detectors, BOS, etc.
c     by default, all geom except ST is on.  Use -nogeom or nogeom 'ALL' to turn all 
c        geom off, then use GEOM card to turn back on

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 18-nov-94

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_init.F,v $')
      parameter (crevis= '$Revision: 1.9 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2005/09/26 18:16:41 $')
      parameter (cautho= '$Author: avakian $')
      parameter (crname= 'CLAS_INIT')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_init.F,v 1.9 2005/09/26 18:16:41 avakian Exp $
     1 '/
      save


c  include files:
c  --------------
#include "bcs.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcflag.inc"
#include "gconst.inc"
#include "lcgeom.inc"
c_end_inc

c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
      integer i
c_end_var


c  executable code:
c  ----------------

c  ???variables...should come from BOS geometry input file---------------

c  half thickness of the coil
      hthmg=7.2

c  initialize parameters in the /lcgeom/
c  tangens of half sector angle
      tnhase = tan(pi*rnsect)

c array for often used sinii and cosini, i.e. to rotate each sector to the x-axis
      do i=1,nsect
        sinsec(i) = sin(float(i-1) * (twopi*rnsect))
        cossec(i) = cos(float(i-1) * (twopi*rnsect))
      enddo

c  ----------------------------------------------------------------------


c  initialize all control flags
      call init_flags

c  initialize BOS banks, open bos output file if requested
      call init_bos

c  initialize CLAS materials
      call init_mate

c  initialize CLAS media
      call init_tmed

c  initialize magnetic field
      call init_mg

c  initialize detectors, etc.
      if(.not.nogeom(cc))  call init_cc
      if(.not.nogeom(dc))  call init_dc
      if(.not.nogeom(ec))  call init_ec
      if(.not.nogeom(ec1)) call init_ec1
      if(.not.nogeom(sc))  call init_sc
      if(.not.nogeom(st))  call init_st
      if(.not.nogeom(ic))  call init_ic
      return
      end
	
c------------------------------------------------------------------------------




