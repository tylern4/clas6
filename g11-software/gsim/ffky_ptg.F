      subroutine ffky_ptg

c_begin_doc

c  Documentation for subroutine ffky_ptg

c  $Id: ffky_ptg.F,v 1.6 2003/02/21 22:15:25 lcsmith Exp $

c  Purpose:
c  --------
c	ffkey defs for polarized target

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 7-nov-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: ffky_ptg.F,v $')
      parameter (crevis= '$Revision: 1.6 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2003/02/21 22:15:25 $')
      parameter (cautho= '$Author: lcsmith $')
      parameter (crname= 'ffky_ptg')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: ffky_ptg.F,v 1.6 2003/02/21 22:15:25 lcsmith Exp $
     1 '/
      save


c  include files:
c  --------------
#include "ffpar_ptg.inc"
c_end_inc

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

c   polarized target angle, default is 0 degrees
      call ffkey('PTGANGLE',ptg_angle,1,'REAL')
      ptg_angle=0.

c   polarized target field type, default is 0 (or helix=2?)
      call ffkey('PTGIFIELD',ptg_ifield,1,'INTE')
      ptg_ifield=0

c   polarized target field, default is 51.0 kG
      call ffkey('PTGFIELDM',ptg_fieldm,1,'REAL')
      ptg_fieldm=51.

c  polarized target field scale factor, default 1.
      call ffkey('PTGSCALE',ptg_scale,1,'REAL')
      ptg_scale=1.

c  max. radius to take polarized target field into account
      call ffkey('PTGMAXRAD',ptg_maxradius,1,'REAL')
      ptg_maxradius=150.

      return
      end

c---------------------------------------------------------------------------------

