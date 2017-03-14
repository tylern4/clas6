      subroutine ffky_sol

c_begin_doc

c  Documentation for subroutine ffky_sol

c  $Id: ffky_sol.F,v 1.2 2011/03/31 19:52:57 fxgirod Exp $

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
      parameter (cfile=  '$RCSfile: ffky_sol.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2011/03/31 19:52:57 $')
      parameter (cautho= '$Author: fxgirod $')
      parameter (crname= 'ffky_sol')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: ffky_sol.F,v 1.2 2011/03/31 19:52:57 fxgirod Exp $
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

c
c  ptg_ifield =1 -> PTG,   2-> SOL, >10 calculated from ptg_field(xin,bptg) routine
c
c   target magnet field type, default is 0
c
      call ffkey('TMGIFIELD',ptg_ifield,1,'INTE')
      ptg_ifield=2

c  target magnet field scale factor, default 1.
      call ffkey('TMGSCALE',ptg_scale,1,'REAL')
      ptg_scale=1.

c   target magnet angle, default is 0 degrees
      call ffkey('PTGANGLE',ptg_angle,3,'REAL')
      ptg_angle(1)=0.
      ptg_angle(2)=0.
      ptg_angle(3)=0.

c   target magnet  field maximum, default is 51.0 kG
      call ffkey('PTGFIELDM',ptg_fieldm,1,'REAL')
      ptg_fieldm=46.0


c  max. radius to take  target magnet field into account
c  make sure the same maxradius used in recsis!!!
c
      call ffkey('PTGMAXRAD',ptg_maxradius,1,'REAL')
      ptg_maxradius=150.

      return
      end

c---------------------------------------------------------------------------------

