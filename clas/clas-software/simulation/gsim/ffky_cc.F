      subroutine ffky_cc

c_begin_doc

c  Documentation for subroutine ffky_cc

c  $Id: ffky_cc.F,v 1.3 1998/06/08 19:29:30 fklein Exp $

c  Purpose:
c  --------
c	ffkey defs for cc

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 2-may-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: ffky_cc.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/06/08 19:29:30 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'ffky_cc')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: ffky_cc.F,v 1.3 1998/06/08 19:29:30 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gccuts.inc"
#include "ffpar_cc.inc"
c_end_inc

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

c  cuts are:  cutgam,cutele,cutneu,cuthad,cutmuon
c  defaults are normal geant defaults
      call ffkey('CCCUTS',cc_cuts,5,'REAL')
      cc_cuts(1)=0.001
      cc_cuts(2)=0.001
      cc_cuts(3)=0.010
      cc_cuts(4)=0.010
      cc_cuts(5)=0.010
      
      return
      end

c---------------------------------------------------------------------------------

