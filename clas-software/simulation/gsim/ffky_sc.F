      subroutine ffky_sc

c_begin_doc

c  Documentation for subroutine ffky_sc

c  $Id: ffky_sc.F,v 1.3 1998/06/08 19:29:28 fklein Exp $

c  Purpose:
c  --------
c	ffkey defs for sc

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 2-may-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: ffky_sc.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/06/08 19:29:28 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'ffky_sc')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: ffky_sc.F,v 1.3 1998/06/08 19:29:28 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "ffpar_sc.inc"
c_end_inc

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

c  cuts are:  cutgam,cutele,cutneu,cuthad,cutmuon
      call ffkey('SCCUTS',sc_cuts,5,'REAL')
      sc_cuts(1)=0.001
      sc_cuts(2)=0.001
      sc_cuts(3)=0.001
      sc_cuts(4)=0.001
      sc_cuts(5)=0.0001

      return
      end

c---------------------------------------------------------------------------------

