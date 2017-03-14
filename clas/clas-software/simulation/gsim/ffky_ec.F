      subroutine ffky_ec

c_begin_doc

c  Documentation for subroutine ffky_ec

c  $Id: ffky_ec.F,v 1.3 1998/06/08 19:29:29 fklein Exp $

c  Purpose:
c  --------
c	ffkey defs for ec

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 2-may-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: ffky_ec.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/06/08 19:29:29 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'ffky_ec')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: ffky_ec.F,v 1.3 1998/06/08 19:29:29 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "ffpar_ec.inc"
#include "ecgeom.inc"
c_end_inc

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

c  0 means use no atten, positive means use as mean value, neg read in from file
c  default is to read from file
      call ffkey('ATLEN', atlen,  1, 'REAL')
      atlen=-1.

c  for EC statistics
      call ffkey('POISS', poiss,  1, 'REAL')
      poiss=3.5

c  ec tdc to channel conversion
      call ffkey('ECTDC2CH', ec_tdc_to_channel,  1, 'INTE')
      ec_tdc_to_channel=20

c  cuts are:  cutgam,cutele,cutneu,cuthad,cutmuon
      call ffkey('ECCUTS',ec_cuts,5,'REAL')
      ec_cuts(1)=0.001
      ec_cuts(2)=0.001
      ec_cuts(3)=0.001
      ec_cuts(4)=0.001
      ec_cuts(5)=0.0001
      
      return
      end

c---------------------------------------------------------------------------------

