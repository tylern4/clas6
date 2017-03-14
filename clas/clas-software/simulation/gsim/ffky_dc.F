      subroutine ffky_dc

c_begin_doc

c  Documentation for subroutine ffky_dc

c  $Id: ffky_dc.F,v 1.3 1998/06/08 19:29:28 fklein Exp $

c  Purpose:
c  --------
c	ffkey defs for dc

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 2-may-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: ffky_dc.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/06/08 19:29:28 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'ffky_dc')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: ffky_dc.F,v 1.3 1998/06/08 19:29:28 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "ffpar_dc.inc"
c_end_inc

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

c  dc drift conversion factor from distance to time, default is 400.0
      call ffkey('DRIFT2T',dc_dist_to_time,1,'REAL')
      dc_dist_to_time=400.0

c  dc resolution for each superlayer...default is 200 microns
      call ffkey('DCRESOL',dc_resolution,6,'REAL')
      call vfill(dc_resolution,6,0.020)

c  cuts are:  cutgam,cutele,cutneu,cuthad,cutmuon
      call ffkey('DCCUTS',dc_cuts,5,'REAL')
      dc_cuts(1)=0.001
      dc_cuts(2)=0.001
      dc_cuts(3)=0.001
      dc_cuts(4)=0.001
      dc_cuts(5)=0.0001
      
      return
      end

c---------------------------------------------------------------------------------

