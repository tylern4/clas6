      subroutine ffky_mg

c_begin_doc

c  Documentation for subroutine ffky_mg

c  $Id: ffky_mg.F,v 1.4 2005/09/26 18:16:41 avakian Exp $

c  Purpose:
c  --------
c	ffkey defs for mag

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 2-may-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: ffky_mg.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2005/09/26 18:16:41 $')
      parameter (cautho= '$Author: avakian $')
      parameter (crname= 'ffky_mg')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: ffky_mg.F,v 1.4 2005/09/26 18:16:41 avakian Exp $
     1 '/
      save


c  include files:
c  --------------
#include "ffpar_mg.inc"
c_end_inc

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

c  MAGTYPE type torus_field
c   type is magnetic field type (default is bogdan's lookup table)
c       cannot be changed during run
c   torus_field is analytic torus field strength (default is 700) for MAGTYPE 1
      call ffkey('MAGTYPE',magtype,2,'MIXED')
      magtype=3
      torus_field=700.


c  MAGSCALE scale_factor
c   args are magnetic field scale factor for torus and mini (default is 1.)
      call ffkey('MAGSCALE',magscale,2,'REAL')
      magscale(1)=1.
      magscale(2)=1.

c  FIELD type max
c   for tracking media definition, default is helix, 30 kG
      call ffkey('FIELD',init_ifield,2,'MIXED')
      init_ifield=2
      init_fieldm=30.0

c Target MagnetGeometry and FIELD center 
       mg_pos_ff(1) = 0.0
       mg_pos_ff(2) = 0.0
       mg_pos_ff(3) = -56.5
       call ffkey('MGPOS',mg_pos_ff,3,'REAL')

      return
      end

c---------------------------------------------------------------------------------

