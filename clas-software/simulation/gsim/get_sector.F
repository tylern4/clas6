      integer function get_sector(x)

c_begin_doc

c  Documentation for integer function get_sector(x)

c  Purpose:
c  --------
c     returns sector number for input position x(3)

c  Input:
c  ------
c     x(3) is position in overall coord system

c  Author:
c  -------
c     Originally numsct.F from Bogdan

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 30-jan-95

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: get_sector.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:35 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'get_sector')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: get_sector.F,v 1.2 1997/04/24 15:38:35 holtrop Exp $
     1 '/
      save
      
      
c  include files:
c  --------------
#include "gconst.inc"
c_end_inc

c  input/output variables:
c  -----------------------
      real x(3)

c  local variables:
c  ----------------
      real phi
c_end_var


c  executable code:
c  ----------------

      get_sector=1
      if((x(1)**2+x(2)**2).le.1.e-30)return
      
      phi = atan2(x(2),x(1)) + pi/6.
      if(phi.lt.0.)phi = phi + twopi

      get_sector = 1 + mod(ifix(phi/(pi/3.)),6)
      
      return
      end

c---------------------------------------------------------------------------------
