      subroutine brubz(a,a2,r,r2,z,i2,gia,t2,b1,b2)

c_begin_doc

c  Documentation for subroutine brubz

c  $Id: brubz.F,v 1.2 1997/04/24 15:38:04 holtrop Exp $

c  Purpose:
c  --------
c	calculation of br and bz for polarized target field

c  Author:
c  -------
c     I got this from Volker

c  Major modifications:
c  --------------------
c     Elliott Wolin, College of William and Mary, 10-nov-95
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: brubz.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:04 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'BRUBZ')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: brubz.F,v 1.2 1997/04/24 15:38:04 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
c  the following was automatically generated...ejw
      real a,a2,r,r2,z,gia,t2,b1,b2,z1,z2,u0,u1,u2,ak2,e1,e2
      integer i2

c_end_var


c  executable code:
c  ----------------

      z1=z-(gia+t2*i2)
      z2=z1*z1
 51   u1=(a+r)*(a+r)+z2
      u0=sqrt(u1)
      u2=(a-r)*(a-r)+z2
      if(u2.gt.0.000064) goto 50
      a=a+.01
      a2=a*a
      goto 51
 50   ak2=4.*a*r/u1

      if(ak2.lt..9999999)goto 52
      ak2=.9999999
 52   call ellipt(ak2,e1,e2)

      if(r.lt..000001)goto 54
      if(abs(z1).lt..000001) goto 54
      b1=2.*(e2*(a2+r2+z2)/u2-e1)*z1/(r*u0)
      goto 55
 54   b1=0
 55   b2=2.*(e2*(a2-r2-z2)/u2+e1)/u0

      return
      end

c------------------------------------------------------------------------------

