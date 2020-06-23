      subroutine ellipt(c8,e1,e2)

c_begin_doc

c  Documentation for subroutine ellipt

c  $Id: ellipt.F,v 1.2 1997/04/24 15:38:25 holtrop Exp $

c  Purpose:
c  --------
c	calc elliptic integrals for polarized target field calculation

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
      parameter (cfile=  '$RCSfile: ellipt.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:25 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'ELLIPT')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: ellipt.F,v 1.2 1997/04/24 15:38:25 holtrop Exp $
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
      real c8,e1,e2,p92,a8,b8,a9,b9,c9
      integer i9
c_end_var


c  executable code:
c  ----------------

      p92=1.5707963
      a8=1.
      i9=2
      b8=sqrt(1.-c8)

 60   a9=(a8+b8)*.5
      b9=sqrt(a8*b8)
      c9=(a8-b8)*.5
      c8=c8+i9*c9*c9
      i9=i9+i9
      a8=a9
      b8=b9
      if(abs(c9).gt..000001) goto 60

      e1=p92/a8
      e2=(1.-c8*.5)*e1

      return
      end
	
c------------------------------------------------------------------------------

