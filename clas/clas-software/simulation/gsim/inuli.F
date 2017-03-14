      subroutine inuli(r,r2,z,rik,i1,n1,n2,t1,zauk,zbuk,zaok,zbok,s1,s2)

c_begin_doc

c  Documentation for subroutine inuli

c  $Id: inuli.F,v 1.2 1997/04/24 15:38:45 holtrop Exp $

c  Purpose:
c  --------
c     part of polarized target field calculation
c     integration ueber die variable laenge

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
      parameter (cfile=  '$RCSfile: inuli.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:45 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'INULI')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: inuli.F,v 1.2 1997/04/24 15:38:45 holtrop Exp $
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
      real r,r2,z,rik,t1,zauk,zbuk,zaok,zbok,s1,s2,a,a2
      real gia,gib,t2,t3,b1,b2
      integer i1,i2,n1,n2,j2
c_end_var


c  executable code:
c  ----------------

      a=rik+t1*i1
      a2=a*a
      gia=zauk+(zaok-zauk)*i1/n1
      gib=zbuk+(zbok-zbuk)*i1/n1
      t2=(gib-gia)/n2
      s1=0
      s2=0

      do 40 i2=0,n2,n2
         j2=i2
         call brubz(a,a2,r,r2,z,j2,gia,t2,b1,b2)
         s1=s1+b1
         s2=s2+b2
 40   continue

      do 41 i2=1,(n2-1),2
         j2=i2
         call brubz(a,a2,r,r2,z,j2,gia,t2,b1,b2)
         s1=s1+4*b1
         s2=s2+4*b2
 41   continue

      do 42 i2=2,(n2-2),2
         j2=i2
         call brubz(a,a2,r,r2,z,j2,gia,t2,b1,b2)
         s1=s1+2*b1
         s2=s2+2*b2
 42   continue

      t3=abs(t2)
      s1=s1*t3/3
      s2=s2*t3/3

      return
      end

c------------------------------------------------------------------------------

