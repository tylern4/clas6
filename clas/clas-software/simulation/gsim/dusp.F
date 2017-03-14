      subroutine dusp(n,r,z,j,ri,zau,zbu,ra,zao,zbo,h0,s5,s6,d5)

c_begin_doc

c  Documentation for subroutine dusp

c  $Id: dusp.F,v 1.4 1998/09/04 22:09:07 jyun Exp $

c  Purpose:
c  --------
c     part of polarized target magnetic field calculation
c     integration ueber d(i) und alle spulen

c  i/o variables:
c  --------------
c     I haven't a clue as to what any of them are
c     IN)	n=number of coils
c		r, z = positon
c		j = current density
c	        ri, zau, zbu, ra, zao, zbo = magnet coil radius, width, height
c     		h0 = maximum field      
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
      parameter (cfile=  '$RCSfile: dusp.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/09/04 22:09:07 $')
      parameter (cautho= '$Author: jyun $')
      parameter (crname= 'DUSP')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: dusp.F,v 1.4 1998/09/04 22:09:07 jyun Exp $
     1 '/
      save

c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------
      integer n
      real r,ri(4),zau(4),zbu(4),ra(4),zao(4),zbo(4)

c  local variables:
c  ----------------
      real j(4)

c  the following was automatically generated...ejw
      integer k,n1,n2,i1,j1
      real z,h0,s1,s2,s3,s4,s5,s6,d5,r2,rik,rak,zauk,zbuk,zaok,zbok,t1
c_end_var


c  executable code:
c  ----------------

      r2=r*r
      s5=0
      s6=0

      do 30 k=1,n
         rik=ri(k)
         rak=ra(k)
         zauk=zau(k)
         zbuk=zbu(k)
         zaok=zao(k)
         zbok=zbo(k)
         n1=16
         if(r.lt.rik.or.r.gt.rak.or.abs(z).gt.abs(zbok)) go to 34
         n1=100
 34      n2=n1

         t1=(rak-rik)/n1
         s3=0
         s4=0

         do 311 i1=0,n1,n1
            j1=i1
            call inuli(r,r2,z,rik,j1,n1,n2,t1,zauk,zbuk,zaok,zbok,s1,s2)
            s3=s3+s1
            s4=s4+s2
 311     continue

         do 32 i1=1,(n1-1),2
            j1=i1
            call inuli(r,r2,z,rik,j1,n1,n2,t1,zauk,zbuk,zaok,zbok,s1,s2)
            s3=s3+4*s1
            s4=s4+4*s2
 32      continue

         do 33 i1=2,(n1-2),2
            j1=i1
            call inuli(r,r2,z,rik,j1,n1,n2,t1,zauk,zbuk,zaok,zbok,s1,s2)
            s3=s3+2*s1
            s4=s4+2*s2
 33      continue

         s3=s3*t1/3
         s4=s4*t1/3
         s5=s5+s3*j(k)*.0001
         s6=s6+s4*j(k)*.0001
 30   continue

      d5=(s6-h0)/h0

      return
      end
	
c------------------------------------------------------------------------------
