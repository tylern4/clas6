      subroutine ptg_field(x,b)

c_begin_doc

c  Documentation for subroutine ptg_field

c  $Id: ptg_field.F,v 1.10 2011/03/31 19:52:57 fxgirod Exp $

c  Purpose:
c  --------
c       returns polarized target field

c  input variables:
c  ----------------
c     x(3)   position

c  output variables:
c  -----------------
c     b(3)   field at x(3)

c  Author:
c  -------
c     I got this from Volker

c  Major modifications:
c  --------------------
c     Elliott Wolin, College of William and Mary, 10-nov-95
c  Modified  by:
c  -------------
c     Angela Biselli,Dipartimento di fisica,Italy,8-Apr-97
c     biselli@infnge.ge.infn.it
c     
c     Junho Yun, ODU,   Aug, 1998
c     jyun@cebaf.gov
c     New Magnet coil geometry
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: ptg_field.F,v $')
      parameter (crevis= '$Revision: 1.10 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2011/03/31 19:52:57 $')
      parameter (cautho= '$Author: fxgirod $')
      parameter (crname= 'PTG_FIELD')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: ptg_field.F,v 1.10 2011/03/31 19:52:57 fxgirod Exp $
     1 '/
      save


c  include files:
c  --------------
#include "ffpar_ptg.inc"
c_end_inc

c  input/output variables:
c  -----------------------
      real x(3),b(3)

c  local variables:
c  ----------------


#ifdef Linux
#ifndef OSF1
      real sind, cosd
      external sind, cosd
#endif
#endif
#ifdef USEGNU
      real sind, cosd
      external sind, cosd
#endif
      real ri(4),zau(4),zbu(4),ra(4),zao(4),zbo(4)
      real j(4)

c  the following was automatically generated...ejw
      integer n,k
      real h0,f,s5,s6,d5,q1,r,z,babs,ang
c_end_var


c  executable code:
c  ----------------

      n = 4                     ! number of loops
      h0 = ptg_fieldm           ! max field [kg]
      f = 42.24                 ! current [a]

      ri(1) = 11.5		!inner radius of small coil
      ra(1) = 15.15		!outer radius 
      zau(1) = 5.125		!distance fromaxis to each 4 coners
      zbu(1) = 8.25
      zao(1) = 5.125
      zbo(1) = 8.25

      ri(2) = ri(1)
      ra(2) = ra(1)
      zau(2) = -zau(1)
      zbu(2) = -zbu(1)
      zao(2) = -zao(1)
      zbo(2) = -zbo(1)

      ri(3) = 16.75		!same above but for big coil
      ra(3) = 22.
      zau(3) = 7.5
      zbu(3) = 12.5
      zao(3) = 7.5
      zbo(3) = 12.5


      ri(4) = ri(3)
      ra(4) = ra(3)
      zau(4) = -zau(3)
      zbu(4) = -zbu(3)
      zao(4) = -zao(3)
      zbo(4) = -zbo(3)

      
      call dusp(n,0.,0.,j,ri,zau,zbu,ra,zao,zbo,h0,s5,s6,d5)
      q1=h0/s6
      f=f*q1

      do 40 k=1,n
         j(k)=j(k)*q1
 40   continue

      r = sqrt(x(1)**2+x(2)**2)
      z = x(3)
      call dusp(n,r,z,j,ri,zau,zbu,ra,zao,zbo,h0,s5,s6,d5)
      babs=sqrt(s5*s5+s6*s6)

      if(x(1).eq.0.)then
         ang = 1.570796327
      else 
         ang = atan2(x(2),x(1))
      endif

      b(1) = s5*cos(ang)
      b(2) = s5*sin(ang)
      b(3) = s6

c  rotate about the y-axis
      b(1) = -b(3)*sind(ptg_angle(3))+b(1)*cosd(ptg_angle(3))
      b(2) = b(2)
      b(3) = b(3)*cosd(ptg_angle(3)) + b(1)*sind(ptg_angle(3))

        end

