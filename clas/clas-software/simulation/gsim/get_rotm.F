      integer function get_rotm()

c_begin_doc

c  Documentation for integer function get_rotm()

c  Purpose:
c  --------
c     Returns first free rotation matrix number

c  Methods:
c  --------
c     searches zebra rotm link bank for first free rotm number

c  Input:
c  ------

c  Output:
c  -------

c  Controls:
c  ---------

c  Other routines:
c  ---------------

c  Notes:
c  ------

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 31-oct-94

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: get_rotm.F,v $')
      parameter (crevis= '$Revision: 1.5 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2003/02/27 06:12:39 $')
      parameter (cautho= '$Author: juengst $')
      parameter (crname= 'get_rotm')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: get_rotm.F,v 1.5 2003/02/27 06:12:39 juengst Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "gcnum.inc"
#include "gcunit.inc"
c_end_inc
      integer iquest
      common /quest/ iquest(100)

c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
      integer  n
c_end_var


c  executable code:
c  ----------------

      get_rotm=1
      if (JROTM.EQ.0) return
      if (jrotm.LE.0 .OR. jrotm.GT.iquest(6)) then
         call dzveri('check get_rotm',0,'CLFSU')
         if (jrotm.GT.iquest(6)) then
            write(chmail,*)' iquest(11-21):',(iquest(n),n=11,21)
            call gmail(0,1)
         endif
      endif

      do while (lq(jrotm-get_rotm).ne.0)
         get_rotm=get_rotm+1
         if(get_rotm.gt.nrotm)return
      enddo

      return
      end

c---------------------------------------------------------------------------------

