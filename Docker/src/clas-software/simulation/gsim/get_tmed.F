      integer function get_tmed()

c_begin_doc

c  Documentation for integer function get_tmed()

c  Purpose:
c  --------
c     Returns first free media number

c  Methods:
c  --------
c     searches zebra tmed link bank for first free tmed number

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
      parameter (cfile=  '$RCSfile: get_tmed.F,v $')
      parameter (crevis= '$Revision: 1.6 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2008/10/03 13:45:26 $')
      parameter (cautho= '$Author: avakian $')
      parameter (crname= 'get_tmed')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: get_tmed.F,v 1.6 2008/10/03 13:45:26 avakian Exp $
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

      get_tmed=51
      if (JTMED.EQ.0) return
      if (jtmed.LE.0 .OR. jtmed.GT.iquest(6)) then
         call dzveri('check get_tmed',0,'CLFSU')
         if (jtmed.GT.iquest(6)) then
            write(chmail,*)' iquest(11-21):',(iquest(n),n=11,21)
            call gmail(0,1)
         endif
      endif

      do while (lq(jtmed-get_tmed).ne.0)
         get_tmed=get_tmed+1
         if(get_tmed.gt.ntmed)return
      enddo

      return
      end

c---------------------------------------------------------------------------------
