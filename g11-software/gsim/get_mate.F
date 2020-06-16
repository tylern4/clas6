      integer function get_mate()

c_begin_doc

c  Documentation for integer function get_mate()

c  Purpose:
c  --------
c       Returns first free material number

c  Methods:
c  --------
c     searches zebra rotm link bank for first free mate number

c  Author:
c  -------
c       Elliott Wolin, College of William and Mary, 9-nov-94

c  Major revisions:
c  ----------------

c_end_doc

	implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: get_mate.F,v $')
      parameter (crevis= '$Revision: 1.5 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2003/02/27 06:12:39 $')
      parameter (cautho= '$Author: juengst $')
      parameter (crname= 'get_mate')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: get_mate.F,v 1.5 2003/02/27 06:12:39 juengst Exp $
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

        get_mate=1
        if (JMATE.EQ.0) return
        if (jmate.LE.0 .OR. jmate.GT.iquest(6)) then
           call dzveri('check get_mate',0,'CLFSU')
           if (jmate.GT.iquest(6)) then
              write(chmail,*)' iquest(11-21):',(iquest(n),n=11,21)
              call gmail(0,1)
           endif
        endif
	do while (lq(jmate-get_mate).ne.0)
	   get_mate=get_mate+1
	   if(get_mate.gt.nmate)return
	enddo

      return
      end

c---------------------------------------------------------------------------------

