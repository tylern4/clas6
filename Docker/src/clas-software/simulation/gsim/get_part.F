      integer function get_part()

c_begin_doc

c  Documentation for integer function get_part()

c  Purpose:
c  --------
c     Returns first free particle number

c  Methods:
c  --------
c     searches zebra particle link bank for first free part number

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
c     Might need to be changed if GCBANK or zebra bank structure ever changes

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 31-oct-94

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: get_part.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:34 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'get_part')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: get_part.F,v 1.2 1997/04/24 15:38:34 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "gcnum.inc"
c_end_inc


c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

      get_part=1
      if((npart.le.0).or.(jpart.le.0))return

      do while (lq(jpart-get_part).ne.0)
         get_part=get_part+1
         if(get_part.gt.npart)return
      enddo

      return
      end

c---------------------------------------------------------------------------------
