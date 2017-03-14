      subroutine gutrev

c_begin_doc

c  Documentation for subroutine gutreve

c  $Id: gutrev.F,v 1.3 2003/03/25 03:23:56 juengst Exp $

c  Purpose:
c  --------
c     steering for event tracking

c  Notes:
c  ------
c     guser_trev does final decision on tracking, i.e. it calls gtreve
c     kludge...drops '+KYE' bank to enable event number to appear in BOS bank header

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 9-nov-94

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: gutrev.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2003/03/25 03:23:56 $')
      parameter (cautho= '$Author: juengst $')
      parameter (crname= 'GUTREV')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: gutrev.F,v 1.3 2003/03/25 03:23:56 juengst Exp $
     1 '/
      save


c  include files:
c  --------------
#include "bcs.inc"
c_end_inc

c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
      integer iky,mdrop
c_end_var


c  executable code:
c  ----------------

        CALL CCTREV

c  kludge...to get around BOS/FPACK bug
      iky=mdrop(iw,'+KYE',0)

c  prepare packages for event tracking
      call clas_trev

c  decide whether to track event or not
      call guser_trev

      return
      end

c---------------------------------------------------------------------------------

