      subroutine guout

c_begin_doc

c  Documentation for subroutine guout

c  $Id: guout.F,v 1.2 1997/04/24 15:38:37 holtrop Exp $

c  Purpose:
c  --------
c     controls end of event processing

c  Notes:
c  ------
c     also writes BOS output and calls user histogram filling routines

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 9-nov-94

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: guout.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:37 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'GUOUT')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: guout.F,v 1.2 1997/04/24 15:38:37 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcflag.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
c_end_inc

c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
      integer msgfreq
      parameter (msgfreq=500)

      character*100 crmess
c_end_var


c  executable code:
c  ----------------

c  detector package end of event processing
      call clas_out

c  fill user histograms
      call guser_hist

c  fill BOS banks and write BOS output
      if(.not.nobosout(all))then
         call clas_bos
         call guser_bos
         call bos_out
      endif

c  print message
      if(mod(ievent,msgfreq).eq.0)then
         write(crmess,'('' GSIM processed '',i6,'' events'')')ievent
         call recmes(crname,'I',crmess)
      endif

      return
      end

c---------------------------------------------------------------------------------
