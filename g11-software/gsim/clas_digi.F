      subroutine clas_digi

c_begin_doc

c  Documentation for subroutine clas_digi

c  $Id: clas_digi.F,v 1.3 2004/09/07 16:44:41 devita Exp $

c  Purpose:
c  --------
c       digitizes events 

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 1-dec-94

c  Major revisions:
c  ----------------

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_digi.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2004/09/07 16:44:41 $')
      parameter (cautho= '$Author: devita $')
      parameter (crname= 'clas_digi')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_digi.F,v 1.3 2004/09/07 16:44:41 devita Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcbank.inc"
#include "ffpar_st.inc"
c_end_inc

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

c  obtain trigger time needed for subsequent digitizations
      call digi_trig

      if(.not.nodigi(all))then
         if(.not.nodigi(cc))  call digi_cc
         if(.not.nodigi(dc))  call digi_dc
         if(.not.nodigi(ec))  call digi_ec
         if(.not.nodigi(ec1)) call digi_ec1
         if(.not.nodigi(sc))  call digi_sc
         if(.not.nodigi(st))  then
            if(st_type.eq.0) then
               call digi_st
            else
               call digi_stnew
            endif
         endif
c  release unused space in jdigi ??? is this needed ???
         call grleas(jdigi)
      endif

      return
      end

c---------------------------------------------------------------------------

