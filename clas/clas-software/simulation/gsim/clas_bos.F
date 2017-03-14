      subroutine clas_bos

c_begin_doc

c  Documentation for subroutine clas_bos

c  $Id: clas_bos.F,v 1.8 2009/12/22 14:56:09 natasha Exp $

c  Purpose:
c  --------
c	fills clas bos banks

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 30-jun-95

c  Major revisions:
c  ----------------

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_bos.F,v $')
      parameter (crevis= '$Revision: 1.8 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/12/22 14:56:09 $')
      parameter (cautho= '$Author: natasha $')
      parameter (crname= 'CLAS_BOS')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_bos.F,v 1.8 2009/12/22 14:56:09 natasha Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcflag.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_st.inc"
#include "ffpar_ic.inc"
#include "ffpar_sh.inc"
c_end_inc

c  local variables:
c   ----------------
c_end_var


c  executable code:
c  ----------------

c  head bank filled in kine_xxxx

c  fill MCEV bank...MCVX,MCTK filled in kine_xxxx 
      call bos_mcev

c  fill TAG Result bank for photoproduction
      call bos_tagr

c  fill bos banks for each detector
      IF(RAWBANKS_IN)THEN
        if(.not.nobosout(cc))  call bos_cc_t
        if(.not.nobosout(dc))  call bos_dc_t
        if(.not.nobosout(ec))  call bos_ec_t
        if(.not.nobosout(ec1)) call bos_ec1_t
        if(.not.nobosout(sc))  call bos_sc_t
      ELSE
        if(.not.nobosout(cc))   call bos_cc
        if(.not.nobosout(dc))   call bos_dc
        if(.not.nobosout(ec))   call bos_ec
        if(.not.nobosout(ec1))  call bos_ec1
        if(.not.nobosout(sc))   call bos_sc
        if(.not.nobosout(sh)) call bos_sh
c
        if(.not.nobosout(ic))  then
          if(abs(ic_mod).ge.2) call bos_ic1
        endif
c
        if(.not.nobosout(st))  then
           if(st_type.eq.0) then
              call bos_st
           else
              call bos_stnew
           endif
        endif
      ENDIF
      return
      end

c------------------------------------------------------------------------

