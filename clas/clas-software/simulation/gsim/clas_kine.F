      subroutine clas_kine

c_begin_doc

c  Documentation for subroutine clas_kine

c  $Id: clas_kine.F,v 1.6 2006/08/03 20:13:54 hayk Exp $

c  Purpose:
c  --------
c     calls kinematics generator specified on KINE card in FFREAD input file
c     if "noffread" then checks for command line flag or env var and sets KINE (in clas_init)
c     if neither, uses Geant default KINE 0

c   KINE     Calls               Description
c   ----     -----               -----------
c   <=0      guser_kine          user-supplied kinematics generator
c     1      kine_mcin           reads MCIN banks from bos file
c     2      kine_liz            reads "Liz-format" ascii file
c     3      kine_bogdan         kinematics for SDA testing

c  JWP; 4/2/97 -- added kine_lund
c     5      kine_lund           reads "Lund-format" ascii file

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 27-apr-95

c  Major revisions:
c  ----------------

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_kine.F,v $')
      parameter (crevis= '$Revision: 1.6 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2006/08/03 20:13:54 $')
      parameter (cautho= '$Author: hayk $')
      parameter (crname= 'CLAS_KINE')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_kine.F,v 1.6 2006/08/03 20:13:54 hayk Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gckine.inc"
#include "ffpar.inc"
#include "processevt.inc"
#include "ffpar_tg.inc"
c_end_inc

c  local variables:
c  ----------------
      character*80 crmess
      logical      lbckgrnd
c_end_var


c  executable code:
c  ---------------

c Test if the background generator should be turned on, and fixup ikine  

c H.Hakobyan
      check_finger=0
c

      if(abs(ikine).gt.10)then
         lbckgrnd = .true.
         if(ikine.le.0)then
            ikine = ikine+10
         else
            ikine = ikine-10
         endif
      else
         lbckgrnd = .false.   
      endif
                         
c  user-defined kinematics generator
      if(ikine.le.0)then
         call guser_kine

c  get kinematics data from bos file
      elseif(ikine.eq.1)then
         call kine_mcin
         
c  get kinematics data from "Liz-format" ascii file
      elseif(ikine.eq.2)then
         call kine_liz
         
c  Bogdan kinematics for SDA testing
      elseif(ikine.eq.3)then
         call kine_bogdan

C  Do nothing.
      elseif(ikine.eq.4)then
         write(crmess,'(1x,'' Kinematics set to none !!! Ikine ='',i2)')ikine
         call recmes(crname,'I',crmess)
         
c  JWP; 4/2/97 -- added kine_lund
c  get kinematics dat from "Lund-format" ascii file
      else if (ikine .eq. 5) then
         call kine_lund

cStep MCTK and MCVX with existing raw banks (DC0,EC,SC,CC,EC1)
      else if (ikine .eq. 6) then
         call kine_mcin_t

c  illegal kine card
      else
         write(crmess,'(1x,''?illegal kine card, ikine='',i2)')ikine
         call recmes(crname,'F',crmess)
      endif

c Generate background events. CLAS_BACKGR resets the ikine background flag if
c it executes properly so that clas_backgr will be called next event also.

      if(lbckgrnd)then
         call clas_backgr
      endif

      process_evt = .true.
      if(noacc_ff(1).ne.0.or.noacc_ff(2).ne.0.or.noacc_ff(3).ne.0) then
         call processevt
      endif

      return
      end
	
c------------------------------------------------------------------------------
