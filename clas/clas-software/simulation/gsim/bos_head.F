      subroutine bos_head(class,run,event)

c_begin_doc

c  Documentation for subroutine bos_head

c  $Id: bos_head.F,v 1.4 1998/02/11 23:40:36 holtrop Exp $

c  Purpose:
c  --------
c	creates bos HEAD bank

c  input:
c  ------
c     class     event class 
c     run       run number
c     event     event number

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 30-jun-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: bos_head.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/02/11 23:40:36 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'BOS_HEAD')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: bos_head.F,v 1.4 1998/02/11 23:40:36 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "bcs.inc"
c_end_inc

c  input/output variables:
c  -----------------------
      integer class,run,event

c  local variables:
c   ----------------
      integer nbank,head,unixtime
c_end_var


c  executable code:
c  ----------------

      call getunixtime(unixtime)

c fill bos banks...fpack wants the head bank to be number 0
C      head=nbank('HEAD',0,8,1)
C      iw(head+ 1) = 2                   !version of event format
C      iw(head+ 2) = run                 !run number
C      iw(head+ 3) = event               !event number
C      iw(head+ 4) = unixtime            !32 bit unix time
C     iw(head+ 5) = -2                  !geant
C     iw(head+ 6) = 0                   !ROC status
C     iw(head+ 7) = class               !event class
C     iw(head+ 8) = 1                   !prescale factor
      call makehead(class,run,event)
      
      return
      end

c------------------------------------------------------------------------

