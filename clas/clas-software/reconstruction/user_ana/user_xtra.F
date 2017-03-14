      SUBROUTINE USER_XTRA(err)
c
c_begin_doc
c  RCS ID string
c  $Id: user_xtra.F,v 1.7 2009/02/18 14:06:40 fklein Exp $
c
c  Documentation for subroutine USER_XTRA
c
c  Purpose:
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Wed Mar 15 14:05:36 EST 1995
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "user_control.inc"
#include "clas_offsets.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      integer err(10)
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: user_xtra.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.7 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2009/02/18 14:06:40 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: user_xtra.F,v 1.7 2009/02/18 14:06:40 fklein Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='USER_XTRA')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      integer mamind
      integer namhead, namhevt, namevnt
      integer ind_head, ind_hevt, ind_evnt
      integer ntracks, ioff
      logical lfirst, lskip
      data lfirst,lskip /.true.,.true./
c_end_var
c
c  executable code for routine USER_XTRA:
c  -------------------------------------
c
      IF (lfirst) THEN
         lfirst=.false.
c        CRMESS='This is a DUMMY routine, this message written once'
        CRMESS='Write BOS for events with at least 1 TB track and '//
     &     'special events'
        CALL RECMES(CRNAME,'I',CRMESS)
        if(.not.lskip) then
           namhead=MAMIND(iw,'HEAD')
           namhevt=MAMIND(iw,'HEVT')
           namevnt=MAMIND(iw,'EVNT')
           if(namhead.eq.0 .or. namhevt.eq.0 .or. namevnt.eq.0) then
              CALL RECMES(CRNAME,'F','HEAD or HEVT or EVNT not defined') 
           endif
        endif
      ENDIF

cccc writing out all the events (pass0)
      err(1)=0
      err(2)=0   ! 2nd data stream: only epics and tagger info (all events)
      if(lskip) return

cfk: check whether seb (HEVT) and HEAD banks have the same event no:
      ind_hevt=iw(namhevt)
      if(ind_hevt.eq.0) err(1)=-1
      ind_evnt=iw(namevnt)
      if(ind_evnt.eq.0) err(1)=-2
      ind_head=iw(namhead)
      if(ind_head.gt.0) then
         if( iw(ind_head+head_evtclass).eq.0 .or. iw(ind_head+head_evtclass).gt.15) then
            err(1)=0
         elseif(err(1).gt.0) then
            if(iw(ind_head+head_nevent).eq.iw(ind_hevt+hevt_nevent)) then
               ntracks=0
               if(iw(ind_evnt).gt.0) then
                  do ioff=0,iw(ind_evnt-4)-1,iw(ind_evnt-5)
                     if(iw(ind_evnt+ioff+evnt_dcstat).gt.0) ntracks=ntracks+1
                  enddo
               endif
               if(ntracks.gt.0) err(1)=0
            endif
         endif
      endif
      
      return
      end


c      nami = mamind(IW,'HEAD')
c      ind = IW(nami)
c      if(ind.gt.0)then
c         if(iw(ind+5).lt.10) err(1)=0
c      endif
c      err(1) = 0
c
c
c------------------------------------------------------------------------------




