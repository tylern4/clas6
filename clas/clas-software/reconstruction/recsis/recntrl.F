      subroutine recntrl
      implicit none
      save
c
#include "bcs.inc"
#include "clasmdl.inc"
#include "run_control.inc"
c
      character*132  crcsid, crmess
      character*8 crname/'recntrl '/
      data crcsid/
     &'$Id: recntrl.F,v 1.32 2008/03/16 04:33:21 fklein Exp $
     &'/
c
      integer*4 ind, nrun_skipped, nevt_skipped, nevt_count
      integer nrec, mlink, nevent_physics
      logical*4 lfirst, lchange, lbeg_run_called, lend_run_called,
     &   lend_last, lrun_skip, levt_skip, levt_check, lphysics, lgo
      data lfirst/.true./, lchange/.false./, lbeg_run_called/.false./,
     &   lend_run_called/.false./, lend_last/.true./
      data nrun_skipped/0/, nevt_skipped/0/, nevt_count/0/
      data nevent_physics/0/

c---------------------
c clear global flags |
c---------------------
      begjob = .false.
      revent = .false.
      endrun = .false.
      begrun = .false.
      othdat = .false.
      lscaler = .false.
      lepics = .false.
c------------------------
c     clear local flags |
c------------------------
      lphysics = .false.
      lgo = .false.
c------------------------------------------------------
c     if first time through, ever, do begin job stuff |
c------------------------------------------------------
      if (lfirst) then
         begjob = .true.
      end if
c--------------------
c     get head bank |
c--------------------
c
      ind = mlink(iw,'HEAD',0)
      if (ind .eq. 0) then
         crmess='No header data found'
         call recmes(crname,'W',crmess)
         crmess='Skipping this event and will try to read the next event'
         call recmes(crname,'W',crmess)
         return
      end if
      trec = trec + 1

      prev_event = nevent

      version = iw(ind+1)
      nccrun  = iw(ind+2)
      nevent  = iw(ind+3)
      evtime  = iw(ind+4)
      evtype  = iw(ind+5)
      rocstat = iw(ind+6)
      evtclas = iw(ind+7)
      evtwght = iw(ind+8)

c
c-----------------------------------
c     answer some simple questions |
c-----------------------------------
c
c     has the run number changed?
c
      if (nccrun .gt .lccrun) then
         lchange = .true.
cfk  start from 0
         nevent_physics = 0
      else
         lchange = .false.
      end if

      nrec = nrec + 1

      if (evtclas .ge. 1 .and. evtclas .le. 15) then ! physics event
         nevent_physics = nevent
      endif
c     
c     has begin run been called this run?
c
      if (lchange) lbeg_run_called = .false.
c
c     if at run transition, had end run been called for the last run?
c
cfk      if (lchange .and. .not. lend_run_called .and. .not. lfirst) then
      if (lchange .and. .not. lend_run_called .and. lccrun.gt.0) then
         lend_last = .false.
      else
         lend_last = .true.
      end if
c
c     are we checking the event count?
c
      if (nevt_to_process .gt. 0 .or. nevt_to_process.eq.-1 ) then
         levt_check = .true.
      else
         levt_check = .false.
      end if
c     
c     is this a begin run (prestart) record?
c     
      if ((evtype .eq. 0 .or. evtype .le. -1)
     &     .and. evtclas .eq. 17 .and. .not. lbeg_run_called) then
         begrun = .true.
      end if
c     
c     is this a go record?
c     
      if ((evtype .eq. 0 .or. evtype .le. -1)
     &     .and. evtclas .eq. 18) then
         lgo = .true.
      end if
c     
c     is this an end run record?
c     
      if ((evtype .eq. 0 .or. evtype .le. -1)
     &     .and. evtclas .eq. 20) then ! end of run
         endrun = .true.
      end if
c
c     are we skipping this event?
c
      if (nevt_to_skip .gt. 0 .and. evtclas .ne. 0
     &     .and. .not. begrun .and. .not. lgo .and. .not. endrun) then
         levt_skip = .true.
      else
         levt_skip = .false.
      end if
c
c     deal with run skipping counter before setting run skip flag
c
      if (lrun_skip) then
         if (lchange .or. lfirst) then
            nrun_skipped = nrun_skipped + 1
            if (nrun_skipped .gt. nrun_to_skip) then
               nrun_to_skip = 0
               nrun_skipped = 0
               lpause = .true.
            end if
         end if
      end if
c
c     are we skipping this run?
c
      if (nrun_to_skip .gt. 0) then
         lrun_skip = .true.
      else
         lrun_skip = .false.
      end if
c
c--------------------------------
c     set flags for this record |
c--------------------------------
c
c     if the last run did not have endrun called, call it now
c
      if (.not. lend_last) then
         endrun = .true.
c        Tell the user this.
         write (crmess, 1000) lccrun, nccrun
 1000    format ('Run number changed WITHOUT ENDRUN record!! ',
     &        'old run=', i6, ' new run=',i6)
         CALL RECMES(CRNAME,'W',CRMESS)
         ITALLY_MISSED(20) = ITALLY_MISSED(20) + 1
      end if
c
c     if we are not skipping this record...
c
      if (.not. (lrun_skip .or. levt_skip)) then
c     
         lccrun = nccrun

c        begin run record
c     
         if (begrun) then
            lbeg_run_called = .true.
         end if
c     
c        end run record except for endrun associated with
c        a run number change (i.e., online version).
c
         if (endrun .and. .not. lchange) then
            lend_run_called = .true.
         end if
c
c     if the run number has changed, indicate that endrun been not yet been 
c     called for the new run.
c
         if (lchange) lend_run_called = .false.

cfk  do this also for epics and scaler events:
cfk  statistics banks have to be initiatlized

            if (.not. lbeg_run_called) then
               begrun = .true.
               lbeg_run_called = .true.
            end if
c
c        type 1-9 records
c
         if (evtype .ne. 0 .and. abs(evtype) .lt. 10) then
c
c           if begin run has not been called at this point, call it. We are
c           missing the prestart record in this run.
c
cfk            if (.not. lbeg_run_called) then
cfk               begrun = .true.
cfk               lbeg_run_called = .true.
cfk            end if
c
            if (evtclas .ge. 1 .and. evtclas .le. 15) then ! physics event
               lphysics = .true.
               if (levent_class(evtclas) .or. levent_all) revent = .true.
            else
               othdat = .true. 
            end if
cfk  store event no of previous 'physics' event in ROCSTAT of epics and scaler event
         else if (evtype .eq. 10) then ! scaler event
            call recmes(crname,'D','Scaler event seen')
            iw(ind+6) = nevent_physics
            lscaler = .true.
         else if (evtype .ge. 100) then ! epics database event
            call recmes(crname,'D','Epics event seen')
            lepics = .true.
            iw(ind+6) = nevent_physics
         end if
c
      else
         tevent_skip = tevent_skip + 1
         if (evtype .lt. 1)
     &      itally_skiped(evtclas) = itally_skiped(evtclas) + 1
      end if
c
c     count events if count checking is on
c
      if (levt_check .and. lphysics) then
         nevt_count = nevt_count + 1
         if (nevt_count .eq. nevt_to_process .or. nevt_to_process.eq.-1) then ! hit the limit with this event
            nevt_count = 0 ! zero counter
            nevt_to_process = 0 ! turn off count check
            if (lexit_pend) then
               lexit = .true.
            else
               lpause = .true.
            endif
         end if
      end if
c
c-------------------------
c     increment counters |
c-------------------------
c
c     deal with event skipping counter
c
      if (levt_skip) then
         nevt_skipped = nevt_skipped + 1
         if (nevt_skipped .eq. nevt_to_skip) then ! hit limit
            nevt_skipped = 0
            nevt_to_skip = 0
            lpause = .true.
         end if
      endif
c
c     count events being processed
c
      if (revent) then
         tevent_seen = tevent_seen + 1
         itally(evtclas) = itally(evtclas) + 1
      end if
c--------------------------------
c     set up for next iteration |
c--------------------------------
      lfirst = .false.

cfk  update only when event being processed
cfk      lccrun = nccrun
c
c  Put out the RUN and EVENT stamp every now and then
c
      IF (MOD(NREC,NSTAMP) .EQ. 0) THEN
       CALL RECMES(CRNAME,'N','  ')
       if (nrec .gt. 2147483647) then
          nrec = 0
       endif
      ENDIF
      return
      end


