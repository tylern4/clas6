      SUBROUTINE RE_ET_GET(ISTAT, LSKIP_TO_SOCKET)
      IMPLICIT NONE

#include "et.inc"
#include "run_control.inc"
#include "bcs.inc"

c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RE_ET_GET')
      PARAMETER (CRAUTH='Gilfoyle')

      LOGICAL LSKIP_TO_SOCKET
      INTEGER ISTAT, EF, id1,id2,id3,id4,mamind
      integer Nold, elapsed_rec, timeout, get_et_event, count, mycnt
      REAL    IPC_CHECK_INTERVAL, min_sleep, time_interval
      real    elapsed_time, Told, current_time, last_evnt_time
      real    time_since_last_evnt, max_time_between_evnts

      parameter (min_sleep = 0.1)
      parameter (max_time_between_evnts = 1.0)
      parameter (time_interval = 10.0)
      DATA IPC_CHECK_INTERVAL/0.0/ ! seconds, i. e., go as fast as you can
      data timeout/0/
      data count/0/

      SAVE

c
c get the data here (istat=1). if we're unsuccessful we keep
c trying. we bail out (istat=-1) only if we're told to (lexit=.true.)
c or if the ET system disappears (ef<0). 
c we also check to see if its been a long time (max_time_between_evnts)
c since we got any data. if it has been, then go to sleep using the
c ipc_check routine and set the record rate to zero.
c
      IF ( .NOT. LEXIT) THEN 
         CALL IPC_CHECK(IPC_CHECK_INTERVAL)
         call bdrop(iw,'R')
         if(et_session(1:8).eq.'clasprod') then
           EF = GET_ET_EVENT(iw,timeout)
         else
           timeout=1000
           EF = GET_ET_EVENT(iw,timeout)
           if(ef.eq.2) then
             mycnt=mycnt+1
             if(mod(mycnt,100).eq.0) print *,' empty event: ',ef,mycnt
           endif
         endif
         call timex(current_time)

         IF (EF.EQ.-1) THEN
            ISTAT = 1
            et_ok = 1
            call timex(last_evnt_time)
            time_since_last_evnt = 0
            number_events_read = number_events_read + 1
         ELSE IF (EF.EQ.0 .OR. EF.EQ.1) THEN
            et_ok = 0
            istat = -1
            lexit = .true.
            call recmes(crname,'W',
     1             'Exiting because ET ring was detached!!!!!!!') 
         ELSE
            et_ok = 1
            LSKIP_TO_SOCKET = .TRUE.
            time_since_last_evnt = current_time - last_evnt_time
         END IF   
         
         if (time_since_last_evnt .gt. max_time_between_evnts) then
            rec_rate = 0.0
            call ipc_check(min_sleep)
         end if

         elapsed_time = current_time - Told
c
c get the event/record rate every TIME_INTERVAL seconds by
c passing number_events_read and rec_rate at this time.
c
         if (elapsed_time.ge.time_interval) then
            elapsed_rec = number_events_read - nold
            rec_rate = float(elapsed_rec)/elapsed_time
            Nold = number_events_read
            Told = current_time
         end if

      ELSE   
         ISTAT = -1
      END IF

      RETURN
      END
