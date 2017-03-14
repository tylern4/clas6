      subroutine status_poll_callback(msg)

c  fills user info for status_poll

c  ejw, 1-oct-96
c  stolen from ejw, gpg 7/15/97

      implicit none
      save

#include "run_control.inc"
#include "et.inc"

      integer msg


c  executable code:
c  ----------------

c  processing stats
c
c  number of events
c
      call ipc_status_append_str(msg,'Total number of events read')
      call ipc_status_append_int4(msg,number_events_read)
c
c Event processing rate.
c
      call ipc_status_append_str(msg,'Record Rate')
      call ipc_status_append_real8(msg,rec_rate)

      return
      end


c-----------------------------------------------------------


