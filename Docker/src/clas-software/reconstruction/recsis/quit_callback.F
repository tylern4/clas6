      subroutine quit_callback(csignal)

c  stops run upon receipt of termination signal or control message

c  ejw, 1-oct-96
c  modified for online recsis, gpg 14-july-98


      implicit none
      save

#include "run_control.inc"

      integer csignal
      CHARACTER*(*)  CRNAME
      PARAMETER (CRNAME='quit_callback')

c  executable code:
c  ----------------

      lexit=.true.

      call recmes(crname,'I',' ')
      call recmes(crname,'I','Online RECSIS quit callback received termination signal.')
      call recmes(crname,'I','   ...will stop after finishing this event')
      call recmes(crname,'I',' ')

      return
      end


c-----------------------------------------------------------


