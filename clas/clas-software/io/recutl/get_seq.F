      subroutine get_seq(iseqno)
      implicit none
      save
#include "re_units.inc"
#include "run_control.inc"
      integer*4 iseqno, iseqno_save, iseqno_out
      integer*4 istat
      character*20 evivar
      character*128 filnam
      data iseqno_save/-1/
      if (ltmp .eq. 0) stop 'temp unit not initialized'
      if (iseqno_save .eq. -1) then
         evivar = 'RECSIS_RUNTIME'
         call revinm(evivar,'recseq.ini',filnam)
         open (unit = ltmp, file = filnam, status = 'old',
     &        iostat = istat)
         if (istat .ne. 0) then
            call recmes('get_seq','W',
     &           'error opening sequence number file')
            call recmes('get_seq','W', filnam)
            call recmes('get_seq','W',
     &           '  Setting sequence number to zero.')
            iseqno_save = 0
         else
            read (ltmp,*) iseqno_save
C            close (unit = ltmp)
            iseqno_out = iseqno_save + 1
            if (iseqno_out .ge. 10000) iseqno_out = 1
            rewind(ltmp)
C            open (unit = ltmp, file = filnam, status = 'old')
            write (ltmp,100) iseqno_out
 100        format (i4)
         end if
         close (unit = ltmp)
      endif
      iseqno = iseqno_save
      return
      end
