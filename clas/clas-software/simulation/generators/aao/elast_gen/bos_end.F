      SUBROUTINE BOS_END(recname)
      character*8 recname
      integer ierr

#include "bcs.inc"

      call fwbos(iw,33,'0',ierr)
c puneetk Jan 12, 2012: commented the line below because breaks compiler
c This bosta only seems to dump bos related info to a log file at end of
c code.
c      call bosta
      call fparm('CLOSE recname')

      RETURN
      END
