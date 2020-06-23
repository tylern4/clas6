      SUBROUTINE fsgen_END_BOS(ierr)
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S.Stepanyan     Created:  Tue Mar 25 14:32:25 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     
#include "parm2.inc"
      character*8 recname
      integer status,ierr
C
#include "bcs.inc"
C
      if(bout.eq.0)then
c         call fwbos(iw,jout,'0',ierr)
         call bosClose(jout)
      endif
      if(bin.eq.0)then
         call bosClose(jin)
      endif
      RETURN
      END


