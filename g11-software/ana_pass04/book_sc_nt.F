      subroutine book_sc_nt(ntid)
      IMPLICIT NONE
      integer ntid

#include "SCnt.inc"

      call hbname(ntid,'SC',nSC,'nSC[0,288]:I,'//
     1'secSC(nSC)[1,6]:I,idSC(nSC)[1,48]:I,'//
     1'TDCLSC(nSC)[0,4095]:I,ADCLSC(nSC)[0,16383]:I,'//  
     1'TDCRSC(nSC)[0,4095]:I,ADCRSC(nSC)[0,16383]:I')

      return
      end
