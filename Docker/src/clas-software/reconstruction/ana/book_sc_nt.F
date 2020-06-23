      subroutine book_sc_nt(ntid)
      IMPLICIT NONE
      integer ntid

#include "SCnt.inc"

      call hbname(ntid,'SC',nSC,'nSC[0,342]:I,'//
     1'secSC(nSC)[1,6]:I,idSC(nSC)[0,57]:I,'//
     1'TDCLSC(nSC)[0,16383]:I,ADCLSC(nSC)[0,16383]:I,'//  
     1'TDCRSC(nSC)[0,16383]:I,ADCRSC(nSC)[0,16383]:I')

      return
      end
