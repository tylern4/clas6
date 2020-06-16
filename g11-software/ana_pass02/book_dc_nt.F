      subroutine book_dc_nt(ntid)
      IMPLICIT NONE
      integer ntid
      
#include "DCnt.inc"

      call hbname(ntid,'DC',nDC,'nDC[0,512]:I,'// 
     1'secDC(nDC)[1,6]:I,layerDC(nDC)[1,36]:I,'//
     1'WireDC(nDC)[1,193]:I,TDCDC(nDC)[0,16383]:I')

      return
      end
