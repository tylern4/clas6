      subroutine book_cc_nt(ntid)
      IMPLICIT NONE
      integer ntid
      
#include "CCnt.inc"

      call hbname(ntid,'CC',nCC,'nCC[0,216]:I,'//  
     1'secCC(nCC)[1,6]:I,idCC(nCC)[1,36]:I,'//  
     1'TDCCC(nCC)[0,4095]:I,ADCCC(nCC)[0,16383]:I')

      return
      end
