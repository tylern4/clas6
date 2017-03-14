      subroutine book_tgbi_nt(id)
      IMPLICIT NONE
      
#include "tgbi_nt.inc"

      integer id
      
      call hbname(id,'TGBI',latch1,'latch1:I')

      return
      end
