      subroutine book_tgpb_nt(id)
      IMPLICIT NONE
      
#include "tgpb_nt.inc"

      integer id
      
      call hbname(id,'TGPB',nphotons,'nphotons[0,100]:I'// 
     1',tgpb_pointer(nphotons):I'//
     1',tgpb_Time(nphotons):R'//
     1',tgpb_Energy(nphotons):R'//
     1',tgpb_dt(nphotons):R')

      return
      end
