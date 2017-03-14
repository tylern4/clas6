      subroutine book_mvrt_nt(id)
      IMPLICIT NONE
      
#include "mvrt_nt.inc"

      integer id,nwrite
      
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Lee Cole Smith')
      PARAMETER (CRNAME='BOOK_MVRT_NT')
      
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking ntuple for MVRT'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
      call hbname(id,'MVRT',ntrk_mvrt,'ntrk_mvrt[0,40]:I'// 
     1',x_mvrt:R'//
     1',y_mvrt:R'//
     1',z_mvrt:R')

      return
      end


