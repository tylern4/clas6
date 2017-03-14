      subroutine book_tagr_nt(id)
      IMPLICIT NONE
      
#include "tagr_nt.inc"

      integer id
      
      call hbname(id,'TAGR',nphot_tagr,'nphot_tagr[0,100]:I'// 
     1',tagr_E_gamma(nphot_tagr):R'//
     1',tagr_ttag(nphot_tagr):R'//
     1',tagr_tpho(nphot_tagr):R'//
     1',tagr_stat(nphot_tagr):I'//
     1',tagr_Tid(nphot_tagr):I'//
     1',tagr_Eid(nphot_tagr):I')

      return
      end
