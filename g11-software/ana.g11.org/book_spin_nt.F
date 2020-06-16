      subroutine book_spin_nt(id)
      IMPLICIT NONE
      
#include "SPINnt.inc"

      integer id,nwrite
      
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Kyungseon Joo')
      PARAMETER (CRNAME='BOOK_SPIN_NT')
      
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking ntuple for SPIN'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
      call hbname(id,'SPIN',nSPN,'nSPN[0,10]:I'// 
     1',xSPIN(nSPN):I'//
     1',ySPIN(nSPN):I'//
     1',zSPIN(nSPN):I')

      return
      end
