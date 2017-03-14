      subroutine book_part_nt(id)
      IMPLICIT NONE
      
#include "PARTnt.inc"

      integer id,nwrite
      
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Lee Cole Smith')
      PARAMETER (CRNAME='BOOK_PART_NT')
      
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking ntuple for PART'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
      call hbname(id,'PART',nPRT,'nPRT[0,10]:I'// 
     1',pidPART(nPRT):I'//
     1',xPART(nPRT):R'//
     1',yPART(nPRT):R'//
     1',zPART(nPRT):R'//
     1',ePART(nPRT):R'//
     1',pxPART(nPRT):R'//
     1',pyPART(nPRT):R'//
     1',pzPART(nPRT):R'//
     1',qPART(nPRT):R'//
     1',flagsPART(nPRT):I')

      return
      end
