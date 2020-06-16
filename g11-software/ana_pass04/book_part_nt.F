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
      
      call hbname(id,'PART',nPRT,'nPRT[0,20]:I'// 
     1',pidPART(nPRT):I'//
     1',xPART(nPRT):R'//
     1',yPART(nPRT):R'//
     1',zPART(nPRT):R'//
     1',ePART(nPRT):R'//
     1',pxPART(nPRT):R'//
     1',pyPART(nPRT):R'//
     1',pzPART(nPRT):R'//
     1',qPART(nPRT):R'//
     1',trkPART(nPRT)[-16,16]:I'//
     1',qpiPART(nPRT):R'//
     1',qtrPART(nPRT):R'//
     1',flgPART(nPRT):I')


      call hbname(id,'TBID', nTBID,'nTBID[0,100]:I,'//
     1'trkTBID(nTBID)[0,20]:I,secTBID(nTBID)[0,6]:I,bTBID(nTBID):R,vtTBID(nTBID):R,'//
     2'scsTBID(nTBID)[0,6]:I,sciTBID(nTBID)[0,20]:I,sctTBID(nTBID):R,'//
     3'scqTBID(nTBID):R,scvTBID(nTBID):R,scbTBID(nTBID):R,'//
     4'ecsTBID(nTBID)[0,6]:I,eciTBID(nTBID)[0,20]:I,ectTBID(nTBID):R,'//
     5'ecqTBID(nTBID):R,ecvTBID(nTBID):R,ecbTBID(nTBID):R,'//
     6'stsTBID(nTBID)[0,6]:I,stiTBID(nTBID)[0,20]:I,sttTBID(nTBID):R,'//
     7'stqTBID(nTBID):R,stvTBID(nTBID):R,stbTBID(nTBID):R,'//
     8'lcsTBID(nTBID)[0,6]:I,lciTBID(nTBID)[0,20]:I,lctTBID(nTBID):R,'//
     9'lcqTBID(nTBID):R,lcvTBID(nTBID):R,lcbTBID(nTBID):R') 


      return
      end
