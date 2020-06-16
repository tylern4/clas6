      subroutine book_ec_nt(ntid)
      IMPLICIT NONE
      
#include "ECnt.inc"
      
      integer ntid,nwrite
      
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Lee Cole Smith')
      PARAMETER (CRNAME='BOOK_EC_NT')
      
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking ntuple for EC'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

      call hbname(ntid,'EC',nEC,'nEC[0,1296]:I,secEC(nEC)[1,6]:I,'//
     1'layerEC(nEC)[1,6]:I,stripEC(nEC)[1,36]:I,'//
     1'TDCEC(nEC)[0,4095]:I,ADCEC(nEC)[0,16383]:I')
          
      return
      end
