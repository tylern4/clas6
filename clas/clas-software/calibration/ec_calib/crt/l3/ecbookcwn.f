      subroutine ecbook(id)
      IMPLICIT NONE
      integer id

      include 'ECnt.inc'

      call hbname(id,'EC',nEC,'nEC[0,1296]:I  
     1,secEC(nEC)[1,6]:I  
     1,layerEC(nEC)[1,6]:I  
     1,stripEC(nEC)[1,36]:I  
     1,TDCEC(nEC)[0,4095]:I  
     1,ADCEC(nEC)[0,16383]:I')

      return

      end
