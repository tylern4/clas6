      subroutine scbook(id)
      IMPLICIT NONE
      integer id

      include 'SCnt.inc'
      
      call hbname(id,'SC',nSC,'nSC[0,288]:I  
     1,secSC(nSC)[1,6]:I  
     1,idSC(nSC)[1,48]:I  
     1,TDCLSC(nSC)[1,4095]:I  
     1,ADCLSC(nSC)[1,8191]:I  
     1,TDCRSC(nSC)[1,4095]:I  
     1,ADCRSC(nSC)[1,8191]:I')

      return

      end
