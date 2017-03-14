      subroutine fill_sc_nt
      
#include "bcs.inc"
#include "SCnt.inc"
#include "sccal.inc" 
           
      integer min, max, idummy
      integer name_i,ind,nrow,nword,i,j,k,sector,ifirst,bar
      integer*2 I16(2000)            ! 16 bits
      integer*4 I32(1000)            ! 32 bits
      data ifirst/0/ 
      equivalence (I16(1), I32(1))
      save name_i
      
      if (ifirst.eq.0) then
        name_i = mamind(iw,'SC  ')
        if (name_i.eq.0) then
          print *,' NO SC bank registered'
          return
        endif
        ifirst = 1
      endif
      
      nSC = 0
      ind = name_i +1

 30   ind = iw(ind-1)
      if (ind.EQ.0) goto 99
      
      nword   = iw(ind)
      sector  = iw(ind-2)
      nrow    = iw(ind-4)
        
      do j=1,nword
         I32(j)=IW(ind+j)       ! bos stored in 32 bit
      end do
        
      k = 0
      
      do 20 i = 1, nrow 
         nSC 		= nSC + 1
         secSC(nSC )	= sector
         k 		= k + 1
         idummy        = I16(k)
         idSC (nSC ) 	= min(n_scpd_sec,max(1,idummy))
         bar		= idSC(nSC) 
         if(bar.eq.0) bar=1
         k 		= k + 1
         idummy        = I16(k)
         TDCLSC (nSC ) = min(16383,max(0,idummy))
         k 		= k + 1
         idummy        = I16(k)
         ADCLSC (nSC ) = min(16383,max(0,
     &                       idummy-IFIX(scped(bar,1,sector))))
         k 		= k + 1
         idummy        = I16(k)
         TDCRSC (nSC ) = min(16383,max(0,idummy))
         k 		= k + 1
         idummy        = I16(k)
         ADCRSC (nSC ) = min(16383,max(0,
     &                        idummy-IFIX(scped(bar,2,sector))))
         if (k.GT.nword*2) goto 30
 20   continue

      goto 30

 99   return
      end
