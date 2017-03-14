      subroutine fill_cc_nt
      
#include "bcs.inc"
#include "CCnt.inc"
#include "cccal.inc"
      
      integer max, idummy
      integer name_i,ind,nrow,nword,i,j,k,sector,ifirst
      integer*2 I16(2000)            ! 16 bits
      integer*4 I32(1000)            ! 32 bits
      data ifirst/0/ 
      equivalence (I16(1), I32(1))
      save name_i
      
      if (ifirst.eq.0) then
        name_i = mamind(iw,'CC  ')
        if (name_i.eq.0) then
          print *,' NO CC bank registered'
          return
        endif
        ifirst = 1
      endif
      
      ind = name_i +1
      nCC = 0

 30   ind = iw(ind-1)
      if (ind.EQ.0) goto 99
      
      nword   = iw(ind)
      sector  = iw(ind-2)
      nrow    = iw(ind-4)
        
      do j=1,nword
         I32(j)=IW(ind+j)       ! bos stored in 32 bit
      end do
        
      k = 0
        
      do i = 1, nrow
         nCC 		= nCC +1
         secCC(nCC)	= sector
         k 		= k + 1
         idCC(nCC) 	= I16(k)
         chan		= idCC(nCC)
         k 		= k + 1
         TDCCC(nCC)	= I16(k)
         k 		= k + 1
         idummy        = I16(k)
         ADCCC(nCC)	= max(0,IFIX(idummy-ccpedes(sector,chan)))
         if (k.GT.nword*2) goto 30
      enddo

      goto 30
        
 99   return
      end
