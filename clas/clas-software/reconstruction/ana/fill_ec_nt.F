      subroutine fill_ec_nt
      
      implicit none
      save
      
#include "bcs.inc"
#include "ECnt.inc"
#include "eccal.inc"

      integer min, max, mod
      integer mamind, nami, idummy
      integer ind,nd,nrow,i,j,k
      integer sec,str,lay
      integer*2 I16(2000)            ! 16 bits
      integer*4 I32(1000)            ! 32 bits
      data nami/0/ 
      equivalence (I16(1), I32(1))

      if (nami.eq.0) nami = mamind(iw,'EC  ')
      ind = nami + 1
      
      nEC = 0

30    ind 	= iw(ind-1)
      if (ind.eq.0) go to 99
      
      nd 	= iw(ind)
      sec	= iw(ind-2)
      nrow  	= iw(ind-4)
        
      do j=1,nd
        I32(j)=IW(ind+j)       ! bos stored in 32 bit
      enddo
      
      k = 0

      do i = 1, nrow
        nEC		= nEC + 1
        secEC(nEC)	= sec 
        k		= k + 1
        layerEC(nEC)	= i16(k)/256
        idummy          = I16(k)
        stripEC(nEC)	= mod(idummy,256)
        k		= k + 1
        idummy          = I16(k)
        tdcEC(nEC)	= max(0,min(4095,idummy))
        k		= k + 1
        str		= stripEC(nEC)
        lay		= layerEC(nEC)
        idummy          = I16(k)
        adcEC(nEC)	= max(0,IFIX(idummy-ecped(str,lay,sec)))
        if (k.GT.nd*2) goto 30
      enddo
        
      goto 30
        
99    return
      end

