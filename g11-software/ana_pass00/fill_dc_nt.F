      subroutine fill_dc_nt
      
#include "bcs.inc"
#include "DCnt.inc"
      
      integer max, min, mod, idummy
      integer name_i,ind,nrow,nword,i,j,k,sector,ifirst
      integer*2 I16(2000)            ! 16 bits
      integer*4 I32(1000)            ! 32 bits
      data ifirst/0/ 
      equivalence (I16(1), I32(1))
      save name_i
      
      if (ifirst.eq.0) then
        name_i = mamind(iw,'DC  ')
        if (name_i.eq.0) then
          print *,' NO DC bank registered'
          return
        endif
        ifirst = 1
      endif
      
      ind = iw(name_i)
      nDC = 0

30    if (ind.ne.0) then
      
        nword   = iw(ind)
        sector  = iw(ind-2)
        nrow    = iw(ind-4)
        
        do j=1,nword
          I32(j)=IW(ind+j)       ! bos stored in 32 bit
        end do

        k = 0
        
        do i = 1, nrow 
          nDC 			= nDC + 1
          secDC(nDC)		= sector
          k 			= k + 1
          idummy              = I16(k)
          layerDC(nDC)        = max(1,min(36,idummy/256))
          WireDC(nDC)         = max(1,min(193,mod(idummy,256)))
          k                     = k + 1
          idummy              = I16(k)
          TDCDC(nDC)         = max(0,min(16382,idummy))
        enddo
        
        ind = iw(ind-1)
        goto 30
        
      endif
      
      return
      end
