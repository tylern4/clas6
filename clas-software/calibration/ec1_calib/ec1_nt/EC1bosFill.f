      subroutine EC1bosFill
*
      IMPLICIT NONE
      SAVE
      include "bcs.inc"
*
      include "EC1nt.inc"
*
      integer namind, nami
      integer ind,nd,nrow,j,k
      integer sect
      integer*2 I16(2000)            ! 16 bits
      integer*4 I32(1000)            ! 32 bits
      data nami/0/ 
      equivalence (I16(1), I32(1))
*
      If (nami.eq.0) nami = namind('EC1 ')
      ind=nami+1               ! initialize ind
      nEC1 = 0
*
30        continue
      ind = IW (ind-1)         !next index
          If(ind.eq.0) go to 99
*
      sect =IW(ind-2)
      nrow = IW( ind -4 )
      nd = IW( ind )
*
      do j=1,nd
        I32(j)=IW(ind+j)       ! bos stored in 32 bit
      end do
*
      k = 0
*
      do j = 1, nrow 
*
        nEC1 = nEC1 +1
        sector (nEC1 ) = sect
        k = K + 1
        layer (nEC1 ) = I16( k )/256
        strip (nEC1 ) = mod ( I16( k ), 256)
        k = K + 1
        TDCL (nEC1 ) = I16( k )
        k = K + 1
        ADCL (nEC1 ) = I16( k )
        k = K + 1
        TDCR (nEC1 ) = I16( k )
        k = K + 1
        ADCR (nEC1 ) = I16( k )
*
       enddo
*
      goto 30
99    return
      end
