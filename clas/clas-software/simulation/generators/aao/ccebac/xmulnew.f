c ts 11-1-2007

      subroutine xmulnew(zamp,j2,l2,zout)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
      dimension zamp(6),zout(8)
      
      zout      = 0
      zei       = (0.d0,1.d0)
      jjx       = j2
      jx        = (jjx + 1)/2  ! j+1/2
      xxx       = sqrt(dble(jx-1)/dble(jx+1))
      ll        = l2/2
      zfac      = 1.d0/dble(2*jx)/zei

      if(j2-l2.lt.0) then    ! j = l - 1/2

c      jl    = (jjx +1)/2

      if(jx.gt.1) then
       zout(2)  = zfac*( zamp(2) +  zamp(1)/xxx)
      end if
       zout(4)  = zfac*(-zamp(2) +  zamp(1)*xxx)
       zout(8)  = sqrt(2.d0)*zfac*zamp(3)
       
      else                 ! j = l + 1/2

c      jl    = (jjx -1)/2

      if(jx.gt.1) then
       zout(3)  = zfac*(-zamp(2)-zamp(1)/xxx)
      end if
       zout(1)  = zfac*(-zamp(2)+zamp(1)*xxx)
       zout(7)  =-sqrt(2.d0)*zfac*zamp(3)

      end if

      return
      end

