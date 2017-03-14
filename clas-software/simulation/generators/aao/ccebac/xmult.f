      subroutine xmult(egam,egam0,pon,wcm,j2,l2,iso,zamp,zmuls)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)

      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c ts 10-30-2007
      common /etapi / jetapi
c

      dimension zmul(3),zamp(6),zmuls(8,0:maxl,3)

c ts 10-30-2007
      if(jetapi.eq.1) then
      fmeson = fpio
      else if(jetapi.eq.2) then
      fmeson = feta
      end if
c ts
      emf  = sqrt(fpio**2 + pon**2)
      ebf  = sqrt(fnuc**2 + pon**2)
      ebi  = sqrt(fnuc**2 + egam**2)

      xfac = 4.d0*pi**2/wcm*sqrt(ebf*emf*abs(egam0)*ebi)/2.d0/pi
      
      zei = (0.d0,1.d0)
      zmul = 0

      jjx       = j2
      jx        = (jjx + 1)/2  ! j+1/2
      xxx       = sqrt(dble(jx-1)/dble(jx+1))
      ll        = l2/2
      zfac  = xfac/dble(2*jx)/zei

c      write(*,*)'zfac',zfac,fpio,fnuc,pon


      if(j2-l2.lt.0) then    ! j = l - 1/2

c      jl    = (jjx +1)/2

      if(jx.gt.1) then
       zmul(1) = zfac*( zamp(2) +  zamp(1)/xxx)
       zmuls(2,ll,iso) = zmul(1)
      end if
       zmul(2) = zfac*(-zamp(2) +  zamp(1)*xxx)
       zmul(3) = sqrt(2.d0)*zfac*zamp(3)
       zmuls(4,ll,iso) = zmul(2)
       zmuls(8,ll,iso) = zmul(3)
       
      else                 ! j = l + 1/2

c      jl    = (jjx -1)/2

      if(jx.gt.1) then
       zmul(2) = zfac*(-zamp(2)-zamp(1)/xxx)
       zmuls(3,ll,iso) = zmul(2)
      end if
       zmul(1) = zfac*(-zamp(2)+zamp(1)*xxx)
       zmul(3) =-sqrt(2.d0)*zfac*zamp(3)
       zmuls(1,ll,iso) = zmul(1)
       zmuls(7,ll,iso) = zmul(3)

      end if

      return
      end

