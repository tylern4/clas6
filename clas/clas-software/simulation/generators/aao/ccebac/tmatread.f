      subroutine tmatread(isw3)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)

       COMMON/ELEM / zwf(maxwcm,maxlsj,maxmom,maxmb*5,2)
       common/ELEM2/zsigma(maxwcm,maxlsj,maxres,maxres)
     &            ,zf(maxwcm,maxlsj,5,maxmb,maxmom,maxres)

       common /ELEM3/ ztpin(maxwcm,maxlsj)
     1                ,ztmx(maxwcm,maxlsj)
     2               ,ztres(maxwcm,maxlsj)
     3            ,zsigsato(maxwcm,maxlsj,maxres,maxres)

       common/ELEM4/ zfmb(maxwcm,maxlsj,maxmom,maxmb,5,maxres)

      common/nstardt/nstar0(maxlsj),amstar(maxres,maxlsj)
     &    ,coup(5,maxmb,maxres,maxlsj),cut(5,maxmb,maxres,maxlsj)

       common/mesh00/nc,ne0,np1,ich(maxmb),
     1   ze0(maxwcm),am1(maxmb),am2(maxmb),istab(maxmb)
     1  ,zp(maxmom,maxmb,maxwcm),zwp(maxmom,maxmb,maxwcm)
     1  ,zp0(maxmb,maxwcm),zgreen(maxmom,maxmb,maxwcm)
      common/pot1/nchh,Ldt(5,maxmb,maxlsj),isdt(5,maxmb,maxlsj)
     &                                    ,nLsdt(maxmb,maxlsj)
      common/chdat1/njLs,jpind(maxlsj) ,Lpind(maxlsj)
     &                  ,ispind(maxlsj),itpind(maxlsj)
      common/chdat2/nch,ich0(maxmb)
c
c===========================================================
      if(isw3.eq.1) then

      read(51)zsigma,zsigsato,zf,ztpin,ztmx,ztres,zwf,zfmb

      return
      end if
c============================================================
      ion   = np1
c
      do 100 ie = 1,ne0
      do 180 ii  = 1,njls

      kres       = nstar0(ii)

      if(kres.ne.0) then

c  1/G(N*), zsigsato = m_0 + Sigma

      do irf = 1,kres
      do iri = 1,kres
      read(51,*)irfx,irix,xr,xi,xr1,xi1
      zsigma(ie,ii,irf,iri) = xr + (0.d0,1.d0)*xi
      zsigsato(ie,ii,irf,iri) = xr1+ (0.d0,1.d0)*xi1
      end do
      end do


c   <pi N|Gamma|N*>

      do ire1 = 1,kres
      read(51,*)ire1x,xr1,xi1,xr2,xi2
      zf(ie,ii,1,1,ion,ire1)= xr1 + (0.d0,1.d0)*xi1
      zf(ie,ii,1,2,ion,ire1)= xr2 + (0.d0,1.d0)*xi2
c      print*,ie,ii,1,imbpn,ion,ire1,xr,xi
      end do
      end if
c
c   <pi N| T|pi N> amplitude
c
c     full  non-res  res
c
      read(51,*)x1,x2,x3,x4,x5,x6
      ztpint = x1 + (0.d0,1.d0)*x2
      ztpinb = x3 + (0.d0,1.d0)*x4
      ztpinr = x5 + (0.d0,1.d0)*x6
      ztpin(ie,ii) = ztpint
      ztmx (ie,ii) = ztpinb
      ztres(ie,ii) = ztpinr
c
c
c  <pi N|(1 + gV)^{^1}|MB>
c  <N*|Gamma G_0|MB>
c
      do imb = 1,nch
      do lsj = 1,nLsdt(imb,ii)   !
      do ip  = 1,np1

      read(51,*)imbx,lsjx,ipx,xr1,xi1,xr2,xi2
      kkki   = (imbx - 1)*5 + lsjx
      zwf(ie,ii,ip,kkki,1) = xr1 + (0.d0,1.d0)*xi1
      zwf(ie,ii,ip,kkki,2) = xr2 + (0.d0,1.d0)*xi2

      if(kres.ne.0) then
      do ires = 1,kres
      read(51,*)iresx,xr,xi
      zfmb(ie,ii,ip,imb,lsj,ires)= xr + (0.d0,1.d0)*xi
      end do
      end if

      end do
      end do
      end do

 180  continue  ! lsj    loop
 100  continue  ! energy loop

      return
      end

