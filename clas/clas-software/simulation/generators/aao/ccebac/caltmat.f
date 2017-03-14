      SUBROUTINE caltmat(iflag,isw)
      IMPLICIT REAL*8(A-H,O-Y)
      IMPLICIT COMPLEX*16(Z)
      parameter(nmat=392)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)

      character allwaves*3

      dimension zCMM(nmat,nmat),zVTT(nmat,nmat)
      dimension xx(100),wx(100)
      dimension zv(nmat,nmat),zt(nmat,nmat)

      common / input2 / np,icres,icpot


      common/waves/allwaves(maxlsj)
      common/chdat1/njLs,jpind(maxlsj) ,Lpind(maxlsj)
     &                  ,ispind(maxlsj),itpind(maxlsj)
      common/chdat2/nch,ich0(maxmb)
      common/pot1/nchh,Ldt(5,maxmb,maxlsj),isdt(5,maxmb,maxlsj)
     &                                    ,nLsdt(maxmb,maxlsj)
      common/nstardt/nstar0(maxlsj),amstar(maxres,maxlsj)
     &    ,coup(5,maxmb,maxres,maxlsj),cut(5,maxmb,maxres,maxlsj)
       common/mesh00/nc,ne0,np1,ich(maxmb),
     1   ze0(maxwcm),am1(maxmb),am2(maxmb),istab(maxmb)
     1  ,zp(maxmom,maxmb,maxwcm),zwp(maxmom,maxmb,maxwcm)
     1  ,zp0(maxmb,maxwcm),zgreen(maxmom,maxmb,maxwcm)


      common/zpott/zpp(maxmom,maxmb),zvv(maxmom,maxmb,5,maxmom,maxmb,5)
      common/wfdt/zwf(5,maxmb,maxmom,5,maxmb,maxmom)

       COMMON/ELEM/ZCC_sato(maxwcm,maxlsj,maxmom,maxmb*5,2)
       common/ELEM2/zsigma(maxwcm,maxlsj,maxres,maxres)
     &            ,zf(maxwcm,maxlsj,5,maxmb,maxmom,maxres)


       common /ELEM3/ ztpin(maxwcm,maxlsj)
     1                ,ztmx(maxwcm,maxlsj)
     2               ,ztres(maxwcm,maxlsj)
     3            ,zsigsato(maxwcm,maxlsj,maxres,maxres)

       common/ELEM4/ zfmb(maxwcm,maxlsj,maxmom,maxmb,5,maxres)
      
      dimension ztmx12(maxwcm,maxlsj)
      dimension ztpinen(maxwcm,maxlsj),zramp(5,maxmb,5,maxmb)
     1 ,ztotamp(5,maxmb,5,maxmb,maxwcm,maxlsj)

c tsato 08-26-2007
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c tsato 08-26-2007


      dimension iphase(5)
      data iphase/-1,-1,-1,1,1/         ! assume pn en pd sn rn

c---------------------------------------------------------------
c define mesh points for each channel for all energies considered
c---------------------------------------------------------------

       call momentum(isw)

c--------------------------------------------------------
c      calculate all propagators
c-------------------------------------------------------

       call propagator
c
c      caculate G*p**2*wp
c
       do 290 ie=1,ne0
       do ic=1,nc
       do ip=1,np1
       zpx=zp(ip,ic,ie)
       if(ip.ne.np1)then
       zgreen(ip,ic,ie)=zgreen(ip,ic,ie)*zpx**2*zwp(ip,ic,ie)
       else
       zgreen(ip,ic,ie)=zwp(ip,ic,ie)*zpx**2
       end if
       end do
       end do
  290  continue
c
       np2=2*np1

c-----------------------------------------------------
       do 20 ie  = 1,ne0

       do ic     = 1,nc
       do ip     = 1,np1
       zpp(ip,ic)= zp(ip,ic,ie)

c       if(ic.eq.1) then 
c          write(99,1010)ip,ie,zpp(ip,ic)
c 1010     format(1h ,2i3,2e15.5)
c       end if

       end do
       end do

       do 10 i=1,njLs

       call potential(np1,i)

c
c   zcmm = 1 - VG_0
c

       n1 = 0
       do 11 ic1 = 1,nc
       nLs1      = nLsdt(ic1,i)
       do 11 iLs1= 1,nLs1
       nmax1     = np1

       do 11 ip1 = 1,nmax1
       n1        = n1+1
       n2        = 0
       do 12 ic2 = 1,nc
       nLs2      = nLsdt(ic2,i)
       do 12 iLs2= 1,nLs2
       nmax2     = np1

       do 12 ip2 = 1,nmax2
       zp2        = zp(ip2,ic2,ie)
       n2        = n2+1

       zv(n1,n2)  = zvv(ip1,ic1,iLs1,ip2,ic2,iLs2)

       zcmm(n1,n2)=-zvv(ip1,ic1,iLs1,ip2,ic2,iLs2)
     1           * zgreen(ip2,ic2,ie)

c       if(ic1.eq.1.and.ic2.eq.1) then
c       if(ip1.eq.np1.and.ip2.eq.np1) then
c          write(99,1011)ic1,ic2,ip1,ip2,zv(n1,n2),zcmm(n1,n2)
c 1011     format(1h ,4i3,2e15.5,2f10.5)
c       end if
c       end if


       if(n1.eq.n2) zcmm(n1,n2) = zcmm(n1,n2)+1.d0
   12  continue
   11  continue

c
c    zcmm -> (1 - V G_0)^(-1)
c

       if(n1.ne.n2)write(6,*)' error in defining cmm'
       if(n1.ne.n2)stop
       
       ncm =  n1

       call matinc(zcmm,ncm,zvtt,0,zdeterm,nmat)

c ts
c
!< ELEM ! only on flag 3
       if (iflag.eq.3) then

       n2=0
       do 612 ic2=1,nc
       nLs2=nLsdt(ic2,i)
       do 612 iLs2=1,nLs2
       do 612 ip2=1,np1
       n2=n2+1
 
        iid=(ic2-1)*5+ils2
c ts 11-1-2007
        zcc_sato(ie,i,ip2,iid,1)=zcmm(np1,n2)
        zcc_sato(ie,i,ip2,iid,2)=zcmm(np2,n2)
 612    continue
       endif
!> ELEM

c
c  zt -> (1 - V G_0)^(-1) V
c

       zt         = 0

       do 13 ip1  = 1,ncm
       do 13 ip2  = 1,ncm

       do ip3     = 1,ncm
       zt(ip1,ip2)= zt(ip1,ip2)+zcmm(ip1,ip3)*zv(ip3,ip2)
       end do
   13  continue


c
c      store scattering wavefunctions
c
       n1=0
       do 711 ic1=1,nc
       nLs1=nLsdt(ic1,i)
       do 711 iLs1=1,nLs1
       do 711 ip1=1,np1
       n1=n1+1

       n2=0
       do 712 ic2=1,nc
       nLs2=nLsdt(ic2,i)
       do 712 iLs2=1,nLs2
       do 712 ip2=1,np1
       n2=n2+1
       zwf(iLs1,ic1,ip1,iLs2,ic2,ip2)=zcmm(n1,n2)
  712  continue
  711  continue
c
c      on-shell pi N -> pi N
c
       ztmx(ie,i)=zt(np1,np1)

c       write(998,*)i,ie,ztmx(ie,i)

c
c      on-shell pi N -> eta N
c
       ztmx12(ie,i)=zt(np2,np1)
c
c      calculate resonant amplitudes
c
       call resamp(ie,i,zramp,iflag)
c
c      total meson-baryon on-shell amplitude
c
       n1=0
       do 81 ic1=1,nc
       nLs1=nLsdt(ic1,i)
       do 81 iLs1=1,nLs1
       n1=n1+np1
       n2=0
       do 82 ic2=1,nc
       nLs2=nLsdt(ic2,i)
       do 82 iLs2=1,nLs2
       n2=n2+np1
       ztotamp(iLs1,ic1,iLs2,ic2,ie,i)
     1 =zt(n1,n2)+zramp(iLs1,ic1,iLs2,ic2)

   82  continue
   81  continue
c
c      pi-N on-shell t-matrix
c
       ztpin(ie,i)= zramp(1,1,1,1)*icres+ztmx(ie,i)*icpot
       ztres(ie,i)= zramp(1,1,1,1)

c
c      pi N -> eta N t-matrix
c
       ztpinen(ie,i)=zramp(1,1,1,2)+ztmx12(ie,i)

c
       write(*,2000)ze0(ie),allwaves(i),ztmx(ie,i),zramp(1,1,1,1)
 2000  format(1h ,2e15.5,a3,8e15.5)

c tsato 08-26-2007
c
c  pi-N amplitude tot non-res res
c
       qpio  = zp(np1,1,ie)
       wcm   = ze0(ie)
      ff     = - pi*qpio*sqrt(fpio**2+qpio**2)
     &           *sqrt(fnuc**2+qpio**2)/wcm

      write(122,1111)wcm/1000,allwaves(i),ztpin(ie,i)*ff,ztmx(ie,i)*ff
     &                       ,ztres(ie,i)*ff
 1111 format(1h ,e15.6,2x,a3,2x,40e15.6)
c tsato 08-26-2007


      kres   = nstar0(i)
      do imbx = 1,nch
      do lsjx = 1,nLsdt(imbx,i)   !
      do ipx  = 1,np1

      if(kres.ne.0) then
      imb0 = ich0(imbx)  !!!!!!!!!!!! correct?????
      do ires = 1,kres
      zfmb(ie,i,ipx,imbx,lsjx,ires)=
     & zf(ie,i,lsjx,imbx,ipx,ires)*iphase(imb0)*zgreen(ipx,imbx,ie)
      end do
      end if

      end do
      end do
      end do

   10  continue  ! loop lsj
   20  continue  ! loop Wcm
c
       RETURN
       END
