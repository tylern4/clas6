      subroutine outemamp
      implicit real*8(a-h,o-y)
      implicit complex*16(z)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)

      character cl(0:4)*1,cj(9)*1
      character cfname*3
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / input1 / mxq,q2(maxq2)
      common / ctest / ieout,iqout

      common/chdat1/njLs,jpind(maxlsj) ,Lpind(maxlsj)
     &                  ,ispind(maxlsj),itpind(maxlsj)

      common/nstardt/nstar0(maxlsj),amstar(maxres,maxlsj)
     &    ,coup(5,maxmb,maxres,maxlsj),cut(5,maxmb,maxres,maxlsj)

       common/mesh00/nc,ne0,np1,ich(maxmb),
     1   ze0(maxwcm),am1(maxmb),am2(maxmb),istab(maxmb)
     1  ,zp(maxmom,maxmb,maxwcm),zwp(maxmom,maxmb,maxwcm)
     1  ,zp0(maxmb,maxwcm),zgreen(maxmom,maxmb,maxwcm)

       common /ELEM3/ ztpin(maxwcm,maxlsj)
     1                ,ztmx(maxwcm,maxlsj)
     2               ,ztres(maxwcm,maxlsj)
     3            ,zsigsato(maxwcm,maxlsj,maxres,maxres)

      common / cres / nres(2*maxl+1,0:maxl,3)
     &               ,xmres(2*maxl+1,0:maxl,3,maxres)
     &               ,cfac(2*maxl+1,0:maxl,3,maxres)

      common / elmamp/ zxmbrn(maxwcm,maxq2,8,0:maxl,5)
     3                ,zxmnres(maxwcm,maxq2,8,0:maxl,5)
     4                ,zxmclo(maxwcm,maxq2,8,0:maxl,5)
     5                ,zxmbare(maxwcm,maxq2,8,0:maxl,5)



      dimension fiso(3)

      data cl/'s','p','d','f','g'/
      data cj/'1','2','3','4','5','6','7','8','9'/

      fiso(1)  = sqrt(3.d0/2.d0) ! for 3/2
      fiso(2)  = -1.d0/sqrt(3.d0)! for 1/2p
      fiso(3)  = -1.d0/sqrt(3.d0)! for 1/2n

c===================================================================
c
c  output results
c
      do 8000  i = 1,njls
      ii        = i 
      jpin   =jpind(ii)
      Lpin   =Lpind(ii)
      ispin  =ispind(ii)
      itpin  =itpind(ii)
      kres   = nstar0(ii)
      cfname = cl(lpin/2)//cj(itpin)//cj(jpin)
c
c   masses with self energy
c
      open(unit=112,file=cfname//'-112',form='formatted',
     &              status='unknown')
c
c  piN amplitude
c
      open(unit=122,file=cfname//'-122',form='formatted',
     &              status='unknown')

c
c  W dependence at Q2=0
c
c      open(unit=132,file=cfname//'-132',form='formatted',
c     &              status='unknown')
      open(unit=142,file=cfname//'-142',form='formatted',
     &              status='unknown')

c
c  Q dependece at W=ieout
c
c      open(unit=152,file=cfname//'-152',form='formatted',
c     &              status='unknown')
      open(unit=162,file=cfname//'-162',form='formatted',
     &              status='unknown')
c
c  Q dependece of helicity amplitude
c
c      open(unit=151,file=cfname//'-151',form='formatted',
c     &              status='unknown')
      open(unit=161,file=cfname//'-161',form='formatted',
     &              status='unknown')
c
      do 8001 ie = 1,ne0
      wcm   = ze0(ie)
      qpio       = zp(np1,1,ie)
c
c--------------------------------------------------------------------
c   m + Sigma
c
      if(kres.eq.2) then
         write(112,1111)wcm/1000,zsigsato(ie,ii,1,1)
     &  ,zsigsato(ie,ii,1,2)
     &  ,zsigsato(ie,ii,2,1),zsigsato(ie,ii,2,2)
      else if(kres.eq.1) then
         write(112,1111)wcm/1000,zsigsato(ie,ii,1,1)
      end if

c
c--------------------------------------------------------------------
c  pi-N amplitude tot non-res res
c
      ff = - pi*qpio*sqrt(fpio**2+qpio**2)
     &       *sqrt(fnuc**2+qpio**2)/wcm

      write(122,1111)wcm/1000,ztpin(ie,ii)*ff,ztmx(ie,ii)*ff
     &                       ,ztres(ie,ii)*ff
c
c======================================================================
      do 8002 iq = 1,mxq

      q2x = q2(iq)

c
c  multipole amplitudes for SL normalization
c
c  I=3/2 or 1/2p
c
c  132 ( 1+iT)v separation
c  W Q2, ME(bg), ME(Cloud),
c        MM(bg), MM(Cloud),
c        MS(bg), MS(cloud)
c
c  142 (our definition)
c  W Q2,ME(brn), ME(bg), ME(Cloud), 
c       MM(brn), MM(bg), MM(Cloud),
c       MS(brn), MS(bg), MS(cloud)
c
c
      xsll = fpio/scale*1000          ! 10^3 /m_pi 
c
      if(itpin.eq.3) then
      iso  = 1
      else if(itpin.eq.1) then
      iso  = 2
      end if
      if(jpin.lt.lpin) then
         ims = 8
         ime = 2
         imm = 4
      else if(jpin.gt.lpin) then
         ims = 7
         ime = 1
         imm = 3
      end if

c
c--------------------------------------------------------------------
c  K-matrix non-res meson cloud
c
      xsllx= xsll*fiso(iso)
      lx   = lpin/2

c--------------------------------------------------
c      if(iq.eq.iqout) then

c      write(132,1111)wcm/1000,q2x/1000000,
c     &               zxmknres(ie,iq,ime,lx,iso)*xsllx,
c     &               zxmkclo (ie,iq,ime,lx,iso)*xsllx,
c     &               zxmknres(ie,iq,imm,lx,iso)*xsllx,
c     &               zxmkclo (ie,iq,imm,lx,iso)*xsllx,
c     &               zxmknres(ie,iq,ims,lx,iso)*xsllx,
c     &               zxmkclo (ie,iq,ims,lx,iso)*xsllx

      write(142,1111)wcm/1000,q2x/1000000,
     & zxmbare(ie,iq,ime,lx,iso)*xsllx,zxmnres(ie,iq,ime,lx,iso)*xsllx,
     & zxmclo(ie,iq,ime,lx,iso)*xsllx,
     & zxmbare(ie,iq,imm,lx,iso)*xsllx,zxmnres(ie,iq,imm,lx,iso)*xsllx,
     & zxmclo(ie,iq,imm,lx,iso)*xsllx,
     & zxmbare(ie,iq,ims,lx,iso)*xsllx,zxmnres(ie,iq,ims,lx,iso)*xsllx,
     & zxmclo(ie,iq,ims,lx,iso)*xsllx

c      write(142,1111)wcm/1000,q2x/1000000,
c     & zxmbrn(ie,iq,ime,lx,iso)*xsllx,zxmnres(ie,iq,ime,lx,iso)*xsllx,
c     & zxmclo(ie,iq,ime,lx,iso)*xsllx,
c     & zxmbrn(ie,iq,imm,lx,iso)*xsllx,zxmnres(ie,iq,imm,lx,iso)*xsllx,
c     & zxmclo(ie,iq,imm,lx,iso)*xsllx,
c     & zxmbrn(ie,iq,ims,lx,iso)*xsllx,zxmnres(ie,iq,ims,lx,iso)*xsllx,
c     & zxmclo(ie,iq,ims,lx,iso)*xsllx
c      end if

c---------------------------------------------------
c      if(ie.eq.ieout) then
c      write(152,1111)wcm/1000,q2x/1000000,
c     &               zxmknres(ie,iq,ime,lx,iso)*xsllx,
c     &               zxmkclo (ie,iq,ime,lx,iso)*xsllx,
c     &               zxmknres(ie,iq,imm,lx,iso)*xsllx,
c     &               zxmkclo (ie,iq,imm,lx,iso)*xsllx,
c     &               zxmknres(ie,iq,ims,lx,iso)*xsllx,
c     &               zxmkclo (ie,iq,ims,lx,iso)*xsllx

c      write(162,1111)wcm/1000,q2x/1000000,
c     & zxmbrn(ie,iq,ime,lx,iso)*xsllx,zxmnres(ie,iq,ime,lx,iso)*xsllx,
c     & zxmclo(ie,iq,ime,lx,iso)*xsllx,
c     & zxmbrn(ie,iq,imm,lx,iso)*xsllx,zxmnres(ie,iq,imm,lx,iso)*xsllx,
c     & zxmclo(ie,iq,imm,lx,iso)*xsllx,
c     & zxmbrn(ie,iq,ims,lx,iso)*xsllx,zxmnres(ie,iq,ims,lx,iso)*xsllx,
c     & zxmclo(ie,iq,ims,lx,iso)*xsllx
c      end if

c-----------------------------------------------------
c  Q2 dependence of effective-helicity amplitude at resonance energy
c
c      unit 10^-3 gev^{-1/2}
c
      if(nres(jpin,lpin,itpin).ne.0) then

      xmas = xmres(jpin,lpin,itpin,1)

      if(jpin.eq.1.and.lpin.eq.0) then

c      write(*,*)xmas,wcm,xmas-6,xmas+4

      end if

      if(wcm.gt.xmas-6.d0.and.wcm.lt.xmas+4.d0) then

c      write(152,1111)wcm/1000,q2x/1000000,
c     &               zxmknres(ie,iq,ime,lx,iso)*xsllx,
c     &               zxmkclo (ie,iq,ime,lx,iso)*xsllx,
c     &               zxmknres(ie,iq,imm,lx,iso)*xsllx,
c     &               zxmkclo (ie,iq,imm,lx,iso)*xsllx,
c     &               zxmknres(ie,iq,ims,lx,iso)*xsllx,
c     &               zxmkclo (ie,iq,ims,lx,iso)*xsllx

      write(162,1111)wcm/1000,q2x/1000000,
     & zxmbrn(ie,iq,ime,lx,iso)*xsllx,zxmnres(ie,iq,ime,lx,iso)*xsllx,
     & zxmclo(ie,iq,ime,lx,iso)*xsllx,
     & zxmbrn(ie,iq,imm,lx,iso)*xsllx,zxmnres(ie,iq,imm,lx,iso)*xsllx,
     & zxmclo(ie,iq,imm,lx,iso)*xsllx,
     & zxmbrn(ie,iq,ims,lx,iso)*xsllx,zxmnres(ie,iq,ims,lx,iso)*xsllx,
     & zxmclo(ie,iq,ims,lx,iso)*xsllx


      xslx = xsll/cfac(jpin,lpin,itpin,1)

c      zkanse = zxmkclo (ie,iq,ime,lx,iso)*xslx
c      zkansm = zxmkclo (ie,iq,imm,lx,iso)*xslx
c      zkanss = zxmkclo (ie,iq,ims,lx,iso)*xslx

      zanse = zxmclo (ie,iq,ime,lx,iso)*xslx
      zansm = zxmclo (ie,iq,imm,lx,iso)*xslx
      zanss = zxmclo (ie,iq,ims,lx,iso)*xslx

      lxx   = lpin/2
      xxl   = lxx
c

      if(jpin.gt.lpin) then
c         zka1 = - ( dble(lxx + 2)*zkanse + dble(lxx)*zkansm)/2.d0
         za1  = - ( dble(lxx + 2)*zanse  + dble(lxx)*zansm )/2.d0

c         zka3 =   (zkanse - zkansm)*sqrt(xxl*(xxl+2.d0))/2.d0
         za3  =   (zanse -  zansm )*sqrt(xxl*(xxl+2.d0))/2.d0

c         zkss = - dble(lxx+1)/sqrt(2.d0)*zkanss
         zss  = - dble(lxx+1)/sqrt(2.d0)*zanss

      else if(jpin.lt.lpin) then
         leff= lxx -1
         xeff= leff

c         zka1 = - ( dble(leff)*zkanse - dble(leff+2)*zkansm)/2.d0
         za1  = - ( dble(leff)*zanse  - dble(leff+2)*zansm )/2.d0

c         zka3 = - sqrt(xeff*(xeff+2.d0))*(zkanse + zkansm)/2.d0
         za3  = - sqrt(xeff*(xeff+2.d0))*(zanse  + zansm )/2.d0

c         zkss = - dble(leff+1)/sqrt(2.d0)*zkanss  
         zss  = - dble(leff+1)/sqrt(2.d0)*zanss  

      end if

c      write(151,1111)wcm/1000,q2x/1000000,zka1,zka3,zkss
      write(161,1111)wcm/1000,q2x/1000000,za1 ,za3 ,zss
 
      end if
      end if
c--------------------------------------------------------------------

 1111 format(1h ,40e15.6)

 8002 continue
 8001 continue
      close(unit=112)
      close(unit=122)
c      close(unit=132)
      close(unit=142)
c      close(unit=152)
      close(unit=162)
c      close(unit=151)
      close(unit=161)
 8000 continue

      return
      end

