c current kernel 10-17-2006-mod
c
      subroutine potential(np1,istate)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
      PARAMETER (MAXPAR=500,maxmb=5,maxres=5,maxlsj=20)
      parameter(maxwcm=100,maxq2=20,maxmom=50)
      complex*16 v
      common/zpott/zp(maxmom,maxmb),v(maxmom,maxmb,5,maxmom,maxmb,5)
      common/pot1/nchh,Ldt(5,maxmb,maxlsj),isdt(5,maxmb,maxlsj)
     &                                    ,nLsdt(maxmb,maxlsj)
      common/chdat1/njLs,jpind(maxlsj) ,Lpind(maxlsj)
     &                  ,ispind(maxlsj),itpind(maxlsj)
      common/chdat2/nch,ich0(maxmb)
      dimension icpot(20,20),zpotout(20,6,6,3)
      common / cpidx / index(3,-3:3)
      common / cdfi  / meshx,mxx,mxj,mxm
      common / csw   / gsw(20,20)

c bjulia starts

      common / cswv  / swv(20,20)
      character*2 chn(5),cnf,cni
c      data chn/'pn','en','sn','rn','pd'/
!> bjulia change order
      data chn/'pn','en','pd','sn','rn'/
!< bjulia
!<bjulia
c     channels in cc.f
c
c      ich      = 1   ----  pi-N
c                 2   ----  eta-N
c                 3   ----  pi-Delta
c                 4   ----  sigma-N
c                 5   ----  rho-N
c
c---------------------------------------------------------------
c      ich = 1    pi   N  -> pi  N
c            2    pi   N  -> eta N
c            3    et   N  -> eta N
c            4    pi   N  -> sig N
c            5    et   N  -> sig N
c            6    si   N  -> sig N
c            7    pi   N  -> rho N
c            8    eta  N  -> rho N
c            9    sig  N  -> rho N
c            10   rho  N  -> rho N
c            11   pi   N  -> pi D
c            12   eta  N  -> pi D
c            13   sigm N  -> pi D
c            14   rho  N  -> pi D
c            15   pi   D  -> pi D
c
c     conversion
c

      icpot(1,1)=1
      icpot(1,2)=2
      icpot(2,2)=3
      icpot(1,4)=4
      icpot(2,4)=5
      icpot(4,4)=6
      icpot(1,5)=7
      icpot(2,5)=8
      icpot(4,5)=9
      icpot(5,5)=10
      icpot(1,3)=11
      icpot(2,3)=12
      icpot(4,3)=13
      icpot(5,3)=14
      icpot(3,3)=15

c     zpot(jj,idexf,idexi,iso) iso = 2 x isospin
c                                     
c                   idexf,idxi
c     index(1,-1) = 1
c     index(1, 1) = 2
c     index(3,-3) = 3
c     index(3,-1) = 4
c     index(3, 1) = 5
c     index(3, 3) = 6
c                                                            
c     index is used as
c                                                                              
c    idx = index(js,jl - jj)
c                                                                              
c     jj <-- 2 x j
c     js <-- 2 x s
c     jl <-- 2 x L
c                                                                              
c     which  means
c                  s
c     index= 1     1/2    L = j - 1/2
c            2     1/2    L = j + 1/2
c            3     3/2    L = j - 3/2
c            4     3/2    L = j - 1/2
c            5     3/2    L = j + 1/2
c            6     3/2    L = j + 3/2
c
c
      gsw  = 1  ! at first set all mechanism on
c-----------------------------------------------------
c  for test  gamma  tsato
c
c      x1 = 0   ! ND
c      x2 = 0   ! NE
c      x3 = 0   ! pip
c      x4 = 0   ! cc
c      x5 = 1   ! rho
c      x6 = 0   ! omega
c      x7 = 0   !
c      x8 = 0   ! delta
c      gsw(1,1) = x1
c      gsw(2,1) = x2
c      gsw(3,1) = x3
c      gsw(4,1) = x4
c      gsw(5,1) = x5
c      gsw(6,1) = x6
c      gsw(7,1) = x7
c      gsw(8,1) = x8
c------------------------------------------------------
c
      call setsato
c bjulia starts
c      call setsato06282006
c bjulia ends
c
c
c     mxj= 2 * (maximum j) defined in subroutine setsato
c 
      
      iso=itpind(istate)
      j=jpind(istate)

      do 2 ip1=1,np1
      do 2 ip2=1,np1
      do 2 ichbb1=1,nchh
      do 2 ichbb2=1,nchh


cts1-17-2007
c      ich1=ich0(ichbb1)
c      ich2=ich0(ichbb2)

      ich1=ichbb1
      ich2=ichbb2

      zqfx=zp(ip1,ich1)
      zqix=zp(ip2,ich2)

      ich=icpot(ich1,ich2)

cc bjulia starts
       swv=1.
c
c  switch off 
c
      swv(7,5)     = 0   ! a1-exchange pn-rn
      swv(15,2)    = 0   ! not yet pd-pd
      swv(15,3)    = 0   ! not yet pd-pd
c
c  pn-pn SL model
cts1-17-2007
c       swv(1,5)     = 0   ! sigma-exchange in pi-N
c---------------------------------------------------
c 03-14-2007!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c no cc terms

       swv(7,4)     = 0
       swv(10,3)    = 0
       if(iwarn.eq.1234) then
       write(*,*)'*********** no CT ***************'
       iwarn = 1234
       end if
c ---------------------------------------------
c 1 if you want to reproduce original harry's code
       ioriginal=0
       if (ioriginal.eq.1) then

       swv=0.
c pin pin
       swv(1,1)=1
       swv(1,2)=1
       swv(1,3)=1
       swv(1,4)=1
       swv(1,5)=1
c en pin 
       swv(2,1)=1.
       swv(2,2)=1.
c en en
       swv(3,1)=1.
       swv(3,2)=1.
c pn pd
       swv(11,1)=1.
       swv(11,2)=1.

c en pd
       swv(12 ,1)=1
       endif
c  -----------------------------
       
cts1-17-2007
c       cni=chn(ich2)
c       cnf=chn(ich1)
       cni=chn(ich0(ich2))
       cnf=chn(ich0(ich1))


       call subvme(zqfx,zqix,cnf,cni,zpotout)

      ni1=nLsdt(ich1,istate)
      ni2=nLsdt(ich2,istate)

      do i1=1,ni1
      do i2=1,ni2
       is1 = isdt(i1,ich1,istate)
       L1  = Ldt(i1,ich1,istate)
       Ljm1= L1-j
       idx1= index(is1,Ljm1)

       is2 = isdt(i2,ich2,istate)
       L2  = Ldt(i2,ich2,istate)
       Ljm2= L2-j
       idx2= index(is2,Ljm2)

       zz=zpotout(j,idx1,idx2,iso)

c       write(998,*)cnf,cni,zqfx,zqix,zz

c      v(ip1,ich1,i1,ip2,ich2,i2)=zz
c>22 Nov 06
c      print*,ichbb1,ich1,ichbb2,ich2
       v(ip1,ichbb1,i1,ip2,ichbb2,i2)=zz
c      write(*,2224) ip1,ichbb1,i1,ip2,ichbb2,i2,zz
 2224  format(6I4,2x,2E14.6)
c< 22 Nov 06

c      if (ip1.eq.ip2)  print*,ip1,ip2,ich,zz
c      if(ip1.eq.ip2)then
c      print*,ich1,ich2,abs(zqfx),abs(zqix)
c      if(abs(zz).gt.1.e-20)then

c      if (ich1.eq.3.and.ich2.eq.2) then
c      write(98,1121)real(zqfx),real(zqix),zz
c 1121 format(a15,2e12.4,I4)
c      endif

c      if (abs(zqfx).gt.200.and.abs(zqfx).lt.450) then 
c      if (abs(zqix).gt.200.and.abs(zqix).lt.450) then 
c       zvprint=0.
c      if (abs(v(ip1,ich1,i1,ip2,ich2,i2)).gt.1E-10) 
c    1 zvprint=v(ip1,ich1,i1,ip2,ich2,i2)
c     write(6,1121)' zqfx zqix ich:',abs(zqfx),abs(zqix),ich
c     write(6,1122)chn(ich1),chn(ich2),j,iso,L1,is1,L2,is2
c    1,zvprint
c 1122 format('ic1 ic2 j I L1,is1 L2,is2:',2a4,6i3, '   V=' 2e12.4)
c      endif
c      endif

c      write(6,1122)j,iso,L1,is1,L2,is2 
c     1,v(ip1,ich1,i1,ip2,ich2,i2)
c 1122 format('j I  (L1,is1)  (L2,is2):',6i3, '   V=' 2e12.4)
c       end if
c      end if
      end do
      end do
    4  continue
    3 continue
    2 continue 
      return
      end



c----------------------------------------------
c
c  T. Sato's fast subroutine mb-10-17-2006.f
c
c date 24 Oct 2006 
c
c modifications to the original file are either 
c comment out: ccccc 
c    or noted: bjulia 
c ---------------------------------------------

c modified so that parameters can be varied by minuit /cpar/
c----------------------------------------------------------------
c
      subroutine setcpl
      implicit real*8(a-h,o-z)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / c2004 / igfrm,idfrm,idafrm1,idafrm2,xdafac,famasn,famasd
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / fisos  / fiso(3,20,20),mxpot(20)
c bjulia minuit
      common /freepar/cpar(500)
c



      
c
      scaleg   = scale/1000.d0
c
c  0-  octet
c
      fpic    = 1.3957018D-01/scaleg
      fpi0    = 1.349766D-01/scaleg
      fkaonp  = 4.93677D-01/scaleg
      fkaon0  = 4.97648D-01/scaleg
      feta    = 5.4775D-01/scaleg
      fetap   = 9.5778D-01/scaleg

c
c use Harry's number !!!!!!!!!!!!!!!!!!!!!!!!
c
c       api=138.5
c       aeta=547.45
      fpio    = 1.385d-01/scaleg            ! SL model
      feta    = 5.4745d-01/scaleg            ! sl model

c
c   1- Octet
c
      frho    = 7.758D-01/scaleg
      fomeg   = 7.8259D-01/scaleg
      fphi    = 1.019456D+00/scaleg
      fkaonsp = 8.9166D-01/scaleg
      fkaons0 = 8.9610D-01/scaleg

      fmrho      = 7.69d-01/scaleg          ! sl model
      frhoe     = 7.65d-01/scaleg          ! sl model
      fmomg      = 7.826d-01/scaleg         ! sl model

c       arho=811.7
c     write(*,*)'!!!!!! mod rho'
c      fmrho    = 0.8117d0/scaleg
c
c  0+  octet
c
      fsigm     = 850.d0 /scale
c       asigma=896.8
      fsigm     = 896.8d0/scale

      fsigme    = 650.d0 /scale

!>bjulia
      fsigm=cpar(1)/scale
      fsigme=fsigm
!<bjulia

      fma0      = 982.7d0/scale
      fmf0      = 974.1d0/scale
      fma1      = 1260.d0/scale
c
c   1/2+ Octet
c      
      fproton = 9.3827203D-01/scaleg
      fneutro = 9.3956536D-01/scaleg
      flambda = 1.115683D+00 /scaleg
      fsigmap = 1.18937D+00/scaleg
      fsigma0 = 1.192642D+00/scaleg
      fsigmam = 1.197449D+00/scaleg
      fxi0    = 1.31483D+00/scaleg
      fxim    = 1.32131D+00/scaleg

c       amn=938.5
      fnuc    = 9.385d-01/scaleg             ! SL model
c
c  decouplet
c
      fbdelta   = 1.2320D+00/scaleg
      fbsigmasp = 1.3828D+00/scaleg
      fbsigmas0 = 1.3837D+00/scaleg
      fbsigmasm = 1.3872D+00/scaleg
      fblambda  = 1.407D+00/scaleg
      fbsigmap  = 1.18937D+00/scaleg
      fbsigma0  = 1.192642D+00/scaleg
      fbxis0    = 1.53180D+00/scaleg
      fbxism    = 1.32131D+00/scaleg
      fbomega   = 1.67245D+00/scaleg

      fdel       = 1.236d0/scaleg            !  sl model
c       amdel=1232.
c      fdel       = 1.232d0/scaleg

      fdelgm     = 1.2380403d0/scaleg        !  sl model
c-------------------------------------------------------------
c
      scalec   = scale/fm
c
c  coupling constant
c
       gpin    = sqrt(0.08d0*4.d0*pi)       ! piNN
       xpid    = 1.204d0
c bjulia test SL for cole smith
c       xpid    = 1.1664d0
        xpid=cpar(2)
c bjulia endtest
       gpindx  = sqrt(72.d0/25.d0)*gpin
       gpind   = gpindx*xpid                ! piN-Delta
       genn    = cpar(3)                     ! eta-NN
       grnn    = cpar(4)                     ! rho-NN
       xkrho   = 1.825d0                    ! Kappa-rho-NN
c bjulia test SL for cole smith
c       xkrho   = 2.8496d0                    ! Kappa-rho-NN
        xkrho=cpar(5)
c end test
       gonn    = cpar(6)                     ! omega-NN
       xkomg   = cpar(7)                     ! Kappa-omega-NN


       gsinn   = 12.8d0                     ! sigma-NN  
       gsinn   = cpar(8)     
       grnp    = 31.79995d0                 ! rho-NN x rho-pipi
       grnp    = 38.4329d0
c bjulia test SL for cole smith 
c       grnp    = 37.133d0
       grnp=cpar(9)
c bjulia endtest

       grpp    = grnp/grnn                  ! rho-pipi
       gpidd   = cpar(10)                     ! pi-DD
       grnd    = cpar(11)                    ! rho-ND
       gsipp   = cpar(12)
       gsisi   = cpar(13)
       gopr    = cpar(14)
       grdd    = cpar(15)
       xkrdd   = cpar(16)
c
c   vertex function   [V^2/(V^2 + q^2)]^m
c
c  numbers are in fm-1
c
       vnnpi   = 3.2551d0/scalec   !pi-NN
       vndpi   = 3.29d0/scalec     !pi-ND
c bjulia test SL for cole smith
c       vnnpi   = 3.6177d0/scalec   !pi-NN
c       vndpi   = 3.2132d0/scalec     !pi-ND

        vnnpi=cpar(17)/scalec

        vndpi=cpar(18)/scalec
c end test

       vnnrho  = 6.2305d0/scalec   !rho-NN
       vrpp    = 6.2305d0/scalec   !rho-pipi
c bjulia test SL for cole smith
c       vnnrho  = 7.4639d0/scalec   !rho-NN
c       vrpp    = 7.4639d0/scalec   !rho-pipi
        vnnrho  = cpar(19)/scalec
        vrpp    = cpar(20)/scalec
c end test
       vnnomg  = cpar(21)/scalec    !omega-NN
       vnnet   = cpar(22)/scalec          !eta-NN
       vnnsi   = cpar(23)/scale     !sigma-NN
       vndrh   = cpar(24)/scale     !rho-ND
       vddpi   = cpar(25)/scale     !pi-DD
       vsipp   = cpar(26)/scale      !sigma-pi pi
       vsisi   = cpar(27)/scale
       vopr    =  cpar(28)/scale
       vddrh   = cpar(29)/scale
       mnnpi   = 2
       mndpi   = 2
       mnnrho  = 2
       mrpp    = 2
       mnnomg  = 2
       mnnet   = 2
       mnnsi   = 2
       mndrh   = 2
       mddpi   = 2
       mnna0   = 2
       ma0pe   = 2
       mnnf0   = 2
       mf0ee   = 2
       msipp   = 2
       msisi   = 2
       mopr    = 2
       mnna1   = 2
       ma1pr   = 2
       mddrh   = 2
c  electromagnetic coupling (we do not need for meson-baryon potential

      fopi     = 4.d0*pi
      alpha    = 1.d0/137.03604d0
      ep       = sqrt(alpha*fopi)

c       grpg    = 0.1027d0          !??? dimension
c        gopg    = 0.3247d0           !??? dimension
c        print*,grpg,gopg
c       gde2    = 0
c       gdc2    = 0
c       gdm1    = 0
c
c  vector current
c
       igfrm   = 1
       idfrm   = 2
c
c  axial vector current
c
       idafrm1  = 1
       idafrm2  = 1
       xdafac   = 1
       famasn   = 1020.d0  ! MeV
       famasd   = 1020.d0  ! MeV
c
       gdm1    = 1.85d0
       gde2    = 0.025d0
       gdc2    = -0.238d0
c
c  isospin factor of optential
c
c   pi N -> pi N
      fiso(1,1,1) =    3  ! nd
      fiso(3,1,1) =    0
      fiso(1,1,2) =  - 1  ! ne
      fiso(3,1,2) =    2


      fiso(1,1,3) =    4.d0/3.d0  ! delta exchange
      fiso(3,1,3) =    1.d0/3.d0
      fiso(1,1,4) =   -2          ! rho
      fiso(3,1,4) =    1

      fiso(1,1,5) =    1  ! sigma
      fiso(3,1,5) =    1
      fiso(1,1,6) =    0  ! anti-delta S-chan
      fiso(3,1,6) =    1

      mxpot(1)    =    6
c   pi N -> eta N
      fiso(1,2,1) =  - sqrt(3.d0)
      fiso(3,2,1) =    0
      fiso(1,2,2) =  - sqrt(3.d0)
      fiso(3,2,2) =    0
      fiso(1,2,3) =  - sqrt(3.d0)
      fiso(3,2,3) =    0
      mxpot(2)    =    2  ! 11-02-2006
c  eta N -> eta N
      fiso(1,3,1) =    1
      fiso(3,3,1) =    0
      fiso(1,3,2) =    1
      fiso(3,3,2) =    0
      fiso(1,3,3) =    1
      fiso(3,3,3) =    0
      mxpot(3)    =    2  ! 11-02-2006
c   pi N -> sigma N
      fiso(1,4,1) =  - sqrt(3.d0)
      fiso(3,4,1) =    0
      fiso(1,4,2) =  - sqrt(3.d0)
      fiso(3,4,2) =    0
      fiso(1,4,3) =  - sqrt(3.d0)
      fiso(3,4,3) =    0
      mxpot(4)    =    3
c  eta N -> sigma N
      fiso(1,5,1) =    1
      fiso(3,5,1) =    0
      fiso(1,5,2) =    1
      fiso(3,5,2) =    0
      mxpot(5)    =    2
c  sigma N -> sigma N
      fiso(1,6,1) =    1
      fiso(3,6,1) =    0
      fiso(1,6,2) =    1
      fiso(3,6,2) =    0
      fiso(1,6,3) =    1
      fiso(3,6,3) =    0
      mxpot(6)    =    2  ! 11-02-2006
c  pi N -> rho N
      fiso(1,7,1) =    3
      fiso(3,7,1) =    0
      fiso(1,7,2) =   -1
      fiso(3,7,2) =    2
      fiso(1,7,3) =   -2
      fiso(3,7,3) =    1
      fiso(1,7,4) =   -2
      fiso(3,7,4) =    1
      fiso(1,7,5) =   -2
      fiso(3,7,5) =    1
      fiso(1,7,6) =    1
      fiso(3,7,6) =    1
      mxpot(7)    =    6
c  eta N -> rho N
      fiso(1,8,1) =  - sqrt(3.d0)
      fiso(3,8,1) =    0
      fiso(1,8,2) =  - sqrt(3.d0)
      fiso(3,8,2) =    0
      mxpot(8)    =    2
c  sigma N -> rho N
      fiso(1,9,1) =  - sqrt(3.d0)
      fiso(3,9,1) =    0
      fiso(1,9,2) =  - sqrt(3.d0)
      fiso(3,9,2) =    0
      mxpot(9)    =    2
c  rho N -> rho N
      fiso(1,10,1) =    3
      fiso(3,10,1) =    0
      fiso(1,10,2) =   -1
      fiso(3,10,2) =    2
      fiso(1,10,3) =   -2
      fiso(3,10,3) =    1
      fiso(1,10,4) =   -2
      fiso(3,10,4) =    1
       mxpot(10)    =   3   ! 11-02-2006
c   pi N -> pi D (idx 11)
      fiso(1,11,1) =   sqrt(6.d0)
      fiso(3,11,1) =   0
      fiso(1,11,2) =   sqrt(8.d0/3.d0)
      fiso(3,11,2) = - sqrt(5.d0/3.d0)
      fiso(1,11,3) = - sqrt(2.d0/3.d0)
      fiso(3,11,3) = - sqrt(5.d0/3.d0)
      fiso(1,11,4) =   0
      fiso(3,11,4) = - sqrt(15.d0/4.d0)
      fiso(1,11,5) =   sqrt(25.d0/6.d0)
      fiso(3,11,5) =   sqrt(5.d0/3.d0)
      mxpot(11)    =   5
c  eta N -> pi D
      fiso(1,12,1) = - sqrt(2.d0)
      fiso(3,12,1) =   0
      mxpot(12)    =   1
c  sigma N -> pi D
      fiso(1,13,1) = - sqrt(2.d0)
      fiso(3,13,1) =   0
      mxpot(13)    =   1
c  rho N -> pi D
      fiso(1,14,1) =   sqrt(6.d0)
      fiso(3,14,1) =   0
      fiso(1,14,2) =   sqrt(8.d0/3.d0)
      fiso(3,14,2) =  -sqrt(5.d0/3.d0)
      fiso(1,14,3) =   0
      fiso(3,14,3) =   -sqrt(15.d0)/2.d0
      fiso(1,14,4) =   sqrt(25.d0/6.d0)
      fiso(3,14,4) =   sqrt(5.d0/3.d0)
      mxpot(14)    =   4
c  pi D  -> pi D
      fiso(1,15,1) =   2
      fiso(3,15,1) =   0
      fiso(1,15,2) =   0
      fiso(3,15,2) =   15.d0/4.d0
      fiso(1,15,3) =   -5.d0/2.d0
      fiso(3,15,3) =    11.d0/4.d0
      fiso(1,15,4) =   -5.d0/2.d0
      fiso(3,15,4) =   -1.d0
      mxpot(15)    =   4

      return
      end



c$$$c
c$$$c
c$$$c  03-06-2006 Correct inconsistent use of cg-del
c$$$c  07-24-2006 introduce fast routine of piN-> rhoN
c$$$c  08-04-2006 done rhoN->piD
c$$$c  10-17-2006 rhoN-rhoN
c$$$c  11-02-2006 modify mxpot(3), mxpot(2) -> 2 a0,f0 drop, c23 was included
c$$$c
c$$$c===================================================================
c$$$c
c$$$c  new routine 2006
c$$$c
c$$$c  non resonant meson-baryon potential 10-17-2006  T.S.
c$$$c
c$$$c            1    2    3    4     5       6
c$$$c vpn2pn  1  c9   c10  c11  c12   c13    s-anti-delta
c$$$c vpn2en  2  c15  c16
c$$$c ven2en  3  c18  c19
c$$$c vpn2sn  4  c21  c22  c23                        
c$$$c ven2sn  5  c25  c26
c$$$c vsn2sn  6  c28  c29 
c$$$c vpn2rn  7  c31  c32  c33  c34   A1     c35  
c$$$c ven2rn  8  c38  c39
c$$$c vsn2rn  9  c41  c42
c$$$c vrn2rn 10  c44a c44b c46
c$$$c vpn2pd 11  c48  c49  c50  c51   c52
c$$$c ven2pd 12  c53
c$$$c vsn2pd 13  c54
c$$$c vrn2pd 14  c56  c57
c$$$c vpd2pd 15  c61            c64
c$$$c
c$$$c V1  1,2,3,4,6=SL model
c$$$c# V4  c23 is not included<-- included 11-02
c$$$c V7  ignore 5 a1-exchange
c$$$c V14 not yet c58,c59  -- delta intermediate
c$$$c V15 not yet c62,c63  -- delta intermediate
c$$$c
c$$$c----------------------------------------------------------------------
c$$$c
c$$$c  input momentum and channel
c$$$c        zpf,zpi,chnf,chni=pn,en,sn,rn,pd
c$$$c
c$$$c  output potential zpot(20,6,6,3)
c$$$c     zpot(jj,idexf,idexi,iso) iso = 2 x isospin
c$$$c
c$$$c                  spin   orbital angular momentum
c$$$c                   s      L
c$$$c     index= 1     1/2    L = j - 1/2
c$$$c            2     1/2    L = j + 1/2
c$$$c            3     3/2    L = j - 3/2
c$$$c            4     3/2    L = j - 1/2
c$$$c            5     3/2    L = j + 1/2
c$$$c            6     3/2    L = j + 3/2
c$$$c
c$$$c     jj <-- 2 x j
c$$$c     js <-- 2 x s
c$$$c     jl <-- 2 x L
c$$$c
c$$$c
c$$$c    idexf/i = index(js,jl - jj)
c$$$c
c$$$c                   idexf,idxi
c$$$c     index(1,-1) = 1
c$$$c     index(1, 1) = 2
c$$$c     index(3,-3) = 3
c$$$c     index(3,-1) = 4
c$$$c     index(3, 1) = 5
c$$$c     index(3, 3) = 6
c$$$c
c$$$c=======================================================================
c$$$c-----------------------------------------------------------------
c$$$c  vertex function used SL model
c$$$c
c$$$c    (c^2/(c^2 + q^2))**m
c$$$c-----------------------------------------------------------------
c$$$      complex*16 function zvtx(zq,cut,m)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$      zvtx = (cut**2/(cut**2 + zq**2))**m
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------------
c$$$      subroutine subvme(zpf,zpi,chnf,chni,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$      character chnf*2,chni*2
c$$$      dimension zpot(20,6,6,3),zpotc(20,6,6,3)
c$$$
c$$$
c$$$c-----------------------------------------------
c$$$      if(chni.eq.'pn') then
c$$$         if(chnf.eq.'pn') then
c$$$            call vpn2pn(zpf,zpi,zpot)
c$$$         else if(chnf.eq.'en') then
c$$$            call vpn2en(zpf,zpi,zpot)
c$$$         else if(chnf.eq.'sn') then
c$$$            call vpn2sn(zpf,zpi,zpot)
c$$$         else if(chnf.eq.'rn') then
c$$$            call vpn2rn(zpf,zpi,zpot)
c$$$         else if(chnf.eq.'pd') then
c$$$            call vpn2pd(zpf,zpi,zpot)
c$$$         end if
c$$$c------------------------------------------------
c$$$      else if(chni.eq.'en') then
c$$$         if(chnf.eq.'pn') then
c$$$            call vpn2en(zpi,zpf,zpotc)
c$$$            fsign  = 1
c$$$            call csubvme(zpot,zpotc,fsign)
c$$$         else if(chnf.eq.'en') then
c$$$            call ven2en(zpf,zpi,zpot)
c$$$         else if(chnf.eq.'sn') then
c$$$            call ven2sn(zpf,zpi,zpot)
c$$$         else if(chnf.eq.'rn') then
c$$$            call ven2rn(zpf,zpi,zpot)
c$$$         else if(chnf.eq.'pd') then
c$$$            call ven2pd(zpf,zpi,zpot)
c$$$         end if
c$$$
c$$$c------------------------------------------------
c$$$      else if(chni.eq.'sn') then
c$$$         if(chnf.eq.'pn') then
c$$$            call vpn2sn(zpi,zpf,zpotc)
c$$$            fsign  = -1
c$$$            call csubvme(zpot,zpotc,fsign)
c$$$         else if(chnf.eq.'en') then
c$$$            call ven2sn(zpi,zpf,zpotc)
c$$$            fsign  = -1
c$$$            call csubvme(zpot,zpotc,fsign)
c$$$         else if(chnf.eq.'sn') then
c$$$            call vsn2sn(zpf,zpi,zpot)
c$$$         else if(chnf.eq.'rn') then
c$$$            call vsn2rn(zpf,zpi,zpot)
c$$$         else if(chnf.eq.'pd') then
c$$$            call vsn2pd(zpf,zpi,zpot)
c$$$         end if
c$$$
c$$$      else if(chni.eq.'pd') then
c$$$         if(chnf.eq.'pn') then
c$$$            call vpn2pd(zpi,zpf,zpotc)
c$$$            fsign  = 1
c$$$            call csubvme(zpot,zpotc,fsign)
c$$$         else if(chnf.eq.'en') then
c$$$            call ven2pd(zpi,zpf,zpotc)
c$$$            fsign  = 1
c$$$            call csubvme(zpot,zpotc,fsign)
c$$$         else if(chnf.eq.'sn') then
c$$$            call vsn2pd(zpi,zpf,zpotc)
c$$$            fsign  = -1
c$$$            call csubvme(zpot,zpotc,fsign)
c$$$         else if(chnf.eq.'rn') then
c$$$            call vrn2pd(zpi,zpf,zpotc)
c$$$            fsign  = -1
c$$$            call csubvme(zpot,zpotc,fsign)
c$$$         else if(chnf.eq.'pd') then
c$$$            call vpd2pd(zpf,zpi,zpot)
c$$$         end if
c$$$
c$$$      else if(chni.eq.'rn') then
c$$$         if(chnf.eq.'pn') then
c$$$            call vpn2rn(zpi,zpf,zpotc)
c$$$            fsign  = -1
c$$$            call csubvme(zpot,zpotc,fsign)
c$$$         else if(chnf.eq.'en') then
c$$$            call ven2rn(zpi,zpf,zpotc)
c$$$            fsign  = -1
c$$$            call csubvme(zpot,zpotc,fsign)
c$$$         else if(chnf.eq.'sn') then
c$$$            call vsn2rn(zpi,zpf,zpotc)
c$$$            fsign  = 1
c$$$            call csubvme(zpot,zpotc,fsign)
c$$$         else if(chnf.eq.'rn') then
c$$$            call vrn2rn(zpf,zpi,zpot)
c$$$         else if(chnf.eq.'pd') then
c$$$            call vrn2pd(zpf,zpi,zpot)
c$$$         end if
c$$$
c$$$      end if
c$$$
c$$$      return
c$$$      end
c$$$
c$$$      subroutine csubvme(zpot,zpotc,fsign)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$      dimension zpot(20,6,6,3),zpotc(20,6,6,3)
c$$$      do jx = 1,20
c$$$      do i1 = 1,6
c$$$      do i2 = 1,6
c$$$      do i3 = 1,3
c$$$      zpot(jx,i2,i1,i3) = fsign*zpotc(jx,i1,i2,i3)
c$$$      end do
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      return
c$$$      end
c$$$c-------------------------------------------------------------
c$$$c
c$$$c  potential 1  pi-N -> pi N
c$$$c
c$$$c  1 s- nucleon
c$$$c  2 u- nucleon
c$$$c  3 u- delta
c$$$c  4 t- rho 
c$$$c  5 t- sigma
c$$$c  6 s- anti-delta
c$$$c
c$$$      subroutine vpn2pn(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-----------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$cccc  fleg mesh = mxx, maxl = 10
c$$$
c$$$c
c$$$c  zvme(L,I,J),    L prbital angular momentum
c$$$c                  I type of diagram
c$$$c                  J = 1 F00, 2 F01,  3 F10,  4 F11
c$$$c
c$$$      common / cswv  / swv(20,20)
c$$$      dimension zvme(0:20,10,4),zpot(20,6,6,3)
c$$$
c$$$      mxl    = 10
c$$$      ich    = 1      ! piN-piN
c$$$      mxp    = mxpot(ich)
c$$$
c$$$      pi2    = 2.d0*pi
c$$$      fmi    = fpio
c$$$      fbi    = fnuc
c$$$      fmf    = fpio
c$$$      fbf    = fnuc
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$
c$$$      frhos  = frhoe**2
c$$$      zsip  = zwi + fnuc
c$$$      zsim  = zwi - fnuc
c$$$      zsfp  = zwf + fnuc
c$$$      zsfm  = zwf - fnuc
c$$$      zenum   = zebf - zebi
c$$$      zenup   = zebf + zebi
c$$$      zopim   = zemf    - zemi
c$$$      zopip   = zemf    + zemi
c$$$
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$
c$$$      zvrta =  zvtx(zqi,vnnpi,mnnpi)*zvtx(zqf,vnnpi,mnnpi)
c$$$      zvrtb =  zvtx(zqi,vnnpi,mndpi)*zvtx(zqf,vnnpi,mnnpi)
c$$$      zvrtc =  zvtx(zqi,vndpi,mndpi)*zvtx(zqf,vndpi,mndpi)
c$$$      zvrtf =  zvtx(zqi,vndpi,mndpi)*zvtx(zqf,vndpi,mndpi)
c$$$
c$$$      zffa     = gpin**2/fpio**2  *zfac*zvrta*swv(ich,1)
c$$$      zffb     = gpin**2/fpio**2  *zfac*zvrtb*swv(ich,2)
c$$$      zffc     = gpind**2/fpio**2 *zfac*zvrtc*swv(ich,3)
c$$$      zfff     = gpind**2/fpio**2 *zfac*zvrtf*swv(ich,6)
c$$$
c$$$c------------------------------------------------------
c$$$
c$$$      do jx  = 1,mxj,2
c$$$      do idf = 1,6
c$$$      do idi = 1,6
c$$$      do iso = 1,3
c$$$      zpot(jx,idf,idi,iso)=0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      do  lx     = 0,mxl
c$$$      do  k1     = 1,10
c$$$      do  k2     = 1,4
c$$$      zvme(lx,k1,k2)  = 0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$c-------------------------------------------------------
c$$$c   nucleon direct
c$$$c
c$$$      zvme(0,1,1) = zffa*(zwi - fnuc)*(zwf - fnuc)*
c$$$     &                   (1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))
c$$$      zvme(0,1,4) = zffa*(zwi + fnuc)*(zwf + fnuc)*zdi*zdf*
c$$$     &                   (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))
c$$$
c$$$      zqfqi  = zqf *zqi
c$$$      zemfi = zemf*zemi
c$$$
c$$$      do ix = 1,mxx
c$$$      xxx   = xgau(ix)
c$$$      www   = wgau(ix)
c$$$
c$$$      zqfix = zqfqi*xxx
c$$$      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)
c$$$      zvrtd =  zvtx(zqx,vrpp ,mrpp )*zvtx(zqx,vnnrho,mnnrho)
c$$$      zvrte =  zvtx(zqx,vnnsi,mnnsi)*zvtx(zqx,vsipp,msipp)
c$$$      zffd     =-grnn*grpp/2.d0   *zfac*zvrtd*swv(ich,4)
c$$$      zffe     =-gsinn*gsipp/fpio*zfac*zvrte*swv(ich,5)
c$$$c
c$$$c   nucleon exchange
c$$$c
c$$$      zuu   = fmf2 - 2.d0*(zebi*zemf + zqfix)
c$$$      zxa00 = - fnuc*(zemfi - 2.d0*zqfix)
c$$$      zxa11 = - fnuc*zqfqi
c$$$      zxb00 = zwi*(zemfi + 2.d0*zqfix) -  fmi2*zemf - fmf2*zemi
c$$$      zxb11 = - zwi* zqfqi
c$$$      zxff  = - zwi*zemf + fmf2
c$$$      zxfp  = - zwi*zemi + fmi2
c$$$      zxee  = - fnuc*zemf
c$$$      zxep  =   fnuc*zemi
c$$$
c$$$      ztmp1 = zxa00 + zxb00 - zdi*zdf*(zxa11-zxb11)
c$$$     &       -zqi*zdi*(zxee + zxff) + zqf*zdf*(zxep - zxfp)
c$$$      ztmp4 = zxa11 + zxb11 - zdi*zdf*(zxa00-zxb00)
c$$$     &       -zqf*zdi*(zxep + zxfp) + zqi*zdf*(zxee - zxff)
c$$$      ztmp1a = ztmp1*zffb*www/zuu/2.d0
c$$$      ztmp4a = ztmp4*zffb*www/zuu/2.d0
c$$$
c$$$      zup   = fmi2 - 2.d0*(zebf*zemi + zqfix)
c$$$      zxb00 = zwf*(zemfi + 2.d0*zqfix) -  fmi2*zemf - fmf2*zemi
c$$$      zxb11 = - zwf* zqfqi
c$$$      zxff  = - zwf*zemf + fmf2
c$$$      zxfp  = - zwf*zemi + fmi2
c$$$
c$$$      ztmp1 = zxa00 + zxb00 - zdi*zdf*(zxa11-zxb11)
c$$$     &       -zqi*zdi*(zxee + zxff) + zqf*zdf*(zxep - zxfp)
c$$$      ztmp4 = zxa11 + zxb11 - zdi*zdf*(zxa00-zxb00)
c$$$     &       -zqf*zdi*(zxep + zxfp) + zqi*zdf*(zxee - zxff)
c$$$      ztmp1b = ztmp1*zffb*www/zup/2.d0
c$$$      ztmp4b = ztmp4*zffb*www/zup/2.d0
c$$$
c$$$      ztmp11 = ztmp1a + ztmp1b
c$$$      ztmp44 = ztmp4a + ztmp4b
c$$$c
c$$$c  rho meson
c$$$c
c$$$c------------------------------------------
c$$$      zqfi    = zemf*zemi - zqfix
c$$$      zqfin   = zebf*zebi - zqfix
c$$$      
c$$$      zenum2  = zebf**2 - zebi**2
c$$$      zfrap1  = zenup + xkrho*zopip + zenum*zenum2/frhos
c$$$      zfrap2  = - zopip/frhos
c$$$      zfram1  = (zenum*zopip-zenum2)/frhos
c$$$      zfrbp1  = -2.d0*fnuc-xkrho/2.d0/fnuc*zopip*zenup
c$$$      zfrbp2  = -xkrho/fnuc
c$$$      zfrbm1  = -xkrho/2.d0/fnuc*zenum
c$$$      zfrcp1  =  xkrho/fnuc*zqi*zqf
c$$$      zzr1   = 1.d0/(2.d0*fpio**2 - frhos - 2.d0*zqfi)/2.d0
c$$$      zzr2   = 1.d0/(2.d0*fnuc**2 - frhos - 2.d0*zqfin)/2.d0
c$$$      zzrp   = zzr1 + zzr2
c$$$      zzrm   = -zzr1*zopim + zzr2*zenum
c$$$      zhpot4a  = zzrp*(zfrap1 +zfrap2*zqx**2)+ zzrm*zfram1 +zfrap2
c$$$      zhpot4b  = zzrp*(zfrbp1 +zfrbp2*zqfix )+ zzrm*zfrbm1
c$$$      zhpot4c  = zzrp* zfrcp1
c$$$
c$$$      zhpot41  = zffd*(zhpot4a+zhpot4b-zdi*zdf*zhpot4c)
c$$$      zhpot42  = zffd*(zdi*zdf*(zhpot4a-zhpot4b)+zhpot4c)
c$$$c
c$$$c    new delta cross
c$$$c
c$$$      zxp01        =  zebi - zemf
c$$$      zxp02        =  zebf - zemi
c$$$      zpiqi        =  zebi*zemi + zqi**2
c$$$      zpiqf        =  zebi*zemf + zqfix
c$$$      zpfqi        =  zebf*zemi + zqfix
c$$$      zpfqf        =  zebf*zemf + zqf**2
c$$$c
c$$$      zalf11      =  (zpiqi - zqfi    )/fdel - fnuc
c$$$      zalf12      =  (zpiqf - fpio**2)/fdel - fnuc
c$$$      zalf21      =  (zpfqi - fpio**2)/fdel - fnuc
c$$$      zalf22      =  (zpfqf - zqfi    )/fdel - fnuc
c$$$      zdd1        =  3.d0*(fnuc**2+fpio**2-fdel**2-2.d0*zpiqf)
c$$$      zdd2        =  3.d0*(fnuc**2+fpio**2-fdel**2-2.d0*zpfqi)
c$$$      zbpa1        = (zwi+zalf11)*(zwf+zalf12)*(zxp01-fdel)
c$$$     &             +(zwi+zalf11)*(zwf-zalf12)*(zebf-fnuc)
c$$$     &             +(zwi-zalf11)*(zwf+zalf12)*(zebi-fnuc)
c$$$     &             +(-zqfi+(zalf11+fnuc)*(zalf12+fnuc))*
c$$$     &              (zxp01-zebf-zebi+2.d0*fnuc+fdel)
c$$$      zbma1        = (zwi-zalf11)*(zwf-zalf12)*(zxp01+fdel)
c$$$     &             +(zwi-zalf11)*(zwf+zalf12)*(zebf+fnuc)
c$$$     &             +(zwi+zalf11)*(zwf-zalf12)*(zebi+fnuc)
c$$$     &             +(-zqfi+(zalf11+fnuc)*(zalf12+fnuc))*
c$$$     &              (zxp01-zebf-zebi-2.d0*fnuc-fdel)
c$$$      zbpa2        = (zwi+zalf21)*(zwf+zalf22)*(zxp02-fdel)
c$$$     &             +(zwi+zalf21)*(zwf-zalf22)*(zebf-fnuc)
c$$$     &             +(zwi-zalf21)*(zwf+zalf22)*(zebi-fnuc)
c$$$     &             +(-zqfi+(zalf21+fnuc)*(zalf22+fnuc))*
c$$$     &              (zxp02-zebf-zebi+2.d0*fnuc+fdel)
c$$$      zbma2        = (zwi-zalf21)*(zwf-zalf22)*(zxp02+fdel)
c$$$     &             +(zwi-zalf21)*(zwf+zalf22)*(zebf+fnuc)
c$$$     &             +(zwi+zalf21)*(zwf-zalf22)*(zebi+fnuc)
c$$$     &             +(-zqfi+(zalf21+fnuc)*(zalf22+fnuc))*
c$$$     &              (zxp02-zebf-zebi-2.d0*fnuc-fdel)
c$$$c
c$$$c    delta  cross
c$$$c
c$$$      zhpot31  =  zffc*(zbpa1/zdd1+zbpa2/zdd2)/2.d0
c$$$      zhpot32  =  zffc*(zbma1/zdd1+zbma2/zdd2)/2.d0*zdi*zdf
c$$$
c$$$c
c$$$c   delta direct
c$$$c
c$$$      zhnewd   = 1.d0/6.d0*zqi*zqf*zdi*zdf
c$$$     &         *(1.d0/(fdel+zwi)+1.d0/(zwf+fdel))
c$$$      zhpot61  = zfff*(
c$$$     &    zemf*zemi/3.d0/fdel**2*(zwf+zwi+2.d0*fdel)
c$$$     &  - (zqi**2*zemf/(zebi+fnuc)+ zemi*zqf**2/(zebf+fnuc))/3.d0/fdel
c$$$     &  - zhnewd)
c$$$c
c$$$      zhpot62  = zfff*(
c$$$     &      zemf*zemi/3.d0/fdel**2*(zwf+zwi-2.d0*fdel)*zdi*zdf
c$$$     &    +(zemf/(zebf+fnuc)+zemi/(zebi+fnuc))*zqi*zqf/3.d0/fdel
c$$$     &    + zhnewd*3.d0*xxx)
c$$$c
c$$$c  sigma
c$$$c
c$$$      zzss1   = 1.d0/(2.d0*fpio**2 - fsigme**2 - 2.d0*zqfi )/2.d0
c$$$      zzss2   = 1.d0/(2.d0*fnuc**2 - fsigme**2 - 2.d0*zqfin)/2.d0
c$$$      zhpot51 = zffe*zqfi*(zzss1 + zzss2)
c$$$      zhpot52 =-zhpot51*zdi*zdf
c$$$c
c$$$      do il = 0,mxl
c$$$      flee  = fleg(il,ix)
c$$$      zvme(il,2,1) = zvme(il,2,1) + flee * ztmp11
c$$$      zvme(il,2,4) = zvme(il,2,4) + flee * ztmp44
c$$$
c$$$      zvme(il,3,1) = zvme(il,3,1) + flee * zhpot31*www
c$$$      zvme(il,3,4) = zvme(il,3,4) + flee * zhpot32*www
c$$$
c$$$      zvme(il,4,1) = zvme(il,4,1) + flee * zhpot41*www
c$$$      zvme(il,4,4) = zvme(il,4,4) + flee * zhpot42*www
c$$$
c$$$      zvme(il,5,1) = zvme(il,5,1) + flee * zhpot51*www
c$$$      zvme(il,5,4) = zvme(il,5,4) + flee * zhpot52*www
c$$$
c$$$      zvme(il,6,1) = zvme(il,6,1) + flee * zhpot61*www
c$$$      zvme(il,6,4) = zvme(il,6,4) + flee * zhpot62*www
c$$$
c$$$      end do
c$$$
c$$$      end do
c$$$
c$$$c
c$$$c  lx = 1 l=l'=j-1/2  -> L1 = j - 1/2   L2 = j + 1/2
c$$$c  lx = 2 l=l'=j+1/2  -> L1 = j + 1/2   L2 = j - 1/2
c$$$c
c$$$c
c$$$      do jx = 1,mxj,2
c$$$      do lx = 1,2
c$$$
c$$$       iss  = (-1)**lx
c$$$       l1   = (jx + iss)/2
c$$$       l2   = (jx - iss)/2
c$$$
c$$$      
c$$$      do iso = 1,3,2
c$$$      zsum   = 0
c$$$      do ip  = 1,mxp
c$$$      ztmp   = (zvme(l1,ip,1)+zvme(l2,ip,4))*fiso(iso,ich,ip)
c$$$      zsum   = zsum + ztmp
c$$$      end do
c$$$      zpot(jx,lx,lx,iso) = zsum
c$$$
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------
c$$$c  potential 2 pi N -> eta N
c$$$c  
c$$$c  1 s- nucleon
c$$$c  2 u- nucleon
c$$$c
c$$$      subroutine vpn2en(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-----------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$      dimension zvme(0:20,10,4),zpot(20,6,6,3)
c$$$
c$$$      mxl    = 10
c$$$      ich    = 2      ! piN-enN
c$$$      mxp    = mxpot(ich)
c$$$
c$$$      pi2    = 2.d0*pi
c$$$      fmi    = fpio
c$$$      fbi    = fnuc
c$$$      fmf    = feta
c$$$      fbf    = fnuc
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$
c$$$      zvrta =  zvtx(zqf,vnnet,mnnet)*zvtx(zqi,vnnpi,mnnpi)
c$$$      zvrtb =  zvtx(zqf,vnnet,mnnet)*zvtx(zqi,vnnpi,mnnpi)
c$$$      zffa     = genn*gpin/fpio/feta*zfac*zvrta*swv(ich,1)
c$$$      zffb     = genn*gpin/fpio/feta*zfac*zvrtb*swv(ich,2)
c$$$
c$$$c------------------------------------------------------
c$$$
c$$$      do jx  = 1,mxj,2
c$$$      do idf = 1,6
c$$$      do idi = 1,6
c$$$      do iso = 1,3
c$$$      zpot(jx,idf,idi,iso)=0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      do  lx     = 0,mxl
c$$$      do  k1     = 1,10
c$$$      do  k2     = 1,4
c$$$      zvme(lx,k1,k2)  = 0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$c-------------------------------------------------------
c$$$c   nucleon direct
c$$$c
c$$$      zvme(0,1,1) = zffa*(zwi - fnuc)*(zwf - fnuc)*
c$$$     &                   (1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))
c$$$      zvme(0,1,4) = zffa*(zwi + fnuc)*(zwf + fnuc)*zdi*zdf*
c$$$     &                   (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))
c$$$
c$$$      zqfi  = zqf *zqi
c$$$      zemfi = zemf*zemi
c$$$
c$$$      do ix = 1,mxx
c$$$      xxx   = xgau(ix)
c$$$      www   = wgau(ix)
c$$$
c$$$      zqfix = zqfi*xxx
c$$$      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)
c$$$c
c$$$c   nucleon exchange
c$$$c
c$$$      zuu   = fmf2 - 2.d0*(zebi*zemf + zqfix)
c$$$      zxa00 = - fnuc*(zemfi - 2.d0*zqfix)
c$$$      zxa11 = - fnuc*zqfi
c$$$      zxb00 = zwi*(zemfi + 2.d0*zqfix) -  fmi2*zemf - fmf2*zemi
c$$$      zxb11 = - zwi* zqfi
c$$$      zxff  = - zwi*zemf + fmf2
c$$$      zxfp  = - zwi*zemi + fmi2
c$$$      zxee  = - fnuc*zemf
c$$$      zxep  =   fnuc*zemi
c$$$
c$$$      ztmp1 = zxa00 + zxb00 - zdi*zdf*(zxa11-zxb11)
c$$$     &       -zqi*zdi*(zxee + zxff) + zqf*zdf*(zxep - zxfp)
c$$$      ztmp4 = zxa11 + zxb11 - zdi*zdf*(zxa00-zxb00)
c$$$     &       -zqf*zdi*(zxep + zxfp) + zqi*zdf*(zxee - zxff)
c$$$      ztmp1a = ztmp1*zffb*www/zuu/2.d0
c$$$      ztmp4a = ztmp4*zffb*www/zuu/2.d0
c$$$
c$$$      zup   = fmi2 - 2.d0*(zebf*zemi + zqfix)
c$$$      zxb00 = zwf*(zemfi + 2.d0*zqfix) -  fmi2*zemf - fmf2*zemi
c$$$      zxb11 = - zwf* zqfi
c$$$      zxff  = - zwf*zemf + fmf2
c$$$      zxfp  = - zwf*zemi + fmi2
c$$$
c$$$      ztmp1 = zxa00 + zxb00 - zdi*zdf*(zxa11-zxb11)
c$$$     &       -zqi*zdi*(zxee + zxff) + zqf*zdf*(zxep - zxfp)
c$$$      ztmp4 = zxa11 + zxb11 - zdi*zdf*(zxa00-zxb00)
c$$$     &       -zqf*zdi*(zxep + zxfp) + zqi*zdf*(zxee - zxff)
c$$$      ztmp1b = ztmp1*zffb*www/zup/2.d0
c$$$      ztmp4b = ztmp4*zffb*www/zup/2.d0
c$$$
c$$$      ztmp11 = ztmp1a + ztmp1b
c$$$      ztmp44 = ztmp4a + ztmp4b
c$$$
c$$$      do il = 0,mxl
c$$$      flee  = fleg(il,ix)
c$$$      zvme(il,2,1) = zvme(il,2,1) + flee * ztmp11
c$$$      zvme(il,2,4) = zvme(il,2,4) + flee * ztmp44
c$$$      end do
c$$$
c$$$      end do
c$$$
c$$$c
c$$$
c$$$      do jx = 1,mxj,2
c$$$      do lx = 1,2
c$$$       iss  = (-1)**lx
c$$$       l1   = (jx + iss)/2
c$$$       l2   = (jx - iss)/2
c$$$      do iso = 1,3,2
c$$$      zsum   = 0
c$$$      do ip  = 1,mxp
c$$$      ztmp   = (zvme(l1,ip,1)+zvme(l2,ip,4))*fiso(iso,ich,ip)
c$$$      zsum   = zsum + ztmp
c$$$      end do
c$$$      zpot(jx,lx,lx,iso) = zsum
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------
c$$$c  potential 3 eta N -> eta N
c$$$c  
c$$$c  1 s- nucleon
c$$$c  2 u- nucleon
c$$$c
c$$$      subroutine ven2en(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-----------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$      dimension zvme(0:20,10,4),zpot(20,6,6,3)
c$$$
c$$$      mxl    = 10
c$$$      ich    = 3      ! piN-enN
c$$$      mxp    = mxpot(ich)
c$$$
c$$$      pi2    = 2.d0*pi
c$$$      fmi    = feta
c$$$      fbi    = fnuc
c$$$      fmf    = feta
c$$$      fbf    = fnuc
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$
c$$$      zvrta =  zvtx(zqf,vnnet,mnnet)*zvtx(zqi,vnnet,mnnet)
c$$$      zvrtb =  zvtx(zqf,vnnet,mnnet)*zvtx(zqi,vnnet,mnnet)
c$$$      zffa     = (genn/feta)**2*zfac*zvrta*swv(ich,1)
c$$$      zffb     = (genn/feta)**2*zfac*zvrtb*swv(ich,2)
c$$$c------------------------------------------------------
c$$$      do jx  = 1,mxj,2
c$$$      do idf = 1,6
c$$$      do idi = 1,6
c$$$      do iso = 1,3
c$$$      zpot(jx,idf,idi,iso)=0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      do  lx     = 0,mxl
c$$$      do  k1     = 1,10
c$$$      do  k2     = 1,4
c$$$      zvme(lx,k1,k2)  = 0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$c-------------------------------------------------------
c$$$c   nucleon direct
c$$$c
c$$$      zvme(0,1,1) = zffa*(zwi - fnuc)*(zwf - fnuc)*
c$$$     &                   (1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))
c$$$      zvme(0,1,4) = zffa*(zwi + fnuc)*(zwf + fnuc)*zdi*zdf*
c$$$     &                   (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))
c$$$
c$$$      zqfi  = zqf *zqi
c$$$      zemfi = zemf*zemi
c$$$
c$$$      do ix = 1,mxx
c$$$      xxx   = xgau(ix)
c$$$      www   = wgau(ix)
c$$$
c$$$      zqfix = zqfi*xxx
c$$$      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)
c$$$c
c$$$c   nucleon exchange
c$$$c
c$$$      zuu   = fmf2 - 2.d0*(zebi*zemf + zqfix)
c$$$      zxa00 = - fnuc*(zemfi - 2.d0*zqfix)
c$$$      zxa11 = - fnuc*zqfi
c$$$      zxb00 = zwi*(zemfi + 2.d0*zqfix) -  fmi2*zemf - fmf2*zemi
c$$$      zxb11 = - zwi* zqfi
c$$$      zxff  = - zwi*zemf + fmf2
c$$$      zxfp  = - zwi*zemi + fmi2
c$$$      zxee  = - fnuc*zemf
c$$$      zxep  =   fnuc*zemi
c$$$
c$$$      ztmp1 = zxa00 + zxb00 - zdi*zdf*(zxa11-zxb11)
c$$$     &       -zqi*zdi*(zxee + zxff) + zqf*zdf*(zxep - zxfp)
c$$$      ztmp4 = zxa11 + zxb11 - zdi*zdf*(zxa00-zxb00)
c$$$     &       -zqf*zdi*(zxep + zxfp) + zqi*zdf*(zxee - zxff)
c$$$      ztmp1a = ztmp1*zffb*www/zuu/2.d0
c$$$      ztmp4a = ztmp4*zffb*www/zuu/2.d0
c$$$
c$$$      zup   = fmi2 - 2.d0*(zebf*zemi + zqfix)
c$$$      zxb00 = zwf*(zemfi + 2.d0*zqfix) -  fmi2*zemf - fmf2*zemi
c$$$      zxb11 = - zwf* zqfi
c$$$      zxff  = - zwf*zemf + fmf2
c$$$      zxfp  = - zwf*zemi + fmi2
c$$$
c$$$      ztmp1 = zxa00 + zxb00 - zdi*zdf*(zxa11-zxb11)
c$$$     &       -zqi*zdi*(zxee + zxff) + zqf*zdf*(zxep - zxfp)
c$$$      ztmp4 = zxa11 + zxb11 - zdi*zdf*(zxa00-zxb00)
c$$$     &       -zqf*zdi*(zxep + zxfp) + zqi*zdf*(zxee - zxff)
c$$$      ztmp1b = ztmp1*zffb*www/zup/2.d0
c$$$      ztmp4b = ztmp4*zffb*www/zup/2.d0
c$$$
c$$$      ztmp11 = ztmp1a + ztmp1b
c$$$      ztmp44 = ztmp4a + ztmp4b
c$$$
c$$$      do il = 0,mxl
c$$$      flee  = fleg(il,ix)
c$$$      zvme(il,2,1) = zvme(il,2,1) + flee * ztmp11
c$$$      zvme(il,2,4) = zvme(il,2,4) + flee * ztmp44
c$$$      end do
c$$$
c$$$      end do
c$$$c
c$$$
c$$$      do jx = 1,mxj,2
c$$$      do lx = 1,2
c$$$       iss  = (-1)**lx
c$$$       l1   = (jx + iss)/2
c$$$       l2   = (jx - iss)/2
c$$$      do iso = 1,3,2
c$$$      zsum   = 0
c$$$      do ip  = 1,mxp
c$$$      ztmp   = (zvme(l1,ip,1)+zvme(l2,ip,4))*fiso(iso,ich,ip)
c$$$      zsum   = zsum + ztmp
c$$$      end do
c$$$      zpot(jx,lx,lx,iso) = zsum
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------
c$$$c  potential 4 pi N -> sigma N
c$$$c  
c$$$c  1 s- nucleon
c$$$c  2 u- nucleon
c$$$c
c$$$      subroutine vpn2sn(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-----------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$      dimension zvme(0:20,10,4),zpot(20,6,6,3)
c$$$
c$$$      mxl    = 10
c$$$      ich    = 4      ! piN-sigamN
c$$$      mxp    = mxpot(ich)
c$$$      zi     = (0.d0,1.d0)
c$$$      pi2    = 2.d0*pi
c$$$
c$$$      fmi    = fpio
c$$$      fbi    = fnuc
c$$$      fmf    = fsigm
c$$$      fbf    = fnuc
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$
c$$$      zvrta =  zvtx(zqf,vnnsi,mnnsi)*zvtx(zqi,vnnpi,mnnpi)
c$$$      zvrtb =  zvtx(zqf,vnnsi,mnnsi)*zvtx(zqi,vnnpi,mnnpi)
c$$$      zffa  = zi*gsinn*gpin/fpio   *zfac*zvrta*swv(ich,1)
c$$$      zffb  = zi*gsinn*gpin/fpio   *zfac*zvrtb*swv(ich,2)
c$$$
c$$$c------------------------------------------------------
c$$$
c$$$      do jx  = 1,mxj,2
c$$$      do idf = 1,6
c$$$      do idi = 1,6
c$$$      do iso = 1,3
c$$$      zpot(jx,idf,idi,iso)=0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      do  lx     = 0,mxl
c$$$      do  k1     = 1,10
c$$$      do  k2     = 1,4
c$$$      zvme(lx,k1,k2)  = 0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$c-------------------------------------------------------
c$$$c   nucleon direct   cc  2-> f01,   3-> f10
c$$$      zvme(0,1,2) =-zffa*(zwi + fnuc)*zdi*
c$$$     &                   (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))
c$$$      zvme(0,1,3) = zffa*(zwi - fnuc)*zdf*
c$$$     &                   (1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))
c$$$
c$$$      zqfi  = zqf *zqi
c$$$      zemfi = zemf*zemi
c$$$      zxxx1 = (zebi-zebf)*zemi+zqi**2
c$$$      zxxx2 = (zebi-zebf)**2
c$$$
c$$$      do ix = 1,mxx
c$$$      xxx   = xgau(ix)
c$$$      www   = wgau(ix)
c$$$      zqfix = zqfi*xxx
c$$$      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)
c$$$
c$$$      zvrtc = zvtx(zqx,vnnpi,mnnpi)*zvtx(zqx,vsipp,msipp)       
c$$$      zffc  =-zi*gsipp*gpin/fpio**2*zfac*zvrtc
c$$$     &      *(zxxx1 - zqfix)/(zxxx2 - zqx**2 - fpio**2)
c$$$     &      * 2.d0*fnuc*swv(ich,3)
c$$$      ztmp2x= - zffc*zdi*www
c$$$      ztmp3x=   zffc*zdf*www
c$$$c
c$$$c   nucleon exchange
c$$$c
c$$$      zuu1   = fmf2 - 2.d0*(zebi*zemf + zqfix)
c$$$      zuu2   = fmi2 - 2.d0*(zebf*zemi + zqfix)
c$$$      zxu1   = 1.d0/zuu1
c$$$      zxu12  = 1.d0/zuu1 + 1.d0/zuu2
c$$$      zyyy   = 2.d0*(zemfi- zqfix) - zwf*zwi + fnuc**2
c$$$      zxc00 = (1.d0 + 2.d0*fnuc**2*zxu12 + zyyy*zxu1)/2.d0
c$$$      zxd00 = (2.d0*fnuc*zwi*zxu12 + fnuc*(zwi-zwf)*zxu1)/2.d0
c$$$      ztmp2 = - zdi*(zxc00 + zxd00)*www*zffb
c$$$      ztmp3 =   zdf*(zxc00 - zxd00)*www*zffb
c$$$
c$$$c      if(ix.eq.3) then
c$$$c      write(*,9999)xxx,zxc00,zxd00
c$$$c      write(*,9999)zxu12*fm**2,zxu1*fm**2,zyyy/fm**2
c$$$c 9999 format(1h ,10e15.5)
c$$$c      end if
c$$$c      ztmp2 = 0
c$$$
c$$$      do il = 0,mxl
c$$$      flee  = fleg(il,ix)
c$$$      zvme(il,2,2) = zvme(il,2,2) + flee * ztmp2
c$$$      zvme(il,2,3) = zvme(il,2,3) + flee * ztmp3
c$$$      zvme(il,3,2) = zvme(il,3,2) + flee * ztmp2x
c$$$      zvme(il,3,3) = zvme(il,3,3) + flee * ztmp3x
c$$$      end do
c$$$
c$$$      end do
c$$$
c$$$c
c$$$
c$$$      do jx = 1,mxj,2
c$$$       lmin = (jx -1)/2
c$$$       lplu = (jx +1)/2
c$$$      do iso = 1,3,2
c$$$      zsum12 = 0
c$$$      zsum21 = 0
c$$$      do ip  = 1,mxp
c$$$      zsum12 = zsum12 
c$$$     & -(zvme(lplu,ip,3)+zvme(lmin,ip,2))*fiso(iso,ich,ip)
c$$$c                    f10              f01
c$$$      zsum21 = zsum21 
c$$$     & -(zvme(lmin,ip,3)+zvme(lplu,ip,2))*fiso(iso,ich,ip)
c$$$      end do
c$$$      zpot(jx,1,2,iso) = zsum12
c$$$      zpot(jx,2,1,iso) = zsum21
c$$$      end do
c$$$      end do
c$$$
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------
c$$$c  potential 5 eta N -> sigma N
c$$$c  
c$$$c  1 s- nucleon
c$$$c  2 u- nucleon
c$$$c
c$$$      subroutine ven2sn(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-----------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$      dimension zvme(0:20,10,4),zpot(20,6,6,3)
c$$$
c$$$      mxl    = 10
c$$$      ich    = 5      ! etaN-sigamN
c$$$      mxp    = mxpot(ich)
c$$$      zi     = (0.d0,1.d0)
c$$$      pi2    = 2.d0*pi
c$$$
c$$$      fmi    = feta
c$$$      fbi    = fnuc
c$$$      fmf    = fsigm
c$$$      fbf    = fnuc
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$
c$$$      zvrta =  zvtx(zqf,vnnsi,mnnsi)*zvtx(zqi,vnnet,mnnet)
c$$$      zvrtb =  zvtx(zqf,vnnsi,mnnsi)*zvtx(zqi,vnnet,mnnet)
c$$$      zffa     = zi*gsinn*genn/feta   *zfac*zvrta*swv(ich,1)
c$$$      zffb     = zi*gsinn*genn/feta   *zfac*zvrtb*swv(ich,2)
c$$$c------------------------------------------------------
c$$$
c$$$      do jx  = 1,mxj,2
c$$$      do idf = 1,6
c$$$      do idi = 1,6
c$$$      do iso = 1,3
c$$$      zpot(jx,idf,idi,iso)=0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      do  lx     = 0,mxl
c$$$      do  k1     = 1,10
c$$$      do  k2     = 1,4
c$$$      zvme(lx,k1,k2)  = 0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$c-------------------------------------------------------
c$$$c   nucleon direct
c$$$c
c$$$cc  2-> f01,   3-> f10
c$$$      zvme(0,1,2) =-zffa*(zwi + fnuc)*zdi*
c$$$     &                   (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))
c$$$      zvme(0,1,3) = zffa*(zwi - fnuc)*zdf*
c$$$     &                   (1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))
c$$$
c$$$      zqfi  = zqf *zqi
c$$$      zemfi = zemf*zemi
c$$$
c$$$      do ix = 1,mxx
c$$$      xxx   = xgau(ix)
c$$$      www   = wgau(ix)
c$$$
c$$$      zqfix = zqfi*xxx
c$$$      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)
c$$$c
c$$$c   nucleon exchange
c$$$c
c$$$      zuu1   = fmf2 - 2.d0*(zebi*zemf + zqfix)
c$$$      zuu2   = fmi2 - 2.d0*(zebf*zemi + zqfix)
c$$$      zxu1   = 1.d0/zuu1
c$$$      zxu12  = 1.d0/zuu1 + 1.d0/zuu2
c$$$      zyyy   = 2.d0*(zemfi- zqfix) - zwf*zwi + fnuc**2
c$$$      zxc00 = (1.d0 + 2.d0*fnuc**2*zxu12 + zyyy*zxu1)/2.d0
c$$$      zxd00 = (2.d0*fnuc*zwi*zxu12 + fnuc*(zwi-zwf)*zxu1)/2.d0
c$$$      ztmp2 = - zdi*(zxc00 + zxd00)*www*zffb
c$$$      ztmp3 =   zdf*(zxc00 - zxd00)*www*zffb
c$$$
c$$$      do il = 0,mxl
c$$$      flee  = fleg(il,ix)
c$$$      zvme(il,2,2) = zvme(il,2,2) + flee * ztmp2
c$$$      zvme(il,2,3) = zvme(il,2,3) + flee * ztmp3
c$$$      end do
c$$$
c$$$      end do
c$$$
c$$$c
c$$$
c$$$      do jx = 1,mxj,2
c$$$       lmin = (jx -1)/2
c$$$       lplu = (jx +1)/2
c$$$      do iso = 1,3,2
c$$$      zsum12 = 0
c$$$      zsum21 = 0
c$$$      do ip  = 1,mxp
c$$$      zsum12 = zsum12 
c$$$     & -(zvme(lplu,ip,3)+zvme(lmin,ip,2))*fiso(iso,ich,ip)
c$$$c                    f10              f01
c$$$      zsum21 = zsum21 
c$$$     & -(zvme(lmin,ip,3)+zvme(lplu,ip,2))*fiso(iso,ich,ip)
c$$$      end do
c$$$      zpot(jx,1,2,iso) = zsum12
c$$$      zpot(jx,2,1,iso) = zsum21
c$$$      end do
c$$$      end do
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------
c$$$c  potential 6 sigma N -> sigma N
c$$$c  
c$$$c  1 s- nucleon
c$$$c  2 u- nucleon
c$$$c  3 t- sigma
c$$$c
c$$$      subroutine vsn2sn(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-----------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$      dimension zvme(0:20,10,4),zpot(20,6,6,3)
c$$$
c$$$      mxl    = 10
c$$$      ich    = 6      ! sigmaN-sigmaN
c$$$      mxp    = mxpot(ich)
c$$$      zi     = (0.d0,1.d0)
c$$$      pi2    = 2.d0*pi
c$$$
c$$$      fmi    = fsigm
c$$$      fbi    = fnuc
c$$$      fmf    = fsigm
c$$$      fbf    = fnuc
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$
c$$$      zvrta =  zvtx(zqf,vnnsi,mnnsi)*zvtx(zqi,vnnsi,mnnsi)
c$$$      zvrtb =  zvtx(zqf,vnnsi,mnnsi)*zvtx(zqi,vnnsi,mnnsi)
c$$$      zffa     = gsinn*gsinn*zfac*zvrta*swv(ich,1)
c$$$      zffb     = gsinn*gsinn*zfac*zvrtb*swv(ich,2)
c$$$c------------------------------------------------------
c$$$
c$$$      do jx  = 1,mxj,2
c$$$      do idf = 1,6
c$$$      do idi = 1,6
c$$$      do iso = 1,3
c$$$      zpot(jx,idf,idi,iso)=0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      do  lx     = 0,mxl
c$$$      do  k1     = 1,10
c$$$      do  k2     = 1,4
c$$$      zvme(lx,k1,k2)  = 0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$c-------------------------------------------------------
c$$$c   nucleon direct
c$$$c
c$$$      zvme(0,1,1) = zffa*(1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))
c$$$      zvme(0,1,4) = zffa*(1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))*zdi*zdf
c$$$
c$$$      zqfi  = zqf *zqi
c$$$      zemfi = zemf*zemi
c$$$
c$$$      do ix = 1,mxx
c$$$      xxx   = xgau(ix)
c$$$      www   = wgau(ix)
c$$$
c$$$      zqfix = zqfi*xxx
c$$$      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)
c$$$
c$$$      zvrtc =  zvtx(zqx,vnnsi,mnnsi)*zvtx(zqx,vsisi,msisi)
c$$$      zffc     = 6.d0*gsinn*gsisi*zfac*zvrtc*swv(ich,3)
c$$$c
c$$$c   nucleon exchange
c$$$c
c$$$      zuu    = fmf2 - 2.d0*(zebi*zemf + zqfix)
c$$$      ztmp1a = (3.d0*fnuc-zwf)        /zuu
c$$$      ztmp4a =-(3.d0*fnuc+zwf)*zdi*zdf/zuu
c$$$      zup    = fmi2 - 2.d0*(zebf*zemi + zqfix)
c$$$      ztmp1b = (3.d0*fnuc-zwi)        /zup
c$$$      ztmp4b =-(3.d0*fnuc+zwi)*zdi*zdf/zup
c$$$
c$$$      zxx    = zffb*www/2.d0
c$$$      ztmp11 = (ztmp1a + ztmp1b)*zxx
c$$$      ztmp44 = (ztmp4a + ztmp4b)*zxx
c$$$
c$$$      do il = 0,mxl
c$$$      flee  = fleg(il,ix)
c$$$      zvme(il,2,1) = zvme(il,2,1) + flee * ztmp11
c$$$      zvme(il,2,4) = zvme(il,2,4) + flee * ztmp44
c$$$      end do
c$$$
c$$$      end do
c$$$
c$$$c
c$$$
c$$$      do jx = 1,mxj,2
c$$$      do lx = 1,2
c$$$       iss  = (-1)**lx
c$$$       l1   = (jx + iss)/2
c$$$       l2   = (jx - iss)/2
c$$$      do iso = 1,3,2
c$$$      zsum   = 0
c$$$      do ip  = 1,mxp
c$$$      ztmp   = (zvme(l1,ip,1)+zvme(l2,ip,4))*fiso(iso,ich,ip)
c$$$      zsum   = zsum + ztmp
c$$$      end do
c$$$      zpot(jx,lx,lx,iso) = zsum
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------
c$$$c  potential 7 pi N -> rho N
c$$$c  
c$$$c  1 s- nucleon
c$$$c  2 u- nucleon
c$$$c  3 t- pion
c$$$c  4 c  contact
c$$$c  5 A1
c$$$c  6 t- comega
c$$$c                             7
c$$$c-------------------------------------------------------------------
c$$$      subroutine vpn2rn(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-------------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$
c$$$      common / cefflo / cg1h(-2:2,-1:1,3)
c$$$     &     ,xef(20,6,-2:2,-3:3,10)
c$$$      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
c$$$     &     ,jss(20,6,10),jll(20,6,10)
c$$$     &     ,jip(20,6,10)
c$$$
c$$$
c$$$      dimension zpot(20,6,6,3)
c$$$      dimension zxfa(0:1,0:1,10),zxga(0:1,0:1,10),zxha(0:1,0:1,10)
c$$$      dimension zxfb(0:1,0:1,3),zxgb(0:1,0:1,3),zxhb(0:1,0:1,3)
c$$$      dimension zxia(0:1,0:1,10),zxib(0:1,0:1,3)
c$$$      dimension zvme(20,-2:2,-1:1,-1:1,3)
c$$$c--------------------------------------------------------------
c$$$
c$$$
c$$$      mxl    = 10
c$$$      ich    = 7
c$$$      mxp    = mxpot(ich)
c$$$
c$$$      zi     = (0.d0,1.d0)
c$$$      pi2    = 2.d0*pi
c$$$      fmi    = fpio
c$$$      fbi    = fnuc
c$$$      fmf    = fmrho
c$$$      fbf    = fnuc
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$      zqfi   = zqf*zqi
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$
c$$$      zsip  = zwi + fnuc
c$$$      zsim  = zwi - fnuc
c$$$      zsfp  = zwf + fnuc
c$$$      zsfm  = zwf - fnuc
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$
c$$$c------------- use all same vertex function ----------------
c$$$      zvrta =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnpi,mnnpi)
c$$$      zvrtb =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnpi,mnnpi)
c$$$      zvrtd =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnpi,mnnpi)
c$$$      zffa     = zi*grnn*gpin/fpio/2.d0*zfac*zvrta*swv(ich,1)
c$$$      zffb     = zi*grnn*gpin/fpio/2.d0*zfac*zvrtb*swv(ich,2)
c$$$      zffd     =-zi*grnn*gpin/fpio     *zfac*zvrtd*swv(ich,4)
c$$$c------------------------------------------------------------
c$$$
c$$$      zxfa = 0
c$$$      zxga = 0
c$$$      zxha = 0
c$$$      zxia = 0
c$$$
c$$$      xkx   = xkrho/(2.d0*fnuc)
c$$$      xkxo  = xkomg/(4.d0*fnuc)
c$$$c
c$$$c s-channel nucleon exchange
c$$$c
c$$$      zalf = 1.d0 - xkx*zsfm
c$$$      zgam = 1.d0 + xkx*zsfp
c$$$      zbet = (zsip**2/(zwi**2-fnuc**2)+zsip*zsfp/(zwf**2-fnuc**2))/2.d0
c$$$      zdel = (zsim**2/(zwi**2-fnuc**2)+zsim*zsfm/(zwf**2-fnuc**2))/2.d0
c$$$      zxfa(0,1,1)=-zdi*zalf*zbet*zffa
c$$$      zxfa(1,0,1)=-zdf*zgam*zdel*zffa
c$$$      zxha(0,0,1)=-    zalf*zdel*zffa
c$$$      zxha(1,1,1)=-zdf*zdi*zgam*zbet*zffa
c$$$c
c$$$c u-channel nucleon exchange
c$$$c
c$$$      zalf1 = (1 - xkx*(fnuc-zwf))*zffb
c$$$      zbet1 = (1 - xkx*(fnuc+zwf))*zffb
c$$$      zf101 = zdi*zalf1
c$$$      zf110 = zdf*zbet1
c$$$      zh100 =         zalf1
c$$$      zh111 = zdi*zdf*zbet1
c$$$
c$$$      zalf2 = (1 + xkx*(zebf-zemf-fnuc))*zffb
c$$$      zbet2 = (1 - xkx*(zebf-zemf+fnuc))*zffb
c$$$      zgam2a= zebi-zemi+fnuc
c$$$      zgam2b= zebi-zemi-fnuc
c$$$      zdel2a= zebi+zemi+fnuc
c$$$      zdel2b= zebi+zemi-fnuc
c$$$      zf201 = zdi*zgam2a*zalf2
c$$$      zf210 = zdf*zgam2b*zbet2
c$$$      zg201 = zdi*2.d0*zqi*zalf2
c$$$      zg210 = zdf*2.d0*zqi*zbet2
c$$$      zh200 =-zdel2b*zalf2
c$$$      zh211 =-zdi*zdf*zdel2a*zbet2
c$$$
c$$$      zalf3 = (1.d0 -xkx*(zwf-fnuc))*zffb
c$$$      zbet3 = (1.d0 +xkx*(zwf+fnuc))*zffb
c$$$      zgam3a= zgam2a
c$$$      zgam3b= zgam2b
c$$$      zdel3a= zdel2b
c$$$      zdel3b= zdel2a
c$$$      zf301 = zdi*zgam3a*zalf3
c$$$      zf310 =-zdf*zgam3b*zbet3
c$$$      zg301 = zdi*2.d0*zqi*zalf3
c$$$      zg310 =-zdf*2.d0*zqi*zbet3
c$$$      zh300 =        -zdel3a*zalf3
c$$$      zh311 = zdi*zdf*zdel3b*zbet3
c$$$c  contact term
c$$$      zxfa(0,1,4) = -zdi*zffd
c$$$      zxfa(1,0,4) = -zdf*zffd
c$$$      zxha(0,0,4) = -    zffd
c$$$      zxha(1,1,4) = -zdi*zdf*zffd
c$$$      
c$$$      zxkxb = xkx*zffb
c$$$
c$$$c
c$$$c  angular projection
c$$$c
c$$$
c$$$      zvme = 0
c$$$
c$$$      do ix = 1,mxx
c$$$      www   = wgau(ix)
c$$$      cc    = xgau(ix)
c$$$      ss    = sqrt(1.d0 -  cc**2)
c$$$      c2    = sqrt((1.d0 + cc)/2.d0)
c$$$      s2    = sqrt((1.d0 - cc)/2.d0)
c$$$      zqfix = zqfi*cc
c$$$      zkky  = zemf*zemi - zqfix
c$$$
c$$$c------------------------------------------------------------
c$$$      zqx2  =  zqf2 + zqi2 - zqfix*2.d0
c$$$      zqx   =  sqrt(zqx2)
c$$$      zvrtc =  zvtx(zqx,vnnpi,mnnpi)*zvtx(zqx,vrpp ,mrpp )  *swv(ich,3)
c$$$      zvrte =  zvtx(zqx,vnna1 ,mnna1 )*zvtx(zqx,vnnpi,mnnpi)*swv(ich,5)
c$$$      zvrtf =  zvtx(zqx,vnnomg,mnnomg)*zvtx(zqx,vopr ,mopr )*swv(ich,6)
c$$$      zffc  =-zi*grpp*gpin/fpio     *zfac*zvrtc
c$$$      zffe  = zi*grnn*gpin/fpio     *zfac*zvrte
c$$$      zfff  =    gonn*gopr/fmomg    *zfac*zvrtf
c$$$c------------------------------------------------------------
c$$$c        u-channel nucleon exchange
c$$$      zkkx  = 2.d0*(zemf*zemi - zqfix)
c$$$      zuu   = 1.d0/(fmf2 - 2.d0*(zebi*zemf+zqfix))
c$$$      zuup  = 1.d0/(fmi2 - 2.d0*(zebf*zemi+zqfix))
c$$$      za1   = -(zwi-zwf)*zemi*zuu + (1.d0+zuu/zuup)/2.d0
c$$$      za2   = -(zwi-zwf)*zuu/2.d0
c$$$      za3   = -(zuu + zuup)*fnuc
c$$$      zxfa(0,1,2) = za1*zf101+za2*(zf201-zxkxb*zkkx*zdi)
c$$$     &                       +za3*(zf301-zxkxb*zkkx*zdi)
c$$$      zxfa(1,0,2) = za1*zf110+za2*(zf210+zxkxb*zkkx*zdf)
c$$$     &                       +za3*(zf310-zxkxb*zkkx*zdf)
c$$$      zxga(0,1,2) =           za2*zg201+za3*zg301
c$$$      zxga(1,0,2) =           za2*zg210+za3*zg310
c$$$      zxha(0,0,2) = za1*zh100+za2*(zh200-zxkxb*zkkx)
c$$$     &                       +za3*(zh300-zxkxb*zkkx)
c$$$      zxha(1,1,2) = za1*zh111+za2*(zh211+zxkxb*zkkx*zdi*zdf)
c$$$     &                       +za3*(zh311-zxkxb*zkkx*zdi*zdf)
c$$$c  t-channel pion exchange
c$$$      zpion       = -2.d0*fnuc/((zebi-zebf)**2 - zqx2 - fpio**2)*zffc
c$$$      ztmp        = zebi - zemi - zwf
c$$$      zxfa(0,1,3) =-zpion*ztmp*zdi
c$$$      zxfa(1,0,3) = zpion*ztmp*zdf
c$$$      zxga(0,1,3) =-zpion*zqi*zdi*2.d0
c$$$      zxga(1,0,3) = zpion*zqi*zdf*2.d0
c$$$c  t-channel omega exchange
c$$$      zomeg1= 1.d0/((zebi-zebf)**2 - zqx2 - fmomg**2)
c$$$      zomeg2= 1.d0/((zemi-zemf)**2 - zqx2 - fmomg**2)
c$$$      za1   =-xkxo*((zwi+zwf)*zomeg1+2.d0*zwi*zomeg2)
c$$$      za2   = (1.d0+xkomg)/2.d0*(zomeg1+zomeg2)
c$$$      za3   = xkxo*(zwi-zwf)*zomeg2
c$$$      za23p = (za2+za3)*zdi
c$$$      za23m = (za2-za3)*zdf
c$$$      zfx   = zfff*zi
c$$$      zxia(0,0,6)=-zfx*zqfi*(za1+za2+za3)
c$$$      zxia(1,1,6)=-zfx*zqfi*zdi*zdf*(za2-za3-za1)
c$$$      zxfa(0,1,6)=-zfx*(zkky*za23p+zqfi*za23m)
c$$$      zxfa(1,0,6)=-zfx*(zkky*za23m+zqfi*za23p)
c$$$      zxga(0,1,6)=-zfx*zqi*zemf*za23p
c$$$      zxga(1,0,6)=-zfx*zqi*zemf*za23m
c$$$      zxha(0,0,6)=+zfx*(zemf*zqi*za23p+zemi*zqf*za23m)
c$$$      zxha(1,1,6)=+zfx*(zemi*zqf*za23p+zemf*zqi*za23m)
c$$$c---------------------------------------------------------------
c$$$
c$$$      do ix1= 0,1
c$$$      do ix2= 0,1
c$$$      do iso= 1,3,2
c$$$      iy1   = ih1(ix1,ix2)
c$$$      iy2   = ih2(ix1,ix2)
c$$$      zxfb(ix1,ix2,iso) = 0
c$$$      zxgb(ix1,ix2,iso) = 0
c$$$      zxhb(ix1,ix2,iso) = 0
c$$$      zxib(ix1,ix2,iso) = 0
c$$$      do ic = 1,mxp
c$$$      xiso  = fiso(iso,ich,ic)
c$$$      zxfb(ix1,ix2,iso)=zxfb(ix1,ix2,iso)+
c$$$     &(zqf*zxfa(ix1,ix2,ic)+zemf*(cc*zxga(ix1,ix2,ic)+zxha(iy1,iy2,ic)))
c$$$     & /fmrho*xiso
c$$$      zxgb(ix1,ix2,iso) = zxgb(ix1,ix2,iso)+
c$$$     &                    zxga(ix1,ix2,ic)*ss/sqrt(2.d0)*xiso
c$$$      zxhb(ix1,ix2,iso) = zxhb(ix1,ix2,iso)-
c$$$     &                    zxha(ix1,ix2,ic)*sqrt(2.d0)*xiso
c$$$      zxib(ix1,ix2,iso) = zxib(ix1,ix2,iso)+
c$$$     &                    zxia(ix1,ix2,ic)*ss/sqrt(2.d0)*xiso
c$$$      end do
c$$$
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$
c$$$      do imf = -1,1,2
c$$$      do imi = -1,1,2
c$$$      do iso = 1,3,2
c$$$      dhlf   = dfun(1,imi,imf,ix)*www
c$$$      zxf    =zxfb(0,0,iso)     +zxfb(0,1,iso)*imi
c$$$     &       +zxfb(1,0,iso)*imf+zxfb(1,1,iso)*imf*imi
c$$$      zxg    =zxgb(0,0,iso)    +zxgb(0,1,iso)*imi
c$$$     &       +zxgb(1,0,iso)*imf+zxgb(1,1,iso)*imf*imi
c$$$      zxh    =zxhb(0,0,iso)    +zxhb(0,1,iso)*imi
c$$$     &       +zxhb(1,0,iso)*imf+zxhb(1,1,iso)*imf*imi
c$$$      zxi    =zxib(0,0,iso)    +zxib(0,1,iso)*imi
c$$$     &       +zxib(1,0,iso)*imf+zxib(1,1,iso)*imf*imi
c$$$      zz0   = dhlf*zxf
c$$$      zzp   = dhlf*zxg
c$$$      zzm   =-zzp
c$$$      zzz   = dhlf*zxi
c$$$      if(imf.eq.-1) then
c$$$      zzp = zzp + dfun(1,imi,1,ix)*www*zxh
c$$$      else if(imf.eq.1) then
c$$$      zzm = zzm - dfun(1,imi,-1,ix)*www*zxh
c$$$      end if
c$$$
c$$$      do jx = 1,mxj,2
c$$$      zvme(jx, 2,imf,imi,iso)=zvme(jx, 2,imf,imi,iso)
c$$$     &                        +dfun(jx,imi,imf+2,ix)*(zzp+zzz)
c$$$      zvme(jx, 0,imf,imi,iso)=zvme(jx, 0,imf,imi,iso)
c$$$     &                        +dfun(jx,imi,imf,ix)  *zz0
c$$$      zvme(jx,-2,imf,imi,iso)=zvme(jx,-2,imf,imi,iso)
c$$$     &                        +dfun(jx,imi,imf-2,ix)*(zzm+zzz)
c$$$      end do ! jx
c$$$
c$$$      end do ! iso
c$$$      end do ! imf
c$$$      end do ! imi
c$$$
c$$$      end do ! ix cos
c$$$
c$$$c
c$$$c           lsj scheme
c$$$c
c$$$      icci = 1
c$$$      iccf = 5
c$$$      zpot = 0
c$$$      imiz = 0
c$$$      do 200 jjx = 1,mxj,2
c$$$
c$$$      do 300 idxf= 1,6
c$$$      do 300 idxi= 1,6
c$$$      ipxi  = jip(jjx,idxi,icci)
c$$$      ipxf  = jip(jjx,idxf,iccf)
c$$$      iptest = ipxi*ipxf
c$$$      if(iptest.eq.1) then
c$$$
c$$$      do 210 imfz= -2,2,2
c$$$      do 210 ibfz= -1,1,2
c$$$      xxf = xef(jjx,idxf,imfz,ibfz,iccf)
c$$$
c$$$      if(abs(xxf).gt.1.d-20)then
c$$$
c$$$      do 220 ibiz= -1,1,2
c$$$      www = xef(jjx,idxi,imiz,ibiz,icci)*xxf
c$$$
c$$$      if(abs(www).gt.1.d-20) then
c$$$      do iso = 1,3
c$$$      zpot(jjx,idxf,idxi,iso) = zpot(jjx,idxf,idxi,iso)
c$$$     &         + www*zvme(jjx,imfz,ibfz,ibiz,iso)
c$$$      end do
c$$$      end if
c$$$
c$$$ 220  continue
c$$$
c$$$      end if
c$$$
c$$$ 210  continue
c$$$
c$$$      end if  ! parity test
c$$$
c$$$ 300  continue
c$$$ 200  continue
c$$$
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------------
c$$$c  potential 8 eta N -> rho N
c$$$c  
c$$$c  1 s- nucleon
c$$$c  2 u- nucleon
c$$$c                             8
c$$$c-------------------------------------------------------------------
c$$$      subroutine ven2rn(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-------------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$
c$$$      common / cefflo / cg1h(-2:2,-1:1,3)
c$$$     &     ,xef(20,6,-2:2,-3:3,10)
c$$$      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
c$$$     &     ,jss(20,6,10),jll(20,6,10)
c$$$     &     ,jip(20,6,10)
c$$$
c$$$
c$$$      dimension zpot(20,6,6,3)
c$$$      dimension zxfa(0:1,0:1,10),zxga(0:1,0:1,10),zxha(0:1,0:1,10)
c$$$      dimension zxfb(0:1,0:1,3),zxgb(0:1,0:1,3),zxhb(0:1,0:1,3)
c$$$      dimension zvme(20,-2:2,-1:1,-1:1,3)
c$$$c--------------------------------------------------------------
c$$$
c$$$
c$$$      mxl    = 10
c$$$      ich    = 8
c$$$      mxp    = mxpot(ich)
c$$$
c$$$      zi     = (0.d0,1.d0)
c$$$      pi2    = 2.d0*pi
c$$$      fmi    = feta
c$$$      fbi    = fnuc
c$$$      fmf    = fmrho
c$$$      fbf    = fnuc
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$      zqfi   = zqf*zqi
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$
c$$$      zsip  = zwi + fnuc
c$$$      zsim  = zwi - fnuc
c$$$      zsfp  = zwf + fnuc
c$$$      zsfm  = zwf - fnuc
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$
c$$$c------------- use all same vertex function ----------------
c$$$      zvrta =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnet,mnnet)
c$$$      zvrtb =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnet,mnnet)
c$$$      zffa     = zi*grnn*genn/feta/2.d0*zfac*zvrta*swv(ich,1)
c$$$      zffb     = zi*grnn*genn/feta/2.d0*zfac*zvrtb*swv(ich,2)
c$$$c------------------------------------------------------------
c$$$
c$$$      zxfa = 0
c$$$      zxga = 0
c$$$      zxha = 0
c$$$
c$$$      xkx   = xkrho/(2.d0*fnuc)
c$$$c
c$$$c s-channel nucleon exchange
c$$$c
c$$$      zalf = 1.d0 - xkrho/(2.d0*fnuc)*zsfm
c$$$      zgam = 1.d0 + xkrho/(2.d0*fnuc)*zsfp
c$$$      zbet = (zsip**2/(zwi**2-fnuc**2)+zsip*zsfp/(zwf**2-fnuc**2))/2.d0
c$$$      zdel = (zsim**2/(zwi**2-fnuc**2)+zsim*zsfm/(zwf**2-fnuc**2))/2.d0
c$$$      zxfa(0,1,1)=-zdi*zalf*zbet*zffa
c$$$      zxfa(1,0,1)=-zdf*zgam*zdel*zffa
c$$$      zxha(0,0,1)=-    zalf*zdel*zffa
c$$$      zxha(1,1,1)=-zdf*zdi*zgam*zbet*zffa
c$$$c
c$$$c u-channel nucleon exchange
c$$$c
c$$$      zalf1 = (1 - xkx*(fnuc-zwf))*zffb
c$$$      zbet1 = (1 - xkx*(fnuc+zwf))*zffb
c$$$      zf101 = zdi*zalf1
c$$$      zf110 = zdf*zbet1
c$$$      zh100 =         zalf1
c$$$      zh111 = zdi*zdf*zbet1
c$$$
c$$$      zalf2 = (1 + xkx*(zebf-zemf-fnuc))*zffb
c$$$      zbet2 = (1 - xkx*(zebf-zemf+fnuc))*zffb
c$$$      zgam2a= zebi-zemi+fnuc
c$$$      zgam2b= zebi-zemi-fnuc
c$$$      zdel2a= zebi+zemi+fnuc
c$$$      zdel2b= zebi+zemi-fnuc
c$$$      zf201 = zdi*zgam2a*zalf2
c$$$      zf210 = zdf*zgam2b*zbet2
c$$$      zg201 = zdi*2.d0*zqi*zalf2
c$$$      zg210 = zdf*2.d0*zqi*zbet2
c$$$      zh200 =-zdel2b*zalf2
c$$$      zh211 =-zdi*zdf*zdel2a*zbet2
c$$$
c$$$      zalf3 = (1.d0 -xkx*(zwf-fnuc))*zffb
c$$$      zbet3 = (1.d0 +xkx*(zwf+fnuc))*zffb
c$$$      zgam3a= zgam2a
c$$$      zgam3b= zgam2b
c$$$      zdel3a= zdel2b
c$$$      zdel3b= zdel2a
c$$$      zf301 = zdi*zgam3a*zalf3
c$$$      zf310 =-zdf*zgam3b*zbet3
c$$$      zg301 = zdi*2.d0*zqi*zalf3
c$$$      zg310 =-zdf*2.d0*zqi*zbet3
c$$$      zh300 =        -zdel3a*zalf3
c$$$      zh311 = zdi*zdf*zdel3b*zbet3
c$$$
c$$$      zxkxb = xkx*zffb
c$$$
c$$$
c$$$
c$$$c
c$$$c  angular projection
c$$$c
c$$$
c$$$      zvme = 0
c$$$
c$$$      do ix = 1,mxx
c$$$      www   = wgau(ix)
c$$$      cc    = xgau(ix)
c$$$      ss    = sqrt(1.d0 -  cc**2)
c$$$      c2    = sqrt((1.d0 + cc)/2.d0)
c$$$      s2    = sqrt((1.d0 - cc)/2.d0)
c$$$      zqfix = zqfi*cc
c$$$      zkkx  = 2.d0*(zemf*zemi - zqfix)
c$$$      zuu   = 1.d0/(fmf2 - 2.d0*(zebi*zemf+zqfix))
c$$$      zuup  = 1.d0/(fmi2 - 2.d0*(zebf*zemi+zqfix))
c$$$      za1   = -(zwi-zwf)*zemi*zuu + (1.d0+zuu/zuup)/2.d0
c$$$      za2   = -(zwi-zwf)*zuu/2.d0
c$$$      za3   = -(zuu + zuup)*fnuc
c$$$      zxfa(0,1,2) = za1*zf101+za2*(zf201-zxkxb*zkkx*zdi)
c$$$     &                       +za3*(zf301-zxkxb*zkkx*zdi)
c$$$      zxfa(1,0,2) = za1*zf110+za2*(zf210+zxkxb*zkkx*zdf)
c$$$     &                       +za3*(zf310-zxkxb*zkkx*zdf)
c$$$      zxga(0,1,2) =           za2*zg201+za3*zg301
c$$$      zxga(1,0,2) =           za2*zg210+za3*zg310
c$$$      zxha(0,0,2) = za1*zh100+za2*(zh200-zxkxb*zkkx)
c$$$     &                       +za3*(zh300-zxkxb*zkkx)
c$$$      zxha(1,1,2) = za1*zh111+za2*(zh211+zxkxb*zkkx*zdi*zdf)
c$$$     &                       +za3*(zh311-zxkxb*zkkx*zdi*zdf)
c$$$
c$$$
c$$$
c$$$      do ix1= 0,1
c$$$      do ix2= 0,1
c$$$      do iso= 1,3,2
c$$$      iy1   = ih1(ix1,ix2)
c$$$      iy2   = ih2(ix1,ix2)
c$$$      zxfb(ix1,ix2,iso) = 0
c$$$      zxgb(ix1,ix2,iso) = 0
c$$$      zxhb(ix1,ix2,iso) = 0
c$$$      do ic = 1,mxp
c$$$      xiso  = fiso(iso,ich,ic)
c$$$      zxfb(ix1,ix2,iso)=zxfb(ix1,ix2,iso)+
c$$$     &(zqf*zxfa(ix1,ix2,ic)+zemf*(cc*zxga(ix1,ix2,ic)+zxha(iy1,iy2,ic)))
c$$$     & /fmrho*xiso
c$$$      zxgb(ix1,ix2,iso) = zxgb(ix1,ix2,iso)+
c$$$     &                    zxga(ix1,ix2,ic)*ss/sqrt(2.d0)*xiso
c$$$      zxhb(ix1,ix2,iso) = zxhb(ix1,ix2,iso)-
c$$$     &                    zxha(ix1,ix2,ic)*sqrt(2.d0)*xiso
c$$$      end do
c$$$
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$
c$$$      do imf = -1,1,2
c$$$      do imi = -1,1,2
c$$$      do iso = 1,3,2
c$$$      dhlf   = dfun(1,imi,imf,ix)*www
c$$$      zxf    =zxfb(0,0,iso)     +zxfb(0,1,iso)*imi
c$$$     &       +zxfb(1,0,iso)*imf+zxfb(1,1,iso)*imf*imi
c$$$      zxg    =zxgb(0,0,iso)    +zxgb(0,1,iso)*imi
c$$$     &       +zxgb(1,0,iso)*imf+zxgb(1,1,iso)*imf*imi
c$$$      zxh    =zxhb(0,0,iso)    +zxhb(0,1,iso)*imi
c$$$     &       +zxhb(1,0,iso)*imf+zxhb(1,1,iso)*imf*imi
c$$$      zz0   = dhlf*zxf
c$$$      zzp   = dhlf*zxg
c$$$      zzm   =-zzp
c$$$      if(imf.eq.-1) then
c$$$      zzp = zzp + dfun(1,imi,1,ix)*www*zxh
c$$$      else if(imf.eq.1) then
c$$$      zzm = zzm - dfun(1,imi,-1,ix)*www*zxh
c$$$      end if
c$$$
c$$$      do jx = 1,mxj,2
c$$$      zvme(jx, 2,imf,imi,iso)=zvme(jx, 2,imf,imi,iso)
c$$$     &                        +dfun(jx,imi,imf+2,ix)*zzp
c$$$      zvme(jx, 0,imf,imi,iso)=zvme(jx, 0,imf,imi,iso)
c$$$     &                        +dfun(jx,imi,imf,ix)  *zz0
c$$$      zvme(jx,-2,imf,imi,iso)=zvme(jx,-2,imf,imi,iso)
c$$$     &                        +dfun(jx,imi,imf-2,ix)*zzm
c$$$      end do ! jx
c$$$
c$$$      end do ! iso
c$$$      end do ! imf
c$$$      end do ! imi
c$$$
c$$$      end do ! ix cos
c$$$
c$$$c
c$$$c           lsj scheme
c$$$c
c$$$      icci = 2  ! eta
c$$$      iccf = 5  ! rho
c$$$      zpot = 0
c$$$      imiz = 0
c$$$      do 200 jjx = 1,mxj,2
c$$$
c$$$      do 300 idxf= 1,6
c$$$      do 300 idxi= 1,6
c$$$      ipxi  = jip(jjx,idxi,icci)
c$$$      ipxf  = jip(jjx,idxf,iccf)
c$$$      iptest = ipxi*ipxf
c$$$      if(iptest.eq.1) then
c$$$
c$$$      do 210 imfz= -2,2,2
c$$$      do 210 ibfz= -1,1,2
c$$$      xxf = xef(jjx,idxf,imfz,ibfz,iccf)
c$$$
c$$$      if(abs(xxf).gt.1.d-20)then
c$$$
c$$$      do 220 ibiz= -1,1,2
c$$$      www = xef(jjx,idxi,imiz,ibiz,icci)*xxf
c$$$
c$$$      if(abs(www).gt.1.d-20) then
c$$$      do iso = 1,3
c$$$      zpot(jjx,idxf,idxi,iso) = zpot(jjx,idxf,idxi,iso)
c$$$     &         + www*zvme(jjx,imfz,ibfz,ibiz,iso)
c$$$      end do
c$$$      end if
c$$$
c$$$ 220  continue
c$$$
c$$$      end if
c$$$
c$$$ 210  continue
c$$$
c$$$      end if  ! parity test
c$$$
c$$$ 300  continue
c$$$ 200  continue
c$$$
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------------
c$$$c  potential 9 sigma N -> rho N
c$$$c  
c$$$c  1 s- nucleon
c$$$c  2 u- nucleon
c$$$c                             9
c$$$c-------------------------------------------------------------------
c$$$      subroutine vsn2rn(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-------------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$
c$$$      common / cefflo / cg1h(-2:2,-1:1,3)
c$$$     &     ,xef(20,6,-2:2,-3:3,10)
c$$$      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
c$$$     &     ,jss(20,6,10),jll(20,6,10)
c$$$     &     ,jip(20,6,10)
c$$$
c$$$
c$$$      dimension zpot(20,6,6,3)
c$$$      dimension zxfa(0:1,0:1,10),zxga(0:1,0:1,10),zxha(0:1,0:1,10)
c$$$      dimension zxfb(0:1,0:1,3),zxgb(0:1,0:1,3),zxhb(0:1,0:1,3)
c$$$      dimension zvme(20,-2:2,-1:1,-1:1,3)
c$$$c--------------------------------------------------------------
c$$$
c$$$
c$$$      mxl    = 10
c$$$      ich    = 9
c$$$      mxp    = mxpot(ich)
c$$$
c$$$      zi     = (0.d0,1.d0)
c$$$      pi2    = 2.d0*pi
c$$$      fmi    = fsigm
c$$$      fbi    = fnuc
c$$$      fmf    = fmrho
c$$$      fbf    = fnuc
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$      zqfi   = zqf*zqi
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$
c$$$      zsip  = zwi + fnuc
c$$$      zsim  = zwi - fnuc
c$$$      zsfp  = zwf + fnuc
c$$$      zsfm  = zwf - fnuc
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$
c$$$c------------- use all same vertex function ----------------
c$$$      zvrta =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnsi,mnnsi)*swv(ich,1)
c$$$      zvrtb =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnsi,mnnsi)*swv(ich,2)
c$$$      zffa     = grnn*gsinn/2.d0*zfac*zvrta
c$$$      zffb     = grnn*gsinn/2.d0*zfac*zvrtb
c$$$c------------------------------------------------------------
c$$$
c$$$      zxfa = 0
c$$$      zxga = 0
c$$$      zxha = 0
c$$$
c$$$c
c$$$c s-channel nucleon exchange
c$$$c
c$$$      xkx   = xkrho/(2.d0*fnuc)
c$$$      zalf = 1.d0 - xkx*zsfm
c$$$      zgam = 1.d0 + xkx*zsfp
c$$$      zbet = ( zsip/(zwi**2-fnuc**2)+zsfp/(zwf**2-fnuc**2))/2.d0
c$$$      zdel = (-zsim/(zwi**2-fnuc**2)-zsfm/(zwf**2-fnuc**2))/2.d0
c$$$      zxfa(0,0,1)=         zalf*zbet*zffa
c$$$      zxfa(1,1,1)= zdi*zdf*zgam*zdel*zffa
c$$$      zxha(0,1,1)= zdi*    zalf*zdel*zffa
c$$$      zxha(1,0,1)= zdf*    zgam*zbet*zffa
c$$$c
c$$$c u-channel nucleon exchange
c$$$c
c$$$      zalf1 = (1 + xkx*(fnuc-zwf))*zffb
c$$$      zbet1 = (1 + xkx*(fnuc+zwf))*zffb
c$$$      zf100 =         zalf1
c$$$      zf111 = zdi*zdf*zbet1
c$$$      zh101 = zdi*zalf1
c$$$      zh110 = zdf*zbet1
c$$$
c$$$      zalf2 = (1 - xkx*(fnuc-zebf+zemf))*zffb
c$$$      zbet2 = (1 - xkx*(fnuc+zebf-zemf))*zffb
c$$$      zf200 =          zalf2
c$$$      zf211 = -zdi*zdf*zbet2
c$$$      zh201 =  zdi*    zalf2
c$$$      zh210 = -zdf*    zbet2
c$$$
c$$$      zalf3 = (1.d0 +xkx*(zwf-fnuc))*zffb
c$$$      zbet3 = (1.d0 -xkx*(zwf+fnuc))*zffb
c$$$      zgam3 = zebi-zemi-fnuc
c$$$      zeta3 = zebi-zemi+fnuc
c$$$      zf300 =        -zgam3*zalf3
c$$$      zf311 = zdi*zdf*zeta3*zbet3
c$$$
c$$$      zg300 = -2.d0*zqi        *zalf3
c$$$      zg311 =  2.d0*zqi*zdi*zdf*zbet3
c$$$
c$$$      zh301 = zdi*(zwi+fnuc)*zalf3
c$$$      zh310 =-zdf*(zwi-fnuc)*zbet3
c$$$
c$$$      zxkxb = xkx*zffb
c$$$c
c$$$c  angular projection
c$$$c
c$$$
c$$$      zvme = 0
c$$$
c$$$      do ix = 1,mxx
c$$$      www   = wgau(ix)
c$$$      cc    = xgau(ix)
c$$$      ss    = sqrt(1.d0 -  cc**2)
c$$$      c2    = sqrt((1.d0 + cc)/2.d0)
c$$$      s2    = sqrt((1.d0 - cc)/2.d0)
c$$$      zqfix = zqfi*cc
c$$$      zkkx  = 2.d0*(zemf*zemi - zqfix)
c$$$      zuu   = 1.d0/(fmf2 - 2.d0*(zebi*zemf+zqfix))
c$$$      zuup  = 1.d0/(fmi2 - 2.d0*(zebf*zemi+zqfix))
c$$$      za3   = - (zuu + zuup)/2.d0
c$$$      za1   = -2.d0*fnuc*za3
c$$$      za2   = (zwi-zwf)/2.d0*zuu
c$$$
c$$$      zxfa(0,0,2) = za1*zf100+za2*zf200 + za3*(zf300-zxkxb*zkkx)
c$$$      zxfa(1,1,2) = za1*zf111+za2*zf211 + za3*(zf311-zxkxb*zkkx*zdi*zdf)
c$$$      zxga(0,0,2) = za3*zg300
c$$$      zxga(1,1,2) = za3*zg311
c$$$      zxha(0,1,2) = za1*zh101 + za2*zh201 + za3*(zh301-zxkxb*zkkx*zdi)
c$$$      zxha(1,0,2) = za1*zh110 + za2*zh210 + za3*(zh310-zxkxb*zkkx*zdf)
c$$$
c$$$      do ix1= 0,1
c$$$      do ix2= 0,1
c$$$      do iso= 1,3,2
c$$$      iy1   = ih1(ix1,ix2)
c$$$      iy2   = ih2(ix1,ix2)
c$$$      zxfb(ix1,ix2,iso) = 0
c$$$      zxgb(ix1,ix2,iso) = 0
c$$$      zxhb(ix1,ix2,iso) = 0
c$$$      do ic = 1,mxp
c$$$      xiso  = fiso(iso,ich,ic)
c$$$      zxfb(ix1,ix2,iso)=zxfb(ix1,ix2,iso)+
c$$$     &(zqf*zxfa(ix1,ix2,ic)+zemf*(cc*zxga(ix1,ix2,ic)+zxha(iy1,iy2,ic)))
c$$$     & /fmrho*xiso
c$$$      zxgb(ix1,ix2,iso) = zxgb(ix1,ix2,iso)+
c$$$     &                    zxga(ix1,ix2,ic)*ss/sqrt(2.d0)*xiso
c$$$      zxhb(ix1,ix2,iso) = zxhb(ix1,ix2,iso)-
c$$$     &                    zxha(ix1,ix2,ic)*sqrt(2.d0)*xiso
c$$$      end do
c$$$
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$
c$$$      do imf = -1,1,2
c$$$      do imi = -1,1,2
c$$$      do iso = 1,3,2
c$$$      dhlf   = dfun(1,imi,imf,ix)*www
c$$$      zxf    =zxfb(0,0,iso)    +zxfb(0,1,iso)*imi
c$$$     &       +zxfb(1,0,iso)*imf+zxfb(1,1,iso)*imf*imi
c$$$      zxg    =zxgb(0,0,iso)    +zxgb(0,1,iso)*imi
c$$$     &       +zxgb(1,0,iso)*imf+zxgb(1,1,iso)*imf*imi
c$$$      zxh    =zxhb(0,0,iso)    +zxhb(0,1,iso)*imi
c$$$     &       +zxhb(1,0,iso)*imf+zxhb(1,1,iso)*imf*imi
c$$$      zz0   = dhlf*zxf
c$$$      zzp   = dhlf*zxg
c$$$      zzm   =-zzp
c$$$      if(imf.eq.-1) then
c$$$      zzp = zzp + dfun(1,imi,1,ix)*www*zxh
c$$$      else if(imf.eq.1) then
c$$$      zzm = zzm - dfun(1,imi,-1,ix)*www*zxh
c$$$      end if
c$$$
c$$$      do jx = 1,mxj,2
c$$$      zvme(jx, 2,imf,imi,iso)=zvme(jx, 2,imf,imi,iso)
c$$$     &                        +dfun(jx,imi,imf+2,ix)*zzp
c$$$      zvme(jx, 0,imf,imi,iso)=zvme(jx, 0,imf,imi,iso)
c$$$     &                        +dfun(jx,imi,imf,ix)  *zz0
c$$$      zvme(jx,-2,imf,imi,iso)=zvme(jx,-2,imf,imi,iso)
c$$$     &                        +dfun(jx,imi,imf-2,ix)*zzm
c$$$      end do ! jx
c$$$
c$$$      end do ! iso
c$$$      end do ! imf
c$$$      end do ! imi
c$$$
c$$$      end do ! ix cos
c$$$
c$$$c
c$$$c           lsj scheme
c$$$c
c$$$      icci = 4   ! sigma
c$$$      iccf = 5   ! rho
c$$$      zpot = 0
c$$$      imiz = 0
c$$$      do 200 jjx = 1,mxj,2
c$$$
c$$$      do 300 idxf= 1,6
c$$$      do 300 idxi= 1,6
c$$$      ipxi  = jip(jjx,idxi,icci)
c$$$      ipxf  = jip(jjx,idxf,iccf)
c$$$      iptest = ipxi*ipxf
c$$$      if(iptest.eq.1) then
c$$$
c$$$      do 210 imfz= -2,2,2
c$$$      do 210 ibfz= -1,1,2
c$$$      xxf = xef(jjx,idxf,imfz,ibfz,iccf)
c$$$
c$$$      if(abs(xxf).gt.1.d-20)then
c$$$
c$$$      do 220 ibiz= -1,1,2
c$$$      www = xef(jjx,idxi,imiz,ibiz,icci)*xxf
c$$$
c$$$      if(abs(www).gt.1.d-20) then
c$$$      do iso = 1,3
c$$$      zpot(jjx,idxf,idxi,iso) = zpot(jjx,idxf,idxi,iso)
c$$$     &         + www*zvme(jjx,imfz,ibfz,ibiz,iso)
c$$$      end do
c$$$      end if
c$$$
c$$$ 220  continue
c$$$
c$$$      end if
c$$$
c$$$ 210  continue
c$$$
c$$$      end if  ! parity test
c$$$
c$$$ 300  continue
c$$$ 200  continue
c$$$
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------------
c$$$c  potential 10 rho N -> rho N
c$$$c  
c$$$c  1 s- nucleon
c$$$c  2 u- nucleon
c$$$c  3   contact
c$$$c                             10
c$$$c-------------------------------------------------------------------
c$$$      subroutine vrn2rn(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-------------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$
c$$$      common / cefflo / cg1h(-2:2,-1:1,3)
c$$$     &     ,xef(20,6,-2:2,-3:3,10)
c$$$      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
c$$$     &     ,jss(20,6,10),jll(20,6,10)
c$$$     &     ,jip(20,6,10)
c$$$      dimension zpot(20,6,6,3)
c$$$      dimension zvme(20,-2:2,-1:1,-2:2,-1:1,3)
c$$$      dimension zans1(-2:2,-1:1,-2:2,-1:1)
c$$$      dimension zans2(-2:2,-1:1,-2:2,-1:1)
c$$$      dimension zans3(-2:2,-1:1,-2:2,-1:1)
c$$$c      dimension zux4(-2:2),zux5(-2:2)
c$$$      dimension zua(3,4),zub(3,4),zuc(3,4),zud(3,2),zuax(4),zuf(3)
c$$$      dimension zqfei(-2:2),zqief(-2:2)
c$$$      dimension zuoa(4),zuoap(4),zuob(4),zuoc(4),zuod(2)
c$$$      dimension zfff(-2:2),zffi(-2:2),fff(-2:2)
c$$$      dimension xdd(-2:2,-2:2)
c$$$
c$$$      ich    = 10
c$$$      zi     = (0.d0,1.d0)
c$$$      pi2    = 2.d0*pi
c$$$      s2     = sqrt(2.d0)
c$$$      fmi    = fmrho
c$$$      fbi    = fnuc
c$$$      fmf    = fmrho
c$$$      fbf    = fnuc
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$      zqfi   = zqf*zqi
c$$$      zwfm   = zwf - fnuc
c$$$      zwfp   = zwf + fnuc
c$$$      zwim   = zwi - fnuc
c$$$      zwip   = zwi + fnuc
c$$$
c$$$      zfff(-2) = 1
c$$$      zfff( 2) = 1
c$$$      zfff( 0) = zemf/fmf
c$$$      zffi(-2) = 1
c$$$      zffi( 2) = 1
c$$$      zffi( 0) = zemi/fmi
c$$$      fff( 2)  = sqrt(2.d0)
c$$$      fff(-2)  = sqrt(2.d0)
c$$$      fff( 0)  = 1
c$$$c
c$$$c
c$$$c------------------------------------------------------------
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$      zvrta =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnrho,mnnrho)
c$$$      zffa     = grnn**2/4.d0           *zfac*zvrta*swv(ich,1)
c$$$     &                                  *fiso(1,ich,1)
c$$$      zvrtb =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnrho,mnnrho)
c$$$      zffb     = grnn**2/4.d0           *zfac*zvrtb*swv(ich,2)
c$$$      zvrtc =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnrho,mnnrho)
c$$$      zffc     = grnn**2/xkrho/8.d0/fnuc*zfac*zvrtc*swv(ich,3)  !10-17-2006
c$$$
c$$$c      write(*,*)zffa,swv(ich,1),zffb,swv(ich,2)
c$$$c-------------------------------------------------------------
c$$$      xkx   = xkrho/2.d0/fnuc
c$$$      zxaa  = (1.d0/zwim + 1.d0/zwfm)/2.d0*zffa
c$$$      zxbb  =-(1.d0/zwip + 1.d0/zwfp)/2.d0*zffa
c$$$      zyaa  =  1.d0-xkx*zwfm
c$$$      zybb  = (1.d0+xkx*zwfp)*zdf
c$$$      zycc  =  1.d0-xkx*zwim
c$$$      zydd  =-(1.d0+xkx*zwip)*zdi
c$$$
c$$$      zza00 = zyaa*zycc*zxaa
c$$$      zza11 = zybb*zydd*zxbb
c$$$      zzb01 = zyaa*zydd*zxbb
c$$$      zzb10 = zybb*zycc*zxaa
c$$$      zzc01 =-zyaa*zydd*zxaa
c$$$      zzc10 =-zybb*zycc*zxbb
c$$$      zzd00 =-zyaa*zycc*zxbb
c$$$      zzd11 =-zybb*zydd*zxaa
c$$$
c$$$
c$$$      zzg00=(zqf*zqi*zza00+zqi*zemf*zzb10+zqf*zemi*zzc01
c$$$     &     +zemf*zemi*zzd11)/fmrho**2
c$$$      zzg11=(zqf*zqi*zza11+zqi*zemf*zzb01+zqf*zemi*zzc10
c$$$     &     +zemf*zemi*zzd00)/fmrho**2
c$$$      zzh00=(zqf*zzc01+zemf*zzd11)*sqrt(2.d0)/fmrho
c$$$      zzh11=(zqf*zzc10+zemf*zzd00)*sqrt(2.d0)/fmrho
c$$$      zzi00=(zqi*zzb10+zemi*zzd11)*sqrt(2.d0)/fmrho
c$$$      zzi11=(zqi*zzb01+zemi*zzd00)*sqrt(2.d0)/fmrho
c$$$c
c$$$c
c$$$c
c$$$      yy2    =  2.d0*xkx
c$$$      yy2s   =  2.d0*xkx**2
c$$$c  a
c$$$      zua(1,1)= (1.d0+xkx*zwfm)*(1.d0+xkx*zwim)+yy2s*(-zwfm*zwim)
c$$$      zua(1,2)=-(1.d0-xkx*zwfp)*(1.d0-xkx*zwip)+yy2s*(+zwfp*zwip)
c$$$      zua(1,3)=-(1.d0+xkx*zwfm)*(1.d0-xkx*zwip)+yy2s*(-zwfm*zwip)
c$$$      zua(1,4)= (1.d0-xkx*zwfp)*(1.d0+xkx*zwim)+yy2s*(+zwfp*zwim)
c$$$      zub(1,1)=-yy2*(1.d0-xkx*zwfm)
c$$$      zub(1,2)=-yy2*(1.d0+xkx*zwfp)
c$$$      zub(1,3)= zub(1,1)
c$$$      zub(1,4)= zub(1,2)
c$$$      zuc(1,1)=-yy2*(1.d0-xkx*zwim)
c$$$      zuc(1,2)=-yy2*(1.d0+xkx*zwip)
c$$$      zuc(1,3)= zuc(1,2)
c$$$      zuc(1,4)= zuc(1,1)
c$$$      zud(1,1)= -2.d0*yy2s
c$$$      zud(1,2)=  2.d0*yy2s
c$$$c b
c$$$      zuax(1)= (1.d0-xkx*zwfm)*(1.d0-xkx*zwim)-yy2s*(+zwfm*zwim)
c$$$      zuax(2)= (1.d0+xkx*zwfp)*(1.d0+xkx*zwip)-yy2s*(+zwfp*zwip)
c$$$      zuax(3)= (1.d0-xkx*zwfm)*(1.d0+xkx*zwip)-yy2s*(-zwfm*zwip)
c$$$      zuax(4)= (1.d0+xkx*zwfp)*(1.d0-xkx*zwim)-yy2s*(-zwfp*zwim)
c$$$      zua(2,1)=-yy2*zemi*( 1.d0+xkx*zwfm)-yy2*zemf*( 1.d0+xkx*zwim)
c$$$      zua(2,2)=-yy2*zemi*(-1.d0+xkx*zwfp)-yy2*zemf*(-1.d0+xkx*zwip)
c$$$      zua(2,3)=-yy2*zemi*(-1.d0-xkx*zwfm)-yy2*zemf*(-1.d0+xkx*zwip)
c$$$      zua(2,4)=-yy2*zemi*( 1.d0-xkx*zwfp)-yy2*zemf*( 1.d0+xkx*zwim)
c$$$      zub(2,1)= yy2*( 1.d0+xkx*zwfm + yy2*zemf)
c$$$      zub(2,2)= yy2*(-1.d0+xkx*zwfp + yy2*zemf)
c$$$      zub(2,3)= yy2*(-1.d0-xkx*zwfm + yy2*zemf)
c$$$      zub(2,4)= yy2*( 1.d0-xkx*zwfp + yy2*zemf)
c$$$      zuc(2,1)= yy2*( 1.d0+xkx*zwim + yy2*zemi)
c$$$      zuc(2,2)= yy2*(-1.d0+xkx*zwip + yy2*zemi)
c$$$      zuc(2,3)= yy2*( 1.d0-xkx*zwip + yy2*zemi)
c$$$      zuc(2,4)= yy2*(-1.d0-xkx*zwim + yy2*zemi)
c$$$      zud(2,1)= -2.d0*yy2s
c$$$      zud(2,2)= -2.d0*yy2s
c$$$c  c-------------------
c$$$      tmp1   = 1.d0 +yy2s*fmi**2/2.d0
c$$$      tmp2   = yy2*fmi**2
c$$$      zua(3,1)=-tmp1*( zwfm+zwim)-tmp2+yy2*zwfm*zwim
c$$$      zua(3,2)=-tmp1*( zwfp+zwip)+tmp2-yy2*zwfp*zwip
c$$$      zua(3,3)=-tmp1*(-zwfm+zwip)+tmp2+yy2*zwfm*zwip
c$$$      zua(3,4)=-tmp1*(-zwfp+zwim)-tmp2-yy2*zwfp*zwim
c$$$      zub(3,1)= 2.d0*(tmp1-yy2*zwfm)
c$$$      zub(3,2)= 2.d0*(tmp1+yy2*zwfp)
c$$$      zub(3,3)= 2.d0*(tmp1-yy2*zwfm)
c$$$      zub(3,4)= 2.d0*(tmp1+yy2*zwfp)
c$$$      zuc(3,1)= 2.d0*(tmp1-yy2*zwim)
c$$$      zuc(3,2)= 2.d0*(tmp1+yy2*zwip)
c$$$      zuc(3,3)= 2.d0*(tmp1+yy2*zwip)
c$$$      zuc(3,4)= 2.d0*(tmp1-yy2*zwim)
c$$$      zud(3,1)=  4.d0*yy2
c$$$      zud(3,2)= -4.d0*yy2
c$$$
c$$$
c$$$c      zua = 0
c$$$c      zub = 0
c$$$c      zuc = 0
c$$$c      zud = 0
c$$$c      zua(3,1)=-tmp2
c$$$c      zua(3,2)=+tmp2
c$$$c      zua(3,3)=+tmp2
c$$$c      zua(3,4)=-tmp2
c$$$c      zua(3,1)=  zwfm*zwim
c$$$c      zua(3,2)= -zwfp*zwip
c$$$c      zua(3,3)=  zwfm*zwip
c$$$c      zua(3,4)= -zwfp*zwim
c$$$c      zub(3,1)= -2.d0*zwfm
c$$$c      zub(3,2)=  2.d0*zwfp
c$$$c      zub(3,3)= -2.d0*zwfm
c$$$c      zub(3,4)=  2.d0*zwfp
c$$$c      zuc(3,1)= -2.d0*zwim
c$$$c      zuc(3,2)=  2.d0*zwip
c$$$c      zuc(3,3)=  2.d0*zwip
c$$$c      zuc(3,4)= -2.d0*zwim
c$$$c      zud(3,1)=  4.d0
c$$$c      zud(3,2)= -4.d0
c$$$
c$$$      do k = 1,3
c$$$      zua(k,2) = zua(k,2)*zdi*zdf
c$$$      zua(k,3) = zua(k,3)*zdi
c$$$      zua(k,4) = zua(k,4)*zdf
c$$$      zub(k,2) = zub(k,2)*zdi*zdf
c$$$      zub(k,3) = zub(k,3)*zdi
c$$$      zub(k,4) = zub(k,4)*zdf
c$$$      zuc(k,2) = zuc(k,2)*zdi*zdf
c$$$      zuc(k,3) = zuc(k,3)*zdi
c$$$      zuc(k,4) = zuc(k,4)*zdf
c$$$      zud(k,2) = zud(k,2)*zdi*zdf
c$$$      end do
c$$$      zuax(2) = zuax(2)*zdi*zdf
c$$$      zuax(3) = zuax(3)*zdi
c$$$      zuax(4) = zuax(4)*zdf
c$$$
c$$$      zefacxf  = zemf/fmf
c$$$      zefacxi  = zemi/fmi
c$$$      zefac0f  = zqf/fmf
c$$$      zefac0i  = zqi/fmi
c$$$
c$$$      zvme = 0
c$$$      do ix = 1,mxx
c$$$      www   = wgau(ix)
c$$$      cc    = xgau(ix)
c$$$      ss    = sqrt(1.d0 -  cc**2)
c$$$
c$$$      zkfi  = zemf*zemi - zqfi*cc    ! k'*k
c$$$      zaa1  = zqf2+zqi2+2.d0*zqfi*cc+fbi2
c$$$      zaa2  = 1.d0/((zebi-zemf)**2 -zaa1)  ! 1/(u-m^2)
c$$$      zaa3  = 1.d0/((zebf-zemi)**2 -zaa1)  ! 1/(u'-m^2)
c$$$      zuf(1)  = fnuc/2.d0*(zaa2+zaa3)
c$$$      zuf(2)  = (zwi*zaa2+zwf*zaa3)/2.d0
c$$$      zuf(3)  = -(zaa2+zaa3)/2.d0
c$$$      xdd(2, 2) = (1.d0+cc)/2.d0
c$$$      xdd(2, 0) = -ss/s2
c$$$      xdd(2,-2) = (1.d0-cc)/2.d0
c$$$      xdd(0, 2) = ss/s2
c$$$      xdd(0, 0) = cc
c$$$      xdd(0,-2) = -ss/s2
c$$$      xdd(-2, 2) = (1.d0-cc)/2.d0
c$$$      xdd(-2, 0) = ss/s2
c$$$      xdd(-2,-2) = (1.d0+cc)/2.d0
c$$$c
c$$$      zqfei(0) = (zqi*zemf-cc*zqf*zemi)/fmi    ! k'*e
c$$$      zqfei(2) =   ss*zqf/s2
c$$$      zqfei(-2)= - zqfei(2)
c$$$      zqief(0) = (zqf*zemi-cc*zqi*zemf)/fmi    ! k*e'
c$$$      zqief(2) = - ss*zqi/s2
c$$$      zqief(-2)= - zqief(2)
c$$$
c$$$      do kx = 1,4
c$$$      zuoa(kx)= zuf(1)*zua(1,kx)+zuf(2)*zua(2,kx)+zuf(3)*zua(3,kx)
c$$$      end do
c$$$  
c$$$      zzz    =(zuf(1)*yy2s-zuf(3)*yy2*2.d0)*zkfi
c$$$      zuoa(1)=zuoa(1) + zzz
c$$$      zuoa(2)=zuoa(2) - zzz*zdi*zdf
c$$$      zuoa(3)=zuoa(3) - zzz*zdi
c$$$      zuoa(4)=zuoa(4) + zzz*zdf
c$$$      zuoap(1)=zuf(2)*(zuax(1)-yy2s*zkfi)
c$$$      zuoap(2)=zuf(2)*(zuax(2)-yy2s*zkfi*zdi*zdf)
c$$$      zuoap(3)=zuf(2)*(zuax(3)-yy2s*zkfi*zdi)
c$$$      zuoap(4)=zuf(2)*(zuax(4)-yy2s*zkfi*zdf)
c$$$
c$$$
c$$$      do kx = 1,4
c$$$      zuob(kx)=zuf(1)*zub(1,kx)+zuf(2)*zub(2,kx)+zuf(3)*zub(3,kx)
c$$$      zuoc(kx)=zuf(1)*zuc(1,kx)+zuf(2)*zuc(2,kx)+zuf(3)*zuc(3,kx)
c$$$      end do
c$$$      do kx = 1,2
c$$$      zuod(kx)=zuf(1)*zud(1,kx)+zuf(2)*zud(2,kx)+zuf(3)*zud(3,kx)
c$$$      end do
c$$$
c$$$      zans2  = 0
c$$$      zans3  = 0
c$$$
c$$$      do isf = -1,1,2
c$$$      do isi = -1,1,2
c$$$      xdfun  = dfun(1, isi, isf,ix)
c$$$      xdfuni = dfun(1,-isi, isf,ix)
c$$$      xdfunf = dfun(1, isi,-isf,ix)
c$$$      xdfunfi= dfun(1,-isi,-isf,ix)
c$$$      xfi   = isf*isi
c$$$c-----------------------------------------------------
c$$$      zzz9 =  zuod(1) + zuod(2)*xfi
c$$$      do imf = -2,2,2
c$$$      do imi = -2,2,2
c$$$      zans2(imf,isf,imi,isi) = zans2(imf,isf,imi,isi)
c$$$     &                      +  zqfei(imi)*zqief(imf)*zzz9*xdfun
c$$$      end do
c$$$      end do
c$$$c------------------------------------------------------
c$$$      zzz5=  (zuob(1) + zuob(2)*xfi)*zefac0i
c$$$      zzz7=  (zuoc(1) + zuoc(2)*xfi)*zefac0f
c$$$      do imx = -2,2,2
c$$$      zans2(0,isf,imx,isi)=zans2(0,isf,imx,isi)+zzz7*zqfei(imx)*xdfun
c$$$      zans2(imx,isf,0,isi)=zans2(imx,isf,0,isi)+zzz5*zqief(imx)*xdfun
c$$$      end do
c$$$c-------------------------------------------------------
c$$$      zzz6=  zuob(3)     + zuob(4)*xfi
c$$$      zzz8=  zuoc(3)*xfi + zuoc(4)
c$$$      zzz60= zzz6*zefacxi
c$$$      zzz80= zzz8*zefacxf
c$$$      do imx = -2,2,2
c$$$      zans2(0,isf,imx,isi)=zans2(0,isf,imx,isi)+zzz80*zqfei(imx)*xdfun
c$$$      zans2(imx,isf,0,isi)=zans2(imx,isf,0,isi)+zzz60*zqief(imx)*xdfun
c$$$      zans2(-2*isf,isf,imx,isi)=zans2(-2*isf,isf,imx,isi)
c$$$     &        +zzz8*zqfei(imx)*s2*xdfunf
c$$$      zans2(imx,isf,-2*isi,isi)=zans2(imx,isf,-2*isi,isi)
c$$$     &        +zzz6*zqief(imx)*s2*xdfuni
c$$$      end do
c$$$c-------------------------------------------------------
c$$$      zzz1 =( zuoa(1)+zuoap(1)+ ( zuoa(2)+zuoap(2))*xfi)*zefac0f*zefac0i
c$$$      zzz2 = -zuoa(1)+zuoap(1)+ (-zuoa(2)+zuoap(2))*xfi
c$$$      zzz3 =(  zuoa(3)+zuoap(3)      + ( zuoa(4)+zuoap(4))*xfi)*zefac0f
c$$$      zzz4 =((-zuoa(3)+zuoap(3))*xfi    -zuoa(4)+zuoap(4)     )*zefac0i
c$$$
c$$$      zans2(0,isf,0,isi) = zans2(0,isf,0,isi)+zzz1*xdfun
c$$$
c$$$      do imx = -2,2,2
c$$$      zimxi  = zffi(imx)*fff(imx)*dfun(1,isi+imx,isf,ix)
c$$$      zimxf  = zfff(imx)*fff(imx)*dfun(1,isi,isf+imx,ix)
c$$$      zans2(0,isf,imx,isi) = zans2(0,isf,imx,isi) + zzz3*zimxi
c$$$      zans2(imx,isf,0,isi) = zans2(imx,isf,0,isi) + zzz4*zimxf
c$$$      zans3(0,isf,imx,isi) = zans3(0,isf,imx,isi) + zimxi*zefac0f
c$$$     &                      *(-zdi + zdf*xfi)*2.d0
c$$$      zans3(imx,isf,0,isi) = zans3(imx,isf,0,isi) - zimxf*zefac0i
c$$$     &                      *(-zdi*xfi + zdf)*2.d0
c$$$      end do
c$$$
c$$$      do imf = -2,2,2
c$$$      do imi = -2,2,2
c$$$      zfacx  = zfff(imf)*zffi(imi)
c$$$      faccx  = fff(imf)*fff(imi)*dfun(1,isi+imi,isf+imf,ix)*xfi
c$$$      faccy  = xdfun*xdd(imi,imf)
c$$$      zans2(imf,isf,imi,isi) = zans2(imf,isf,imi,isi)
c$$$     &  +   zzz2*zfacx*(2.d0*faccy - faccx)
c$$$      zans3(imf,isf,imi,isi) = zans3(imf,isf,imi,isi)
c$$$     &   +  2.d0*zfacx*(   - faccy + faccx)*(1.d0-zdi*zdf*xfi)
c$$$      end do
c$$$      end do
c$$$c--------------------------------------------------------------
c$$$      end do
c$$$      end do
c$$$
c$$$      zans1  = 0
c$$$      do isf = -1,1,2
c$$$      do isi = -1,1,2
c$$$      zans1(     0,isf,     0,isi)=dfun(1, isi, isf,ix)
c$$$     &                             *(zzg00 + zzg11*isf*isi)
c$$$      zans1(     0,isf,-2*isi,isi)=dfun(1,-isi, isf,ix)
c$$$     &                             *(zzh00+zzh11*isf*isi)
c$$$      zans1(-2*isf,isf,     0,isi)=dfun(1, isi,-isf,ix)
c$$$     &                             *(zzi00 + zzi11*isf*isi)
c$$$      zans1(-2*isf,isf,-2*isi,isi)=dfun(1,-isi,-isf,ix)
c$$$     &                             *(zzd11 + zzd00*isf*isi)*2.d0
c$$$      end do
c$$$      end do
c$$$
c$$$c
c$$$c  j-projection
c$$$c
c$$$      do imf  = -1,1,2
c$$$      do imi  = -1,1,2
c$$$      do immf = -2,2,2
c$$$      do immi = -2,2,2
c$$$
c$$$c      if(ix.eq.1) then
c$$$c      write(1,1010)immf,imf,immi,imi
c$$$c     &  ,zans2(immf,imf,immi,imi)*zffb
c$$$c     &  ,zans1(immf,imf,immi,imi)/fiso(1,ich,1),cc
c$$$c 1010 format(1h ,'new ',4i3,5e15.5)
c$$$c      end if
c$$$
c$$$      ihi = imi + immi
c$$$      ihf = imf + immf
c$$$      do jx = 1,mxj,2
c$$$      ddd   = dfun(jx,ihi,ihf,ix)*www
c$$$      zvme(jx,immf,imf,immi,imi,1)=zvme(jx,immf,imf,immi,imi,1)
c$$$     & +ddd*(zans1(immf,imf,immi,imi)
c$$$     &      +zans2(immf,imf,immi,imi)*fiso(1,ich,2)*zffb
c$$$     &      +zans3(immf,imf,immi,imi)*fiso(1,ich,3)*zffc)
c$$$      zvme(jx,immf,imf,immi,imi,3)=zvme(jx,immf,imf,immi,imi,3)
c$$$     & +ddd*(zans2(immf,imf,immi,imi)*fiso(3,ich,2)*zffb
c$$$     &      +zans3(immf,imf,immi,imi)*fiso(3,ich,3)*zffc)
c$$$      end do ! jx
c$$$      end do  ! immf
c$$$      end do  ! immi
c$$$      end do ! imf
c$$$      end do ! imi
c$$$
c$$$c      write(1,1012)ix,cc
c$$$c 1012 format(1h ,'new ',i3,e15.5)
c$$$      end do
c$$$
c$$$
c$$$c
c$$$c           lsj scheme
c$$$c
c$$$      icci = 5
c$$$      iccf = 5
c$$$      jmf  = 2
c$$$      jmi  = 2
c$$$      jbf  = 1
c$$$      jbi  = 1
c$$$
c$$$      zpot = 0
c$$$      do 200 jjx = 1,mxj,2
c$$$
c$$$      do 300 idxf= 1,6
c$$$      do 300 idxi= 1,6
c$$$      ipxi  = jip(jjx,idxi,icci)
c$$$      ipxf  = jip(jjx,idxf,iccf)
c$$$      iptest = ipxi*ipxf
c$$$      if(iptest.eq.1) then
c$$$
c$$$      do 210 imfz= -jmf,jmf,2
c$$$      do 210 ibfz= -jbf,jbf,2
c$$$      xxxf = xef(jjx,idxf,imfz,ibfz,iccf)
c$$$
c$$$      if(abs(xxxf).gt.1.d-20)then
c$$$
c$$$      do 220 imiz= -jmi,jmi,2
c$$$      do 220 ibiz= -jbi,jbi,2
c$$$      www = xef(jjx,idxi,imiz,ibiz,icci)*xxxf
c$$$
c$$$      if(abs(www).gt.1.d-20) then
c$$$      do iso = 1,3
c$$$      zpot(jjx,idxf,idxi,iso) = zpot(jjx,idxf,idxi,iso)
c$$$     &         + www*zvme(jjx,imfz,ibfz,imiz,ibiz,iso)
c$$$      end do
c$$$      end if
c$$$
c$$$ 220  continue
c$$$
c$$$      end if
c$$$
c$$$ 210  continue
c$$$
c$$$      end if  ! parity test
c$$$
c$$$ 300  continue
c$$$ 200  continue
c$$$
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------
c$$$c  potential 11 pi N -> pi D
c$$$c  
c$$$c  1 s- nucleon
c$$$c  2 u- nucleon
c$$$c  3 t- rho
c$$$c  4 s- Delta
c$$$c  5 u- Delta
c$$$c
c$$$      subroutine vpn2pd(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-----------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$      common / cpidx / index(3,-3:3)
c$$$
c$$$      dimension zvme(20,-3:3,-1:1,3),zpot(20,6,6,3)
c$$$      dimension zepsf(0:3,-2:2),zepsfkf(-2:2),zsnuc(-3:3)
c$$$      dimension                 zepsfki(-2:2),zunuc(-3:3)
c$$$      dimension zc10(-2:2),zc01(-2:2),zcrho(-3:3)
c$$$      dimension zsd01(-2:2),zsd10(-2:2),zsdel(-3:3)
c$$$      dimension zud01(-2:2),zud10(-2:2),zudel(-3:3)
c$$$
c$$$      mxl    = 10
c$$$      ich    = 11
c$$$      mxp    = mxpot(ich)
c$$$      zi     = (0.d0,1.d0)
c$$$      pi2    = 2.d0*pi
c$$$      ss2    = 1.d0/sqrt(2.d0)
c$$$      ss3    = 1.d0/sqrt(3.d0)
c$$$      ss23   = sqrt(2.d0)/sqrt(3.d0)
c$$$
c$$$      fmi    = fpio
c$$$      fbi    = fnuc
c$$$      fmf    = fpio
c$$$      fbf    = fdel
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$      zwfip  = zwf + zwi
c$$$      zwfim  = zwf - zwi
c$$$      fmdnp  = fdel + fnuc
c$$$      fmdnm  = fdel - fnuc
c$$$      zqfi   = zqf *zqi
c$$$      zemfi  = zemf*zemi
c$$$      
c$$$
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$
c$$$      zvrta =  zvtx(zqf,vndpi,mndpi)*zvtx(zqi,vnnpi,mnnpi)
c$$$      zvrtb =  zvtx(zqi,vndpi,mndpi)*zvtx(zqf,vnnpi,mnnpi)
c$$$      zvrtd =  zvtx(zqf,vddpi,mddpi)*zvtx(zqi,vndpi,mndpi)
c$$$      zvrte =  zvtx(zqi,vddpi,mddpi)*zvtx(zqf,vndpi,mndpi)
c$$$      zffa     = gpind*gpin/fpio**2*zfac*zvrta*swv(ich,1)
c$$$      zffb     = gpind*gpin/fpio**2*zfac*zvrtb*swv(ich,2)
c$$$c correct sign 6/2/2006 - 6/28/2006
c$$$      zffd     =-gpidd*gpind/fpio**2*zfac*zvrtd*swv(ich,4)
c$$$      zffe     =-gpidd*gpind/fpio**2*zfac*zvrte*swv(ich,5)
c$$$c----  s-channel nucleon only isospin 1/2 ---------------
c$$$      zs1101 = -((zwi+fnuc)*zqi /(zwi**2 - fnuc**2)
c$$$     &         + (zwf+fnuc)*zqi /(zwf**2 - fnuc**2))/2.d0
c$$$      zs1200 =  ((zwi+fnuc)*zemi/(zwi**2 - fnuc**2)
c$$$     &         + (zwf+fnuc)*zemi/(zwf**2 - fnuc**2))/2.d0
c$$$      zs2100 =  ((zwi-fnuc)*zemi/(zwi**2 - fnuc**2)
c$$$     &         + (zwf-fnuc)*zemi/(zwf**2 - fnuc**2))/2.d0
c$$$      zs2201 = -((zwi-fnuc)*zqi /(zwi**2 - fnuc**2)
c$$$     &         + (zwf-fnuc)*zqi /(zwf**2 - fnuc**2))/2.d0
c$$$      zs01x   = zs1101 - zs1200*zdi
c$$$      zs10x   =        + zs2100*zdf - zs2201*zdi*zdf 
c$$$      zs01    = zs01x*zffa*fiso(1,ich,1)
c$$$      zs10    = zs10x*zffa*fiso(1,ich,1)
c$$$c-----   u-channel nucleon exchange -----------------------
c$$$c
c$$$c   1/(u - m^2)[zu00a + zu01a*l + zu10a*l' + zu11a*l*l'
c$$$c             +(zu00b + zu01b*l + zu10b*l' + zu11b*l*l')*x]
c$$$c
c$$$      zemfqi= zemf*zqi
c$$$      zemfqf= zemf*zqf
c$$$      zdfi  = zdf*zdi
c$$$      zebip = zebi+fnuc
c$$$      zebim = zebi-fnuc
c$$$      zu01ax = -zemfqi+zebim*zqf*zdfi
c$$$     &         -zdi*(zemf*zebip-fpio**2)
c$$$     &         -zdf*zqfi
c$$$      zu10ax = -zebip*zqf+zemfqi*zdfi
c$$$     &         +zdi*zqfi
c$$$     &         +zdf*(zemf*zebim-fpio**2)
c$$$      zu01b =-2.d0*zqfi*zdi*zffb
c$$$      zu10b = 2.d0*zqfi*zdf*zffb
c$$$      zu01a = zu01ax*zffb
c$$$      zu10a = zu10ax*zffb
c$$$c--------------  s-channel delta  isospin 3/2 only--------------------
c$$$      zzzz   = zwf**2 - fdel**2
c$$$      zxxx   = zffd/zzzz*fiso(3,11,4)
c$$$      zxx1   = (zwf**2 - fdel**2)*fdel*zwi - 6.d0*fdel*zemi*zwf**2
c$$$      zxx2   = (zwf**2 - fdel**2)*fdel*fnuc
c$$$     &        +(4.d0*fdel**2+2.d0*zwf**2)*zemi*zwf
c$$$      zxaa   = zxx1 + zxx2
c$$$      zxbb   = zxx1 - zxx2
c$$$      zyaa   = (fdel+zwf)*( zemi*zwf-(fnuc+zwi)*fdel)
c$$$      zybb   = (fdel-zwf)*(-zemi*zwf+(fnuc-zwi)*fdel)
c$$$      zdalf1 = - (fdel - zwf)**2 * zxxx
c$$$      zdbet1 =   (fdel + zwf)**2 * zxxx
c$$$      zdalf2 = (zxaa/3.d0/fdel**2 + zyaa*2.d0/fdel/3.d0)*zxxx
c$$$      zdbet2 = (zxbb/3.d0/fdel**2 + zybb*2.d0/fdel/3.d0)*zxxx
c$$$c------------- u channel delta -----------------------------------
c$$$      zcefx105a=-fdel*zwf
c$$$      zcefx105b=0.0
c$$$      zcefx105c=fdel*zwf*(2.0*fdel**2+2.0*fdel*fnuc+fnuc**2-fpio**2)
c$$$      zcefx205a=0.0
c$$$      zcefx205b=0.0
c$$$      zcefx205c=3.0*fdel**2*(-2.0*fdel*zwi+fnuc*zwf-fnuc*zwi)
c$$$      zcefx305a=-2.0*fdel*zwi+fnuc*zwf-fnuc*zwi
c$$$      zcefx305b=0.0
c$$$      zcefx305c=-fdel**3*zwi+fdel**2*fnuc*zwf-fdel**2*fnuc*zwi+fdel*
c$$$     . fnuc**2*zwf+2.0*fdel*fnuc**2*zwi-3.0*fdel*fpio**2*zwi-fnuc**3*
c$$$     . zwf+fnuc**3*zwi+fnuc*fpio**2*zwf-fnuc*fpio**2*zwi
c$$$      zcefx15a=-fdel*fnuc
c$$$      zcefx15b=0.0
c$$$      zcefx15c=fdel*(-2.0*fdel**3-2.0*fdel**2*fnuc+2.0*fdel*fpio**2+
c$$$     . fnuc**3-fnuc*fpio**2)
c$$$      zcefx25a=0.0
c$$$      zcefx25b=-6.0*fdel**2
c$$$      zcefx25c=3.0*fdel**2*(-2.0*fdel*fnuc-fnuc**2+zwf*zwi)
c$$$      zcefx35a=-2.0*fdel*fnuc-fnuc**2+zwf*zwi
c$$$      zcefx35b=2.0*(-fdel**2-fdel*fnuc+fnuc**2-fpio**2)
c$$$      zcefx35ab=-2
c$$$      zcefx35c=-fdel**3*fnuc-fdel**2*fnuc**2+fdel**2*zwf*zwi+2.0*fdel*
c$$$     . fnuc**3-3.0*fdel*fnuc*fpio**2+fdel*fnuc*zwf*zwi+fnuc**4-fnuc**
c$$$     . 2*fpio**2-fnuc**2*zwf*zwi+fpio**2*zwf*zwi
c$$$c  part2
c$$$      zcef105a=0.0
c$$$      zcef105b=-2.0*fdel*zwi
c$$$      zcef105c=fdel*(4.0*fdel**2*zwf-3.0*fdel**2*zwi+2.0*fdel*fnuc*
c$$$     . zwf-fpio**2*zwi+zwf**2*zwi)
c$$$      zcef205a=0.0
c$$$      zcef205b=0.0
c$$$      zcef205c=-6.0*fdel**3*zwi
c$$$      zcef305a=fdel*zwf
c$$$      zcef305b=-6.0*fdel*zwi
c$$$      zcef305c=fdel*(-fdel**2*zwf-3.0*fdel**2*zwi-fpio**2*zwf-2.0*
c$$$     . fpio**2*zwi+3.0*zwf**2*zwi)
c$$$      zcef15a=0.0
c$$$      zcef15b=-2.0*fdel*fnuc
c$$$      zcef15c=fdel*(-2.0*fdel**3-3.0*fdel**2*fnuc+2.0*fdel*fpio**2-
c$$$     . 2.0*fdel*zwf**2+2.0*fdel*zwf*zwi-fnuc*fpio**2+fnuc*zwf**2)
c$$$      zcef25a=-3.0*fdel**2
c$$$      zcef25b=0.0
c$$$      zcef25c=3.0*fdel**3*(fdel-2.0*fnuc)
c$$$      zcef35a=-2.0*fdel**2-fpio**2+zwf**2
c$$$      zcef35b=2.0*fdel*(fdel-3.0*fnuc)
c$$$      zcef35ab=-2.d0
c$$$      zcef35c=fdel*(2.0*fdel**3-3.0*fdel**2*fnuc+2.0*fdel*fpio**2-
c$$$     . fdel*zwf**2-2.0*fnuc*fpio**2+3.0*fnuc*zwf**2)
c$$$c
c$$$c------------------------------------------------------
c$$$
c$$$      do jx  = 1,mxj,2
c$$$      do idf = 1,6
c$$$      do idi = 1,6
c$$$      do iso = 1,3
c$$$      zpot(jx,idf,idi,iso)=0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      do  jx     = 1,mxj,2
c$$$      do  md     = -3,3,2
c$$$      do  mn     = -1,1,2
c$$$      do  iso    = 1,3,2
c$$$      zvme(jx,md,mn,iso)  = 0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$      end do
c$$$c-------------------------------------------------------
c$$$
c$$$
c$$$      do ix = 1,mxx
c$$$      www   = wgau(ix)
c$$$      c     = xgau(ix)
c$$$      s     = sqrt(1.d0 -  c**2)
c$$$      c2    = sqrt((1.d0 + c)/2.d0)
c$$$      s2    = sqrt((1.d0 - c)/2.d0)
c$$$      zqfix = zqfi*c
c$$$
c$$$c----------------- u nucleon -------------------------------------
c$$$      zenn  = sqrt(fnuc**2 + zqi2 + zqf2 + 2.d0*zqfix)
c$$$      zenp  = zenn + fnuc
c$$$      zenm  = zenn - fnuc
c$$$      zu01cx = zemfqi+(-zemf+zenp)*zqf*zdfi
c$$$     &   -(zemf*zenm-zqf2-2.d0*zqfix)*zdi + zqfi*zdf
c$$$      zu10cx = zemfqf - zenm*zqf-zemfqi*zdfi
c$$$     &   +(zemf*zenp-zqf2-2.d0*zqfix)*zdf - zqfi*zdi
c$$$      zu10c  = zu10cx*zffb
c$$$      zu01c  = zu01cx*zffb
c$$$
c$$$      zxxx  = 1.d0/(fpio**2 - 2.d0*(zebi*zemf+zqfix))
c$$$      zyyy  = (1.d0/(zebf-zemi+zenn)-1.d0/(zebi-zemf+zenn))/4.d0/zenn
c$$$      zu10  = (zu10a + zu10b*c)*zxxx + zu10c*zyyy
c$$$      zu01  = (zu01a + zu01b*c)*zxxx + zu01c*zyyy
c$$$c------------------t rho -------------------------------------------
c$$$      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)
c$$$      zvrtc =  zvtx(zqx,vndrh,mndrh)*zvtx(zqx,vrpp, mrpp )       
c$$$      zffc  = grnd*grpp/fmrho*zfac   *zvrtc*swv(ich,3)
c$$$      zdrho1= zffc/((zebi-zebf)**2 - zqx**2 - fmrho**2)
c$$$      zdrho2= zffc/((zemi-zemf)**2 - zqx**2 - fmrho**2)
c$$$c----------------- u delta -----------------------------------------
c$$$      xxx   = 3.d0*fdel**2
c$$$      zkfki = zemf*zemi - zqfix
c$$$      zuu   = (zebi - zemf)**2 - (zqi2+zqf2 + 2.d0*zqfix)
c$$$      zxxx  = zffe/(zuu - fdel**2)/2.d0/xxx
c$$$      zud1051=zxxx*(zcefx105a*zuu + zcefx105b*zkfki + zcefx105c)
c$$$      zud2051=zxxx*(zcefx205a*zuu + zcefx205b*zkfki + zcefx205c)
c$$$      zud3051=zxxx*(zcefx305a*zuu + zcefx305b*zkfki + zcefx305c)
c$$$      zud151=zxxx*(zcefx15a*zuu+zcefx15b*zkfki+zcefx15c)
c$$$      zud251=zxxx*(zcefx25a*zuu+zcefx25b*zkfki+zcefx25c)
c$$$      zud351=zxxx*(zcefx35a*zuu+zcefx35b*zkfki+zcefx35c-2.d0*zuu*zkfki)
c$$$c--------------------------------------------------------------------
c$$$      zuu   = (zebf - zemi)**2 - (zqi2+zqf2 + 2.d0*zqfix)
c$$$      zxxx  = zffe/(zuu - fdel**2)/2.d0/xxx
c$$$      zud1052=zxxx*(zcef105a*zuu + zcef105b*zkfki + zcef105c)
c$$$      zud2052=zxxx*(zcef205a*zuu + zcef205b*zkfki + zcef205c)
c$$$      zud3052=zxxx*(zcef305a*zuu + zcef305b*zkfki + zcef305c)
c$$$      zud152=zxxx*(zcef15a*zuu + zcef15b*zkfki + zcef15c)
c$$$      zud252=zxxx*(zcef25a*zuu + zcef25b*zkfki + zcef25c)
c$$$      zud352=zxxx*(zcef35a*zuu + zcef35b*zkfki + zcef35c-2.d0*zuu*zkfki)
c$$$c  test-----------------------------------
c$$$c      zud1051 = 0
c$$$c      zud2051 = 0
c$$$c      zud3051 = 0
c$$$c      zud151 = 0
c$$$c      zud251 = 0
c$$$c      zud351 = 0
c$$$c-------------------------------------------------------------------
c$$$      zepsf(0,2) = 0
c$$$      zepsf(1,2) = - ss2*c
c$$$      zepsf(2,2) =   ss2*zi
c$$$      zepsf(3,2) = + ss2*s
c$$$
c$$$      zepsf(0,-2) = 0
c$$$      zepsf(1,-2) =  ss2*c
c$$$      zepsf(2,-2) =  ss2*zi
c$$$      zepsf(3,-2) = -ss2*s
c$$$
c$$$      zepsf(0,0) = -  zqf/fbf
c$$$      zepsf(1,0) = s*zebf/fbf
c$$$      zepsf(2,0) = 0
c$$$      zepsf(3,0) = c*zebf/fbf
c$$$
c$$$      do imx = -2,2,2
c$$$      zepsfkf(imx) = zepsf(0,imx)*zemf
c$$$     &             -(zepsf(3,imx)*c + zepsf(1,imx)*s)*zqf
c$$$      zepsfki(imx) = zepsf(0,imx)*zemi-zepsf(3,imx) *zqi
c$$$
c$$$c for rho-exchange
c$$$      zxkfpi       = zepsf(0,imx)*zebi+zepsf(3,imx) *zqi
c$$$      zxkfim       = zepsfkf(imx)-zepsfki(imx)
c$$$      zxkfip       = zepsfkf(imx)+zepsfki(imx)
c$$$      zraa   =zxkfpi*zwfip*zdrho1+(zxkfim*zwfip-zxkfip*zwfim)*zdrho2
c$$$      zrbb   =(- zxkfpi*fmdnm+zxkfip*fmdnp)*zdrho1
c$$$     &       +(- zxkfim*fmdnm+zxkfip*fmdnp)*zdrho2
c$$$      zc01(imx)= zdi*(-zraa-zrbb)/2.d0
c$$$      zc10(imx)= zdf*(-zraa+zrbb)/2.d0
c$$$c for s-delta
c$$$      zalf     = zepsfki(imx)*zdalf1 + zepsfkf(imx)*zdalf2
c$$$      zbet     = zepsfki(imx)*zdbet1 + zepsfkf(imx)*zdbet2
c$$$      zsd01(imx) = zdi*zalf
c$$$      zsd10(imx) = zdf*zbet
c$$$c for u-delta
c$$$      zud05    = zepsfki(imx)*(zud1051+zud1052)
c$$$     &          +zepsfkf(imx)*(zud2051+zud2052)
c$$$     &          +(zxkfpi-zepsfkf(imx))*zud3051
c$$$     &          +(      -zepsfki(imx))*zud3052
c$$$
c$$$      zud5     = zepsfki(imx)*(zud151+zud152)
c$$$     &          +zepsfkf(imx)*(zud251+zud252)
c$$$     &          +(zxkfpi-zepsfkf(imx))*zud351
c$$$     &          +(      -zepsfki(imx))*zud352
c$$$      zud01(imx)= zdi*(-zud05-zud5)
c$$$      zud10(imx)= zdf*(-zud05+zud5)
c$$$
c$$$      end do
c$$$
c$$$      do isn = -1,1,2
c$$$      xisn = isn
c$$$
c$$$      zsu  = dfun(1,isn, 1,ix)*(zs01*xisn+zs10)
c$$$      zsd  = dfun(1,isn,-1,ix)*(zs01*xisn-zs10)
c$$$      zuu  = dfun(1,isn, 1,ix)*(zu01*xisn+zu10)
c$$$      zud  = dfun(1,isn,-1,ix)*(zu01*xisn-zu10)
c$$$
c$$$      zsnuc( 3)  =     zepsfkf( 2)*zsu
c$$$      zsnuc( 1)  = ss3*zepsfkf( 2)*zsd + ss23*zepsfkf(0)*zsu
c$$$      zsnuc(-1)  = ss3*zepsfkf(-2)*zsu + ss23*zepsfkf(0)*zsd
c$$$      zsnuc(-3)  =     zepsfkf(-2)*zsd
c$$$
c$$$      zunuc( 3)  =     zepsfki( 2)*zuu
c$$$      zunuc( 1)  = ss3*zepsfki( 2)*zud + ss23*zepsfki(0)*zuu
c$$$      zunuc(-1)  = ss3*zepsfki(-2)*zuu + ss23*zepsfki(0)*zud
c$$$      zunuc(-3)  =     zepsfki(-2)*zud
c$$$      
c$$$
c$$$      zcrho( 3)  =    dfun(1,isn, 1,ix)*( zc10( 2) + zc01( 2)*xisn)
c$$$      zcrho( 1)  =ss3*dfun(1,isn,-1,ix)*(-zc10( 2) + zc01( 2)*xisn)
c$$$     &          +ss23*dfun(1,isn, 1,ix)*( zc10( 0) + zc01( 0)*xisn)
c$$$      zcrho(-1)  =ss3*dfun(1,isn, 1,ix)*( zc10(-2) + zc01(-2)*xisn)
c$$$     &          +ss23*dfun(1,isn,-1,ix)*(-zc10( 0) + zc01( 0)*xisn)
c$$$      zcrho(-3)  =    dfun(1,isn,-1,ix)*(-zc10(-2) + zc01(-2)*xisn)
c$$$
c$$$      zsdel( 3)  =    dfun(1,isn, 1,ix)*( zsd10( 2) + zsd01( 2)*xisn)
c$$$      zsdel( 1)  =ss3*dfun(1,isn,-1,ix)*(-zsd10( 2) + zsd01( 2)*xisn)
c$$$     &          +ss23*dfun(1,isn, 1,ix)*( zsd10( 0) + zsd01( 0)*xisn)
c$$$      zsdel(-1)  =ss3*dfun(1,isn, 1,ix)*( zsd10(-2) + zsd01(-2)*xisn)
c$$$     &          +ss23*dfun(1,isn,-1,ix)*(-zsd10( 0) + zsd01( 0)*xisn)
c$$$      zsdel(-3)  =    dfun(1,isn,-1,ix)*(-zsd10(-2) + zsd01(-2)*xisn)
c$$$
c$$$      zudel( 3)  =    dfun(1,isn, 1,ix)*( zud10( 2) + zud01( 2)*xisn)
c$$$      zudel( 1)  =ss3*dfun(1,isn,-1,ix)*(-zud10( 2) + zud01( 2)*xisn)
c$$$     &          +ss23*dfun(1,isn, 1,ix)*( zud10( 0) + zud01( 0)*xisn)
c$$$      zudel(-1)  =ss3*dfun(1,isn, 1,ix)*( zud10(-2) + zud01(-2)*xisn)
c$$$     &          +ss23*dfun(1,isn,-1,ix)*(-zud10( 0) + zud01( 0)*xisn)
c$$$      zudel(-3)  =    dfun(1,isn,-1,ix)*(-zud10(-2) + zud01(-2)*xisn)
c$$$
c$$$      do jx = 1,mxj,2
c$$$      do isd= -3,3,2
c$$$      zvme(jx,isd,isn,1) = zvme(jx,isd,isn,1)+www*dfun(jx,isn,isd,ix)
c$$$     &   *(zsnuc(isd)+zunuc(isd)*fiso(1,ich,2)+zcrho(isd)*fiso(1,ich,3)
c$$$     &    +zudel(isd)*fiso(1,ich,5))
c$$$      zvme(jx,isd,isn,3) = zvme(jx,isd,isn,3)+www*dfun(jx,isn,isd,ix)
c$$$     &   *(zsdel(isd)+zunuc(isd)*fiso(3,ich,2)+zcrho(isd)*fiso(3,ich,3)
c$$$     &    +zudel(isd)*fiso(3,ich,5))
c$$$      end do  !ise
c$$$      end do  !jx
c$$$
c$$$      end do  !isn
c$$$
c$$$      end do  ! ix loop cos
c$$$
c$$$c
c$$$c           lsj scheme
c$$$c
c$$$      jsf    = 3
c$$$      jsi    = 1
c$$$      ipi    = -1  ! pion parity
c$$$      ipf    = -1  ! pion parity
c$$$
c$$$      do jjx = 1,mxj,2
c$$$      do lfx = abs(jjx-jsf),jjx+jsf,2
c$$$      do lix = abs(jjx-jsi),jjx+jsi,2
c$$$      lix2   = lix/2
c$$$      lfx2   = lfx/2
c$$$      ipxi   = (-1)**lix2*ipi
c$$$      ipxf   = (-1)**lfx2*ipf
c$$$      iptest = ipxi*ipxf
c$$$
c$$$      if(iptest.eq.1) then
c$$$
c$$$      lff  = lfx - jjx
c$$$      lii  = lix - jjx
c$$$      idxf = index(jsf,lff)
c$$$      idxi = index(jsi,lii)
c$$$
c$$$      do iso = 1,3
c$$$      zsum   = 0
c$$$      do isf  = -jsf,jsf,2
c$$$      do isi  = -jsi,jsi,2
c$$$      ww  = flsjxh(lfx,jsf,isf,jjx)*flsjxh(lix,jsi,isi,jjx)
c$$$      zsum= zsum+ww*zvme(jjx,isf,isi,iso)
c$$$      end do
c$$$      end do
c$$$      zpot(jjx,idxf,idxi,iso) = zsum
c$$$      end do  !iso
c$$$
c$$$      end if
c$$$
c$$$      end do  !lf
c$$$      end do  !li
c$$$      end do  !j
c$$$
c$$$      return
c$$$      end
c$$$
c$$$c------------------------------------------------------------
c$$$c  potential 12 eta N -> pi D
c$$$c  
c$$$c  1 s- nucleon
c$$$c
c$$$      subroutine ven2pd(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-----------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$      common / cpidx / index(3,-3:3)
c$$$
c$$$      dimension zvme(20,-3:3,-1:1,3),zpot(20,6,6,3)
c$$$      dimension zepsf(0:3,-2:2),zepsfk(-2:2),zsnuc(-3:3)
c$$$
c$$$      ich    = 12
c$$$      mxp    = mxpot(ich)
c$$$      zi     = (0.d0,1.d0)
c$$$      pi2    = 2.d0*pi
c$$$      ss2    = 1.d0/sqrt(2.d0)
c$$$      ss3    = 1.d0/sqrt(3.d0)
c$$$      ss23   = sqrt(2.d0)/sqrt(3.d0)
c$$$
c$$$      fmi    = feta
c$$$      fbi    = fnuc
c$$$      fmf    = fpio
c$$$      fbf    = fdel
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$      
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$
c$$$      zvrta =  zvtx(zqf,vndpi,mndpi)*zvtx(zqi,vnnet,mnnet)
c$$$      zffa     = gpind*genn/fpio/feta*zfac*zvrta*swv(ich,1)
c$$$
c$$$      zs1101 = -((zwi+fnuc)*zqi /(zwi**2 - fnuc**2)
c$$$     &         + (zwf+fnuc)*zqi /(zwf**2 - fnuc**2))/2.d0
c$$$      zs1200 =  ((zwi+fnuc)*zemi/(zwi**2 - fnuc**2)
c$$$     &         + (zwf+fnuc)*zemi/(zwf**2 - fnuc**2))/2.d0
c$$$      zs2100 =  ((zwi-fnuc)*zemi/(zwi**2 - fnuc**2)
c$$$     &         + (zwf-fnuc)*zemi/(zwf**2 - fnuc**2))/2.d0
c$$$      zs2201 = -((zwi-fnuc)*zqi /(zwi**2 - fnuc**2)
c$$$     &         + (zwf-fnuc)*zqi /(zwf**2 - fnuc**2))/2.d0
c$$$c
c$$$c include vertex, coupling constant.isospin
c$$$c
c$$$c  s-channel nucleon only isospin 1/2
c$$$c
c$$$      zs01x   = zs1101 - zs1200*zdi
c$$$      zs10x   =        + zs2100*zdf - zs2201*zdi*zdf 
c$$$      zs01    = zs01x*zffa*fiso(1,ich,1)
c$$$      zs10    = zs10x*zffa*fiso(1,ich,1)
c$$$
c$$$c------------------------------------------------------
c$$$
c$$$      do jx  = 1,mxj,2
c$$$      do idf = 1,6
c$$$      do idi = 1,6
c$$$      do iso = 1,3
c$$$      zpot(jx,idf,idi,iso)=0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      do  jx     = 1,mxj,2
c$$$      do  md     = -3,3,2
c$$$      do  mn     = -1,1,2
c$$$      do  iso    = 1,3,2
c$$$      zvme(jx,md,mn,iso)  = 0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$      end do
c$$$c-------------------------------------------------------
c$$$
c$$$      zqfi  = zqf *zqi
c$$$      zemfi = zemf*zemi
c$$$
c$$$      do ix = 1,mxx
c$$$      www   = wgau(ix)
c$$$      c     = xgau(ix)
c$$$      s     = sqrt(1.d0 -  c**2)
c$$$      c2    = sqrt((1.d0 + c)/2.d0)
c$$$      s2    = sqrt((1.d0 - c)/2.d0)
c$$$
c$$$      zepsf(0,2) = 0
c$$$      zepsf(1,2) = - ss2*c
c$$$      zepsf(2,2) =   ss2*zi
c$$$      zepsf(3,2) = + ss2*s
c$$$
c$$$      zepsf(0,-2) = 0
c$$$      zepsf(1,-2) =  ss2*c
c$$$      zepsf(2,-2) =  ss2*zi
c$$$      zepsf(3,-2) = -ss2*s
c$$$
c$$$      zepsf(0,0) = -  zqf/fbf
c$$$      zepsf(1,0) = s*zebf/fbf
c$$$      zepsf(2,0) = 0
c$$$      zepsf(3,0) = c*zebf/fbf
c$$$
c$$$      do imx = -2,2,2
c$$$      zepsfk(imx) = zepsf(0,imx)*zemf
c$$$     &            -(zepsf(3,imx)*c + zepsf(1,imx)*s)*zqf
c$$$      end do
c$$$
c$$$      do isn = -1,1,2
c$$$      xisn = isn
c$$$
c$$$      zsu  = dfun(1,isn,1,ix)*(  zs01*xisn + zs10)
c$$$      zsd  = dfun(1,isn,-1,ix)*( zs01*xisn - zs10)
c$$$
c$$$      zsnuc( 3)  = zsu*zepsfk(2)
c$$$      zsnuc( 1)  = ss3*zepsfk( 2)*zsd + ss23*zepsfk(0)*zsu
c$$$      zsnuc(-1)  = ss3*zepsfk(-2)*zsu + ss23*zepsfk(0)*zsd
c$$$      zsnuc(-3)  = zsd*zepsfk(-2)
c$$$      
c$$$      do jx = 1,mxj,2
c$$$      do isd= -3,3,2
c$$$      zvme(jx,isd,isn,1) = zvme(jx,isd,isn,1)+www*dfun(jx,isn,isd,ix)
c$$$     &                                   *zsnuc(isd)
c$$$      end do  !ise
c$$$      end do  !jx
c$$$
c$$$      end do  !isn
c$$$
c$$$      end do  ! ix loop cos
c$$$
c$$$c
c$$$c           lsj scheme
c$$$c
c$$$      jsf    = 3
c$$$      jsi    = 1
c$$$      ipi    = -1  ! eta parity
c$$$      ipf    = -1  ! pion parity
c$$$
c$$$      do jjx = 1,mxj,2
c$$$      do lfx = abs(jjx-jsf),jjx+jsf,2
c$$$      do lix = abs(jjx-jsi),jjx+jsi,2
c$$$      lix2   = lix/2
c$$$      lfx2   = lfx/2
c$$$      ipxi   = (-1)**lix2*ipi
c$$$      ipxf   = (-1)**lfx2*ipf
c$$$      iptest = ipxi*ipxf
c$$$
c$$$      if(iptest.eq.1) then
c$$$
c$$$      lff  = lfx - jjx
c$$$      lii  = lix - jjx
c$$$      idxf = index(jsf,lff)
c$$$      idxi = index(jsi,lii)
c$$$
c$$$      do iso = 1,3
c$$$      zsum   = 0
c$$$      do isf  = -jsf,jsf,2
c$$$      do isi  = -jsi,jsi,2
c$$$      ww  = flsjxh(lfx,jsf,isf,jjx)*flsjxh(lix,jsi,isi,jjx)
c$$$      zsum= zsum+ww*zvme(jjx,isf,isi,iso)
c$$$      end do
c$$$      end do
c$$$      zpot(jjx,idxf,idxi,iso) = zsum
c$$$      end do  !iso
c$$$
c$$$      end if
c$$$
c$$$      end do  !lf
c$$$      end do  !li
c$$$      end do  !j
c$$$
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------
c$$$c  potential 13 sigma N -> pi D
c$$$c  
c$$$c  1 s- nucleon
c$$$c
c$$$      subroutine vsn2pd(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-----------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$      common / cpidx / index(3,-3:3)
c$$$
c$$$      dimension zvme(20,-3:3,-1:1,3),zpot(20,6,6,3)
c$$$      dimension zepsf(0:3,-2:2),zepsfk(-2:2),zsnuc(-3:3)
c$$$
c$$$      ich    = 13
c$$$      mxp    = mxpot(ich)
c$$$      zi     = (0.d0,1.d0)
c$$$      pi2    = 2.d0*pi
c$$$      ss2    = 1.d0/sqrt(2.d0)
c$$$      ss3    = 1.d0/sqrt(3.d0)
c$$$      ss23   = sqrt(2.d0)/sqrt(3.d0)
c$$$
c$$$      fmi    = fsigm
c$$$      fbi    = fnuc
c$$$      fmf    = fpio
c$$$      fbf    = fdel
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$      
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$
c$$$      zvrta =  zvtx(zqf,vndpi,mndpi)*zvtx(zqi,vnnsi,mnnsi)
c$$$      zffa     = -zi*gpind*gsinn/fpio*zfac*zvrta*swv(ich,1)
c$$$
c$$$      zs1100 =  (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))/2.d0
c$$$      zs2200 =  (1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))/2.d0
c$$$c
c$$$c include vertex, coupling constant.isospin
c$$$c
c$$$c  s-channel nucleon only isospin 1/2
c$$$c
c$$$      zs00    = zs1100*zffa*fiso(1,ich,1)
c$$$      zs11    = zs2200*zffa*fiso(1,ich,1)*zdi*zdf
c$$$
c$$$c------------------------------------------------------
c$$$
c$$$      do jx  = 1,mxj,2
c$$$      do idf = 1,6
c$$$      do idi = 1,6
c$$$      do iso = 1,3
c$$$      zpot(jx,idf,idi,iso)=0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      do  jx     = 1,mxj,2
c$$$      do  md     = -3,3,2
c$$$      do  mn     = -1,1,2
c$$$      do  iso    = 1,3,2
c$$$      zvme(jx,md,mn,iso)  = 0
c$$$      end do
c$$$      end do
c$$$      end do
c$$$      end do
c$$$c-------------------------------------------------------
c$$$
c$$$      zqfi  = zqf *zqi
c$$$      zemfi = zemf*zemi
c$$$
c$$$      do ix = 1,mxx
c$$$      www   = wgau(ix)
c$$$      c     = xgau(ix)
c$$$      s     = sqrt(1.d0 -  c**2)
c$$$      c2    = sqrt((1.d0 + c)/2.d0)
c$$$      s2    = sqrt((1.d0 - c)/2.d0)
c$$$
c$$$      zepsf(0,2) = 0
c$$$      zepsf(1,2) = - ss2*c
c$$$      zepsf(2,2) =   ss2*zi
c$$$      zepsf(3,2) = + ss2*s
c$$$
c$$$      zepsf(0,-2) = 0
c$$$      zepsf(1,-2) =  ss2*c
c$$$      zepsf(2,-2) =  ss2*zi
c$$$      zepsf(3,-2) = -ss2*s
c$$$
c$$$      zepsf(0,0) = -  zqf/fbf
c$$$      zepsf(1,0) = s*zebf/fbf
c$$$      zepsf(2,0) = 0
c$$$      zepsf(3,0) = c*zebf/fbf
c$$$
c$$$      do imx = -2,2,2
c$$$      zepsfk(imx) = zepsf(0,imx)*zemf
c$$$     &            -(zepsf(3,imx)*c + zepsf(1,imx)*s)*zqf
c$$$      end do
c$$$
c$$$      do isn = -1,1,2
c$$$      xisn = isn
c$$$
c$$$      zsu  = dfun(1,isn,1,ix)*(  zs00 + zs11*isn)
c$$$      zsd  = dfun(1,isn,-1,ix)*( zs00 - zs11*isn)
c$$$
c$$$      zsnuc( 3)  = zsu*zepsfk(2)
c$$$      zsnuc( 1)  = ss3*zepsfk( 2)*zsd + ss23*zepsfk(0)*zsu
c$$$      zsnuc(-1)  = ss3*zepsfk(-2)*zsu + ss23*zepsfk(0)*zsd
c$$$      zsnuc(-3)  = zsd*zepsfk(-2)
c$$$      
c$$$      do jx = 1,mxj,2
c$$$      do isd= -3,3,2
c$$$      zvme(jx,isd,isn,1) = zvme(jx,isd,isn,1)+www*dfun(jx,isn,isd,ix)
c$$$     &                                   *zsnuc(isd)
c$$$      end do  !ise
c$$$      end do  !jx
c$$$
c$$$      end do  !isn
c$$$
c$$$      end do  ! ix loop cos
c$$$
c$$$c
c$$$c           lsj scheme
c$$$c
c$$$      jsf    = 3
c$$$      jsi    = 1
c$$$      ipi    =  1  ! sigma parity
c$$$      ipf    = -1  ! pion  parity
c$$$
c$$$      do jjx = 1,mxj,2
c$$$      do lfx = abs(jjx-jsf),jjx+jsf,2
c$$$      do lix = abs(jjx-jsi),jjx+jsi,2
c$$$      lix2   = lix/2
c$$$      lfx2   = lfx/2
c$$$      ipxi   = (-1)**lix2*ipi
c$$$      ipxf   = (-1)**lfx2*ipf
c$$$      iptest = ipxi*ipxf
c$$$
c$$$      if(iptest.eq.1) then
c$$$
c$$$      lff  = lfx - jjx
c$$$      lii  = lix - jjx
c$$$      idxf = index(jsf,lff)
c$$$      idxi = index(jsi,lii)
c$$$
c$$$      do iso = 1,3
c$$$      zsum   = 0
c$$$      do isf  = -jsf,jsf,2
c$$$      do isi  = -jsi,jsi,2
c$$$      ww  = flsjxh(lfx,jsf,isf,jjx)*flsjxh(lix,jsi,isi,jjx)
c$$$      zsum= zsum+ww*zvme(jjx,isf,isi,iso)
c$$$      end do
c$$$      end do
c$$$      zpot(jjx,idxf,idxi,iso) = zsum
c$$$      end do  !iso
c$$$
c$$$      end if
c$$$
c$$$      end do  !lf
c$$$      end do  !li
c$$$      end do  !j
c$$$
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------------
c$$$c  potential 14 rho N -> pi D
c$$$c  
c$$$c  1 s- nucleon
c$$$c  2 u- nucleon
c$$$c                             14
c$$$c-------------------------------------------------------------------
c$$$      subroutine vrn2pd(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-------------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$
c$$$      common / cefflo / cg1h(-2:2,-1:1,3)
c$$$     &     ,xef(20,6,-2:2,-3:3,10)
c$$$      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
c$$$     &     ,jss(20,6,10),jll(20,6,10)
c$$$     &     ,jip(20,6,10)
c$$$
c$$$      dimension zpot(20,6,6,3)
c$$$      dimension zvme(20,-3:3,-2:2,-1:1,3)
c$$$      dimension zans1(-3:3,-2:2,-1:1),zans2(-3:3,-2:2,-1:1)
c$$$      dimension zpolfk(-2:2),zpolik(-2:2),zpolfi(-2:2,-2:2)
c$$$
c$$$c      fdel = fnuc
c$$$      ich    = 14
c$$$      mxp    = mxpot(ich)
c$$$      zi     = (0.d0,1.d0)
c$$$      pi2    = 2.d0*pi
c$$$
c$$$      fmi    = fmrho
c$$$      fbi    = fnuc
c$$$      fmf    = fpio
c$$$      fbf    = fdel
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$      zqfi   = zqf*zqi
c$$$      zwfm   = zwf - fdel
c$$$      zwfp   = zwf + fdel
c$$$      zwim   = zwi - fnuc
c$$$      zwip   = zwi + fnuc
c$$$
c$$$c------------------------------------------------------------
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$      zvrta =  zvtx(zqi,vnnrho,mnnrho)*zvtx(zqf,vndpi,mndpi)
c$$$      zvrtb =  zvtx(zqi,vndrh ,mndrh )*zvtx(zqf,vnnpi,mnnpi)
c$$$      zffa     =-zi*grnn*gpind/fpio/2.d0      *zfac*zvrta*swv(ich,1)
c$$$      zffb     = zi*grnd*gpin /fpio/fmrho     *zfac*zvrtb*swv(ich,2)
c$$$
c$$$c
c$$$c  s-channel
c$$$c
c$$$      zgam =  (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))/2.d0
c$$$      zdel = -(1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))/2.d0*zdf
c$$$      zalf =  1.d0 - xkrho/(2.d0*fnuc)*( zwi-fnuc)
c$$$      zbet =-(1.d0 - xkrho/(2.d0*fnuc)*(-zwi-fnuc))*zdi
c$$$
c$$$      zffaxx  = zffa*zwf*fiso(1,14,1)
c$$$      zxfa001 = zffaxx*zgam*zalf
c$$$      zxfa111 =-zffaxx*zdel*zbet
c$$$      zxfb011 =-zffaxx*zgam*zbet
c$$$      zxfb101 = zffaxx*zdel*zalf
c$$$      zacef0 = -zqf*zqi/fdel/fmrho
c$$$      zbcef0 = -zqf*zemi/fdel/fmrho
c$$$      zbcef1 = -zqf/fdel*sqrt(2.d0)
c$$$
c$$$      fnxx1     = 2.d0*fnuc
c$$$      fnxx2     = fnuc+fdel
c$$$c-----------------------------------------------------------
c$$$
c$$$
c$$$      zvme = 0
c$$$
c$$$      do ix = 1,mxx
c$$$      www   = wgau(ix)
c$$$      cc    = xgau(ix)
c$$$      ss    = sqrt(1.d0 -  cc**2)
c$$$      c2x   = (1.d0 + cc)/2.d0
c$$$      s2x   = (1.d0 - cc)/2.d0
c$$$      zqfix = zqfi*cc
c$$$      ss22  = ss/sqrt(2.d0)
c$$$c
c$$$c  u-channel nucleon
c$$$c
c$$$      zqx2  =  zqf2 + zqi2 + zqfix*2.d0 ! for u
c$$$      zqfqi1= (zemf*zemi - zqfix)*2.d0*fnxx1
c$$$      zqfqi2= (zemf*zemi - zqfix)*2.d0*fnxx2
c$$$
c$$$      zux1  = (zebi-zemf)**2 - zqx2
c$$$      zux2  = (zebf-zemi)**2 - zqx2
c$$$      zu1   = 1.d0/(zux1 - fnuc**2)/2.d0
c$$$      zu2   = 1.d0/(zux2 - fnuc**2)/2.d0
c$$$      zaa1  = (zwf-zwi)*zu2*zffb
c$$$      zaa2  =-(zu1+zu2)*(zux1-fnuc**2)*zffb
c$$$      zaa3  =-(zu1+zu2)*2.d0*fnuc*zffb
c$$$      zaab  =  zaa1*2.d0*zemf + zaa2
c$$$      zxga00=  zaab + zwfm*(zaa1-zaa3)            ! epsilon 0
c$$$      zxga11= (zaab + zwfp*(zaa1+zaa3))*zdf*zdi
c$$$      zxgb01= (zaab - zwfm*(zaa1+zaa3))*zdi       ! e.s
c$$$      zxgb10= (zaab - zwfp*(zaa1-zaa3))*zdf
c$$$      zxgc00= 2.d0*(zaa3-zaa1)                    ! k'.e
c$$$      zxgc11=-2.d0*(zaa3+zaa1)*zdi*zdf
c$$$c
c$$$      zxgd00=-(zu1*(zwim*(-zux1+fnuc**2+fnxx1*zwfm)-zqfqi1)
c$$$     &      +  zu2*(zwfm*(-zux2+fdel**2+fnxx2*zwim)-zqfqi2))*zffb
c$$$      zxgd11=-(zu1*(zwip*(-zux1+fnuc**2-fnxx1*zwfp)+zqfqi1)
c$$$     &      +  zu2*(zwfp*(-zux2+fdel**2-fnxx2*zwip)+zqfqi2))*zffb
c$$$     &      *zdi*zdf
c$$$      zpolfk(0) = -(zqf*zemi+zebf*zqi*cc)/fdel
c$$$      zpolfk(2) = - zqi*ss22
c$$$      zpolfk(-2)= - zpolfk(2)
c$$$
c$$$      zpolik(0) =  (zqi*zemf-zemi*zqf*cc)/fmrho
c$$$      zpolik(2) =   zqf*ss22
c$$$      zpolik(-2)= - zpolik(2)
c$$$
c$$$      zpolfi( 0, 0) = - (zqf*zqi+zebf*zemi*cc)/fmrho/fdel
c$$$      zpolfi( 0, 2) = zebf*ss22/fdel
c$$$      zpolfi( 0,-2) =-zpolfi(0,2)
c$$$      zpolfi( 2, 0) =-zemi*ss22/fmrho
c$$$      zpolfi(-2, 0) =-zpolfi(2,0)
c$$$      zpolfi( 2, 2) = - c2x
c$$$      zpolfi( 2,-2) = - s2x
c$$$      zpolfi(-2, 2) = zpolfi(2,-2)
c$$$      zpolfi(-2,-2) = zpolfi(2,2)
c$$$
c$$$      zans1 = 0
c$$$      zans2 = 0
c$$$
c$$$      do isf = -1,1,2
c$$$      do isi = -1,1,2
c$$$      zxxd   = zxgd00 + isf*isi*zxgd11
c$$$      zxxc   = zxgc00 + isf*isi*zxgc11
c$$$      zxxa   = zxga00 + isf*isi*zxga11
c$$$      zxxb   =(zxgb01*isi + zxgb10*isf)*isi
c$$$      do imf = -2,2,2
c$$$      izf    = isf + imf
c$$$      weix   = cg1h(imf,isf,3)*www
c$$$      zaaa   = weix*zpolfk(imf)*sqrt(2.d0)*zxxb
c$$$      zbbb   = weix*zpolfk(imf)*(zqi*zxxa+zemi*zxxb)/fmrho
c$$$     &                         *dfun(1,isi,isf,ix)
c$$$      zans2(izf, 0,isi)=zans2(izf, 0,isi) + zbbb
c$$$      if(isi.eq.-1) then
c$$$      zans2(izf, 2,isi)=zans2(izf, 2,isi)+dfun(1, 1,isf,ix)*zaaa
c$$$      else if(isi.eq.1) then
c$$$      zans2(izf,-2,isi)=zans2(izf,-2,isi)+dfun(1,-1,isf,ix)*zaaa
c$$$      end if
c$$$
c$$$      do imi = -2,2,2
c$$$      zzz    = zxxd*zpolfi(imf,imi)+zxxc*zpolfk(imf)*zpolik(imi)
c$$$      zans2(izf,imi,isi) =zans2(izf,imi,isi)+zzz*dfun(1,isi,isf,ix)*weix
c$$$      end do
c$$$
c$$$      end do
c$$$      end do
c$$$      end do
c$$$
c$$$      do isf = -1,1,2
c$$$      do isi = -1,1,2
c$$$      zxxa   = (zxfa001    +zxfa111*isf*isi)*cg1h(0,isf,3)
c$$$      zxxb   = (zxfb011*isi+zxfb101*isf    )*cg1h(0,isf,3)
c$$$      zans1(isf,0,isi)=(zacef0*zxxa+zbcef0*zxxb*isi)
c$$$     &                 *dfun(1,isi,isf,ix)*www
c$$$
c$$$      if(isi.eq.-1) then
c$$$      zans1(isf, 2,isi)=zbcef1*zxxb*isi*dfun(1,1,isf,ix)*www
c$$$      else if(isi.eq.1) then
c$$$      zans1(isf,-2,isi)=zbcef1*zxxb*isi*dfun(1,-1,isf,ix)*www
c$$$      end if
c$$$
c$$$      end do
c$$$      end do
c$$$c
c$$$c  j-projection
c$$$c
c$$$
c$$$      do imf = -3,3,2
c$$$      do imi = -1,1,2
c$$$      do immi = -2,2,2
c$$$
c$$$      ihi = imi + immi
c$$$      do jx = 1,mxj,2
c$$$      zvme(jx,imf,immi,imi,1)=zvme(jx,imf,immi,imi,1)
c$$$     &   +dfun(jx,ihi,imf,ix)*(zans1(imf,immi,imi)
c$$$     &                        +zans2(imf,immi,imi)*fiso(1,14,2))
c$$$      zvme(jx,imf,immi,imi,3)=zvme(jx,imf,immi,imi,3)
c$$$     &   +dfun(jx,ihi,imf,ix)*zans2(imf,immi,imi)*fiso(3,14,2)
c$$$      end do ! jx
c$$$
c$$$      end do  ! immi
c$$$      end do ! imf
c$$$      end do ! imi
c$$$
c$$$      end do
c$$$
c$$$c
c$$$c           lsj scheme
c$$$c
c$$$      iccf = 3
c$$$      jmf  = 0
c$$$      jbf  = 3
c$$$
c$$$      icci = 5
c$$$      jmi  = 2
c$$$      jbi  = 1
c$$$
c$$$      zpot = 0
c$$$      do 200 jjx = 1,mxj,2
c$$$
c$$$      do 300 idxf= 1,6
c$$$      do 300 idxi= 1,6
c$$$      ipxi  = jip(jjx,idxi,icci)
c$$$      ipxf  = jip(jjx,idxf,iccf)
c$$$      iptest = ipxi*ipxf
c$$$      if(iptest.eq.1) then
c$$$
c$$$      imfz = 0
c$$$      do 210 ibfz= -jbf,jbf,2
c$$$      xxf = xef(jjx,idxf,imfz,ibfz,iccf)
c$$$
c$$$      if(abs(xxf).gt.1.d-20)then
c$$$
c$$$      do 220 imiz= -jmi,jmi,2
c$$$      do 220 ibiz= -jbi,jbi,2
c$$$      www = xef(jjx,idxi,imiz,ibiz,icci)*xxf
c$$$
c$$$      if(abs(www).gt.1.d-20) then
c$$$      do iso = 1,3
c$$$      zpot(jjx,idxf,idxi,iso) = zpot(jjx,idxf,idxi,iso)
c$$$     &         + www*zvme(jjx,ibfz,imiz,ibiz,iso)
c$$$      end do
c$$$      end if
c$$$
c$$$ 220  continue
c$$$
c$$$      end if
c$$$
c$$$ 210  continue
c$$$
c$$$      end if  ! parity test
c$$$
c$$$ 300  continue
c$$$ 200  continue
c$$$
c$$$      return
c$$$      end
c$$$c------------------------------------------------------------------
c$$$c  potential 15 pi D -> pi D
c$$$c  
c$$$c  1 s- nucleon
c$$$c  4 t- rho
c$$$c                             15
c$$$c-------------------------------------------------------------------
c$$$      subroutine vpd2pd(zqf,zqi,zpot)
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$c-------------------------------------------------------------------
c$$$      common / const / pi, fm, scale
c$$$      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$      common / cswv  / swv(20,20)
c$$$
c$$$      common / cefflo / cg1h(-2:2,-1:1,3)
c$$$     &     ,xef(20,6,-2:2,-3:3,10)
c$$$      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
c$$$     &     ,jss(20,6,10),jll(20,6,10)
c$$$     &     ,jip(20,6,10)
c$$$
c$$$
c$$$      dimension zpot(20,6,6,3)
c$$$      dimension zvme(20,-3:3,-3:3,3)
c$$$      dimension zedd(-2:2,-2:2)
c$$$
c$$$      ich    = 15
c$$$      mxp    = mxpot(ich)
c$$$      zi     = (0.d0,1.d0)
c$$$      pi2    = 2.d0*pi
c$$$      ss2    = 1.d0/sqrt(2.d0)
c$$$      ss3    = 1.d0/sqrt(3.d0)
c$$$      ss23   = sqrt(2.d0)/sqrt(3.d0)
c$$$
c$$$      fmi    = fpio
c$$$      fbi    = fdel
c$$$      fmf    = fpio
c$$$      fbf    = fdel
c$$$      fmi2   = fmi**2
c$$$      fmf2   = fmf**2
c$$$      fbi2   = fbi**2
c$$$      fbf2   = fbf**2
c$$$
c$$$      zqf2   = zqf**2
c$$$      zqi2   = zqi**2
c$$$      zemf   = sqrt(fmf2+zqf2)
c$$$      zemi   = sqrt(fmi2+zqi2)
c$$$      zebf   = sqrt(fbf2+zqf2)
c$$$      zebi   = sqrt(fbi2+zqi2)
c$$$      zdf    = zqf/(zebf+fbf)
c$$$      zdi    = zqi/(zebi+fbi)
c$$$      zwf    = zemf + zebf
c$$$      zwi    = zemi + zebi
c$$$      zqfi   = zqf*zqi
c$$$
c$$$c------------------------------------------------------------
c$$$      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
c$$$     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
c$$$
c$$$      zvrta =  zvtx(zqi,vndpi,mndpi)*zvtx(zqf,vndpi,mndpi)
c$$$      zffa     = gpind**2/fpio**2*zfac*zvrta*swv(ich,1)
c$$$      zxxx     = zffa*zwi*zwf*fiso(1,15,1)
c$$$      zxfa00 = zxxx*(1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))/2.d0
c$$$      zxfa11 = zxxx*(1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))/2.d0
c$$$     &             *zdi*zdf
c$$$      zpol00   = zqf*zqi/fdel**2
c$$$      xfdel2   = 1.d0/sqrt(2.d0)/fdel
c$$$      zxxa     = (1.d0+xkrdd)*(zwi+zwf)
c$$$      zxxc     = -xkrdd/2.d0/fdel*(zwf-zwi)*(zebf-zebi)
c$$$      zxxb1    = -2.d0*fdel
c$$$      zxxb2    = xkrdd/fdel
c$$$      zxxb3    = - fdel**2 - zwi*zwf + zemf*zemi
c$$$c-----------------------------------------------------------
c$$$      zvme = 0
c$$$
c$$$      do ix = 1,mxx
c$$$      www   = wgau(ix)
c$$$      cc    = xgau(ix)
c$$$      ss    = sqrt(1.d0 -  cc**2)
c$$$      c2    = sqrt((1.d0 + cc)/2.d0)
c$$$      s2    = sqrt((1.d0 - cc)/2.d0)
c$$$      zqfix = zqfi*cc
c$$$
c$$$      zqx2  =  zqf2 + zqi2 - zqfix*2.d0
c$$$      zqx   =  sqrt(zqx2)
c$$$      zvrtd =  zvtx(zqx,vddrh,mddrh)*zvtx(zqx,vrpp ,mrpp )
c$$$      zffd  = -grdd*grpp*zfac*zvrtd*swv(ich,4)
c$$$
c$$$      zrho1 = 1.d0/((zebi-zebf)**2 - zqx2 - frhoe**2)*zffd/2.d0
c$$$      zrho2 = 1.d0/((zemi-zemf)**2 - zqx2 - frhoe**2)*zffd/2.d0
c$$$      zaa   = (zrho1+zrho2)*zxxa
c$$$      zbb   = (zrho1+zrho2)*(zxxb1 + zxxb2*(zxxb3-zqfix))
c$$$      zcc   =  zrho2*zxxc
c$$$      zxfb00 =  zaa+zbb+zcc
c$$$      zxfb11 = (zaa-zbb-zcc)*zdi*zdf
c$$$
c$$$      zedd( 0, 0) = (zqfi-cc*zebf*zebi)/fdel**2
c$$$
c$$$      zedd( 2, 0) =-zebi*ss*xfdel2
c$$$      zedd(-2, 0) =-zedd(2,0)
c$$$
c$$$      zedd( 0, 2) = zebf*ss*xfdel2
c$$$      zedd( 0,-2) =-zedd(0,2)
c$$$
c$$$      zedd( 2, 2) =-(1.d0 + cc)/2.d0
c$$$      zedd(-2,-2) = zedd(2,2)
c$$$      zedd( 2,-2) =-(1.d0 - cc)/2.d0
c$$$      zedd(-2, 2) = zedd(2,-2)
c$$$
c$$$      do imf = -1,1,2
c$$$      do imi = -1,1,2
c$$$      
c$$$      dhlf   = dfun(1,imi,imf,ix)*www
c$$$      zxfa   = (zxfa00+zxfa11*imf*imi)*dhlf
c$$$      zxfb   = (zxfb00+zxfb11*imf*imi)*dhlf
c$$$      do immf = -2,2,2
c$$$      do immi = -2,2,2
c$$$
c$$$c      if(imf.eq.1.and.imi.eq.1) then
c$$$c      write(52,1111)immf,immi,cc,zedd(immf,immi)
c$$$c 1111 format(1h ,2i3,4e15.5)
c$$$c      end if
c$$$
c$$$      ibmf = imf+immf
c$$$      ibmi = imi+immi
c$$$      cefxx = cg1h(immf,imf,3)*cg1h(immi,imi,3)
c$$$
c$$$      if(abs(cefxx).gt.1.d-15) then
c$$$      zsuma  = 0
c$$$      if(immf.eq.0.and.immi.eq.0) then
c$$$      zsuma  = zxfa*zpol00*cefxx
c$$$      end if
c$$$      zsumb  = zxfb*zedd(immf,immi)*cefxx
c$$$      zsum1  = zsuma+zsumb*fiso(1,15,4)
c$$$      zsum3  =       zsumb*fiso(3,15,4)
c$$$      do jx = 1,mxj,2
c$$$      zvme(jx,ibmf,ibmi,1)=zvme(jx,ibmf,ibmi,1)
c$$$     &                     +dfun(jx,ibmi,ibmf,ix)*zsum1
c$$$      zvme(jx,ibmf,ibmi,3)=zvme(jx,ibmf,ibmi,3)
c$$$     &                     +dfun(jx,ibmi,ibmf,ix)*zsum3
c$$$      end do ! jx
c$$$      end if
c$$$
c$$$      end do  ! immi
c$$$      end do  ! immf
c$$$
c$$$      end do ! imf
c$$$      end do ! imi
c$$$
c$$$      end do
c$$$
c$$$c--------------------------------------------------------------
c$$$c           lsj scheme
c$$$c
c$$$      icci = 3
c$$$      iccf = 3
c$$$      zpot = 0
c$$$      imiz = 0
c$$$      imfz = 0
c$$$      do 200 jjx = 1,mxj,2
c$$$
c$$$      do 300 idxf= 1,6
c$$$      do 300 idxi= 1,6
c$$$      ipxi  = jip(jjx,idxi,icci)
c$$$      ipxf  = jip(jjx,idxf,iccf)
c$$$      iptest = ipxi*ipxf
c$$$      if(iptest.eq.1) then
c$$$
c$$$      do 210 ibfz= -3,3,2
c$$$      xxf = xef(jjx,idxf,imfz,ibfz,iccf)
c$$$
c$$$      if(abs(xxf).gt.1.d-20)then
c$$$
c$$$      do 220 ibiz= -3,3,2
c$$$      www = xef(jjx,idxi,imiz,ibiz,icci)*xxf
c$$$
c$$$      if(abs(www).gt.1.d-20) then
c$$$      do iso = 1,3
c$$$      zpot(jjx,idxf,idxi,iso) = zpot(jjx,idxf,idxi,iso)
c$$$     &         + www*zvme(jjx,ibfz,ibiz,iso)
c$$$      end do
c$$$      end if
c$$$
c$$$ 220  continue
c$$$
c$$$      end if
c$$$
c$$$ 210  continue
c$$$
c$$$      end if  ! parity test
c$$$
c$$$ 300  continue
c$$$ 200  continue
c$$$
c$$$
c$$$      return
c$$$      end
c$$$c==========================================================
c$$$c---------------------------------------------------------------------
c$$$c  sqrt(2l+1/2j+1)(l,0,js,s|j,s)
c$$$c
c$$$c   l2 = 2*l, js2 = js*2, is2= 2*s, j2=2*j
c$$$c
c$$$      real*8 function flsjxh(l2,js2,is2,j2)
c$$$      implicit real*8(a-h,o-z)
c$$$
c$$$      flsjxh = 0
c$$$
c$$$      if(j2.gt.l2+js2.or.j2.lt.abs(l2-js2))return
c$$$      if(abs(is2).gt.js2) return
c$$$
c$$$         jj  = (j2 + 1)/2
c$$$         ll  = l2/2
c$$$         is  = is2
c$$$      if(js2.eq.1) then
c$$$         flsjxh = flsj1h(jj,ll,is)
c$$$      else if(js2.eq.3) then
c$$$         flsjxh = flsj3h(jj,ll,is)
c$$$      end if
c$$$      return
c$$$      end
c$$$c----------------------------------------------------------------------
c$$$c
c$$$c  sqrt((2l + 1)/(2j + 1)) (l 0 1/2 s | j s)
c$$$c
c$$$c  ll = l, jj = j+1/2, is = 2* s
c$$$c
c$$$      real*8 function flsj1h(jj,ll,is)
c$$$      implicit real*8(a-h,o-z)
c$$$c
c$$$      flsj1h = 0
c$$$      if(abs(is).ne.1) return
c$$$c
c$$$      isx  = (is + 1)/2
c$$$      if(jj.gt.ll) then
c$$$      flsj1h = 1.d0/sqrt(2.d0)
c$$$      else 
c$$$      flsj1h = dble((-1)**isx)/sqrt(2.d0)
c$$$      end if
c$$$c
c$$$      return
c$$$      end
c$$$c
c$$$c  sqrt((2l + 1)/(2j + 1)) (l 0 3/2 s | j s)
c$$$c
c$$$c  ll = l, jj = j+1/2, is = 2*s
c$$$c
c$$$      real*8 function flsj3h(jj,ll,is)
c$$$      implicit real*8(a-h,o-z)
c$$$
c$$$      j2     = 2*jj - 1
c$$$      l2     = 2*ll
c$$$      flsj3h = 0
c$$$
c$$$      if(abs(j2-l2).gt.3.or.j2+l2.lt.3) return
c$$$      if(abs(is).gt.j2) return
c$$$
c$$$      isx  = (is + 1)/2
c$$$      if(jj.eq.ll+2) then
c$$$         if(isx.eq.2) then
c$$$            flsj3h = sqrt(dble(ll+3)/dble(2*ll+3))/2.d0
c$$$         else if(isx.eq.1) then
c$$$            flsj3h = sqrt(dble(3*ll+3)/dble(2*ll+3))/2.d0
c$$$         else if(isx.eq.0) then
c$$$            flsj3h = sqrt(dble(3*ll+3)/dble(2*ll+3))/2.d0
c$$$         else if(isx.eq.-1) then
c$$$            flsj3h = sqrt(dble(ll+3)/dble(2*ll+3))/2.d0
c$$$         end if
c$$$      else if(jj.eq.ll+1) then
c$$$         if(isx.eq.2) then
c$$$            flsj3h = -sqrt(dble(3*ll+6)/dble(2*ll+3))/2.d0
c$$$         else if(isx.eq.1) then
c$$$            flsj3h = -sqrt(dble(ll)/dble(2*ll+3))/2.d0
c$$$         else if(isx.eq.0) then
c$$$            flsj3h = sqrt(dble(ll)/dble(2*ll+3))/2.d0
c$$$         else if(isx.eq.-1) then
c$$$            flsj3h = sqrt(dble(3*ll+6)/dble(2*ll+3))/2.d0
c$$$         end if
c$$$      else if(jj.eq.ll) then
c$$$         if(isx.eq.2) then
c$$$            flsj3h = sqrt(dble(3*ll-3)/dble(2*ll-1))/2.d0
c$$$         else if(isx.eq.1) then
c$$$            flsj3h = -sqrt(dble(ll+1)/dble(2*ll-1))/2.d0
c$$$         else if(isx.eq.0) then
c$$$            flsj3h = -sqrt(dble(ll+1)/dble(2*ll-1))/2.d0
c$$$         else if(isx.eq.-1) then
c$$$            flsj3h = sqrt(dble(3*ll-3)/dble(2*ll-1))/2.d0
c$$$         end if
c$$$      else if(jj.eq.ll-1) then
c$$$         if(isx.eq.2) then
c$$$            flsj3h = -sqrt(dble(ll-2)/dble(2*ll-1))/2.d0
c$$$         else if(isx.eq.1) then
c$$$            flsj3h =  sqrt(dble(3*ll)/dble(2*ll-1))/2.d0
c$$$         else if(isx.eq.0) then
c$$$            flsj3h = -sqrt(dble(3*ll)/dble(2*ll-1))/2.d0
c$$$         else if(isx.eq.-1) then
c$$$            flsj3h = sqrt(dble(ll-2)/dble(2*ll-1))/2.d0
c$$$         end if
c$$$      end if
c$$$      return
c$$$      end
c$$$c-------------------- initialize necessary parameters---------
c$$$      subroutine setsato
c$$$      implicit real*8(a-h,o-y)
c$$$      implicit complex*16(z)
c$$$      common / const / pi, fm, scale
c$$$      common / cdfi  / meshx,mxx,mxj,mxm          
c$$$      common / cmxj / jjmx                        ! used in elemag
c$$$      common / cpidx / index(3,-3:3)
c$$$c------------------------------------------------------------
c$$$c
c$$$      index(1,-1) = 1
c$$$      index(1, 1) = 2
c$$$      index(3,-3) = 3
c$$$      index(3,-1) = 4
c$$$      index(3, 1) = 5
c$$$      index(3, 3) = 6
c$$$
c$$$      meshx   = 30   ! angular projection mesh 20 x 3
c$$$      mxm     = 5    ! 2 x m_max  for d-function
c$$$c      mxj     = 11
c$$$      mxj     = 9
c$$$      jjmx    = (mxj + 1)/2
c$$$      pi      = acos(-1.d0)
c$$$      fm      = 197.32858d0
c$$$
c$$$      iscale  = 1  ! Harry's code is in MeV
c$$$
c$$$      if(iscale.eq.1) then
c$$$      scale   = 1                ! calculation with MeV unit
c$$$      else if(iscale.eq.2) then
c$$$      scale   = fm               ! calculation with fm unit
c$$$      end if
c$$$
c$$$      scalefm = scale/fm
c$$$
c$$$      call setcpl         !set mass,coupling constant,vertex function
c$$$      call bifc           ! setup N!, etc.
c$$$      call setdfun        ! d-function
c$$$      call setconst
c$$$      return
c$$$      end
c$$$c
c$$$      subroutine setconst
c$$$      implicit real*8(a-h,o-z)
c$$$      common / cefflo / cg1h(-2:2,-1:1,3)
c$$$     &     ,xef(20,6,-2:2,-3:3,10)
c$$$      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
c$$$     &     ,jss(20,6,10),jll(20,6,10)
c$$$     &     ,jip(20,6,10)
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      common / cpidx / index(3,-3:3)
c$$$      dimension jsb(5),jsm(5),ipm(5)
c$$$      data jsb/1,1,3,1,1/
c$$$      data jsm/0,0,0,0,2/
c$$$      data ipm/-1,-1,-1,1,-1/
c$$$c
c$$$c   cg1h = (1,m1/2,1/2,mh/2;jj/2,m1/2+mh/2)
c$$$c
c$$$      x3   = 1.d0/sqrt(3.d0)
c$$$      x2   = 1.d0/sqrt(2.d0)
c$$$      x23  = sqrt(2.d0/3.d0)
c$$$      cg1h( 0, 1,1) = -x3
c$$$      cg1h( 2,-1,1) =  x23
c$$$      cg1h( 0,-1,1) =  x3
c$$$      cg1h(-2, 1,1) = -x23
c$$$
c$$$      cg1h( 0, 1,3) = x23
c$$$      cg1h( 2,-1,3) = x3
c$$$      cg1h( 0,-1,3) = x23
c$$$      cg1h(-2, 1,3) = x3
c$$$      cg1h( 2, 1,3) = 1
c$$$      cg1h(-2,-1,3) = 1
c$$$c
c$$$c   table of lsj <-> m scheme
c$$$c
c$$$c   jj = 2j,  ip= parity
c$$$c
c$$$c   xef(jj,idx,mm,mb,ix)= sqrt(2l+1/2j+1)*(sm,sb|s)(l,s|j)
c$$$c   ss (jj,idx,ix)= 2s
c$$$c   ll (jj,idx,ix)= 2l
c$$$c   ip (jj,idx,ix)= parity
c$$$c
c$$$
c$$$      xef = 0
c$$$      jss = 0
c$$$      jll = 0
c$$$      jip = 0
c$$$
c$$$      do 110 ix = 1,5
c$$$
c$$$      ipmx = ipm(ix)
c$$$      jsbx = jsb(ix)
c$$$      jsmx = jsm(ix)
c$$$      do 100 jx = 1,mxj,2
c$$$      do 200 ijs= abs(jsbx-jsmx),jsbx+jsmx,2
c$$$      do 210 ijl= abs(jx-ijs),jx+ijs,2
c$$$      ipp   = ipmx*(-1)**(ijl/2)
c$$$      lll   = ijl - jx
c$$$      idx   = index(ijs,lll)
c$$$      do 220 mbz = -jsbx,jsbx,2
c$$$      do 220 mmz = -jsmx,jsmx,2
c$$$        mtotz = mbz+mmz
c$$$      if(abs(mtotz).le.ijs) then
c$$$      if(jsmx.eq.0) then
c$$$         xspin = 1
c$$$      else if(jsmx.eq.2) then
c$$$         xspin = cg1h(mmz,mbz,ijs)
c$$$      end if
c$$$      xef(jx,idx,mmz,mbz,ix)= xspin*flsjxh(ijl,ijs,mtotz,jx)
c$$$      jss(jx,idx,ix)= ijs
c$$$      jll(jx,idx,ix)= ijl
c$$$      jip(jx,idx,ix)= ipp
c$$$      end if
c$$$
c$$$ 220  continue
c$$$ 210  continue
c$$$ 200  continue
c$$$ 100  continue
c$$$ 110  continue
c$$$c
c$$$c
c$$$c
c$$$      ih1(0,0) = 1
c$$$      ih2(0,0) = 0
c$$$      ih1(0,1) = 1
c$$$      ih2(0,1) = 1
c$$$      ih1(1,0) = 0
c$$$      ih2(1,0) = 0
c$$$      ih1(1,1) = 0
c$$$      ih2(1,1) = 1
c$$$c
c$$$c  ignore this number!!!!!!!!!!!!!!!!!
c$$$c
c$$$      icpot(1,1)=1
c$$$      icpot(1,2)=2
c$$$      icpot(2,2)=3
c$$$      icpot(1,4)=4
c$$$      icpot(2,4)=5
c$$$      icpot(4,4)=6
c$$$      icpot(1,5)=7
c$$$      icpot(2,5)=8
c$$$      icpot(4,5)=9
c$$$      icpot(5,5)=10
c$$$      icpot(1,3)=11
c$$$      icpot(2,3)=12
c$$$      icpot(4,3)=13
c$$$      icpot(5,3)=14
c$$$      icpot(3,3)=15
c$$$c
c$$$c
c$$$c
c$$$      return
c$$$      end
c$$$ccccc----------------------------------------------------------------
c$$$ccccc
c$$$cccc      subroutine setcpl
c$$$cccc      implicit real*8(a-h,o-z)
c$$$cccc      common / const / pi, fm, scale
c$$$cccc      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c$$$cccc      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
c$$$cccc     &                            xkrho,xkomg,genn,grnp,gsinn
c$$$cccc      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
c$$$cccc     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
c$$$cccc      common / c2004 / igfrm,idfrm,idafrm1,idafrm2,xdafac,famasn,famasd
c$$$cccc      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
c$$$cccc      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
c$$$cccc     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
c$$$cccc      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
c$$$cccc     &                  vsisi,vopr,vnna1,va1pr,vddrh,
c$$$cccc     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
c$$$cccc     &                  msisi,mopr,mnna1,ma1pr,mddrh
c$$$cccc      common / fisos  / fiso(3,20,20),mxpot(20)
c$$$cccc      
c$$$ccccc
c$$$cccc      scaleg   = scale/1000.d0
c$$$ccccc
c$$$ccccc  0-  octet
c$$$ccccc
c$$$cccc      fpic    = 1.3957018D-01/scaleg
c$$$cccc      fpi0    = 1.349766D-01/scaleg
c$$$cccc      fkaonp  = 4.93677D-01/scaleg
c$$$cccc      fkaon0  = 4.97648D-01/scaleg
c$$$cccc      feta    = 5.4775D-01/scaleg
c$$$cccc      fetap   = 9.5778D-01/scaleg
c$$$cccc
c$$$ccccc
c$$$ccccc use Harry's number !!!!!!!!!!!!!!!!!!!!!!!!
c$$$ccccc
c$$$ccccc       api=138.5
c$$$ccccc       aeta=547.45
c$$$cccc      fpio    = 1.385d-01/scaleg            ! SL model
c$$$cccc      feta    = 5.4745d-01/scaleg            ! sl model
c$$$cccc
c$$$ccccc
c$$$ccccc   1- Octet
c$$$ccccc
c$$$cccc      frho    = 7.758D-01/scaleg
c$$$cccc      fomeg   = 7.8259D-01/scaleg
c$$$cccc      fphi    = 1.019456D+00/scaleg
c$$$cccc      fkaonsp = 8.9166D-01/scaleg
c$$$cccc      fkaons0 = 8.9610D-01/scaleg
c$$$cccc
c$$$cccc      fmrho      = 7.69d-01/scaleg          ! sl model
c$$$cccc      frhoe     = 7.65d-01/scaleg          ! sl model
c$$$cccc      fmomg      = 7.826d-01/scaleg         ! sl model
c$$$ccccc       arho=811.7
c$$$ccccc      write(*,*)'!!!!!! mod rho'
c$$$ccccc      fmrho    = 0.8117d0/scaleg
c$$$ccccc
c$$$ccccc  0+  octet
c$$$ccccc
c$$$cccc      fsigm     = 850.d0 /scale
c$$$ccccc       asigma=896.8
c$$$cccc      fsigm     = 896.8d0/scale
c$$$cccc
c$$$cccc      fsigme    = 650.d0 /scale
c$$$cccc      fma0      = 982.7d0/scale
c$$$cccc      fmf0      = 974.1d0/scale
c$$$cccc      fma1      = 1260.d0/scale
c$$$ccccc
c$$$ccccc   1/2+ Octet
c$$$ccccc      
c$$$cccc      fproton = 9.3827203D-01/scaleg
c$$$cccc      fneutro = 9.3956536D-01/scaleg
c$$$cccc      flambda = 1.115683D+00 /scaleg
c$$$cccc      fsigmap = 1.18937D+00/scaleg
c$$$cccc      fsigma0 = 1.192642D+00/scaleg
c$$$cccc      fsigmam = 1.197449D+00/scaleg
c$$$cccc      fxi0    = 1.31483D+00/scaleg
c$$$cccc      fxim    = 1.32131D+00/scaleg
c$$$cccc
c$$$ccccc       amn=938.5
c$$$cccc      fnuc    = 9.385d-01/scaleg             ! SL model
c$$$ccccc
c$$$ccccc  decouplet
c$$$ccccc
c$$$cccc      fbdelta   = 1.2320D+00 /scaleg
c$$$cccc      fbsigmasp = 1.3828D+00/scaleg
c$$$cccc      fbsigmas0 = 1.3837D+00/scaleg
c$$$cccc      fbsigmasm = 1.3872D+00/scaleg
c$$$cccc      fblambda  = 1.407D+00/scaleg
c$$$cccc      fbsigmap  = 1.18937D+00/scaleg
c$$$cccc      fbsigma0  = 1.192642D+00/scaleg
c$$$cccc      fbxis0    = 1.53180D+00/scaleg
c$$$cccc      fbxism    = 1.32131D+00/scaleg
c$$$cccc      fbomega   = 1.67245D+00/scaleg
c$$$cccc
c$$$cccc      fdel       = 1.236d0/scaleg            !  sl model
c$$$ccccc       amdel=1232.
c$$$cccc      fdel       = 1.232d0/scaleg
c$$$cccc
c$$$cccc      fdelgm     = 1.2380403d0/scaleg        !  sl model
c$$$ccccc-------------------------------------------------------------
c$$$ccccc
c$$$cccc      scalec   = scale/fm
c$$$ccccc
c$$$ccccc  coupling constant
c$$$ccccc
c$$$cccc       gpin    = sqrt(0.08d0*4.d0*pi)       ! piNN
c$$$cccc       xpid    = 1.204d0
c$$$cccc       gpindx  = sqrt(72.d0/25.d0)*gpin
c$$$cccc       gpind   = gpindx*xpid                ! piN-Delta
c$$$cccc       genn    = 1.77d0                     ! eta-NN
c$$$cccc       grnn    = 6.1994d0                   ! rho-NN
c$$$cccc       xkrho   = 1.825d0                    ! Kappa-rho-NN
c$$$cccc       gonn    = 11.5d0                     ! omega-NN
c$$$cccc       xkomg   = 0.0d0                      ! Kappa-omega-NN
c$$$cccc
c$$$cccc
c$$$cccc       gsinn   = 12.8d0                     ! sigma-NN       
c$$$cccc       grnp    = 31.79995d0                 ! rho-NN x rho-pipi
c$$$cccc       grnp    = 38.4329d0
c$$$cccc       grpp    = grnp/grnn                  ! rho-pipi
c$$$cccc       gpidd   = 1.78d0                     ! pi-DD
c$$$cccc       grnd    = 16.03d0                    ! rho-ND
c$$$cccc       ga0nn   = 1
c$$$cccc       ga0pe   = 10.02d0
c$$$cccc       ga1nn   = gpins
c$$$cccc       ga1pr   = grnn
c$$$cccc       gf0nn   = 1
c$$$cccc       gf0ee   = 1
c$$$cccc       gsipp   = 1.77d0
c$$$cccc       gsisi   = 2.80d0
c$$$cccc       gopr    = 11.2d0
c$$$cccc       grdd    = 7.68
c$$$cccc       xkrdd   = 6.1
c$$$ccccc
c$$$ccccc   vertex function   [V^2/(V^2 + q^2)]^m
c$$$ccccc
c$$$ccccc  numbers are in fm-1
c$$$ccccc
c$$$cccc       vnnpi   = 3.2551d0/scalec   !pi-NN
c$$$cccc       vndpi   = 3.29d0/scalec     !pi-ND
c$$$cccc       vnnrho  = 6.2305d0/scalec   !rho-NN
c$$$cccc       vrpp    = 6.2305d0/scalec   !rho-pipi
c$$$cccc       vnnomg  = 6.230d0/scalec    !omega-NN
c$$$cccc       vnnet   = 2500.d0/scale     !eta-NN
c$$$cccc       vnnsi   = 1500.d0/scale     !sigma-NN
c$$$cccc       vndrh   = 1300.d0/scale     !rho-ND
c$$$cccc       vddpi   = 1800.d0/scale     !pi-DD
c$$$cccc       vnna0   = 2500.d0/scale     !a0-NN
c$$$cccc       va0pe   = 2500.d0/scale     !a0-pi eta
c$$$cccc       vnnf0   = 2000d0/scale      !f0-NN
c$$$cccc       vf0ee   = 2000d0/scale      !f0-eta eta
c$$$cccc       vsipp   = 600.d0/scale      !sigma-pi pi
c$$$cccc       vsisi   = 2300.d0/scale
c$$$cccc       vopr    =  700.d0/scale
c$$$cccc       vnna1   = 1500.d0/scale
c$$$cccc       va1pr   = 1500.d0/scale
c$$$cccc       vddrh   = 1300.d0/scale
c$$$cccc       mnnpi   = 2
c$$$cccc       mndpi   = 2
c$$$cccc       mnnrho  = 2
c$$$cccc       mrpp    = 2
c$$$cccc       mnnomg  = 2
c$$$cccc       mnnet   = 2
c$$$cccc       mnnsi   = 2
c$$$cccc       mndrh   = 2
c$$$cccc       mddpi   = 2
c$$$cccc       mnna0   = 2
c$$$cccc       ma0pe   = 2
c$$$cccc       mnnf0   = 2
c$$$cccc       mf0ee   = 2
c$$$cccc       msipp   = 2
c$$$cccc       msisi   = 2
c$$$cccc       mopr    = 2
c$$$cccc       mnna1   = 2
c$$$cccc       ma1pr   = 2
c$$$cccc       mddrh   = 2
c$$$ccccc  electromagnetic coupling (we do not need for meson-baryon potential
c$$$cccc       grpg    = 0.1027d0           !??? dimension
c$$$cccc       gopg    = 0.3247d0           !??? dimension
c$$$ccccc       gde2    = 0
c$$$ccccc       gdc2    = 0
c$$$ccccc       gdm1    = 0
c$$$ccccc
c$$$ccccc  vector current
c$$$ccccc
c$$$cccc       igfrm   = 1
c$$$cccc       idfrm   = 2
c$$$ccccc
c$$$ccccc  axial vector current
c$$$ccccc
c$$$cccc       idafrm1  = 1
c$$$cccc       idafrm2  = 1
c$$$cccc       xdafac   = 1
c$$$cccc       famasn   = 1020.d0  ! MeV
c$$$cccc       famasd   = 1020.d0  ! MeV
c$$$ccccc
c$$$cccc       gdm1    = 1.85d0
c$$$cccc       gde2    = 0.025d0
c$$$cccc       gdc2    = -0.238d0
c$$$ccccc
c$$$ccccc  isospin factor of optential
c$$$ccccc
c$$$ccccc   pi N -> pi N
c$$$cccc      fiso(1,1,1) =    3  ! nd
c$$$cccc      fiso(3,1,1) =    0  
c$$$cccc      fiso(1,1,2) =  - 1  ! ne
c$$$cccc      fiso(3,1,2) =    2
c$$$cccc
c$$$cccc
c$$$cccc      fiso(1,1,3) =    4.d0/3.d0  ! delta exchange
c$$$cccc      fiso(3,1,3) =    1.d0/3.d0
c$$$cccc      fiso(1,1,4) =   -2           !rho
c$$$cccc      fiso(3,1,4) =    1
c$$$cccc
c$$$cccc      fiso(1,1,5) =    1  ! sigma
c$$$cccc      fiso(3,1,5) =    1
c$$$cccc
c$$$cccc      fiso(1,1,6) =    0  ! anti-delta S-chan
c$$$cccc      fiso(3,1,6) =    1
c$$$cccc
c$$$cccc      mxpot(1)    =    6
c$$$ccccc   pi N -> eta N
c$$$cccc      fiso(1,2,1) =  - sqrt(3.d0)
c$$$cccc      fiso(3,2,1) =    0
c$$$cccc      fiso(1,2,2) =  - sqrt(3.d0)
c$$$cccc      fiso(3,2,2) =    0
c$$$cccc      fiso(1,2,3) =  - sqrt(3.d0)
c$$$cccc      fiso(3,2,3) =    0
c$$$cccc      mxpot(2)    =    3
c$$$ccccc  eta N -> eta N
c$$$cccc      fiso(1,3,1) =    1
c$$$cccc      fiso(3,3,1) =    0
c$$$cccc      fiso(1,3,2) =    1
c$$$cccc      fiso(3,3,2) =    0
c$$$cccc      fiso(1,3,3) =    1
c$$$cccc      fiso(3,3,3) =    0
c$$$cccc      mxpot(3)    =    3
c$$$ccccc   pi N -> sigma N
c$$$cccc      fiso(1,4,1) =  - sqrt(3.d0)
c$$$cccc      fiso(3,4,1) =    0
c$$$cccc      fiso(1,4,2) =  - sqrt(3.d0)
c$$$cccc      fiso(3,4,2) =    0
c$$$cccc      fiso(1,4,3) =  - sqrt(3.d0)
c$$$cccc      fiso(3,4,3) =    0
c$$$cccc      mxpot(4)    =    3
c$$$ccccc  eta N -> sigma N
c$$$cccc      fiso(1,5,1) =    1
c$$$cccc      fiso(3,5,1) =    0
c$$$cccc      fiso(1,5,2) =    1
c$$$cccc      fiso(3,5,2) =    0
c$$$cccc      mxpot(5)    =    2
c$$$ccccc  sigma N -> sigma N
c$$$cccc      fiso(1,6,1) =    1
c$$$cccc      fiso(3,6,1) =    0
c$$$cccc      fiso(1,6,2) =    1
c$$$cccc      fiso(3,6,2) =    0
c$$$cccc      fiso(1,6,3) =    1
c$$$cccc      fiso(3,6,3) =    0
c$$$cccc      mxpot(6)    =    3
c$$$ccccc  pi N -> rho N
c$$$cccc      fiso(1,7,1) =    3
c$$$cccc      fiso(3,7,1) =    0
c$$$cccc      fiso(1,7,2) =   -1
c$$$cccc      fiso(3,7,2) =    2
c$$$cccc      fiso(1,7,3) =   -2
c$$$cccc      fiso(3,7,3) =    1
c$$$cccc      fiso(1,7,4) =   -2
c$$$cccc      fiso(3,7,4) =    1
c$$$cccc      fiso(1,7,5) =   -2
c$$$cccc      fiso(3,7,5) =    1
c$$$cccc      fiso(1,7,6) =    1
c$$$cccc      fiso(3,7,6) =    1
c$$$cccc      mxpot(7)    =    6
c$$$ccccc  eta N -> rho N
c$$$cccc      fiso(1,8,1) =  - sqrt(3.d0)
c$$$cccc      fiso(3,8,1) =    0
c$$$cccc      fiso(1,8,2) =  - sqrt(3.d0)
c$$$cccc      fiso(3,8,2) =    0
c$$$cccc      mxpot(8)    =    2
c$$$ccccc  sigma N -> rho N
c$$$cccc      fiso(1,9,1) =  - sqrt(3.d0)
c$$$cccc      fiso(3,9,1) =    0
c$$$cccc      fiso(1,9,2) =  - sqrt(3.d0)
c$$$cccc      fiso(3,9,2) =    0
c$$$cccc      mxpot(9)    =    2
c$$$ccccc  rho N -> rho N
c$$$cccc      fiso(1,10,1) =    3
c$$$cccc      fiso(3,10,1) =    0
c$$$cccc      fiso(1,10,2) =   -1
c$$$cccc      fiso(3,10,2) =    2
c$$$cccc      fiso(1,10,3) =   -2
c$$$cccc      fiso(3,10,3) =    1
c$$$cccc      fiso(1,10,4) =   -2
c$$$cccc      fiso(3,10,4) =    1
c$$$cccc      mxpot(10)    =   4      
c$$$ccccc   pi N -> pi D (idx 11)
c$$$cccc      fiso(1,11,1) =   sqrt(6.d0)
c$$$cccc      fiso(3,11,1) =   0
c$$$cccc      fiso(1,11,2) =   sqrt(8.d0/3.d0)
c$$$cccc      fiso(3,11,2) = - sqrt(5.d0/3.d0)
c$$$cccc      fiso(1,11,3) = - sqrt(2.d0/3.d0)
c$$$cccc      fiso(3,11,3) = - sqrt(5.d0/3.d0)
c$$$cccc      fiso(1,11,4) =   0
c$$$cccc      fiso(3,11,4) = - sqrt(15.d0/4.d0)
c$$$cccc      fiso(1,11,5) =   sqrt(25.d0/6.d0)
c$$$cccc      fiso(3,11,5) =   sqrt(5.d0/3.d0)
c$$$cccc      mxpot(11)    =   5
c$$$ccccc  eta N -> pi D
c$$$cccc      fiso(1,12,1) = - sqrt(2.d0)
c$$$cccc      fiso(3,12,1) =   0
c$$$cccc      mxpot(12)    =   1
c$$$ccccc  sigma N -> pi D
c$$$cccc      fiso(1,13,1) = - sqrt(2.d0)
c$$$cccc      fiso(3,13,1) =   0
c$$$cccc      mxpot(13)    =   1
c$$$ccccc  rho N -> pi D
c$$$cccc      fiso(1,14,1) =   sqrt(6.d0)
c$$$cccc      fiso(3,14,1) =   0
c$$$cccc      fiso(1,14,2) =   sqrt(8.d0/3.d0)
c$$$cccc      fiso(3,14,2) =  -sqrt(5.d0/3.d0)
c$$$cccc      fiso(1,14,3) =   0
c$$$cccc      fiso(3,14,3) =   -sqrt(15.d0)/2.d0
c$$$cccc      fiso(1,14,4) =   sqrt(25.d0/6.d0)
c$$$cccc      fiso(3,14,4) =   sqrt(5.d0/3.d0)
c$$$cccc      mxpot(14)    =   4
c$$$ccccc  pi D  -> pi D
c$$$cccc      fiso(1,15,1) =   2
c$$$cccc      fiso(3,15,1) =   0
c$$$cccc      fiso(1,15,2) =   0
c$$$cccc      fiso(3,15,2) =   15.d0/4.d0
c$$$cccc      fiso(1,15,3) =   -5.d0/2.d0
c$$$cccc      fiso(3,15,3) =    11.d0/4.d0
c$$$cccc      fiso(1,15,4) =   -5.d0/2.d0
c$$$cccc      fiso(3,15,4) =   -1.d0
c$$$cccc      mxpot(15)    =   4
c$$$cccc
c$$$cccc      return
c$$$cccc      end
c$$$c
c$$$c      half integer d^j_{m',m)
c$$$c
c$$$c  dfun(2j,2m,2n) = d^{j}_{m,n}
c$$$c
c$$$
c$$$      subroutine setdfun
c$$$      implicit real*8(a-h,o-z)
c$$$      parameter(maxl=20)
c$$$      common / cdff / xgau(100),wgau(100),dfun(20,-5:5,-5:5,100)
c$$$     & ,fleg(0:10,100)
c$$$      common / cdfi / meshx,mxx,mxj,mxm
c$$$      dimension fle(0:maxl),fled(-1:maxl),fledd(-1:maxl)
c$$$      dimension xg(3),wg(3)
c$$$      data xg/ -0.7745966692D0,0.d0,0.7745966692D0/
c$$$      data wg/ 0.5555555555d0,0.8888888888d0,0.5555555555d0/
c$$$      data ngaus/3/
c$$$
c$$$      do 1 lx = 1,20
c$$$      do 1 m1 = -5,5
c$$$      do 1 m2 = -5,5
c$$$      do 1 m3 = 1,100
c$$$ 1    dfun(lx,m1,m2,m3) = 0
c$$$      do 2 lx = 0,10
c$$$      do 2 m1 = 1,100
c$$$ 2    fleg(lx,m1) = 0
c$$$c
c$$$      mxx       = meshx * ngaus
c$$$      dgux      = 2.d0/dble(2*meshx)
c$$$      idx       = 1
c$$$      do 110 nx = 1,meshx
c$$$      do 110 ng = 1,ngaus
c$$$      xgau(idx)   = dgux*(xg(ng)+dble(2*nx-1))-1.d0
c$$$      wgau(idx)   = wg(ng)*dgux
c$$$      x         = xgau(idx)
c$$$
c$$$      call legen(x,fle)
c$$$      do 111 lx = 0,10
c$$$ 111  fleg(lx,idx) = fle(lx)
c$$$
c$$$      ss        = sqrt((1.d0 - x)/2.d0)
c$$$      cc        = sqrt((1.d0 + x)/2.d0)
c$$$c
c$$$      do 120 lx = 1,mxj,2
c$$$      maxm      = min(lx,mxm)
c$$$      do 130 mf = -maxm,maxm,2
c$$$      do 140 mi = -maxm,maxm,2
c$$$      jj        = (lx - 1)/2
c$$$      mfm       = (mf - 1)/2
c$$$      mfp       = (mf + 1)/2
c$$$      mim       = (mi - 1)/2
c$$$      mip       = (mi + 1)/2
c$$$      df1       = 0
c$$$      df2       = 0
c$$$      df3       = 0
c$$$      df4       = 0
c$$$      if(jj.ge.abs(mfm).and.jj.ge.abs(mim))then
c$$$      fac       = sqrt(dble((lx + mf)*(lx + mi)))/dble(2*lx)
c$$$      df1       = fblmmx(jj,mfm,mim,cc,ss)*fac*cc
c$$$      end if
c$$$
c$$$      if(jj.ge.abs(mfp).and.jj.ge.abs(mip))then
c$$$      fac       = sqrt(dble((lx - mf)*(lx - mi)))/dble(2*lx)
c$$$      df2       = fblmmx(jj,mfp,mip,cc,ss)*fac*cc
c$$$      end if
c$$$
c$$$      if(jj.ge.abs(mfm).and.jj.ge.abs(mip))then
c$$$      fac       = sqrt(dble((lx + mf)*(lx - mi)))/dble(2*lx)
c$$$      df3       =-fblmmx(jj,mfm,mip,cc,ss)*fac*ss
c$$$      end if
c$$$
c$$$      if(jj.ge.abs(mfp).and.jj.ge.abs(mim))then
c$$$      fac       = sqrt(dble((lx - mf)*(lx + mi)))/dble(2*lx)
c$$$      df4       = fblmmx(jj,mfp,mim,cc,ss)*fac*ss
c$$$      end if
c$$$
c$$$      dfun(lx,mf,mi,idx) = df1 + df2 + df3 + df4
c$$$
c$$$c      write(*,*)'setd',lx,mf,mi,idx,df1+df2+df3+df4
c$$$c      read(*,*)ixxx
c$$$
c$$$ 140  continue
c$$$ 130  continue
c$$$ 120  continue
c$$$
c$$$      idx       = idx + 1
c$$$ 110  continue
c$$$c
c$$$      return
c$$$      end
c$$$c
c$$$c
c$$$c      <l,mf|exp(-iJ_y*theta)|l,mi>
c$$$c
c$$$      real*8 function fblmmx(l,mf,mi,cc,ss)
c$$$      implicit real*8(a-h,o-z)
c$$$      parameter (n=100,m=50)
c$$$      common / fdbn / h(0:n),dh(-1:m),bb(0:n,0:n)
c$$$c
c$$$      fblmmx  = 0
c$$$      if(l.lt.0.or.abs(mf).gt.l.or.abs(mi).gt.l)return
c$$$c
c$$$c      t2     = theta/2.d0
c$$$c      cc     = cos(t2)
c$$$c      ss     = sin(t2)
c$$$      jmip   = l  + mi
c$$$      jmim   = l  - mi
c$$$      jmfp   = l  + mf
c$$$      jmfm   = l  - mf
c$$$      mfmim  = mf - mi
c$$$      iicos  = 2*l - mfmim
c$$$      iisin  = mfmim
c$$$      kmax   = min(jmip,jmfm)
c$$$      kmin   = max(0,-mfmim)
c$$$      if(kmax.lt.kmin)return
c$$$      sum    = 0
c$$$      do 100 kx = kmin,kmax
c$$$      phase     = (-1)**(mfmim+kx)
c$$$      factor    = h(jmip)*h(jmim)*h(jmfp)*h(jmfm)
c$$$     &           /(h(jmip-kx)*h(kx)*h(jmfm-kx)*h(kx+mfmim))**2
c$$$      sum       = phase*factor*cc**(iicos-2*kx)*ss**(iisin+2*kx)
c$$$     &           + sum
c$$$ 100  continue
c$$$      fblmmx     = sum
c$$$      return
c$$$      end
c$$$c
c$$$c
c$$$c
c$$$c   P_L(z), d P_L/dx, d^2 P_L/d^2 x
c$$$c    fle     fled       fledd
c$$$c
c$$$      subroutine legend(z,fle,fled,fledd)
c$$$      implicit real*8(a-h,o-z)
c$$$      parameter(maxl=20)
c$$$      dimension fle(0:maxl),fled(-1:maxl),fledd(-1:maxl)
c$$$c
c$$$      fle(0)   = 1
c$$$      fle(1)   = z
c$$$c
c$$$      fled(-1) = 0
c$$$      fled(0)  = 0
c$$$      fled(1)  = 1
c$$$c
c$$$      fledd(-1)= 0
c$$$      fledd(0) = 0
c$$$      fledd(1) = 0
c$$$c
c$$$      do 100 i = 2,maxl
c$$$         xi = dble(i)
c$$$         fle(i)  = ( (2.d0*xi-1.d0)*z*fle(i-1)
c$$$     &                     -(xi-1.d0)*fle(i-2) )/xi
c$$$         fled(i) = xi*fle(i-1) + z*fled(i-1)
c$$$         fledd(i)= (xi+1.d0)*fled(i-1) + z*fledd(i-1)
c$$$  100 continue
c$$$c
c$$$      return
c$$$      end
c$$$c
c$$$c
c$$$      subroutine legen(z,fle)
c$$$      implicit real*8(a-h,o-z)
c$$$      parameter(maxl=20)
c$$$      dimension fle(0:maxl)
c$$$c
c$$$      fle(0)   = 1
c$$$      fle(1)   = z
c$$$      do 100 i = 2,maxl
c$$$      fle(i)   = (dble(2*i-1)*z*fle(i-1)-dble(i-1)*fle(i-2))/dble(i)
c$$$  100 continue
c$$$      return
c$$$      end
c$$$      subroutine bifc
c$$$c------------------------------------------------------------
c$$$      implicit real*8(a-h,o-z)
c$$$      parameter (n=100,m=50)
c$$$      common / fdbn / h(0:n),dh(-1:m),bb(0:n,0:n)
c$$$c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
c$$$      e=1.0d0 
c$$$      h(0)=e 
c$$$      dh(-1)=e 
c$$$      bb(0,0)=e 
c$$$      sd=e 
c$$$      do 10 i=1,n 
c$$$      s=i 
c$$$      sd=sd*sqrt(s) 
c$$$   10 h(i)=sd 
c$$$      sd=e 
c$$$      do 20 i=0,m 
c$$$      s=i+i+1 
c$$$      sd=sd*sqrt(s) 
c$$$   20 dh(i)=sd
c$$$      do 30 i=1,n 
c$$$      bb(i,0)=e 
c$$$      bb(0,i)=e 
c$$$      bb(i,i)=e 
c$$$      s=e
c$$$      do 30 j=1,i/2 
c$$$      l=i-j 
c$$$      s=s*dble(l+1)/dble(j)
c$$$      sd=sqrt(s) 
c$$$      bb(j,i)=sd 
c$$$      bb(l,i)=sd 
c$$$      bb(i,j)=s 
c$$$   30 bb(i,l)=s
c$$$      return 
c$$$      end
c$$$
c$$$
