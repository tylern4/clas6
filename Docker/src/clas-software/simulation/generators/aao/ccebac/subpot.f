c
c  02-13-2007 rhn2rhn  contact term corrected
c  03-25-2007 corect mconv
c  04-22-2007 add omega s,u for 'on'
c
c===========================================================================
c
c  gamma N -> MB   Dec 09 2006
c  
c
c
c==========================================================================
c----------------------------------------------------------------
c  this routine should be replaced by Bruno's one
c----------------------------------------------------------------


c 
c This subroutine allows to vary the 
c non resonant parameters only for the 
c electromagnetic part. That is, subjme 
c will use the values we fix here.
c
c The idea is to use the strong interaction ones 
c slightly modified for the electromagnetic study.
c
      subroutine modcpl
      PARAMETER (MAXH  =100)
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

      common / helpar     / hpar(MAXH)
      fsigme    = 650.d0 /scale



ccc this is not very smart!, 
ccc keeps old values saved!

      if (ifirst.ne.7) then 
       ifirst=7
       gpindO   = gpind     
       gennO    = genn      
       grnnO    = grnn     
       xkrhoO   = xkrho    
       gonnO    = gonn  
       xkomgO   = xkomg 

       gsinnO   = gsinn       
       grnpO    = grnp        

       grppO    = grnp/grnn                  ! rho-pipi
       gpiddO   = gpidd       
       grndO    = grnd        


       gsippO   = gsipp       
       gsisiO   = gsisi       
       goprO    = gopr        
       grddO    = grdd        
       xkrddO   = xkrdd       
c
c   vertex function   [V^2/(V^2 + q^2)]^m
c
c  numbers are in fm-1
c
       vnnpiO   = vnnpi 
       vndpiO   = vndpi 
       vnnrhoO  = vnnrho
       vrppO    = vrpp
       vnnomgO  = vnnomg
       vnnetO   = vnnet
       vnnsiO   = vnnsi
       vndrhO   = vndrh
       vddpiO   = vddpi
       vsippO   = vsipp
       vsisiO   = vsisi
       voprO    = vopr
       vddrhO   = vddrh

c  electromagnetic coupling (we do not need for meson-baryon potential

       grpgO    = 0.1027d0 
       gopgO    = 0.3247d0 

       endif




       gpind   = gpindO        * hpar(2)  ! piN-Delta
       genn    = gennO         * hpar(3)  ! eta-NN
       grnn    = grnnO         * hpar(4)  ! rho-NN
       xkrho   = xkrhoO        * hpar(5)  ! Kappa-rho-NN
       gonn    = gonnO         * hpar(6)  ! omega-NN
       

       xkomg   = xkomgO        * hpar(7)  ! Kappa-omega-NN


       gsinn   = gsinnO        * hpar(8)  ! sigma-NN       
       grnp    = grnpO         * hpar(9)  ! rho-NN x rho-pipi

       grpp    = grnp/grnn                  ! rho-pipi
       gpidd   = gpiddO        * hpar(10) ! pi-DD
       grnd    = grndO         * hpar(11) ! rho-ND


       gsipp   = gsippO       * hpar(12) 
       gsisi   = gsisiO       * hpar(13)
       gopr    = goprO        * hpar(14)
       grdd    = grddO        * hpar(15)
       xkrdd   = xkrddO       * hpar(16)
c
c   vertex function   [V^2/(V^2 + q^2)]^m
c
c  numbers are in fm-1
c
       vnnpi   = vnnpiO *hpar(17)!pi-NN
       vndpi   = vndpiO *hpar(18)!pi-ND
       vnnrho  = vnnrhoO*hpar(19)!rho-NN
       vrpp    = vrppO*hpar(20)  !rho-pipi
       vnnomg  = vnnomgO*hpar(21)!omega-NN
       vnnet   = vnnetO*hpar(22) !eta-NN
       vnnsi   = vnnsiO*hpar(23) !sigma-NN
       vndrh   = vndrhO*hpar(24) !rho-ND
       vddpi   = vddpiO*hpar(25) !pi-DD
       vsipp   = vsippO*hpar(26)
       vsisi   = vsisiO*hpar(27)
       vopr    = voprO*hpar(28)
       vddrh   = vddrhO*hpar(29)

c  electromagnetic coupling (we do not need for meson-baryon potential
       grpg    = grpgO *hpar(30)
       gopg    = gopgO *hpar(31)

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

      return
      end







      subroutine setcpla
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
c      fmomg      = fmrho
c       arho=811.7
c      write(*,*)'!!!!!! mod rho'
c      fmrho    = 0.8117d0/scaleg
c
c  0+  octet
c
      fsigm     = 850.d0 /scale
c       asigma=896.8
      fsigm     = 896.8d0/scale

      fsigme    = 650.d0 /scale
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
      fdel       = 1.232d0/scaleg

      fdelgm     = 1.2380403d0/scaleg        !  sl model
c-------------------------------------------------------------
c
      scalec   = scale/fm
c
c  coupling constant
c
       gpin    = sqrt(0.08d0*4.d0*pi)       ! piNN
       xpid    = 1.204d0
       gpindx  = sqrt(72.d0/25.d0)*gpin
       gpind   = gpindx*xpid                ! piN-Delta
       genn    = 1.77d0                     ! eta-NN
       grnn    = 6.1994d0                   ! rho-NN
       xkrho   = 1.825d0                    ! Kappa-rho-NN
       gonn    = 11.5d0                     ! omega-NN
c       gonn  = grnn
       xkomg   = 0.0d0                      ! Kappa-omega-NN
c       xkomg   = xkrho

       gsinn   = 12.8d0                     ! sigma-NN       
       grnp    = 31.79995d0                 ! rho-NN x rho-pipi
       grnp    = 38.4329d0
       grpp    = grnp/grnn                  ! rho-pipi
       gpidd   = 1.78d0                     ! pi-DD
       grnd    = 16.03d0                    ! rho-ND

       ga0nn   = 1
       ga0pe   = 10.02d0
       ga1nn   = gpin
       ga1pr   = grnn
       gf0nn   = 1
       gf0ee   = 1

       gsipp   = 1.77d0
       gsisi   = 2.80d0
       gopr    = 11.2d0
       grdd    = 7.68
       xkrdd   = 6.1
c
c   vertex function   [V^2/(V^2 + q^2)]^m
c
c  numbers are in fm-1
c
       vnnpi   = 3.2551d0/scalec   !pi-NN
       vndpi   = 3.29d0/scalec     !pi-ND
       vnnrho  = 6.2305d0/scalec   !rho-NN
       vrpp    = 6.2305d0/scalec   !rho-pipi
       vnnomg  = 6.230d0/scalec    !omega-NN
       vnnet   = 2500.d0/scale     !eta-NN
       vnnsi   = 1500.d0/scale     !sigma-NN
       vndrh   = 1300.d0/scale     !rho-ND
       vddpi   = 1800.d0/scale     !pi-DD
       vnna0   = 2500.d0/scale     !a0-NN
       va0pe   = 2500.d0/scale     !a0-pi eta
       vnnf0   = 2000d0/scale      !f0-NN
       vf0ee   = 2000d0/scale      !f0-eta eta
       vsipp   = 600.d0/scale      !sigma-pi pi
       vsisi   = 2300.d0/scale
       vopr    =  700.d0/scale
       vnna1   = 1500.d0/scale
       va1pr   = 1500.d0/scale
       vddrh   = 1300.d0/scale
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
       grpg    = 0.1027d0           !??? dimension
       gopg    = 0.3247d0           !??? dimension
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
      fiso(1,1,4) =   -2           !rho
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
c--------------------------------------------------------------------
c  convert into zampj into multipole amplitude ML+/- EL+\- SL+/-
c  for  gamma N -> pi N
c
      subroutine nnmulamp(egam,egam0,pon,wcm,zampj,zmul)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
      parameter(njmx=11,maxl=10)
      common / cdfi  / meshx,mxx,mxj,mxm
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      dimension zmul(8,0:maxl,3),zampj(2*njmx-1,6,6,3) !modified for vector only

      emf  = sqrt(fpio**2 + pon**2)
      ebf  = sqrt(fnuc**2 + pon**2)
      ebi  = sqrt(fnuc**2 + egam**2)
      xfac = 4.d0*pi**2/wcm*sqrt(ebf*emf*abs(egam0)*ebi)/2.d0/pi
      zei = (0.d0,1.d0)

      zmul = 0

      do 200 jjx= 1,2*njmx-1,2
      jx        = (jjx + 1)/2
      xxx       = sqrt(dble(jx-1)/dble(jx+1))

      do 300 id = 1,2
      do 400 is = 1,3

      zfac  = xfac/dble(2*jx)/zei

      if(id.eq.2) then    ! j = l - 1/2

      jl    = (jjx +1)/2

      if(jx.gt.1) then
       zmul(2,jl,is) = zfac*( zampj(jjx,id,2,is)
     &                     +  zampj(jjx,id,1,is)/xxx)
      end if
       zmul(4,jl,is) = zfac*(-zampj(jjx,id,2,is)
     &                     +  zampj(jjx,id,1,is)*xxx)
       zmul(8,jl,is) = sqrt(2.d0)*zfac*zampj(jjx,id,3,is)
       
      else                 ! j = l + 1/2

      jl    = (jjx -1)/2

      if(jx.gt.1) then
       zmul(3,jl,is) = zfac*(-zampj(jjx,id,2,is)
     &                       -zampj(jjx,id,1,is)/xxx)
c       if(is.eq.1.and.jjx.eq.3) then
c       write(*,1000)zmul(3,jl,is),zmul(3,1,1)
c 1000  format(1h ,'p33',10e15.5)
c       end if

      end if
       zmul(1,jl,is) = zfac*(-zampj(jjx,id,2,is)
     &                     +  zampj(jjx,id,1,is)*xxx)
       zmul(7,jl,is) =-sqrt(2.d0)*zfac*zampj(jjx,id,3,is)

      end if

c      write(*,*)jjx,id,is

 400  continue
 300  continue
 200  continue

      return
      end
c
c  gamma + N -> pi  + N   1   pn
c               eta + N   2   en
c               sigma+N   3   sn
c               rho + N   4   rn
c               pi  + D   5   pd
c               omega N   6   on
c       
c
c  zampj(2*j,idx1,idx2,iso)
c
c   idx1 for final meson baryon state
c   idx2 for gamma-N spin state
c                 m_g = 0  epsilon=(1,0,0,0)   time component
c                 m_g = +/- 1 epsilon = -+/sqrt(2)(0,1,+- i,0)
c
c idx1 = 1 s=1/2, l=j-1/2   idx2 = 1 m_g=1   m_n = 1/2
c        2 s=1/2  l=j+1/2          2 m_g=1   m_n =-1/2
c        3 s=3/2  l=j-3/2          3 m_g=0   m_n = 1/2
c        4 s=3/2  l=j-1/2          4 m_g=0   m_n =-1/2
c        5 s=3/2  l=j+1/2          5 m_g=-1  m_n = 1/2
c        6 s=3/2  l=j+3/2          6 m_g=-1  m_n =-1/2
c
c   iso 1 =  3/2
c       2 =  1/2 p
c       3 =  1/2 n
c
c  zamp (mz,bz,lam,jx,iso)
c
c  zampj = 1/(2 pi)^3 1/sqrt(2E_pi 2|q_0|) ....
c
      subroutine subjme(chn,zpf,egam,egam0,zampjm)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
      parameter(njmx=11,imxi=6)
      character chn*2
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / csw   / gsw(20,20)
      common / cmxj / jjmx
      dimension isbi(6),ismi(6)
c      dimension zampj(4,njmx,-1:1,3,4),zmul(6,0:njmx,3)
      dimension zamp (-1:1,-3:3,6,njmx,3),zampjm(2*njmx-1,6,6,3)
      data isbi/1,-1,1,-1,1,-1/
      data ismi/1,1,0,0,-1,-1/

      fmbi     = fnuc
c
      if(chn.eq.'pn') then   !pi-N channel
      jmf    = 0           ! J      of meson
      ipmf   = -1          ! parity of meson
      jbf    = 1           ! 2 x J of baryon      
      ichn   = 1
      fmbf   = fnuc
      fmmf   = fpio
      else if(chn.eq.'en') then ! eta-N channel
      jmf    = 0
      ipmf   = -1
      jbf    = 1
      ichn   = 2
      fmbf   = fnuc
      fmmf   = feta
      else if(chn.eq.'pd') then ! pi-D channel
      jmf    = 0
      ipmf   = -1
      jbf    = 3
      ichn   = 5
      fmbf   = fdel
      fmmf   = fpio
      else if (chn.eq.'sn') then ! sigma-N channel
      jmf    = 0
      ipmf   = 1
      jbf    = 1
      ichn   = 3
      fmbf   = fnuc
      fmmf   = fsigm
      else if(chn.eq.'rn') then ! rho-N channel
      jmf    = 1
      ipmf   = -1
      jbf    = 1
      ichn   = 4
      fmbf   = fnuc
      fmmf   = fmrho
      else if(chn.eq.'on') then ! omega-N channel
      jmf    = 1
      ipmf   = -1
      jbf    = 1
      ichn   = 6
      fmbf   = fnuc
      fmmf   = fmomg
      else
         write(*,*)'not available'
         stop
      end if

c cal helicity amplitude


      if(ichn.eq.5) then
      call dhelamp(egam,egam0,fmbf,fmmf,fmbi,zpf,ichn,zamp
     &             ,isbi,ismi,imxi)
      else

c      write(*,*)'zpf',zpf

      call nhelamp(egam,egam0,fmbf,fmmf,fmbi,jmf,jbf,zpf,ichn,zamp
     &             ,isbi,ismi,imxi)
      end if

c  convert into lsj


      call mconv(zamp,zampjm,jmf,jbf,imxi)

      return

c======================================================================
      end
c
c
c  zampj(2*j,idx1,idx2,iso)
c
c idx1 = 1 s=1/2, l=j-1/2   idx2 = 1 m_g=1   m_n = 1/2
c        2 s=1/2  l=j+1/2          2 m_g=1   m_n =-1/2
c        3 s=3/2  l=j-3/2          3 m_g=0   m_n = 1/2
c        4 s=3/2  l=j-1/2          4 m_g=0   m_n =-1/2
c        5 s=3/2  l=j+1/2          5 m_g=-1  m_n = 1/2
c        6 s=3/2  l=j+3/2          6 m_g=-1  m_n =-1/2
c
c  zamp (mz,bz,lam,jx,iso)
c
      subroutine mconv(zamp,zampj,jmf,jbf,imxi)
      implicit real*8 (a-h,o-y)
      implicit complex*16(z)
c   njmx = j+1/2 njmx2 = 2j  njmx=21, njmx=11
      parameter(njmx=11)
      common / cpidx / index(3,-3:3)
      common / cmxj / jjmx
      dimension zamp(-1:1,-3:3,6,njmx,3),zampj(2*njmx-1,6,6,3)
c

      zampj = 0



      jsmax =     2*jmf + jbf
      jsmin = abs(2*jmf - jbf)

      do 110 jx  = 1,jjmx         ! jx = j + 1/2       
      j2         = 2*jx -1        ! j2 = 2 * j
      do 210 jsx = jsmin,jsmax,2  ! jsx = 2 * s
      do 220 jlx = -jsx,jsx,2
      jl         = (j2 + jlx)/2   
      if(jl.lt.0) go to 220

      idx        = index(jsx,jlx)

      do 211 jmz = -jmf,jmf
      do 211 jbz = -jbf,jbf,2

      if(jmf.eq.1.and.jbf.eq.1) then
            jmzz  = jmz*2
            fspin = fss1h(jmzz,jbz,jsx)
      else if(jmf.eq.0) then
            fspin = 1
      else
            fspin = 0
      end if

      if(jsx.eq.1) then
c            fac = flsj1h(jx,jl,jbz)*fspin  corrected 03-25-2007
            fac = flsj1h(jx,jl,jbz+2*jmz)*fspin
      else if(jsx.eq.3) then
c            fac = flsj3h(jx,jl,jbz)*fspin   corrected 03-25-2007
            fac = flsj3h(jx,jl,jbz+2*jmz)*fspin
      else
            fac = 0
      end if

      do 300 iso = 1,3
      do 300 lam = 1,imxi
c         write(3,1)j2,jsx,jl,iso,lam,fac,zamp(jmz,jbz,lam,jx,iso)
c 1       format(1h ,'fac',5i3,3e15.5)
       zampj(j2,idx,lam,iso)=
     & zampj(j2,idx,lam,iso) + fac*zamp(jmz,jbz,lam,jx,iso)
 300  continue
 211  continue

 221  continue
 220  continue
 210  continue
 110  continue

      return
      end
c
      subroutine nhelamp(egam,egam0,fmbf,fmmf,fmbi,jmf,jbf,
     &                   zpf,ichn,zamp,isbi,ismi,imxi)
      implicit real*8 (a-h,o-y)
      implicit complex*16(z)
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cgmfrm / fg1p,fg1n,fg2p,fg2n,fg1v,fg1s,fg2v,fg2s
     &                 ,fgpr,fgpo,fgdm,fgde,fgdc
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / csw   / gsw(20,20)
      common / cdfi / meshx,mxx,mxj,mxm
      common / cmxj / jjmx
      common / cmat / zsmunu(0:3,0:3,4,4),zgamma(0:3,4,4)
      common / cdirac / ze(4,4),zg5(4,4),zgv(4,4,0:3),zpauli(2,2,3)
     &  ,gg(0:3)
      common / ceps / imax,ie1(24),ie2(24),ie3(24),ie4(24),iph(24)
      dimension isbi(6),ismi(6)
      dimension pgam(0:3),pnui(0:3),zpmf(0:3),zpbf(0:3)
      dimension zui(4,-1:1),zbuf(4,-1:1),zdds(4,4),zkf5(4,4)
      dimension zddu(4,4),zepsq(4,4,-1:1),zepsg(4,4,-1:1),zui5(4,-1:1)
      dimension zgam(4,4,-1:1,0:1),ztpp(-1:1)
      dimension zamp(-1:1,-3:3,6,njmx,3)
      dimension ztmpvr(0:3),ztmpvo(0:3),zgamrho(4,4,-1:1)
      dimension zgamomg(4,4,-1:1)
      dimension zphoton(0:3,-1:1),zcdd(4,4,-1:1),zcde(4,4,-1:1)
      dimension zepsf(0:4,-1:1)
      dimension ztmpr(4,4,-1:1),ztmpf(4,4) ,ztmprf(4,4,-1:1)
      dimension ztmpg(4,4,-1:1),ztmpq(4,4) ,ztmprq(4,4,-1:1)
      dimension ztmprg (4,4,-1:1,-1:1)
      dimension ztmprgx(4,4,-1:1,-1:1)
      dimension ztmpr1(4,4),ztmpr2(4,4,-1:1)
      dimension ztmpr3(-1:1),ztmpr4(-1:1,-1:1),ztmpr5(-1:1)
      dimension zopgv(-1:1,-1:1)
c
c
    
      zei      = (0.d0,1.d0)
      facx1    = sqrt(2.d0)/sqrt(3.d0)
      facx2    = -sqrt(3.d0)
      ss2      = 1.d0/sqrt(2.d0)
      fopi     = 4.d0*pi
      alpha    = 1.d0/137.03604d0
      ep       = sqrt(alpha*fopi)

      do 9 iv = 0,3
      do 9 ix = -1,1
 9    zphoton(iv,ix) = 0
      zphoton(0, 0) = 1
      zphoton(1, 1) = -1.d0/sqrt(2.d0)
      zphoton(2, 1) = -zei /sqrt(2.d0)
      zphoton(1,-1) =  1.d0/sqrt(2.d0)
      zphoton(2,-1) = -zei /sqrt(2.d0)

      zamp = 0

c
c momentum of photon and nucleon
      pgam(0)  = egam0
      pgam(1)  = 0
      pgam(2)  = 0
      pgam(3)  = egam
      pnui(0)   = sqrt(fmbi**2 + egam**2)
      pnui(1)   = 0
      pnui(2)   = 0
      pnui(3)   = -egam
c
c  [sla{q}sla{e} - sla{2}sla{q}]/2
c
c zepsq = i sigma^{mu nu} q_\mu e(gamma)_\nu
c
      do 10 i1 = 1,4
      do 10 i2 = 1,4
      do 10 i3 = -1,1
      zepsg(i1,i2,i3) = 0
 10   zepsq(i1,i2,i3) = 0
      zepsq(1,3,0) = egam
      zepsq(2,4,0) =-egam
      zepsq(3,1,0) = egam
      zepsq(4,2,0) =-egam
      zepsq(1,2,1) = egam *sqrt(2.d0)
      zepsq(1,4,1) = egam0*sqrt(2.d0)
      zepsq(3,2,1) = egam0*sqrt(2.d0)
      zepsq(3,4,1) = egam *sqrt(2.d0)
      zepsq(2,1,-1) = egam *sqrt(2.d0)
      zepsq(2,3,-1) =-egam0*sqrt(2.d0)
      zepsq(4,1,-1) =-egam0*sqrt(2.d0)
      zepsq(4,3,-1) = egam *sqrt(2.d0)
c
c  sla{e}
c
c  zepsg = g. e(gamma)
c
      zepsg(1,1,0) = 1
      zepsg(2,2,0) = 1
      zepsg(3,3,0) =-1
      zepsg(4,4,0) =-1
      zepsg(1,4,1) = sqrt(2.d0)
      zepsg(3,2,1) =-sqrt(2.d0)
      zepsg(2,3,-1) =-sqrt(2.d0)
      zepsg(4,1,-1) = sqrt(2.d0)
c
c  Gamma(isoscalar and vector)
c
c   zgam = F_1 g.e  + F2 (g.q g.e - g.e g.q)/2
c
c
      f1s = fg1s/2.d0
      f1v = fg1v/2.d0
      f2s = fg2s/2.d0/(2.d0*fnuc)
      f2v = fg2v/2.d0/(2.d0*fnuc)
      do 50 i1 = 1,4
      do 50 i2 = 1,4
      do 50 i3 = -1,1
      zgam(i1,i2,i3,0)=f1s*zepsg(i1,i2,i3)+f2s*zepsq(i1,i2,i3)
      zgam(i1,i2,i3,1)=f1v*zepsg(i1,i2,i3)+f2v*zepsq(i1,i2,i3)
 50   continue
c
c  zui = u(p),    zui5 = g_5 u(p)
c
      zpmf(0)  = sqrt(fmmf**2+zpf**2)
      zpbf(0)  = sqrt(fmbf**2+zpf**2)
      zfcc  = 1.d0/(2.d0*pi)**3/sqrt(4.d0*zpmf(0)*abs(pgam(0)))
     & *sqrt((zpbf(0) + fmbf)*(pnui(0) + fmbi)/(4.d0*zpbf(0)*pnui(0)))
      zddi = egam/(pnui(0) + fmbi)
      zddf = zpf /(zpbf(0) + fmbf)
      do 40 ix1 = 1,4
      do 40 ix2 = -1,1
      zui (ix1,ix2) = 0
      zui5(ix1,ix2) = 0
 40   continue
      zui(1,1)  = 1
      zui(3,1)  = - zddi
      zui(2,-1) = 1
      zui(4,-1) = zddi 
      do 20 ix = -1,1,2
      zui5(1,ix) = zui(3,ix)
      zui5(2,ix) = zui(4,ix)
      zui5(3,ix) = zui(1,ix)
      zui5(4,ix) = zui(2,ix)
 20   continue
c
c  zdds = 1/(sla{p}' + sla{k}' - M_N)
c
      zwf   = zpbf(0) + zpmf(0)
      zddsf = 1.d0/(zwf**2 - fnuc**2)
      do 30 ix1 = 1,4
      do 30 ix2 = 1,4
 30   zdds(ix1,ix2) = 0
      zdds(1,1) = ( zwf + fnuc)*zddsf
      zdds(2,2) = zdds(1,1)
      zdds(3,3) = (-zwf + fnuc)*zddsf
      zdds(4,4) = zdds(3,3)
c
c  for pi N
      if(ichn.eq.1) then
      ef     = ep*gpin/fpio
      zcom   = zei*ef*zfcc*zvtx(zpf,vnnpi,mnnpi)
      zgrho  = grnn*grpg*ep/fpio*fgpr*zfcc*gsw(5,1)
      zgomg  = gonn*gopg*ep/fpio*fgpo*zfcc*gsw(6,1)

      zfdele = ep*gpind/fpio*zfcc*gsw(8,1)*zvtx(zpf,vndpi,mndpi)

c      write(*,*)zfcc,zpf,vnnpi,mnnpi
c      write(*,*)ep,gpin,fpio
c      write(*,*)ichn,ef,zcom,zgrho,zgomg,zfdele
c
c  for eta N
c
      else if(ichn.eq.2) then
      ef     = ep*genn/feta
      zcom   = zei*ef*zfcc*zvtx(zpf,vnnet,mnnet)
c
c for sigma-N
c
      else if(ichn.eq.3) then
      ef     = -ep*gsinn
      zcom   = ef*zfcc*zvtx(zpf,vnnsi,mnnsi)
c
c  for rho-N
c
      else if(ichn.eq.4) then
      ef     = ep*grnn/2.d0   ! include 2 from tau/2
      zcom   = ef*zfcc*zvtx(zpf,vnnrho,mnnrho)
c
c  for omega-N
c
      else if(ichn.eq.6) then
      ef     = ep*gonn
      zcom   = ef*zfcc*zvtx(zpf,vnnomg,mnnomg)
      zcomo  = zei*ep*gpin*gopg/fpio**2*zfcc*zvtx(zpf,vnnomg,mnnomg)
      end if

c
c  loop cos theta
c
      do 100 izz = 1,mxx

      cc         = xgau(izz)
      ss         = sqrt(1.d0 - cc**2)
      c2         = sqrt((1.d0 + cc)/2.d0)
      s2         = sqrt((1.d0 - cc)/2.d0)
      wc         = wgau(izz)*2.d0*pi  !!! 2pi

c
c  zpmf = momentum of final meson
c  zpbf = momentum of final Baryon
c
      zpmf(1)    = zpf*sqrt(1.d0-cc**2)
      zpmf(2)    = 0
      zpmf(3)    = zpf*cc
      zpbf(1)    = -zpmf(1)
      zpbf(2)    = 0
      zpbf(3)    = -zpmf(3)
      zqs2       = (pgam(1)-zpmf(1))**2 +(pgam(2)-zpmf(2))**2 +
     &             (pgam(3)-zpmf(3))**2
      zqs        = sqrt(zqs2)
c
c  zubf = bar{u}(-k)
c
      zbuf(1,1) = c2
      zbuf(2,1) = s2
      zbuf(3,1) = c2*zddf
      zbuf(4,1) = s2*zddf
      zbuf(1,-1) =-s2
      zbuf(2,-1) = c2
      zbuf(3,-1) = s2*zddf
      zbuf(4,-1) =-c2*zddf
c
c  zkf5 = sla(k)' g_5
c      
      zkf5(1,1) = -zpf*cc
      zkf5(1,2) = -zpf*ss
      zkf5(2,1) = -zpf*ss
      zkf5(2,2) =  zpf*cc
      zkf5(1,3) =  zpmf(0)
      zkf5(1,4) =  0
      zkf5(2,3) =  0
      zkf5(2,4) =  zpmf(0)
      zkf5(3,1) = -zpmf(0)
      zkf5(3,2) =  0
      zkf5(4,1) =  0
      zkf5(4,2) = -zpmf(0)
      zkf5(3,3) =  zpf*cc
      zkf5(3,4) =  zpf*ss
      zkf5(4,3) =  zpf*ss
      zkf5(4,4) = -zpf*cc
c
c    zddu = 1/(sla{p} - sla{k}' - M_N)
c
      zpbimf= zpmf(0)*pnui(0) - zpmf(3)*pnui(3)
      zdduf =1.d0/(fmbi**2+fmmf**2-2.d0*zpbimf-fnuc**2)
      zpbimf0=pnui(0)-zpmf(0)
      zddu(1,1) = (zpbimf0+fnuc)*zdduf
      zddu(1,2) = 0
      zddu(2,1) = 0
      zddu(2,2) = zddu(1,1)
      zddu(1,3) = (-pnui(3) + zpmf(3))*zdduf
      zddu(1,4) =             zpmf(1) *zdduf
      zddu(2,3) =             zpmf(1) *zdduf
      zddu(2,4) = ( pnui(3) - zpmf(3))*zdduf
      zddu(3,1) = -zddu(1,3)
      zddu(3,2) = -zddu(1,4)
      zddu(4,1) = -zddu(2,3)
      zddu(4,2) = -zddu(2,4)
      zddu(3,3) = (-zpbimf0+fnuc)*zdduf
      zddu(3,4) = 0
      zddu(4,3) = 0
      zddu(4,4) = zddu(3,3)

c==========================  pi =====================================
      if(ichn.eq.1) then
      ztt=(zpbf(0)-pnui(0))**2-(zpbf(1)-pnui(1))**2-(zpbf(3)-pnui(3))**2
c pion pole
      zdpi = 2.d0*fnuc/(ztt - fpio**2)
      ztpp(0)  = (zpmf(0)+pnui(0)-zpbf(0))*zdpi
      ztpp(1)  = sqrt(2.d0)*zpmf(1)*zdpi
      ztpp(-1) =-sqrt(2.d0)*zpmf(1)*zdpi
c vecot meson
      zfrho = zgrho/(ztt - fmrho**2)*zvtx(zqs,vnnrho,mnnrho)
      zfomg = zgomg/(ztt - fmomg**2)*zvtx(zqs,vnnomg,mnnomg)
      do 180 i1 = 1,4
      do 180 i2 = 1,4
      do 181 ix = 0,3
      ztmpv     = 0
      do 182 iy = 0,3
      ztmpv     = ztmpv+zsmunu(ix,iy,i1,i2)*(pnui(iy)-zpbf(iy))
 182  continue
      ztmpvr(ix) = zgamma(ix,i1,i2) + xkrho/(2.d0*fnuc)*ztmpv
      ztmpvo(ix) = zgamma(ix,i1,i2) + xkomg/(2.d0*fnuc)*ztmpv
 181  continue
      ztmpx0          = pgam(3)*zpbf(1)
      zgamrho(i1,i2,0)= ztmpx0*ztmpvr(2)/2.d0
      zgamomg(i1,i2,0)= ztmpx0*ztmpvo(2)
      ztmpx0          =-pgam(3)*zpbf(1)*(-zei)/sqrt(2.d0)
      ztmpx3          = pgam(0)*zpbf(1)*(-zei)/sqrt(2.d0)
      ztmpxs = (pgam(0)*(pnui(3)-zpbf(3))-pgam(3)*(pnui(0)-zpbf(0)))
     &           *(-zei)/sqrt(2.d0)
 
      zgamrho(i1,i2,1)=(ztmpx3* ztmpvr(3) + ztmpx0*ztmpvr(0)
     &                + ztmpxs*(ztmpvr(1) +    zei*ztmpvr(2)))/2.d0
      zgamomg(i1,i2,1)= ztmpx3* ztmpvo(3) + ztmpx0*ztmpvo(0)
     &                + ztmpxs*(ztmpvo(1) +    zei*ztmpvo(2))
      zgamrho(i1,i2,-1)=(ztmpx3* ztmpvr(3) + ztmpx0*ztmpvr(0)
     &                 + ztmpxs*(ztmpvr(1) -    zei*ztmpvr(2)))/2.d0
      zgamomg(i1,i2,-1)= ztmpx3* ztmpvo(3) + ztmpx0*ztmpvo(0)
     &                 + ztmpxs*(ztmpvo(1) -    zei*ztmpvo(2))
 180  continue

      call ndelamp(zcdd,zcde,zphoton,pgam,pnui,zpmf,zpbf)

c=============================  rho ===========================
c
c  gamma N -> rho N
c
      else if(ichn.eq.4) then
c
c  zepsf = e*(rho) 
c
      zepsf(0,1) = 0
      zepsf(1,1) = - ss2*cc
      zepsf(2,1) =   ss2*zei
      zepsf(3,1) = + ss2*ss

      zepsf(0,-1) = 0
      zepsf(1,-1) =  ss2*cc
      zepsf(2,-1) =  ss2*zei
      zepsf(3,-1) = -ss2*ss

      zepsf(0,0) =    zpf/fmmf
      zepsf(1,0) = ss*zpmf(0)/fmmf
      zepsf(2,0) = 0
      zepsf(3,0) = cc*zpmf(0)/fmmf

      do iv1 = 1,4
      do iv2 = 1,4
c
c  ztmpr = e(rho).g,   ztmpg = e(gamma).g
c  ztmpf =      k.g,   ztmpq = (p - p').g
c
      do isr = -1,1
      ztmpr(iv1,iv2,isr) =
     &   zepsf(0,isr)*zgv(iv1,iv2,0) - zepsf(1,isr)*zgv(iv1,iv2,1) 
     & - zepsf(2,isr)*zgv(iv1,iv2,2) - zepsf(3,isr)*zgv(iv1,iv2,3)
      ztmpg(iv1,iv2,isr) =
     &   zphoton(0,isr)*zgv(iv1,iv2,0) - zphoton(1,isr)*zgv(iv1,iv2,1) 
     & - zphoton(2,isr)*zgv(iv1,iv2,2) - zphoton(3,isr)*zgv(iv1,iv2,3)
      end do  !isr

      ztmpf(iv1,iv2) = zpmf(0)*zgv(iv1,iv2,0) - zpf*ss*zgv(iv1,iv2,1)
     &                                        - zpf*cc*zgv(iv1,iv2,3)
      ztmpq(iv1,iv2) = (pnui(0)-zpbf(0))*zgv(iv1,iv2,0)
     &   -(pnui(1)-zpbf(1))*zgv(iv1,iv2,1) 
     &   -(pnui(2)-zpbf(2))*zgv(iv1,iv2,2)
     &   -(pnui(3)-zpbf(3))*zgv(iv1,iv2,3)

      end do !iv2
      end do !iv1

      do iv1 = 1,4
      do iv2 = 1,4
      do isr = -1,1

      zxx = 0
      zxy = 0
      do iv3 = 1,4
      zxx = zxx + ztmpr(iv1,iv3,isr)*ztmpf(iv3,iv2) 
     &          - ztmpr(iv3,iv2,isr)*ztmpf(iv1,iv3) 
      zxy = zxy + ztmpr(iv1,iv3,isr)*ztmpq(iv3,iv2) 
     &          - ztmpr(iv3,iv2,isr)*ztmpq(iv1,iv3) 
      end do !iv3
c 
c  ztmprf = e(rho).g + k/(4 m_N)[ e(rho).g k.g - k.g e(rho).g]
c  ztmprq = e(rho).g + k/(4 m_N)[ e(rho).g (p-p').g - (p-p').g e(rho).g]
c
      ztmprf(iv1,iv2,isr) = ztmpr(iv1,iv2,isr) + xkrho/4.d0/fnuc*zxx
      ztmprq(iv1,iv2,isr) = ztmpr(iv1,iv2,isr) + xkrho/4.d0/fnuc*zxy

      do isg = -1,1
      zyy = 0
      do iv3 = 1,4
      zyy = zyy + ztmpr(iv1,iv3,isr)*ztmpg(iv3,iv2,isg) 
     &          - ztmpr(iv3,iv2,isr)*ztmpg(iv1,iv3,isg) 
      end do !iv3
c
c   ztmprg = k/(4 m_N) [e(rho).g e(gamma).g - e(gamma).g e(rho).g]
c
      ztmprg(iv1,iv2,isr,isg) =   xkrho/4.d0/fnuc*zyy
      end do !isg

      end do !isr
      end do !iv2
      end do !iv1
c
c  ztmr1 = k.g + k/(4 m_N)[ k.g Q.g - Q.g k.g]
c  ztmr2 = e.g + k/(4 m_N)[ e.g Q.g - Q.g e.g]
c
      do iv1 = 1,4
      do iv2 = 1,4

      ztmpr1(iv1,iv2) = ztmpf(iv1,iv2)
      do iv3 = 1,4
      ztmpr1(iv1,iv2) = ztmpr1(iv1,iv2) +  xkrho/4.d0/fnuc*
     & (ztmpf(iv1,iv3)*ztmpq(iv3,iv2)-ztmpq(iv1,iv3)*ztmpf(iv3,iv2))
      end do ! iv3

      do isg = -1,1
      ztmpr2(iv1,iv2,isg) = ztmpg(iv1,iv2,isg)
      do iv3 = 1,4
      ztmpr2(iv1,iv2,isg) = ztmpr2(iv1,iv2,isg) +  xkrho/4.d0/fnuc*
     &                     (ztmpg(iv1,iv3,isg)*ztmpq(iv3,iv2)
     &                     -ztmpq(iv1,iv3)*ztmpg(iv3,iv2,isg))
      end do ! iv3
      end do ! isg

      end do ! iv2
      end do ! iv1
c
c  ztmpr3(i)   = (k' + Q).e(gamm,i)
c  ztmpr4(i,j) = e(gamm,i).e(rho j)
c  ztmpr5(i)   = Q.e(rho i)
c
      zq0    = pnui(0) - zpbf(0)
      zq1    = pnui(1) - zpbf(1)
      zq2    = pnui(2) - zpbf(2)
      zq3    = pnui(3) - zpbf(3)

      do isx = -1,1
       ztmpr3(isx) = zphoton(0,isx)*(zpmf(0)+zq0)
     & - zphoton(1,isx)*(zpmf(1)+zq1) - zphoton(2,isx)*(zpmf(2)+zq2)
     & - zphoton(3,isx)*(zpmf(3)+zq3)
       ztmpr5(isx) = zepsf(0,isx)*zq0 - zepsf(1,isx)*zq1
     &  - zepsf(2,isx)*zq2 - zepsf(3,isx)*zq3
      do isy = -1,1
       ztmpr4(isx,isy) = zphoton(0,isx)*zepsf(0,isy)
     & - zphoton(1,isx)*zepsf(1,isy) - zphoton(2,isx)*zepsf(2,isy)
     & - zphoton(3,isx)*zepsf(3,isy)
      end do
      end do

      zddrho =zq0**2 - zq1**2 - zq2**2 - zq3**2 - fmrho**2

c=============================  omega ===========================
c
c  gamma N -> omega N
c
      else if(ichn.eq.6) then
c
c  zepsf = e*(omega) 
c
      zepsf(0,1) = 0
      zepsf(1,1) = - ss2*cc
      zepsf(2,1) =   ss2*zei
      zepsf(3,1) = + ss2*ss

      zepsf(0,-1) = 0
      zepsf(1,-1) =  ss2*cc
      zepsf(2,-1) =  ss2*zei
      zepsf(3,-1) = -ss2*ss

      zepsf(0,0) =    zpf/fmmf
      zepsf(1,0) = ss*zpmf(0)/fmmf
      zepsf(2,0) = 0
      zepsf(3,0) = cc*zpmf(0)/fmmf
c
c   for t-channel pion exchange
c
      do isg = -1,1
      do ism = -1,1
      zopgv(isg,ism) = 0
      do ix = 1,imax
        k1      = ie1(ix)
        k2      = ie2(ix)
        k3      = ie3(ix)
        k4      = ie4(ix)
        xeps    = iph(ix)
        zopgv(isg,ism)   = zopgv(isg,ism)
     &    - xeps*pgam(k1)*zphoton(k2,isg)*zpmf(k3)*zepsf(k4,ism)
      end do
      end do
      end do

      zddpi = -2.d0*fnuc*zcomo*zvtx(zqs,vnnpi,mnnpi)/
     & ( (zpbf(0)-pnui(0))**2 - zqs2 - fpio**2)

c
c  ztmpr = e(omeg).g,   ztmpg = e(gamma).g
c  ztmpf =      k.g,   ztmpq = (p - p').g
c
      do iv1 = 1,4
      do iv2 = 1,4

      do isr = -1,1
      ztmpr(iv1,iv2,isr) =
     &   zepsf(0,isr)*zgv(iv1,iv2,0) - zepsf(1,isr)*zgv(iv1,iv2,1) 
     & - zepsf(2,isr)*zgv(iv1,iv2,2) - zepsf(3,isr)*zgv(iv1,iv2,3)
      ztmpg(iv1,iv2,isr) =
     &   zphoton(0,isr)*zgv(iv1,iv2,0) - zphoton(1,isr)*zgv(iv1,iv2,1) 
     & - zphoton(2,isr)*zgv(iv1,iv2,2) - zphoton(3,isr)*zgv(iv1,iv2,3)
      end do  !isr

      ztmpf(iv1,iv2) = zpmf(0)*zgv(iv1,iv2,0) - zpf*ss*zgv(iv1,iv2,1)
     &                                        - zpf*cc*zgv(iv1,iv2,3)
      ztmpq(iv1,iv2) = (pnui(0)-zpbf(0))*zgv(iv1,iv2,0)
     &   -(pnui(1)-zpbf(1))*zgv(iv1,iv2,1) 
     &   -(pnui(2)-zpbf(2))*zgv(iv1,iv2,2)
     &   -(pnui(3)-zpbf(3))*zgv(iv1,iv2,3)

      end do !iv2
      end do !iv1
c 
c  ztmprf = e(rho).g + k/(4 m_N)[ e(rho).g k.g - k.g e(rho).g]
c  ztmprq = e(rho).g + k/(4 m_N)[ e(rho).g (p-p').g - (p-p').g e(rho).g]
c

      do iv1 = 1,4
      do iv2 = 1,4
      do isr = -1,1

      zxx = 0
      zxy = 0
      do iv3 = 1,4
      zxx = zxx + ztmpr(iv1,iv3,isr)*ztmpf(iv3,iv2) 
     &          - ztmpr(iv3,iv2,isr)*ztmpf(iv1,iv3) 
      zxy = zxy + ztmpr(iv1,iv3,isr)*ztmpq(iv3,iv2) 
     &          - ztmpr(iv3,iv2,isr)*ztmpq(iv1,iv3) 
      end do !iv3
      ztmprf(iv1,iv2,isr) = ztmpr(iv1,iv2,isr) + xkomg/4.d0/fnuc*zxx
      ztmprq(iv1,iv2,isr) = ztmpr(iv1,iv2,isr) + xkomg/4.d0/fnuc*zxy

      end do !isr
      end do !iv2
      end do !iv1

c      write(*,*)'omega'

      end if
c
c=============================================================
c
c  for all final state
c
      do 210 ixi = 1,imxi
      do 220 isf = -jbf,jbf,2
      do 220 imf = -jmf,jmf

      isi = isbi(ixi)
      igm = ismi(ixi)
       
      jhi   = isi + 2*igm
      jhf   = isf + 2*imf
c=======================================================================
c
c  for piN
c
      if(ichn.eq.1) then
      znss = 0
      znsv = 0
      znus = 0
      znuv = 0
      zccv = 0
      zpip = 0
      zrho = 0
      zomg = 0
      zdele= 0
      do 300 i1 = 1,4
      zpip      = zpip + zbuf(i1,isf)*zui5(i1,isi)
      do 300 i2 = 1,4
      ztmp      = zbuf(i1,isf)*zui (i2,isi)
      zccv      = zccv + zepsg(i1,i2,igm)*zbuf(i1,isf)*zui5(i2,isi)
      zrho      = zrho + ztmp*zgamrho(i1,i2,igm)
      zomg      = zomg + ztmp*zgamomg(i1,i2,igm)
      zdele     = zdele+ ztmp*zcde(i1,i2,igm)
      do 310 i3 = 1,4
      do 310 i4 = 1,4
      znss  = znss + ztmp*zkf5(i1,i3)*zdds(i3,i4)*zgam(i4,i2,igm,0)
      znsv  = znsv + ztmp*zkf5(i1,i3)*zdds(i3,i4)*zgam(i4,i2,igm,1)
      znus  = znus + ztmp*zkf5(i4,i2)*zddu(i3,i4)*zgam(i1,i3,igm,0)
      znuv  = znuv + ztmp*zkf5(i4,i2)*zddu(i3,i4)*zgam(i1,i3,igm,1)
 310  continue
 300  continue
      znss = znss*gsw(1,ichn)
      znsv = znsv*gsw(1,ichn)
      znus = znus*gsw(2,ichn)
      znuv = znuv*gsw(2,ichn)
      zccv =-zccv*gsw(4,ichn)*fg1v
      zpip =-zpip*gsw(3,ichn)*fg1v*ztpp(igm)
      zplus = zcom*(znsv + znuv) + zomg*zfomg + zdele*zfdele*2.d0/3.d0
      zzero = zcom*(znss + znus) + zrho*zfrho

      zmins = zcom*(znsv - znuv + zpip + zccv) + zdele*zfdele/3.d0
      zxxx  = (zplus + 2.d0*zmins)/3.d0
      zamp3 = (zplus - zmins)*facx1
      zamp1p= ( zxxx + zzero)*facx2
      zamp1n= (-zxxx + zzero)*facx2

c      zamp1p=  zplus + zzero  !mod for test eta
c      zamp1n= -zplus + zzero  !mod
c
c  for eta-N
c
      else if(ichn.eq.2) then
      znss = 0
      znsv = 0
      znus = 0
      znuv = 0
      do 400 i1 = 1,4
      do 400 i2 = 1,4
      ztmp      = zbuf(i1,isf)*zui (i2,isi)
      do 410 i3 = 1,4
      do 410 i4 = 1,4
      znss  = znss + ztmp*zkf5(i1,i3)*zdds(i3,i4)*zgam(i4,i2,igm,0)
      znsv  = znsv + ztmp*zkf5(i1,i3)*zdds(i3,i4)*zgam(i4,i2,igm,1)
      znus  = znus + ztmp*zkf5(i4,i2)*zddu(i3,i4)*zgam(i1,i3,igm,0)
      znuv  = znuv + ztmp*zkf5(i4,i2)*zddu(i3,i4)*zgam(i1,i3,igm,1)
 410  continue
 400  continue
      znss = znss*gsw(1,ichn)
      znsv = znsv*gsw(1,ichn)
      znus = znus*gsw(2,ichn)
      znuv = znuv*gsw(2,ichn)
      zamp3  = 0
      zamp1p = zcom*(znss + znsv + znus + znuv)
      zamp1n = zcom*(znss - znsv + znus - znuv)

c
c  for sigma N
c
      else if(ichn.eq.3) then ! sigma???
      znss = 0
      znsv = 0
      znus = 0
      znuv = 0
      do 600 i1 = 1,4
      do 600 i2 = 1,4
      ztmp      = zbuf(i1,isf)*zui (i2,isi)
      do 610 i4 = 1,4
      znss  = znss + ztmp*zdds(i1,i4)*zgam(i4,i2,igm,0)
      znsv  = znsv + ztmp*zdds(i1,i4)*zgam(i4,i2,igm,1)
      znus  = znus + ztmp*zddu(i2,i4)*zgam(i1,i4,igm,0)
      znuv  = znuv + ztmp*zddu(i2,i4)*zgam(i1,i4,igm,1)
 610  continue
 600  continue
      znss = znss*gsw(1,ichn)
      znsv = znsv*gsw(1,ichn)
      znus = znus*gsw(2,ichn)
      znuv = znuv*gsw(2,ichn)
      zamp3  = 0
      zamp1p = zcom*(znss + znsv + znus + znuv)
      zamp1n = zcom*(znss - znsv + znus - znuv)
c
c  for rho N
c
      else if(ichn.eq.4) then ! rho + N
c
c  znss,znus = s/u-channel nucleon , iso scalar elemag
c  znsv,znuv = s/u-channel nucleon , iso vector elemag
c  zncc      = contact term
c  zntt      = t-channel rho exchange
c
      znss = 0
      znsv = 0
      znus = 0
      znuv = 0
      zncc = 0
      zntt = 0
      do  i1 = 1,4
      do  i2 = 1,4
      ztmp      = zbuf(i1,isf)*zui (i2,isi)
      do  i3 = 1,4
      do  i4 = 1,4
      znss=znss-ztmp*ztmprf(i1,i3,imf)*zdds(i3,i4)*zgam(i4,i2,igm,0)
      znsv=znsv-ztmp*ztmprf(i1,i3,imf)*zdds(i3,i4)*zgam(i4,i2,igm,1)
      znus=znus-ztmp*zgam(i1,i3,igm,0)*zddu(i3,i4)*ztmprf(i4,i2,imf)
      znuv=znus-ztmp*zgam(i1,i3,igm,1)*zddu(i3,i4)*ztmprf(i4,i2,imf)

      end do
      end do
      zncc  = zncc + ztmp*ztmprg (i1,i2,imf,igm)
      zntt  = zntt + ztmp/zddrho*
     & (ztmprq(i1,i2,imf)*ztmpr3(igm) - ztmpr1(i1,i2)*ztmpr4(igm,imf)
     & -ztmpr2(i1,i2,igm)*ztmpr5(imf))
      end do
      end do

      znss = znss*gsw(1,ichn)
      znsv = znsv*gsw(1,ichn)
      znus = znus*gsw(2,ichn)
      znuv = znuv*gsw(2,ichn)
      znct = zncc*gsw(3,ichn)*fg1v - zntt*gsw(4,ichn)*fg1v


      zxx3 = 2.d0*znuv - znct
      zxx1 = 3.d0*znsv - znuv + 2.d0*znct
      zxx0 =  znss + znus 
      zamp3  = zcom*facx1*zxx3
      zamp1p = zcom*( zxx1/facx2 + zxx0*facx2)
      zamp1n = zcom*(-zxx1/facx2 + zxx0*facx2)
c      zamp3  = 0
c      zamp1p = zcom*(znss + znus + znsv + znuv)*2
c      zamp1n = zcom*(znss + znus -(znsv + znuv))*2
c
c  for omega N
c
      else if(ichn.eq.6) then ! omega + N
c
c  znss,znus = s/u-channel nucleon , iso scalar elemag
c  znsv,znuv = s/u-channel nucleon , iso vector elemag
      znss = 0
      znsv = 0
      znus = 0
      znuv = 0
      ztpi = 0

      do  i1 = 1,4
      do  i2 = 1,4
      ztmp   = zbuf(i1,isf)*zui (i2,isi)
      ztpi   = ztpi + ztmp*zg5(i1,i2)
      do  i3 = 1,4
      do  i4 = 1,4
      znss=znss-ztmp*ztmprf(i1,i3,imf)*zdds(i3,i4)*zgam(i4,i2,igm,0)
      znsv=znsv-ztmp*ztmprf(i1,i3,imf)*zdds(i3,i4)*zgam(i4,i2,igm,1)
      znus=znus-ztmp*zgam(i1,i3,igm,0)*zddu(i3,i4)*ztmprf(i4,i2,imf)
      znuv=znus-ztmp*zgam(i1,i3,igm,1)*zddu(i3,i4)*ztmprf(i4,i2,imf)
      end do
      end do

      end do
      end do

      znss = znss*gsw(1,ichn)
      znsv = znsv*gsw(1,ichn)
      znus = znus*gsw(2,ichn)
      znuv = znuv*gsw(2,ichn)
      ztpi = ztpi*gsw(3,ichn)*zddpi*zopgv(igm,imf)

      zamp3  = 0
      zamp1p = zcom*(znss + znus + znsv + znuv)  + ztpi
      zamp1n = zcom*(znss + znus -(znsv + znuv)) - ztpi

      end if
c=======================================================================
c
c------------------ for all meson ---------------------
c
      do 230 jx  = 1,jjmx
      j2    = jx*2 -1
      ztmp  = wc*dfun(j2,jhi,jhf,izz)
      zamp(imf,isf,ixi,jx,1) =zamp(imf,isf,ixi,jx,1)+zamp3*ztmp
      zamp(imf,isf,ixi,jx,2) =zamp(imf,isf,ixi,jx,2)+zamp1p*ztmp
      zamp(imf,isf,ixi,jx,3) =zamp(imf,isf,ixi,jx,3)+zamp1n*ztmp
 230  continue
 220  continue
 210  continue

 100  continue
      return
      end
c
c
c   Delta internmediate state
c
      subroutine ndelamp(coutd,coute,cphx,pgam,pnui,ppio,pnuf)
      implicit complex*16(a-h,o-z)
      real*8 pgam,pnui,gmd,ged,gcd
      real*8 pi,fm,fnuc,fpio,fdelx,fdelgm,fmrho,fmomg,feta,fsigm
      real*8 fg1p,fg1n,fg2p,fg2n,fg1v,fg1s,fg2v,fg2s
      real*8 fgpr,fgpo,fgdm,fgde,fgdc
      real*8 gg,fdel,scale,fdelz
      real*8 gpin,gpind,grnn,gonn,grpg,gopg
      real*8 xkrho,xkomg,genn,grnp,gsinn
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdelz,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gmd,ged,gcd,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cgmfrm / fg1p,fg1n,fg2p,fg2n,fg1v,fg1s,fg2v,fg2s
     &                 ,fgpr,fgpo,fgdm,fgde,fgdc
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      common / ceps / imax,ie1(24),ie2(24),ie3(24),ie4(24),iph(24)
      dimension coutd(4,4,-1:1),coute(4,4,-1:1)
     &  ,pnui(0:3),pnuf(0:3),pgam(0:3),ppio(0:3),cphot(0:3)
      dimension
     &  pdele(0:3),pppe(0:3),ceme(0:3),cdele(0:3,0:3,4,4)
     & ,pdeld(0:3),pppd(0:3),cemd(0:3),cdeld(0:3,0:3,4,4)
     & ,cexd(0:3),cexe(0:3),ceee(0:3),ceed(0:3)
      dimension cphx(0:3,-1:1)
c
      fdel     = fdelgm
      cei      = (0.d0,1.d0)

      do 1000 igm = -1,1
      do 1 iv = 0,3
      cphot(iv) = cphx(iv,igm)
 1    continue
c
c     direct delta (without energy denminator)
c
      do 100 k = 0,3
        pdeld(k)  =  pnuf(k) + ppio(k)
        pppd(k)   = (pnui(k) + pdeld(k))/2.d0
        pdele(k)  =  pnui(k) - ppio(k)
        pppe(k)   = (pnuf(k) + pdele(k))/2.d0
  100 continue
c      
      call nprond(pdeld,fdel,cdeld)
      call npropd(pdele,fdel,cdele)
c
      qqq      = pgam(0)**2 - pgam(3)**2
      qppd     = pgam(0)*pppd(0) - pgam(3)*pppd(3)
      qppe     = pgam(0)*pppe(0) - pgam(3)*pppe(3)
      ccdir    =
     &   qqq*(pppd(0)*cphot(0)-pppd(1)*cphot(1)-pppd(2)*cphot(2)
     &                        -pppd(3)*cphot(3))
     &- qppd*(pgam(0)*cphot(0)-pgam(1)*cphot(1)-pgam(2)*cphot(2)
     &                        -pgam(3)*cphot(3))
      ccexc    =
     &   qqq*(pppe(0)*cphot(0)-pppe(1)*cphot(1)-pppe(2)*cphot(2)
     &                        -pppe(3)*cphot(3))
     &- qppe*(pgam(0)*cphot(0)-pgam(1)*cphot(1)-pgam(2)*cphot(2)
     &                        -pgam(3)*cphot(3))
c
      factm    = 3.d0*(fnuc+fdel)/2.d0/fnuc/
     &  ((fnuc+fdel)**2 - qqq)*(gmd*fgdm-ged*fgde)
      facte    = -6.d0*(fnuc+fdel)/fnuc/
     &  ((fnuc+fdel)**2 - qqq)/((fnuc-fdel)**2 - qqq)*ged*fgde
      factc    = 6.d0*(fnuc+fdel)/2.d0/fnuc/
     &  ((fnuc+fdel)**2 - qqq)/((fnuc-fdel)**2 - qqq)*gcd*fgdc      
c
      do 300 k = 0,3
      ceed(k)   = (0.d0,0.d0)
      ceee(k)   = (0.d0,0.d0)
      cexd(k)   = (0.d0,0.d0)
      cexe(k)   = (0.d0,0.d0)
      cemd(k)  = (0.d0,0.d0)
  300 ceme(k)  = (0.d0,0.d0)
      do 310 ix = 1,imax
        k1      = ie1(ix)
        k2      = ie2(ix)
        k3      = ie3(ix)
        k4      = ie4(ix)
        xeps    = iph(ix)
        cemd(k1)= cemd(k1) - xeps*pppd(k3)*pgam(k4)*cphot(k2)
        ceme(k1)= ceme(k1) - xeps*pppe(k3)*pgam(k4)*cphot(k2)
        cexd(k1)= cexd(k1) + xeps*pdeld(k2)*pgam(k3)*cphot(k4)
        cexe(k1)= cexe(k1) + xeps*pdele(k2)*pgam(k3)*cphot(k4)
  310 continue
      do 311 k1 = 0,3
      cemd(k1)  = cemd(k1) * (-factm)
      ceme(k1)  = ceme(k1) * (-factm)
  311 continue
c
      do 314 ix = 1,imax
        k1      = ie1(ix)
        k2      = ie2(ix)
        k3      = ie3(ix)
        k4      = ie4(ix)
        xeps    = dble(iph(ix))*pgam(k4)
        ceed(k1)= ceed(k1) - xeps*cexd(k2)*gg(k2)*pppd(k3)
        ceee(k1)= ceee(k1) - xeps*cexe(k2)*gg(k2)*pppe(k3)
  314 continue
      do 313 k1 = 0,3
      ceed(k1)  = ceed(k1) *(- facte)
      ceee(k1)  = ceee(k1) * facte
  313 continue

      do 320 k1 = 1,4
      do 320 k2 = 1,4
      ctmpd = (0.d0,0.d0)
      ctmpe = (0.d0,0.d0)
c
      do 330 m1 = 0,3
      do 330 m2 = 0,3
        ctmpd   = ctmpd +
     &    cemd(m2)*cdeld(m1,m2,k1,k2)*ppio(m1)*gg(m1)
        ctmpe   = ctmpe +
     &   ceme(m1)*cdele(m1,m2,k1,k2)*ppio(m2)*gg(m2)
c
      do 331 k3 = 1,4
        cxxxx = cei*cg5(k3,k2)*cdeld(m1,m2,k1,k3)*ppio(m1)*gg(m1)
        ctmpd = ctmpd + ceed(m2)*cxxxx
        ctmpd = ctmpd - pgam(m2)*gg(m2)*cxxxx*factc*ccdir
        cyyyy = cei*cg5(k1,k3)*cdele(m1,m2,k3,k2)*ppio(m2)*gg(m2)
        ctmpe = ctmpe + ceee(m1)*cyyyy
        ctmpe = ctmpe - pgam(m1)*gg(m1)*cyyyy*factc*ccexc
  331 continue
  330 continue
c
      coutd(k1,k2,igm) = -ctmpd
      coute(k1,k2,igm) = -ctmpe
  320 continue
c
 1000 continue
      return
      end
c
      subroutine setcmat
      implicit real*8 (a-h,o-y)
      implicit complex*16(z)
      common / cmat / zsmunu(0:3,0:3,4,4),zgamma(0:3,4,4)

      zei     = (0.d0,1.d0)

      do 4 i1 = 0,3
      do 4 i3 = 1,4
      do 4 i4 = 1,4
      zgamma(i1,i3,i4) = 0
      do 4 i2 = 0,3
 4    zsmunu(i1,i2,i3,i4) = 0

c   gamma matrix
      zgamma(0,1,1) = 1
      zgamma(0,2,2) = 1
      zgamma(0,3,3) =-1
      zgamma(0,4,4) =-1
      zgamma(1,1,4) = 1
      zgamma(1,2,3) = 1
      zgamma(1,3,2) =-1
      zgamma(1,4,1) =-1
      zgamma(2,1,4) =-zei
      zgamma(2,2,3) = zei
      zgamma(2,3,2) = zei
      zgamma(2,4,1) =-zei
      zgamma(3,1,3) = 1
      zgamma(3,2,4) =-1
      zgamma(3,3,1) =-1
      zgamma(3,4,2) = 1
c
c  prepare -i sigma~{mu nu}B_nu
c  Note!! g_munu included in zsmunu
c
      zsmunu(0,1,1,4) = -1
      zsmunu(0,1,2,3) = -1
      zsmunu(0,1,3,2) = -1
      zsmunu(0,1,4,1) = -1
      zsmunu(0,2,1,4) =  zei
      zsmunu(0,2,2,3) = -zei
      zsmunu(0,2,3,2) =  zei
      zsmunu(0,2,4,1) = -zei
      zsmunu(0,3,1,3) = -1
      zsmunu(0,3,2,4) =  1
      zsmunu(0,3,3,1) = -1
      zsmunu(0,3,4,2) =  1

      zsmunu(1,0,1,4) = -1
      zsmunu(1,0,2,3) = -1
      zsmunu(1,0,3,2) = -1
      zsmunu(1,0,4,1) = -1
      zsmunu(1,2,1,1) =  zei
      zsmunu(1,2,2,2) = -zei
      zsmunu(1,2,3,3) =  zei
      zsmunu(1,2,4,4) = -zei
      zsmunu(1,3,1,2) = -1
      zsmunu(1,3,2,1) =  1
      zsmunu(1,3,3,4) = -1
      zsmunu(1,3,4,3) =  1

      zsmunu(2,0,1,4) =  zei
      zsmunu(2,0,2,3) = -zei
      zsmunu(2,0,3,2) =  zei
      zsmunu(2,0,4,1) = -zei
      zsmunu(2,1,1,1) = -zei
      zsmunu(2,1,2,2) =  zei
      zsmunu(2,1,3,3) = -zei
      zsmunu(2,1,4,4) =  zei
      zsmunu(2,3,1,2) =  zei
      zsmunu(2,3,2,1) =  zei
      zsmunu(2,3,3,4) =  zei
      zsmunu(2,3,4,3) =  zei

      zsmunu(3,0,1,3) = -1
      zsmunu(3,0,2,4) =  1
      zsmunu(3,0,3,1) = -1
      zsmunu(3,0,4,2) =  1
      zsmunu(3,1,1,2) =  1
      zsmunu(3,1,2,1) = -1
      zsmunu(3,1,3,4) =  1
      zsmunu(3,1,4,3) = -1
      zsmunu(3,2,1,2) = -zei
      zsmunu(3,2,2,1) = -zei
      zsmunu(3,2,3,4) = -zei
      zsmunu(3,2,4,3) = -zei


      return
      end
c---------------------------------------------------------------------
c
c    p = (p^0 , p)
c
c     propagator of spin 3/2  = 1/(p*g -m)
c   ( -g^{mn}+gam^mgam^u/3 + (p^m gam^n - p^n gam^m)/3M
c         + 2p^m p^n /(3M^2) )
c
      subroutine npropd(p,fmas,cout)
      implicit complex*16(a-h,o-z)
      real*8 fmas,gg
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      dimension p(0:3),cout(0:3,0:3,4,4),cgam(4,4)
     &         ,ctmp1(4,4),ctmp3(4,4),ctmp4(4,4)
c
      do 1 i1 = 1,4
      do 1 i2 = 1,4
 1    cgam(i1,i2) = cgv(i1,i2,0)*p(0) - cgv(i1,i2,1)*p(1)
     &            - cgv(i1,i2,2)*p(2) - cgv(i1,i2,3)*p(3)

      ss     = p(0)**2 - p(1)**2 - p(2)**2 - p(3)**2
      fact   = ss - fmas**2
c
      do 200 m1 = 0,3
      do 200 m2 = 0,3
      fac       = 0    
      if(m1.eq.m2) fac = gg(m1)    
c
      do 211 k1 = 1,4
      do 211 k2 = 1,4
      ctmp3(k1,k2) = 0
      do 212 k3 = 1,4
 212  ctmp3(k1,k2) = ctmp3(k1,k2) + cgv(k1,k3,m1)*cgv(k3,k2,m2)
 211  continue
c
      do 100 k1 = 1,4
      do 110 k2 = 1,4
      ctmp4(k1,k2) = ctmp3(k1,k2)/3.d0
     &  - (p(m1)*cgv(k1,k2,m2)- p(m2)*cgv(k1,k2,m1))/3.d0/fmas
      ctmp1(k1,k2) = cgam(k1,k2)/fact
  110 continue
      ctmp4(k1,k1) = ctmp4(k1,k1) - fac + 2.d0*p(m1)*p(m2)/3.d0/fmas**2
      ctmp1(k1,k1) = ctmp1(k1,k1) + fmas/fact
  100 continue
c
      do 220 k1 = 1,4
      do 220 k2 = 1,4
      cout(m1,m2,k1,k2) = 0
      do 221 k3 = 1,4
  221 cout(m1,m2,k1,k2) = cout(m1,m2,k1,k2)+ctmp1(k1,k3)*ctmp4(k3,k2)
  220 continue
  200 continue
      return
      end
c---------------------------------------------------------------------
c
c    p = (sqrt(p^2+M^2) , p)
c
c     M/E sum_mu U^(mu)(p)U^(mu)(p)
c
c   (ssla(p) + M) /(2E)
c   ( -g^{mn}+gam^mgam^u/3 + (p^m gam^n - p^n gam^m)/3M
c         + 2p^m p^n /(3M^2) )
c
      subroutine nprond(pin,fmas,cout)
      implicit complex*16(a-h,o-z)
      real*8 fmas,gg
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      dimension pin(0:3),cout(0:3,0:3,4,4),cgam(4,4)
     &  ,ctmp1(4,4),ctmp3(4,4),ctmp4(4,4)
     &  ,p(0:3)
c
      pdel  = pin(1)**2 + pin(2)**2 + pin(3)**2
      p(0)  = sqrt(fmas**2 + pdel)
      p(1)  = pin(1)
      p(2)  = pin(2)
      p(3)  = pin(3)
      fnrm  = 1.d0/2.d0/p(0)
c
      do 1 k1 = 1,4
      do 1 k2 = 1,4
 1    cgam(k1,k2) = p(0)*cgv(k1,k2,0) - p(1)*cgv(k1,k2,1)
     &            - p(2)*cgv(k1,k2,2) - p(3)*cgv(k1,k2,3)
c
      do 200 m1 = 0,3
      do 200 m2 = 0,3
      fac       = 0    
      if(m1.eq.m2) fac = gg(m1)    
c
      do 210 k1 = 1,4
      do 210 k2 = 1,4
      ctmp3(k1,k2) = 0
      do 211 k3 = 1,4
      ctmp3(k1,k2) = ctmp3(k1,k2) +cgv(k1,k3,m1)*cgv(k3,k2,m2)
 211  continue
  210 continue
c
      do 100 k1 = 1,4
      do 110 k2 = 1,4
      ctmp4(k1,k2) = ctmp3(k1,k2)/3.d0
     &  - (p(m1)*cgv(k1,k2,m2)- p(m2)*cgv(k1,k2,m1))/3.d0/fmas
      ctmp1(k1,k2) = cgam(k1,k2)
  110 continue
      ctmp4(k1,k1) = ctmp4(k1,k1) - fac + 2.d0*p(m1)*p(m2)/3.d0/fmas**2
      ctmp1(k1,k1) = ctmp1(k1,k1) + fmas
  100 continue
c
      do 220 k1 = 1,4
      do 220 k2 = 1,4
      cout(m1,m2,k1,k2) = 0
      do 221 k3 = 1,4
      cout(m1,m2,k1,k2) = cout(m1,m2,k1,k2)+ctmp1(k1,k3)*ctmp4(k3,k2)
     & *fnrm
 221  continue
  220 continue
  200 continue
      return
      end
c
c  gamma + N -> pi + Delta
c
c    input egam,egam0,fmbf,fmmf,fmbi,jmf,jbf,zpf,ichn,isbi,ismi,imxi
c    output  zamp
c
      subroutine dhelamp(egam,egam0,fmbf,fmmf,fmbi,
     &                   zpf,ichn,zamp,isbi,ismi,imxi)
      implicit real*8 (a-h,o-y)
      implicit complex*16(z)
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cgmfrm / fg1p,fg1n,fg2p,fg2n,fg1v,fg1s,fg2v,fg2s
     &                 ,fgpr,fgpo,fgdm,fgde,fgdc
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / csw   / gsw(20,20)
      common / cdfi / meshx,mxx,mxj,mxm
      common / cmxj / jjmx
      common / cmat / zsmunu(0:3,0:3,4,4),zgamma(0:3,4,4)
      common / cdirac / ze(4,4),zg5(4,4),zgv(4,4,0:3),zpauli(2,2,3)
     &  ,gg(0:3)

      dimension isbi(6),ismi(6)
      dimension pgam(0:3),pnui(0:3),zpmf(0:3),zpbf(0:3)
      dimension zui(4,-1:1),zbuf(4,-1:1),zdds(4,4),zkf5(4,4)
      dimension zddu(4,4),zepsq(4,4,-1:1),zepsg(4,4,-1:1)
      dimension zgam(4,4,-1:1,0:1),zbuf5(4,-1:1)
      dimension zamp(-1:1,-3:3,6,njmx,3)
c      dimension ztt(-1:1,-3:3,6,njmx,7)
      dimension zphoton(0:3,-1:1)
      dimension zepsf(0:3,-1:1),zepskf(-1:1)
      dimension znss(-1:1,-1:1),znsv(-1:1,-1:1)
      dimension znus(-1:1,-1:1),znuv(-1:1,-1:1),zduv(-1:1,-1:1)
      dimension zccv(-1:1,-1:1),zpip(-1:1,-1:1),zdus(-1:1,-1:1)
      dimension zxnss(-3:3),zxnsv(-3:3)
      dimension zxnuv(-3:3),zxduv(-3:3),zxdus(-3:3)
      dimension zxccv(-3:3),zxpip(-3:3)
      dimension zqqq(0:3),zqqb(0:3),zpu(0:3),zpdelu(0:3,0:3,4,4)
      dimension zans1(-1:1,4,4),zans2(4,4)
      dimension zphtqq(-1:1),zphtqb(-1:1)
      dimension zepsqq(-1:1),zepsqb(-1:1)
      dimension zsumd1(-1:1,-1:1),zsumd2(-1:1,-1:1),zsumd3(-1:1,-1:1)
      dimension zgq(4,4)
c
      if(ichn.ne.5) return
c
      zei      = (0.d0,1.d0)
      ss2      = 1.d0/sqrt(2.d0)
      fopi     = 4.d0*pi
      alpha    = 1.d0/137.03604d0
      ep       = sqrt(alpha*fopi)

      zphoton = 0
      zamp    = 0
c      ztt     = 0

      zphoton(0, 0) = 1
      zphoton(1, 1) = -1.d0/sqrt(2.d0)
      zphoton(2, 1) = -zei /sqrt(2.d0)
      zphoton(1,-1) =  1.d0/sqrt(2.d0)
      zphoton(2,-1) = -zei /sqrt(2.d0)

c
c  test gauge invariance
c
c      zphoton(0,0) = 1
c      zphoton(3,0) = 1
c      write(*,*)'i1,i2,i3,i4,i5'
c      read(*,*)i1,i2,i3,i4,i5

c      gsw(1,5) = i1
c      gsw(2,5) = i2
c      gsw(3,5) = i3
c      gsw(4,5) = i4
c      gsw(5,5) = i5
c      gsw(1,5) = 0
c      gsw(2,5) = 0
c      gsw(3,5) = 0
c      gsw(4,5) = 0
c      gsw(5,5) = 0
c
c momentum of photon and nucleon
      pgam(0)  = egam0
      pgam(1)  = 0
      pgam(2)  = 0
      pgam(3)  = egam
      pnui(0)   = sqrt(fmbi**2 + egam**2)
      pnui(1)   = 0
      pnui(2)   = 0
      pnui(3)   = -egam
      wcm       = pgam(0)+ pnui(0)

c
c   zgq  = q.g
c
      do i1 = 1,4
      do i2 = 1,4
      zgq(i1,i2) = 0
      do iv = 0,3
      zgq(i1,i2) = zgq(i1,i2) + zgv(i1,i2,iv)*gg(iv)*pgam(iv)
      end do
      end do
      end do
c
c  zepsg = e(g).g
c
      do i1 = 1,4
      do i2 = 1,4
      do ig = -1,1
      zepsg(i1,i2,ig) = 0
      do iv = 0,3
      zepsg(i1,i2,ig)=zepsg(i1,i2,ig)+
     &                zgv(i1,i2,iv)*gg(iv)*zphoton(iv,ig)
      end do
      end do
      end do
      end do
c
c   zepsq = (q.g e.g - e.g q.g)/2
c
      do i1 = 1,4
      do i2 = 1,4
      do ig = -1,1
      zepsq(i1,i2,ig) = 0
      do in = 1,4
      zepsq(i1,i2,ig) = zepsq(i1,i2,ig) + 
     & (zgq(i1,in)*zepsg(in,i2,ig)-zepsg(i1,in,ig)*zgq(in,i2))/2.d0
      end do
      end do
      end do
      end do


c  Gamma(isoscalar and vector)
      f1s = fg1s/2.d0
      f1v = fg1v/2.d0
      f2s = fg2s/2.d0/(2.d0*fnuc)
      f2v = fg2v/2.d0/(2.d0*fnuc)
      do 50 i1 = 1,4
      do 50 i2 = 1,4
      do 50 i3 = -1,1
      zgam(i1,i2,i3,0)=f1s*zepsg(i1,i2,i3)+f2s*zepsq(i1,i2,i3)
      zgam(i1,i2,i3,1)=f1v*zepsg(i1,i2,i3)+f2v*zepsq(i1,i2,i3)
 50   continue
c
c  zui = u(p),   zui5 = g_5 u(p) factor

      zpmf(0)  = sqrt(fmmf**2+zpf**2)
      zpbf(0)  = sqrt(fmbf**2+zpf**2)

      zfcc  = 1.d0/(2.d0*pi)**3/sqrt(4.d0*zpmf(0)*abs(pgam(0)))
     & *sqrt((zpbf(0) + fmbf)*(pnui(0) + fmbi)/(4.d0*zpbf(0)*pnui(0)))
      zddi = egam/(pnui(0) + fmbi)
      zddf = zpf /(zpbf(0) + fmbf)

c
c  1 s-N  2 u-D 3 c  4 pip  5 u-N
c
      efd     = ep*gpind/fpio
      efn     = ep*gpin/fpio
      zcomd   = zei*efd*zfcc
      zcomn   = efn*zfcc
      zcom1   = zcomd*zvtx(zpf,vndpi,mndpi)*gsw(1,5)
      zcom2   = zcomd*zvtx(zpf,vndpi,mndpi)*gsw(2,5)
      zcom3   = zcomd*zvtx(zpf,vndpi,mndpi)*gsw(3,5)
      zcom4   =-zcomd*zvtx(zpf,vndpi,mndpi)*gsw(4,5)
      zcom5   = zcomn*zvtx(zpf,vnnpi,mnnpi)*gsw(5,5)


      do 40 ix1 = 1,4
      do 40 ix2 = -1,1
      zui (ix1,ix2) = 0
 40   continue
      zui(1,1)  = 1
      zui(3,1)  = - zddi
      zui(2,-1) = 1
      zui(4,-1) = zddi 
c    D_S = 1/(sla{p}' + sla{k}' - M_N)
      zwf   = zpbf(0) + zpmf(0)
      zddsf = 1.d0/(zwf**2 - fnuc**2)
      do 30 ix1 = 1,4
      do 30 ix2 = 1,4
 30   zdds(ix1,ix2) = 0
      zdds(1,1) = ( zwf + fnuc)*zddsf
      zdds(2,2) = zdds(1,1)
      zdds(3,3) = (-zwf + fnuc)*zddsf
      zdds(4,4) = zdds(3,3)
c

c
c  loop cos theta
c
      do 100 izz = 1,mxx

      cc         = xgau(izz)
      ss         = sqrt(1.d0 - cc**2)
      c2         = sqrt((1.d0 + cc)/2.d0)
      s2         = sqrt((1.d0 - cc)/2.d0)
      wc         = wgau(izz)*2.d0*pi  !!! 2pi

      zpmf(1)    = zpf*sqrt(1.d0-cc**2)
      zpmf(2)    = 0
      zpmf(3)    = zpf*cc
      zpbf(1)    = -zpmf(1)
      zpbf(2)    = 0
      zpbf(3)    = -zpmf(3)

      zqqq(0)    = pnui(0) - zpbf(0)
      zqqq(1)    = pnui(1) - zpbf(1)
      zqqq(2)    = pnui(2) - zpbf(2)
      zqqq(3)    = pnui(3) - zpbf(3)
c
c
c  bar{u}(-k)
c
      zbuf(1,1) = c2
      zbuf(2,1) = s2
      zbuf(3,1) = c2*zddf
      zbuf(4,1) = s2*zddf
      zbuf(1,-1) =-s2
      zbuf(2,-1) = c2
      zbuf(3,-1) = s2*zddf
      zbuf(4,-1) =-c2*zddf
      do 29 ix = -1,1,2
      zbuf5(1,ix) = zbuf(3,ix)
      zbuf5(2,ix) = zbuf(4,ix)
      zbuf5(3,ix) = zbuf(1,ix)
      zbuf5(4,ix) = zbuf(2,ix)
 29   continue
c
c   sla(k)' g_5
c      
      zkf5(1,1) = -zpf*cc
      zkf5(1,2) = -zpf*ss
      zkf5(2,1) = -zpf*ss
      zkf5(2,2) =  zpf*cc
      zkf5(1,3) =  zpmf(0)
      zkf5(1,4) =  0
      zkf5(2,3) =  0
      zkf5(2,4) =  zpmf(0)
      zkf5(3,1) = -zpmf(0)
      zkf5(3,2) =  0
      zkf5(4,1) =  0
      zkf5(4,2) = -zpmf(0)
      zkf5(3,3) =  zpf*cc
      zkf5(3,4) =  zpf*ss
      zkf5(4,3) =  zpf*ss
      zkf5(4,4) = -zpf*cc
c
c    D_U = 1/(sla{p} - sla{k}' - M_N)
c
      zpbimf= zpmf(0)*pnui(0) - zpmf(3)*pnui(3)
      zdduf =1.d0/(fmbi**2+fmmf**2-2.d0*zpbimf-fnuc**2)
      zpbimf0=pnui(0)-zpmf(0)
      zddu(1,1) = (zpbimf0+fnuc)*zdduf
      zddu(1,2) = 0
      zddu(2,1) = 0
      zddu(2,2) = zddu(1,1)
      zddu(1,3) = (-pnui(3) + zpmf(3))*zdduf
      zddu(1,4) =             zpmf(1) *zdduf
      zddu(2,3) =             zpmf(1) *zdduf
      zddu(2,4) = ( pnui(3) - zpmf(3))*zdduf
      zddu(3,1) = -zddu(1,3)
      zddu(3,2) = -zddu(1,4)
      zddu(4,1) = -zddu(2,3)
      zddu(4,2) = -zddu(2,4)
      zddu(3,3) = (-zpbimf0+fnuc)*zdduf
      zddu(3,4) = 0
      zddu(4,3) = 0
      zddu(4,4) = zddu(3,3)
c
c  delta epsilon
c
      zepsf(0,1) = 0
      zepsf(1,1) = - ss2*cc
      zepsf(2,1) =   ss2*zei
      zepsf(3,1) = + ss2*ss

      zepsf(0,-1) = 0
      zepsf(1,-1) =  ss2*cc
      zepsf(2,-1) =  ss2*zei
      zepsf(3,-1) = -ss2*ss

      zepsf(0,0) = -  zpf/fmbf
      zepsf(1,0) = ss*zpbf(0)/fmbf
      zepsf(2,0) = 0
      zepsf(3,0) = cc*zpbf(0)/fmbf
c
c  epsf * kf, epsf * ki
c
      do 203 imx = -1,1
      zepskf(imx) = zepsf(0,imx)*zpmf(0)
     &            -(zepsf(3,imx)*cc + zepsf(1,imx)*ss)*zpf
      zepsqq(imx) = zepsf(0,imx)*zqqq(0)-zepsf(1,imx)*zqqq(1)
     &             -zepsf(2,imx)*zqqq(2)-zepsf(3,imx)*zqqq(3)
      zphtqq(imx) = zphoton(0,imx)*(zqqq(0)+zpmf(0))
     &             -zphoton(1,imx)*(zqqq(1)+zpmf(1))
     &             -zphoton(2,imx)*(zqqq(2)+zpmf(2))
     &             -zphoton(3,imx)*(zqqq(3)+zpmf(3))
 203  continue


      do iv = 0,3
      zpu(iv) = pnui(iv) - zpmf(iv)
      end do

      call zpropd(zpu,fdel,zpdelu)

      zans1 = 0
      zans2 = 0

      do il1 = 1,4
      do il2 = 1,4

      do iv1 = 0,3
      do iv2 = 0,3

      zxxx   = gg(iv1)*gg(iv2)*zpmf(iv2)
      do il3 = 1,4
      zans2(il1,il2) = zans2(il1,il2) + zgv(il1,il3,iv1) 
     &                *zpdelu(iv1,iv2,il3,il2)*zxxx
      end do !il3

      do imx = -1,1
      zans1(imx,il1,il2) = zans1(imx,il1,il2) +
     &   zxxx*zepsf(iv1,imx)*zpdelu(iv1,iv2,il1,il2)
      end do

      end do !iv2
      end do !iv1

      end do
      end do

      call vtxgmdel(zphoton,zepsf,pnui,pgam,zpmf,zpbf,
     &              zsumd1,zsumd2,zsumd3)

      do 210 ixi = 1,imxi

      isi = isbi(ixi)
      igm = ismi(ixi)
      jhi = isi + 2*igm


      do 220 isf = -1,1,2
      do 220 imf = -1,1

      ztmpcc= (zepsf(0,imf)*zphoton(0,igm)-zepsf(1,imf)*zphoton(1,igm)
     &       - zepsf(2,imf)*zphoton(2,igm)-zepsf(3,imf)*zphoton(3,igm))
     &        * fg1v
C
c      ztmppi = zepsqq(imf)*zphtqq(igm)/
c     & (zqqq(0)**2 - zqqq(1)**2 - zqqq(2)**2 - zqqq(3)**2 - fpio**2)
c     & * fg1v

c----------------- 12-15-2006 temporary -------------------------
      ztmppi = zepsqq(imf)*zphtqq(igm)/
     & ( - zqqq(1)**2 - zqqq(2)**2 - zqqq(3)**2 - fpio**2)
     & * fg1v
c--------------------------------------------------------------

      znss(imf,isf) = 0
      znsv(imf,isf) = 0

      zdus(imf,isf) = 0
      zduv(imf,isf) = 0

      zccv(imf,isf) = 0
      zpip(imf,isf) = 0
      znuv(imf,isf) = 0
      do 300 i1 = 1,4
      ztmp      = zbuf(i1,isf)*zui (i1,isi)
      zccv(imf,isf) = zccv(imf,isf) + ztmp*ztmpcc
      zpip(imf,isf) = zpip(imf,isf) + ztmp*ztmppi

      do 310 i2 = 1,4
      ztmp  = zbuf(i1,isf)*zui (i2,isi)
      ztmp5 = zbuf5(i1,isf)*zui(i2,isi)

      zxx1 = ztmpcc*zans2(i1,i2)
      do i3 = 1,4
      zxx1 = zxx1 - zepsg(i1,i3,igm)*zans1(imf,i3,i2)
      end do
      zduv(imf,isf) =zduv(imf,isf)+ zxx1*ztmp


      do 310 i3 = 1,4
      znss(imf,isf) =znss(imf,isf) + ztmp*zdds(i1,i3)*zgam(i3,i2,igm,0)
     &                              *zepskf(imf)
      znsv(imf,isf) =znsv(imf,isf) + ztmp*zdds(i1,i3)*zgam(i3,i2,igm,1)
     &                              *zepskf(imf)
c--------------------------------------------------------------
      znuv(imf,isf) = znuv(imf,isf) + (ztmp*zsumd1(imf,igm)
     & + zei*ztmp5*(zsumd2(imf,igm)+zsumd3(imf,igm)))
     & *zddu(i1,i3)*zkf5(i3,i2)
c------------------------------------------------------------
 310  continue
 300  continue
 220  continue

      do 231 isf = -3,3,2
      zxnss(isf) = 0
      zxnsv(isf) = 0
      zxduv(isf) = 0
      zxccv(isf) = 0
      zxpip(isf) = 0
      zxnuv(isf) = 0
 231  continue

      do 232 idm = -1,1
      do 232 idb = -1,1,2
      cef = cgdel(idm,idb)
      imf = 2*idm + idb
      zxnss(imf) = zxnss(imf) + znss(idm,idb)*cef
      zxnsv(imf) = zxnsv(imf) + znsv(idm,idb)*cef
      zxduv(imf) = zxduv(imf) + zduv(idm,idb)*cef
      zxccv(imf) = zxccv(imf) + zccv(idm,idb)*cef
      zxpip(imf) = zxpip(imf) + zpip(idm,idb)*cef
      zxnuv(imf) = zxnuv(imf) + znuv(idm,idb)*cef
 232  continue

      do 230 isf = -3,3,2
      zynss = zxnss(isf)*zcom1
      zynsv = zxnsv(isf)*zcom1
      zydus = zxduv(isf)*zcom2*fg1s/2.d0
      zyduv = zxduv(isf)*zcom2*fg1v
      zyccv = zxccv(isf)*zcom3
      zypip = zxpip(isf)*zcom4
      zynuv = zxnuv(isf)*zcom5

      zamp3 = sqrt(10.d0)/3.d0*(zyduv -zynuv + zyccv + zypip)
      zamp1p= -sqrt(2.d0)*(zydus+zynss+zynsv)-sqrt(8.d0)/3.d0*zynuv
     &        -sqrt(2.d0)/3.d0*(zyccv+zypip)-5.d0/sqrt(18.d0)*zyduv
      zamp1n= -sqrt(2.d0)*(zydus+zynss-zynsv)+sqrt(8.d0)/3.d0*zynuv
     &        +sqrt(2.d0)/3.d0*(zyccv+zypip)+5.d0/sqrt(18.d0)*zyduv

      do 230 jx  = 1,jjmx

      imf   = 0
      j2    = jx*2 -1
      jhf   = isf
      ztmp  = wc*dfun(j2,jhi,jhf,izz)
      zamp(imf,isf,ixi,jx,1) =zamp(imf,isf,ixi,jx,1)+zamp3*ztmp
      zamp(imf,isf,ixi,jx,2) =zamp(imf,isf,ixi,jx,2)+zamp1p*ztmp
      zamp(imf,isf,ixi,jx,3) =zamp(imf,isf,ixi,jx,3)+zamp1n*ztmp
c      ztt(imf,isf,ixi,jx,1) = ztt(imf,isf,ixi,jx,1) +zynss*ZTMP
c      ztt(imf,isf,ixi,jx,2) = ztt(imf,isf,ixi,jx,2) +zydus*ZTMP
c      ztt(imf,isf,ixi,jx,3) = ztt(imf,isf,ixi,jx,3) +zynsv*ZTMP
c      ztt(imf,isf,ixi,jx,4) = ztt(imf,isf,ixi,jx,4) +zyduv*ZTMP
c      ztt(imf,isf,ixi,jx,5) = ztt(imf,isf,ixi,jx,5) +zyccv*ZTMP
c      ztt(imf,isf,ixi,jx,6) = ztt(imf,isf,ixi,jx,6) +zypip*ZTMP

 230  continue

 210  continue


 100  continue

c      jjx = 3
c      write(*,*)'jjx'
c      read(*,*)jjx
c      do isf = -jjx,jjx,2
c      write(*,1010)jjx,isf,zamp(0,isf,3,jjx,1),zamp(0,isf,3,jjx,2)
c      write(*,1010)jjx,isf,zamp(0,isf,2,jjx,1),zamp(0,isf,2,jjx,2)
c      write(*,1010)jjx,isf,ztt(0,isf,3,jjx,1),ztt(0,isf,3,jjx,2)
c     &      ,ztt(0,isf,3,jjx,3),ztt(0,isf,3,jjx,4),
c     &       ztt(0,isf,3,jjx,5)+ztt(0,isf,3,jjx,6)
c      end do
c 1010 format(1h ,2i3,20e15.5)
c      stop

      return
      end
c-----------------------------------------------------------------
c
c  calculate epsilon^*(delta)^\nu epsilon(gamma)^mu Gamma_{nu mu}
c
c------------------------------------------------------------------
      subroutine vtxgmdel(zegam,zedel,pnui,pgam,zppio,zpnuf
     &                   ,zsum1,zsum2,zsum3)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)

      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdelz,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gmd,ged,gcd,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cgmfrm / fg1p,fg1n,fg2p,fg2n,fg1v,fg1s,fg2v,fg2s
     &                 ,fgpr,fgpo,fgdm,fgde,fgdc
      common / cdirac / ze(4,4),zg5(4,4),zgv(4,4,0:3),zpauli(2,2,3)
     &  ,gg(0:3)
      common / ceps / imax,ie1(24),ie2(24),ie3(24),ie4(24),iph(24)

      dimension zegam(0:3,-1:1),zedel(0:3,-1:1)
      dimension zpdeld(0:3),zppio(0:3),zpppd(0:3),pnui(0:3),pgam(0:3)
      dimension ztmp(0:3),zpnuf(0:3)
      dimension zsum1(-1:1,-1:1),zsum2(-1:1,-1:1),zsum3(-1:1,-1:1)

      fdel     = fdelgm
c
      do 1000 igm = -1,1
      do 1000 idm = -1,1
c
      do 100 k = 0,3
        zpdeld(k)  =  zpnuf(k) + zppio(k)
        zpppd(k)   = (pnui(k) + zpdeld(k))/2.d0
  100 continue
c      
      qqq      = pgam(0)**2 - pgam(3)**2
      zqppd    = pgam(0)*zpppd(0) - pgam(3)*zpppd(3)
      zqepsd   = pgam(0)*zedel(0,idm) - pgam(3)*zedel(3,idm)
      zccdir   =
     &   qqq*(zpppd(0)*zegam(0,igm)-zpppd(1)*zegam(1,igm)
     &       -zpppd(2)*zegam(2,igm)-zpppd(3)*zegam(3,igm))
     &- zqppd*(pgam(0)*zegam(0,igm)- pgam(1)*zegam(1,igm)
     &        -pgam(2)*zegam(2,igm)- pgam(3)*zegam(3,igm))
c
      factm = 3.d0*(fnuc+fdel)/2.d0/fnuc/
     &  ((fnuc+fdel)**2 - qqq)*(gmd*fgdm-ged*fgde)
      facte = 12.d0*(fnuc+fdel)/2.d0/fnuc/
     &  ((fnuc+fdel)**2 - qqq)/((fnuc-fdel)**2 - qqq)*ged*fgde
      factc = 6.d0*(fnuc+fdel)/2.d0/fnuc/
     &  ((fnuc+fdel)**2 - qqq)/((fnuc-fdel)**2 - qqq)*gcd*fgdc      
c
      do 300 k = 0,3
      ztmp(k)  = 0
  300 continue

      zsum1(idm,igm) = 0
      do 310 ix = 1,imax
        k1      = ie1(ix)
        k2      = ie2(ix)
        k3      = ie3(ix)
        k4      = ie4(ix)
        xeps    = iph(ix)
        zsum1(idm,igm) =zsum1(idm,igm)-xeps*zedel(k1,idm)*zegam(k2,igm)
     &                                     *zpppd(k3)*pgam(k4)*factm
        ztmp(k1)= ztmp(k1) - xeps*zpdeld(k2)*pgam(k3)*zegam(k4,igm)
  310 continue
c
      zsum2(idm,igm)= 0
      do 314 ix = 1,imax
        k1      = ie1(ix)
        k2      = ie2(ix)
        k3      = ie3(ix)
        k4      = ie4(ix)
        xeps    = dble(iph(ix))
        zsum2(idm,igm) = zsum2(idm,igm)
     &  - xeps*zedel(k1,idm)*ztmp(k2)*gg(k2)*zpppd(k3)*pgam(k4)*facte
  314 continue

      zsum3(idm,igm)= factc*zqepsd*zccdir

 1000 continue

      return
      end
c
c-------------------------------------------------------
c   polarization vector    zeps(iv,im)
c
c    iv=0,1,2,3     im=-2,0,2 (im = 2*lambda)
c
c  *  zp momentum,  ze energy, fms mass
c
c  *  direction of quantization axis
c      c  cos(theta)   assumed to be in xz-plane
c  *  direction of momentum 
c    is = 1      paralel to quantization axis
c    is =-1 anti-paralel
c
      subroutine setpolv(zp,ze,fms,c,is,zeps)
      implicit complex*16(z)
      implicit real*8(a-h,o-y)
      dimension zeps(0:3,-2:2)

      zi       = (0.d0,1.d0)
      ss2      = 1.d0/sqrt(2.d0)
      s        = sqrt(1.d0 -  c**2)

      zeps(0,2) =   0
      zeps(1,2) = - ss2*c
      zeps(2,2) = - ss2*zi
      zeps(3,2) = + ss2*s

      zeps(0,-2) =  0
      zeps(1,-2) =  ss2*c
      zeps(2,-2) = -ss2*zi
      zeps(3,-2) = -ss2*s

      zeps(0,0) = dble(is)*zp/fms
      zeps(1,0) =        s*ze/fms
      zeps(2,0) = 0
      zeps(3,0) =        c*ze/fms
      
      return
      end
c
c
c    set metrix and dirac matrix modified 7-8-2005
c
      subroutine setdirac
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &,gg(0:3)
      common / cdirac1 / cgg5(4,4,0:3)  ! gamma_mu gammma_5
c
c   metric
c
      gg(0)    = 1
      gg(1)    = -1
      gg(2)    = -1
      gg(3)    = -1
c
c    clear all
c
      czero          = (0.d0,0.d0)
      do 100 k1      = 1,4
      do 100 k2      = 1,4
        ce(k1,k2)    = czero
        cg5(k1,k2)   = czero
      do 101 k3      = 0,3
        cgv(k1,k2,k3)=czero
  101 continue
  100 continue
      do 102 k1     = 1,2
      do 102 k2     = 1,2
      do 102 k3     = 1,3
        cpauli(k1,k2,k3) = czero
  102 continue
c
c   Pauli Matrix
c
      cpauli(1,2,1) = 1
      cpauli(2,1,1) = 1
      cpauli(1,2,2) = (0.d0,-1.d0)
      cpauli(2,1,2) = (0.d0,1.d0)
      cpauli(1,1,3) = 1
      cpauli(2,2,3) = -1
c
c    unit matrix  and gamma_5
c
      do 111 k1   = 1,4
  111 ce(k1,k1)   = 1.d0
      cg5(1,3)    = 1.d0
      cg5(2,4)    = 1.d0
      cg5(3,1)    = 1.d0
      cg5(4,2)    = 1.d0
c
c    gamma_mu
c
      cgv(1,1,0)  = 1
      cgv(2,2,0)  = 1
      cgv(3,3,0)  = -1
      cgv(4,4,0)  = -1
      do 121 k1   = 1,3
      do 122 kf   = 1,2
      do 122 ki   = 1,2
        cgv(kf,ki+2,k1) =  cpauli(kf,ki,k1)
        cgv(kf+2,ki,k1) = -cpauli(kf,ki,k1)
  122 continue
  121 continue


      do if = 1,4
      do ii = 1,4
      do iv = 0,3
      cxx   = 0
      do in = 1,4
         cxx = cxx + cgv(if,in,iv)*cg5(in,ii)
      end do
      cgg5(if,ii,iv) = gg(iv)*cxx
      end do
      end do
      end do
      return
      end
c---------------------------------------------------------------------
c complex momentum
c    p = (p^0 , p)
c
c     propagator of spin 3/2  = 1/(p*g -m)
c   ( -g^{mn}+gam^mgam^u/3 - (p^m gam^n - p^n gam^m)/3M
c         + 2p^m p^n /(3M^2) )
c
      subroutine zpropd(p,fmas,cout)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      complex*16 p(0:3),zfact
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      dimension cout(0:3,0:3,4,4),cgam(4,4)
     &         ,ctmp1(4,4),ctmp2(4,4),ctmp3(4,4),ctmp4(4,4)
c
      do kf = 1,4
      do ki = 1,4
        cgam(kf,ki) = (0.d0,0.d0)
      do  kmu = 0,3
        cgam(kf,ki) = cgam(kf,ki) + cgv(kf,ki,kmu)*gg(kmu)*p(kmu)
      end do
      end do
      end do

      zfact   = p(0)**2 - p(1)**2 - p(2)**2 - p(3)**2 - fmas**2
c
      do 200 m1 = 0,3
      do 200 m2 = 0,3

      fac       = 0    
      if(m1.eq.m2) fac = gg(m1)    
c
      do 210 k1 = 1,4
      do 210 k2 = 1,4
      ctmp1(k1,k2) = cgv(k1,k2,m1)
      ctmp2(k1,k2) = cgv(k1,k2,m2)
  210 continue
      call subsum2(ctmp1,ctmp2,ctmp3)
c
      do 100 k1 = 1,4
      do 110 k2 = 1,4
      ctmp4(k1,k2) = ctmp3(k1,k2)/3.d0
     &  - (p(m1)*cgv(k1,k2,m2)- p(m2)*cgv(k1,k2,m1))/3.d0/fmas
      ctmp1(k1,k2) = cgam(k1,k2)/zfact
  110 continue
      ctmp4(k1,k1) = ctmp4(k1,k1) - fac + 2.d0*p(m1)*p(m2)/3.d0/fmas**2
      ctmp1(k1,k1) = ctmp1(k1,k1) + fmas/zfact
  100 continue
c
      call subsum2(ctmp1,ctmp4,ctmp2)
      do 220 k1 = 1,4
      do 220 k2 = 1,4
      cout(m1,m2,k1,k2) = ctmp2(k1,k2)
  220 continue

  200 continue

      return
      end
c
c
c  (1,m,2/1,s|3/2,m')
c
c   im = m,  is = 2 * s
c
      real*8 function cgdel(im,is)
      implicit real*8(a-h,o-z)
      cgdel = 0

      if(im.eq.0) then
            cgdel = sqrt(2.d0/3.d0)

      else if(im.eq.1) then
         if(is.eq.1) then
            cgdel = 1
         else if(is.eq.-1)then
            cgdel = 1.d0/sqrt(3.d0)
         end if

      else if(im.eq.-1) then
         if(is.eq.-1) then
            cgdel = 1
         else if(is.eq.1) then
            cgdel = 1.d0/sqrt(3.d0)
         end if

      end if
      return
      end
c
c fss1h = (1,m,1/2,b | s, m + b)
c
c   im = 2*m, ib = 2*b,  js = 2*s
c
c
      real*8 function fss1h(im,ib,js)
      implicit real*8(a-h,o-z)
      dimension fdata(-2:2,-1:1,3)

      s23    = sqrt(2.d0/3.d0)
      s3     = sqrt(1.d0/3.d0)

      fdata = 0

      fdata( 2, 1,1) = 0
      fdata( 2,-1,1) = s23
      fdata( 0, 1,1) =-s3
      fdata( 0,-1,1) = s3
      fdata(-2, 1,1) =-s23
      fdata(-2,-1,1) = 0

      fdata( 2, 1,3) = 1
      fdata( 2,-1,3) = s3
      fdata( 0, 1,3) = s23
      fdata( 0,-1,3) = s23
      fdata(-2, 1,3) = s3
      fdata(-2,-1,3) = 1
      
      fss1h = fdata(im,ib,js)

      return
      end
c
c Nucleon form factor from PRC51 409(95)
c
c
c  add axial form factor
c
      subroutine gamfrm(egam0,egam)
      implicit real*8(a-h,o-z)
c==============================================================
      common / cgmfrm / fg1p,fg1n,fg2p,fg2n,fg1v,fg1s,fg2v,fg2s
     &                 ,fgpr,fgpo,fgdm,fgde,fgdc
      common / cgafrm / fgan,fgac,fgad1,fgad2,fgad3,fgad4
c===========================================================
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / c2004 / igfrm,idfrm,idafrm1,idafrm2,xdafac,famasn,famasd
c=========================================================
c
      qs     = egam0**2 - egam**2
c
c  axial form factor (need to check!)
c
      ga   = 1.26d0
      fma  = 1024.d0/scale
      fpid = 93.d0/scale
      fgan = ga/(1.d0 - qs/fma**2)**2
      fgac = 1.d0/(1.d0 - qs/fma**2)**2/fpid/2.d0
c
c delta
c
c  this part is original one
      s2    = sqrt(2.d0)
      fgad  = ga*6.d0/5.d0/s2/1.17d0
      xydel = sqrt(2.d0)*1000.d0/scale
      xxdel = (1.d0 + 1.21d0*qs/(xydel**2 - qs))
     &         /(1.d0 - qs/fma**2)**2*fgad
c      fgad2 = - fnuc**2/(fnuc+fdel)**2*xxdel/fdel**2
cmodifiedaug7
      fgad2 = - 1.d0/(fnuc+fdel)**2*xxdel
c!test
c      fgad2 = 0
      fgad1 =   2.d0*fdel/(fnuc+fdel)*xxdel
      fgad3 = 0  ! include 1/M^2/sqrt(2)
      fgad4 = 0

c------new part-------------------------------------------------

      ga   = 1.267d0
c      fma  = 1020.d0/fm  ! modified jan-10-2003
      fma  = famasn/scale ! read from file
      fpid = 93.d0/scale
      fgan = ga/(1.d0 - qs/fma**2)**2
      fgac = 1.d0/(1.d0 - qs/fma**2)**2/fpid/2.d0

      s2    = sqrt(2.d0)
      fgad  = xdafac*ga*6.d0/5.d0/s2  ! multiply additional factor


      qonmy  = (fdel**2 - fnuc**2)/2.d0/fdel
      enuon  = sqrt(fnuc**2 + qonmy**2)
      q2nrm = (fdel-fnuc)**2
c      q2nrm  = fdel**2 + fnuc**2 - 2.d0*fdel*enuon

      fmad   = famasd/scale
      xnrmsl = adfrm1(q2nrm,fmad,scale)

      xnrmho = adfrm2(q2nrm,fmad,scale)
c      xnrmho = 1.17d0 ! original paper

      xnrmdp = adfrm3(q2nrm,fmad,scale)
c      write(*,*)fdel*fm,fnuc*fm,q2nrmh,q2nrm
c      write(*,*)q2nrm,xnrmsl,xnrmho,xnrmdp

c
c  determine form factor
c
      if(idafrm1.eq.1) then
         xxdel = fgad / xnrmsl * adfrm1(qs,fmad,scale)
      else if(idafrm1.eq.2) then
         xxdel = fgad / xnrmho * adfrm2(qs,fmad,scale)
      else if(idafrm1.eq.3) then
         xxdel = fgad / xnrmdp * adfrm3(qs,fmad,scale)
      end if
c
c  choice of parametrization
c
      if(idafrm2.eq.1) then  !our new parametrization
      fgad1 = xxdel*(1.d0 + (fdel**2 - fnuc**2)/2.d0/(fdel+fnuc)/fnuc)
      fgad2 = xxdel*(-fnuc/2.d0/(fdel+fnuc))/fnuc**2
      fgad3 = 0
      fgad4 = 0
c     if we include pion pole explicitly here
c      fgad3 = -fgad2 - xxdel/(qs - fpio**2) 

      else if(idafrm2.eq.2) then  !holstein's original one
       fgad2 = - 1.d0/(fnuc+fdel)**2*xxdel
      fgad1 =   2.d0*fdel/(fnuc+fdel)*xxdel
      fgad3 = 0  ! include 1/M^2/sqrt(2)
      fgad4 = 0
      else if(idafrm2.eq.3) then !(old)
      fgad1 = xxdel*(1.d0 + (fdel**2 - fnuc**2)/4.d0/fdel/fnuc)
      fgad2 = xxdel*(-fnuc/4.d0/fdel)/fnuc**2
      fgad3 = -fgad2
      fgad4 = 0
      end if

c--------------------------------------------------------
c
c  nucleon form factor
c
      tau   = -qs/(4.d0*fnuc**2)
      cut   = 0.71d0/scale**2*1.d6
      gdd   = 1.d0/(1.d0 - qs/cut)**2
      fpmom = 2.79285d0    
      fnmom =  -1.91315d0   
c
      qsgev = sqrt(abs(qs)/(1000.d0/scale)**2)
      if(igfrm.eq.1) then
      gep  =  gdd
      gmp  =  fpmom*gdd
      gmn  =  fnmom*gdd
      gen  = -gmn*tau/(1.d0 + 5.6d0*tau)
      else if(igfrm.eq.2) then
      gep   = 1.d0/(1.d0 + 0.62d0*qsgev + 0.68d0*qsgev**2
     &                   + 2.80*qsgev**3 + 0.83d0*qsgev**4)
      gmp   = fpmom
     &            /(1.d0 + 0.35d0*qsgev + 2.44d0*qsgev**2
     &     + 0.5d0 *qsgev**3 + 1.04d0*qsgev**4+ 0.34d0*qsgev**5)
      gmn   =  fnmom
     &            /(1.d0 - 1.74d0*qsgev + 9.29d0*qsgev**2
     &                   - 7.63d0 *qsgev**3 + 4.63d0*qsgev**4)
      gen   = -1.25d0*fnmom*tau/(1.d0 + 18.3d0*tau)*gdd
      else if(igfrm.eq.3) then
      gep   = 1.d0/
     &       (1.d0 + 0.14d0*qsgev + 3.01d0*qsgev**2 
     &    + 0.02d0*qsgev**3 + 1.20d0*qsgev**4 + 0.32d0*qsgev**5)
      gmp   =  fpmom*gep
      gmn   =  fnmom
     &            /(1.d0 - 1.74d0*qsgev + 9.29d0*qsgev**2
     &                   - 7.63d0 *qsgev**3 + 4.63d0*qsgev**4)
      gen   = -1.25d0*fnmom*tau/(1.d0 + 18.3d0*tau)*gdd
      end if
c
      fg1n = (gen + tau*gmn) / (1.d0+tau)
      fg2n = (gmn -     gen) / (1.d0+tau)
      fg1p = (gep + tau*gmp) / (1.d0+tau)
      fg2p = (gmp -     gep) / (1.d0+tau)
      fg1v = fg1p - fg1n
      fg1s = fg1p + fg1n
      fg2v = fg2p - fg2n
      fg2s = fg2p + fg2n
c
c  delta
c
      taud   = -qs/(938.93d0+1238.d0)**2*scale**2
      tauv   = -qs*4.d0/(769.d0+782.6d0)**2*scale**2
      gdelt1 = sqrt(1.d0+taud)/(1.d0+tau)*gep
      delt   = 1200.d0/scale
      gdelt2 = sqrt(1.d0+taud)/(1.d0 - qs/4.d0/delt**2)*gep
c  our parametrization Mar-24
      betyn  = 0.154d0/1.d6*scale**2
      gamyn  = 0.166d0/1.d6*scale**2
c
      if(idfrm.eq.1) then
      gdelt3 = gdd
      else if(idfrm.eq.2) then
      gdelt3 = gdd*(1.d0 + betyn*(-qs))*exp(-gamyn*(-qs))
      end if
c
      fgpr = 1.d0/(1.d0+tauv)
      fgpo = 1.d0/(1.d0+tauv)
      fgdm = gdelt3
      fgde = gdelt3
      fgdc = gdelt3
c
      return
      end
c
c  sl model form factor
c
      real*8 function adfrm1(qs,fma,fm)
      implicit real*8(a-h,o-z)
      betyn   = 0.154d0/1.d6*fm**2
      gamyn   = 0.166d0/1.d6*fm**2
      adfrm1  = (1.d0 + betyn*(-qs))*exp(-gamyn*(-qs))
     &         /(1.d0 - qs/fma**2)**2
      return
      end
c
c  Holstein
c
      real*8 function adfrm2(qs,fma,fm)
      implicit real*8(a-h,o-z)
      xydel  = sqrt(2.d0)*1000.d0/fm
      adfrm2 = (1.d0 + 1.21d0*qs/(xydel**2 - qs))
     &         /(1.d0 - qs/fma**2)**2
      return
      end
c
c  Dipole
c
      real*8 function adfrm3(qs,fma,fm)
      implicit real*8(a-h,o-z)
      adfrm3 = 1.d0/(1.d0 - qs/fma**2)**2
      return
      end
c
c     set of i1,i2,i3,i4   ieps
c
      subroutine setieps
      common / ceps / imax,ie1(24),ie2(24),ie3(24),ie4(24),iph(24)
c
      imax     = 0
      do 10 k1 = 0,3
      do 10 k2 = 0,3
      do 10 k3 = 0,3
      do 10 k4 = 0,3
        iphx   = ieps(k1,k2,k3,k4)
        if(iphx.ne.0) then
          imax = imax + 1
          ie1(imax) = k1
          ie2(imax) = k2
          ie3(imax) = k3
          ie4(imax) = k4
          iph(imax) = iphx
        end if
   10 continue
      return
      end
c      
c
c   ieps = epsilon^{i1,i2,i3,i4}
c
c    epsilon^{0123} = 1
c
      integer function ieps(i1,i2,i3,i4)
      dimension ii(4),io(4)
c
      i12 = i1 - i2
      i13 = i1 - i3
      i14 = i1 - i4
      i23 = i2 - i3
      i24 = i2 - i4
      i34 = i3 - i4
      itot = i12*i13*i14*i23*i24*i34
      ieps = 0
      if(itot.eq.0) return
      ii(1) = i1
      ii(2) = i2
      ii(3) = i3
      ii(4) = i4
c
      iph      = 1
      do 10 jm = 0,2
        jd       = 1
        do 20 jn = 1,4-jm
          if(ii(jn).eq.jm) then
            iph    = iph*(-1)**(jn+1)
          else
            io(jd) = ii(jn)
            jd     = jd + 1
          end if
   20   continue
        do 30 jl = 1,3-jm
   30   ii(jl)   = io(jl)
   10 continue
      ieps = iph
      return
      end
c
      subroutine subsum2(c1,c2,cout)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      dimension cout(4,4),c1(4,4),c2(4,4)
      do 300 k1 = 1,4
      do 300 k2 = 1,4
        cout(k1,k2) = (0.d0,0.d0)
        do 400 k3 = 1,4
          cout(k1,k2) = cout(k1,k2) + c1(k1,k3)*c2(k3,k2)
  400   continue
  300 continue
      return
      end

c==============================================================================
c  MB-MB part mb-10-17-2006.f with correction  Dec 09/2006
c==============================================================================
c
c
c  03-06-2006 Correct inconsistent use of cg-del
c  07-24-2006 introduce fast routine of piN-> rhoN
c  08-04-2006 done rhoN->piD
c  10-17-2006 rhoN-rhoN
c  11-02-2006 modify mxpot(3), mxpot(2) -> 2 a0,f0 drop, c23 was included
c
c
c     index= 1     1/2    L = j - 1/2
c            2     1/2    L = j + 1/2
c            3     3/2    L = j - 3/2
c            4     3/2    L = j - 1/2
c            5     3/2    L = j + 1/2
c            6     3/2    L = j + 3/2
c
      subroutine convlsj(idx,jjj,jll,jss)
      implicit real*8(a-h,o-z)
      if(idx.eq.1) then
         jss = 1
         jll = jjj - 1
      else if(idx.eq.2) then
         jss = 1
         jll = jjj + 1
      else if(idx.eq.3) then
         jss = 3
         jll = jjj - 3
      else if(idx.eq.4) then
         jss = 3
         jll = jjj - 1
      else if(idx.eq.5) then
         jss = 3
         jll = jjj + 1
      else if(idx.eq.6) then
         jss = 3
         jll = jjj + 3
      end if
      return
      end
c===================================================================
c
c  new routine 2006
c
c  non resonant meson-baryon potential 10-17-2006  T.S.
c
c            1    2    3    4     5       6
c vpn2pn  1  c9   c10  c11  c12   c13    s-anti-delta
c vpn2en  2  c15  c16
c ven2en  3  c18  c19
c vpn2sn  4  c21  c22  c23                       
c ven2sn  5  c25  c26
c vsn2sn  6  c28  c29 
c vpn2rn  7  c31  c32  c33  c34   A1     c35  
c ven2rn  8  c38  c39
c vsn2rn  9  c41  c42
c vrn2rn 10  c44a c44b c46
c vpn2pd 11  c48  c49  c50  c51   c52
c ven2pd 12  c53
c vsn2pd 13  c54
c vrn2pd 14  c56  c57
c vpd2pd 15  c61            c64
c vpn2on 16  s    u                           4/22/2007
c
c
c V1  1,2,3,4,6=SL model
c# V4  c23 is not included<-- included 11-02
c V7  ignore 5 a1-exchange
c V14 not yet c58,c59  -- delta intermediate
c V15 not yet c62,c63  -- delta intermediate
c
c----------------------------------------------------------------------
c
c  input momentum and channel
c        zpf,zpi,chnf,chni=pn,en,sn,rn,pd
c
c  output potential zpot(20,6,6,3)
c     zpot(jj,idexf,idexi,iso) iso = 2 x isospin
c
c                  spin   orbital angular momentum
c                   s      L
c     index= 1     1/2    L = j - 1/2
c            2     1/2    L = j + 1/2
c            3     3/2    L = j - 3/2
c            4     3/2    L = j - 1/2
c            5     3/2    L = j + 1/2
c            6     3/2    L = j + 3/2
c
c     jj <-- 2 x j
c     js <-- 2 x s
c     jl <-- 2 x L
c
c
c    idexf/i = index(js,jl - jj)
c
c                   idexf,idxi
c     index(1,-1) = 1
c     index(1, 1) = 2
c     index(3,-3) = 3
c     index(3,-1) = 4
c     index(3, 1) = 5
c     index(3, 3) = 6
c
c=======================================================================
c-----------------------------------------------------------------
c  vertex function used SL model
c
c    (c^2/(c^2 + q^2))**m
c-----------------------------------------------------------------
      complex*16 function zvtx(zq,cut,m)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
      zvtx = (cut**2/(cut**2 + zq**2))**m
      return
      end
c------------------------------------------------------------------
      subroutine subvme(zpf,zpi,chnf,chni,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
      character chnf*2,chni*2
      dimension zpot(20,6,6,3),zpotc(20,6,6,3)


c-----------------------------------------------
      if(chni.eq.'pn') then
         if(chnf.eq.'pn') then
            call vpn2pn(zpf,zpi,zpot)
         else if(chnf.eq.'en') then
            call vpn2en(zpf,zpi,zpot)
         else if(chnf.eq.'sn') then
            call vpn2sn(zpf,zpi,zpot)
         else if(chnf.eq.'rn') then
            call vpn2rn(zpf,zpi,zpot)
         else if(chnf.eq.'pd') then
            call vpn2pd(zpf,zpi,zpot)
         else if(chnf.eq.'on') then
            call vpn2on(zpf,zpi,zpot)
         end if
c------------------------------------------------
      else if(chni.eq.'en') then
         if(chnf.eq.'pn') then
            call vpn2en(zpi,zpf,zpotc)
            fsign  = 1
            call csubvme(zpot,zpotc,fsign)
         else if(chnf.eq.'en') then
            call ven2en(zpf,zpi,zpot)
         else if(chnf.eq.'sn') then
            call ven2sn(zpf,zpi,zpot)
         else if(chnf.eq.'rn') then
            call ven2rn(zpf,zpi,zpot)
         else if(chnf.eq.'pd') then
            call ven2pd(zpf,zpi,zpot)
         end if

c------------------------------------------------
      else if(chni.eq.'sn') then
         if(chnf.eq.'pn') then
            call vpn2sn(zpi,zpf,zpotc)
            fsign  = -1
            call csubvme(zpot,zpotc,fsign)
         else if(chnf.eq.'en') then
            call ven2sn(zpi,zpf,zpotc)
            fsign  = -1
            call csubvme(zpot,zpotc,fsign)
         else if(chnf.eq.'sn') then
            call vsn2sn(zpf,zpi,zpot)
         else if(chnf.eq.'rn') then
            call vsn2rn(zpf,zpi,zpot)
         else if(chnf.eq.'pd') then
            call vsn2pd(zpf,zpi,zpot)
         end if

      else if(chni.eq.'pd') then
         if(chnf.eq.'pn') then
            call vpn2pd(zpi,zpf,zpotc)
            fsign  = 1
            call csubvme(zpot,zpotc,fsign)
         else if(chnf.eq.'en') then
            call ven2pd(zpi,zpf,zpotc)
            fsign  = 1
            call csubvme(zpot,zpotc,fsign)
         else if(chnf.eq.'sn') then
            call vsn2pd(zpi,zpf,zpotc)
            fsign  = -1
            call csubvme(zpot,zpotc,fsign)
         else if(chnf.eq.'rn') then
            call vrn2pd(zpi,zpf,zpotc)
            fsign  = -1
            call csubvme(zpot,zpotc,fsign)
         else if(chnf.eq.'pd') then
            call vpd2pd(zpf,zpi,zpot)
         end if

      else if(chni.eq.'rn') then
         if(chnf.eq.'pn') then
            call vpn2rn(zpi,zpf,zpotc)
            fsign  = -1
            call csubvme(zpot,zpotc,fsign)
         else if(chnf.eq.'en') then
            call ven2rn(zpi,zpf,zpotc)
            fsign  = -1
            call csubvme(zpot,zpotc,fsign)
         else if(chnf.eq.'sn') then
            call vsn2rn(zpi,zpf,zpotc)
            fsign  = 1
            call csubvme(zpot,zpotc,fsign)
         else if(chnf.eq.'rn') then
            call vrn2rn(zpf,zpi,zpot)
         else if(chnf.eq.'pd') then
            call vrn2pd(zpf,zpi,zpot)
         end if

      end if

      return
      end

      subroutine csubvme(zpot,zpotc,fsign)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
      dimension zpot(20,6,6,3),zpotc(20,6,6,3)
      do jx = 1,20
      do i1 = 1,6
      do i2 = 1,6
      do i3 = 1,3
      zpot(jx,i2,i1,i3) = fsign*zpotc(jx,i1,i2,i3)
      end do
      end do
      end do
      end do

      return
      end
c------------------------------------------------------------
c  potential  pi N -> omega N
c  
c  1 s- nucleon
c  2 u- nucleon
c  3 t- rho  not yet
c                             16
c-------------------------------------------------------------------
      subroutine vpn2on(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-------------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)

      common / cefflo / cg1h(-2:2,-1:1,3)
     &     ,xef(20,6,-2:2,-3:3,10)
      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
     &     ,jss(20,6,10),jll(20,6,10)
     &     ,jip(20,6,10)


      dimension zpot(20,6,6,3)
      dimension zxfa(0:1,0:1,10),zxga(0:1,0:1,10),zxha(0:1,0:1,10)
      dimension zxfb(0:1,0:1,3),zxgb(0:1,0:1,3),zxhb(0:1,0:1,3)
      dimension zxia(0:1,0:1,10),zxib(0:1,0:1,3)
      dimension zvme(20,-2:2,-1:1,-1:1,3)
c--------------------------------------------------------------

c
      ich    = 16
      mxl    = 10
      mxp    = 2
      fiso(1,16,1) = -sqrt(3.d0)  ! s
      fiso(3,16,1) = 0
      fiso(1,16,2) = -sqrt(3.d0)  ! u
      fiso(3,16,2) = 0

ctest
c      ich    = 7

      zi     = (0.d0,1.d0)
      pi2    = 2.d0*pi
      fmi    = fpio
      fbi    = fnuc
      fmf    = fmomg
ctest      fmf    = fmrho
      fbf    = fnuc
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2
      zqfi   = zqf*zqi
      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi

      zsip  = zwi + fnuc
      zsim  = zwi - fnuc
      zsfp  = zwf + fnuc
      zsfm  = zwf - fnuc
      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

c------------- use all same vertex function ----------------
      zvrta =  zvtx(zqf,vnnomg,mnnomg)*zvtx(zqi,vnnpi,mnnpi)
      zvrtb =  zvtx(zqf,vnnomg,mnnomg)*zvtx(zqi,vnnpi,mnnpi)
      zffa     = zi*gonn*gpin/fpio*zfac*zvrta*swv(ich,1)
      zffb     = zi*gonn*gpin/fpio*zfac*zvrtb*swv(ich,2)
c------------------------------------------------------------
ctest
c      zvrta =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnpi,mnnpi)
c      zvrtb =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnpi,mnnpi)
c      zffa     = zi*grnn*gpin/fpio/2.d0*zfac*zvrta*swv(ich,1)
c      zffb     = zi*grnn*gpin/fpio/2.d0*zfac*zvrtb*swv(ich,2)
ctest
c-------------------------------------------------------------

      zxfa = 0
      zxga = 0
      zxha = 0
      zxia = 0

      xkx  = xkomg/(2.d0*fnuc)
ctest      xkx  = xkrho/(2.d0*fnuc)
c
c s-channel nucleon exchange
c
      zalf = 1.d0 - xkx*zsfm
      zgam = 1.d0 + xkx*zsfp
      zbet = (zsip**2/(zwi**2-fnuc**2)+zsip*zsfp/(zwf**2-fnuc**2))/2.d0
      zdel = (zsim**2/(zwi**2-fnuc**2)+zsim*zsfm/(zwf**2-fnuc**2))/2.d0
      zxfa(0,1,1)=-zdi*zalf*zbet*zffa
      zxfa(1,0,1)=-zdf*zgam*zdel*zffa
      zxha(0,0,1)=-    zalf*zdel*zffa
      zxha(1,1,1)=-zdf*zdi*zgam*zbet*zffa
c
c u-channel nucleon exchange
c
      zalf1 = (1 - xkx*(fnuc-zwf))*zffb
      zbet1 = (1 - xkx*(fnuc+zwf))*zffb
      zf101 = zdi*zalf1
      zf110 = zdf*zbet1
      zh100 =         zalf1
      zh111 = zdi*zdf*zbet1

      zalf2 = (1 + xkx*(zebf-zemf-fnuc))*zffb
      zbet2 = (1 - xkx*(zebf-zemf+fnuc))*zffb
      zgam2a= zebi-zemi+fnuc
      zgam2b= zebi-zemi-fnuc
      zdel2a= zebi+zemi+fnuc
      zdel2b= zebi+zemi-fnuc
      zf201 = zdi*zgam2a*zalf2
      zf210 = zdf*zgam2b*zbet2
      zg201 = zdi*2.d0*zqi*zalf2
      zg210 = zdf*2.d0*zqi*zbet2
      zh200 =-zdel2b*zalf2
      zh211 =-zdi*zdf*zdel2a*zbet2

      zalf3 = (1.d0 -xkx*(zwf-fnuc))*zffb
      zbet3 = (1.d0 +xkx*(zwf+fnuc))*zffb
      zgam3a= zgam2a
      zgam3b= zgam2b
      zdel3a= zdel2b
      zdel3b= zdel2a
      zf301 = zdi*zgam3a*zalf3
      zf310 =-zdf*zgam3b*zbet3
      zg301 = zdi*2.d0*zqi*zalf3
      zg310 =-zdf*2.d0*zqi*zbet3
      zh300 =        -zdel3a*zalf3
      zh311 = zdi*zdf*zdel3b*zbet3
c  contact term
c      zxfa(0,1,4) = -zdi*zffd
c      zxfa(1,0,4) = -zdf*zffd
c      zxha(0,0,4) = -    zffd
c      zxha(1,1,4) = -zdi*zdf*zffd
      
      zxkxb = xkx*zffb

c
c  angular projection
c

      zvme = 0
      zpot = 0

      do ix = 1,mxx
      www   = wgau(ix)
      cc    = xgau(ix)
      ss    = sqrt(1.d0 -  cc**2)
      c2    = sqrt((1.d0 + cc)/2.d0)
      s2    = sqrt((1.d0 - cc)/2.d0)
      zqfix = zqfi*cc
      zkky  = zemf*zemi - zqfix

c------------------------------------------------------------
      zqx2  =  zqf2 + zqi2 - zqfix*2.d0
      zqx   =  sqrt(zqx2)
      zvrtc =  zvtx(zqx,vnnpi,mnnpi)*zvtx(zqx,vrpp ,mrpp )  *swv(ich,3)
      zvrte =  zvtx(zqx,vnna1 ,mnna1 )*zvtx(zqx,vnnpi,mnnpi)*swv(ich,5)
      zvrtf =  zvtx(zqx,vnnomg,mnnomg)*zvtx(zqx,vopr ,mopr )*swv(ich,6)
      zffc  =-zi*grpp*gpin/fpio     *zfac*zvrtc
      zffe  = zi*grnn*gpin/fpio     *zfac*zvrte
      zfff  =    gonn*gopr/fmomg    *zfac*zvrtf
c------------------------------------------------------------
c        u-channel nucleon exchange
      zkkx  = 2.d0*(zemf*zemi - zqfix)
      zuu   = 1.d0/(fmf2 - 2.d0*(zebi*zemf+zqfix))
      zuup  = 1.d0/(fmi2 - 2.d0*(zebf*zemi+zqfix))
      za1   = -(zwi-zwf)*zemi*zuu + (1.d0+zuu/zuup)/2.d0
      za2   = -(zwi-zwf)*zuu/2.d0
      za3   = -(zuu + zuup)*fnuc
      zxfa(0,1,2) = za1*zf101+za2*(zf201-zxkxb*zkkx*zdi)
     &                       +za3*(zf301-zxkxb*zkkx*zdi)
      zxfa(1,0,2) = za1*zf110+za2*(zf210+zxkxb*zkkx*zdf)
     &                       +za3*(zf310-zxkxb*zkkx*zdf)
      zxga(0,1,2) =           za2*zg201+za3*zg301
      zxga(1,0,2) =           za2*zg210+za3*zg310
      zxha(0,0,2) = za1*zh100+za2*(zh200-zxkxb*zkkx)
     &                       +za3*(zh300-zxkxb*zkkx)
      zxha(1,1,2) = za1*zh111+za2*(zh211+zxkxb*zkkx*zdi*zdf)
     &                       +za3*(zh311-zxkxb*zkkx*zdi*zdf)
c  t-channel pion exchange
c      zpion       = -2.d0*fnuc/((zebi-zebf)**2 - zqx2 - fpio**2)*zffc
c      ztmp        = zebi - zemi - zwf
c      zxfa(0,1,3) =-zpion*ztmp*zdi
c      zxfa(1,0,3) = zpion*ztmp*zdf
c      zxga(0,1,3) =-zpion*zqi*zdi*2.d0
c      zxga(1,0,3) = zpion*zqi*zdf*2.d0
c  t-channel omega exchange
c      zomeg1= 1.d0/((zebi-zebf)**2 - zqx2 - fmomg**2)
c      zomeg2= 1.d0/((zemi-zemf)**2 - zqx2 - fmomg**2)
c      za1   =-xkxo*((zwi+zwf)*zomeg1+2.d0*zwi*zomeg2)
c      za2   = (1.d0+xkomg)/2.d0*(zomeg1+zomeg2)
c      za3   = xkxo*(zwi-zwf)*zomeg2
c      za23p = (za2+za3)*zdi
c      za23m = (za2-za3)*zdf
c      zfx   = zfff*zi
c      zxia(0,0,6)=-zfx*zqfi*(za1+za2+za3)
c      zxia(1,1,6)=-zfx*zqfi*zdi*zdf*(za2-za3-za1)
c      zxfa(0,1,6)=-zfx*(zkky*za23p+zqfi*za23m)
c      zxfa(1,0,6)=-zfx*(zkky*za23m+zqfi*za23p)
c      zxga(0,1,6)=-zfx*zqi*zemf*za23p
c      zxga(1,0,6)=-zfx*zqi*zemf*za23m
c      zxha(0,0,6)=+zfx*(zemf*zqi*za23p+zemi*zqf*za23m)
c      zxha(1,1,6)=+zfx*(zemi*zqf*za23p+zemf*zqi*za23m)
c---------------------------------------------------------------

      do ix1= 0,1
      do ix2= 0,1
      do iso= 1,3,2
      iy1   = ih1(ix1,ix2)
      iy2   = ih2(ix1,ix2)
      zxfb(ix1,ix2,iso) = 0
      zxgb(ix1,ix2,iso) = 0
      zxhb(ix1,ix2,iso) = 0
      zxib(ix1,ix2,iso) = 0
      do ic = 1,mxp
      xiso  = fiso(iso,ich,ic)
      zxfb(ix1,ix2,iso)=zxfb(ix1,ix2,iso)+
     &(zqf*zxfa(ix1,ix2,ic)+zemf*(cc*zxga(ix1,ix2,ic)+zxha(iy1,iy2,ic)))
     & /fmf*xiso
c  correct 4/22/2007
      zxgb(ix1,ix2,iso) = zxgb(ix1,ix2,iso)+
     &                    zxga(ix1,ix2,ic)*ss/sqrt(2.d0)*xiso
      zxhb(ix1,ix2,iso) = zxhb(ix1,ix2,iso)-
     &                    zxha(ix1,ix2,ic)*sqrt(2.d0)*xiso
      zxib(ix1,ix2,iso) = zxib(ix1,ix2,iso)+
     &                    zxia(ix1,ix2,ic)*ss/sqrt(2.d0)*xiso
      end do

      end do
      end do
      end do


      do imf = -1,1,2
      do imi = -1,1,2
      do iso = 1,3,2
      dhlf   = dfun(1,imi,imf,ix)*www
      zxf    =zxfb(0,0,iso)     +zxfb(0,1,iso)*imi
     &       +zxfb(1,0,iso)*imf+zxfb(1,1,iso)*imf*imi
      zxg    =zxgb(0,0,iso)    +zxgb(0,1,iso)*imi
     &       +zxgb(1,0,iso)*imf+zxgb(1,1,iso)*imf*imi
      zxh    =zxhb(0,0,iso)    +zxhb(0,1,iso)*imi
     &       +zxhb(1,0,iso)*imf+zxhb(1,1,iso)*imf*imi
      zxi    =zxib(0,0,iso)    +zxib(0,1,iso)*imi
     &       +zxib(1,0,iso)*imf+zxib(1,1,iso)*imf*imi
      zz0   = dhlf*zxf
      zzp   = dhlf*zxg
      zzm   =-zzp
      zzz   = dhlf*zxi
      if(imf.eq.-1) then
      zzp = zzp + dfun(1,imi,1,ix)*www*zxh
      else if(imf.eq.1) then
      zzm = zzm - dfun(1,imi,-1,ix)*www*zxh
      end if

      do jx = 1,mxj,2
      zvme(jx, 2,imf,imi,iso)=zvme(jx, 2,imf,imi,iso)
     &                        +dfun(jx,imi,imf+2,ix)*(zzp+zzz)
      zvme(jx, 0,imf,imi,iso)=zvme(jx, 0,imf,imi,iso)
     &                        +dfun(jx,imi,imf,ix)  *zz0
      zvme(jx,-2,imf,imi,iso)=zvme(jx,-2,imf,imi,iso)
     &                        +dfun(jx,imi,imf-2,ix)*(zzm+zzz)
      end do ! jx

      end do ! iso
      end do ! imf
      end do ! imi

      end do ! ix cos

c
c           lsj scheme
c
      icci = 1
      iccf = 5
      zpot = 0
      imiz = 0
      do 200 jjx = 1,mxj,2

      do 300 idxf= 1,6
      do 300 idxi= 1,6
      ipxi  = jip(jjx,idxi,icci)
      ipxf  = jip(jjx,idxf,iccf)
      iptest = ipxi*ipxf
      if(iptest.eq.1) then

      do 210 imfz= -2,2,2
      do 210 ibfz= -1,1,2
      xxf = xef(jjx,idxf,imfz,ibfz,iccf)

      if(abs(xxf).gt.1.d-20)then

      do 220 ibiz= -1,1,2
      www = xef(jjx,idxi,imiz,ibiz,icci)*xxf

      if(abs(www).gt.1.d-20) then
      do iso = 1,3
      zpot(jjx,idxf,idxi,iso) = zpot(jjx,idxf,idxi,iso)
     &         + www*zvme(jjx,imfz,ibfz,ibiz,iso)
      end do
      end if

 220  continue

      end if

 210  continue

      end if  ! parity test

 300  continue
 200  continue

      return
      end
c-------------------------------------------------------------
c
c  potential 1  pi-N -> pi N
c
c  1 s- nucleon
c  2 u- nucleon
c  3 u- delta
c  4 t- rho 
c  5 t- sigma
c  6 s- anti-delta
c
      subroutine vpn2pn(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-----------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
cccc  fleg mesh = mxx, maxl = 10

c
c  zvme(L,I,J),    L prbital angular momentum
c                  I type of diagram
c                  J = 1 F00, 2 F01,  3 F10,  4 F11
c
      common / cswv  / swv(20,20)
      dimension zvme(0:20,10,4),zpot(20,6,6,3)

      mxl    = 10
      ich    = 1      ! piN-piN
      mxp    = mxpot(ich)

      pi2    = 2.d0*pi
      fmi    = fpio
      fbi    = fnuc
      fmf    = fpio
      fbf    = fnuc
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2

      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi

      frhos  = frhoe**2
      zsip  = zwi + fnuc
      zsim  = zwi - fnuc
      zsfp  = zwf + fnuc
      zsfm  = zwf - fnuc
      zenum   = zebf - zebi
      zenup   = zebf + zebi
      zopim   = zemf    - zemi
      zopip   = zemf    + zemi

      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

      zvrta =  zvtx(zqi,vnnpi,mnnpi)*zvtx(zqf,vnnpi,mnnpi)
      zvrtb =  zvtx(zqi,vnnpi,mndpi)*zvtx(zqf,vnnpi,mnnpi)
      zvrtc =  zvtx(zqi,vndpi,mndpi)*zvtx(zqf,vndpi,mndpi)
      zvrtf =  zvtx(zqi,vndpi,mndpi)*zvtx(zqf,vndpi,mndpi)

      zffa     = gpin**2/fpio**2  *zfac*zvrta*swv(ich,1)
      zffb     = gpin**2/fpio**2  *zfac*zvrtb*swv(ich,2)
      zffc     = gpind**2/fpio**2 *zfac*zvrtc*swv(ich,3)
      zfff     = gpind**2/fpio**2 *zfac*zvrtf*swv(ich,6)

c------------------------------------------------------

      zpot = 0
      zvme = 0

c-------------------------------------------------------
c   nucleon direct
c
      zvme(0,1,1) = zffa*(zwi - fnuc)*(zwf - fnuc)*
     &                   (1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))
      zvme(0,1,4) = zffa*(zwi + fnuc)*(zwf + fnuc)*zdi*zdf*
     &                   (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))

      zqfqi  = zqf *zqi
      zemfi = zemf*zemi

      do ix = 1,mxx
      xxx   = xgau(ix)
      www   = wgau(ix)

      zqfix = zqfqi*xxx
      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)
      zvrtd =  zvtx(zqx,vrpp ,mrpp )*zvtx(zqx,vnnrho,mnnrho)
      zvrte =  zvtx(zqx,vnnsi,mnnsi)*zvtx(zqx,vsipp,msipp)
      zffd     =-grnn*grpp/2.d0   *zfac*zvrtd*swv(ich,4)
      zffe     =-gsinn*gsipp/fpio*zfac*zvrte*swv(ich,5)
c
c   nucleon exchange
c
      zuu   = fmf2 - 2.d0*(zebi*zemf + zqfix)
      zxa00 = - fnuc*(zemfi - 2.d0*zqfix)
      zxa11 = - fnuc*zqfqi
      zxb00 = zwi*(zemfi + 2.d0*zqfix) -  fmi2*zemf - fmf2*zemi
      zxb11 = - zwi* zqfqi
      zxff  = - zwi*zemf + fmf2
      zxfp  = - zwi*zemi + fmi2
      zxee  = - fnuc*zemf
      zxep  =   fnuc*zemi

      ztmp1 = zxa00 + zxb00 - zdi*zdf*(zxa11-zxb11)
     &       -zqi*zdi*(zxee + zxff) + zqf*zdf*(zxep - zxfp)
      ztmp4 = zxa11 + zxb11 - zdi*zdf*(zxa00-zxb00)
     &       -zqf*zdi*(zxep + zxfp) + zqi*zdf*(zxee - zxff)
      ztmp1a = ztmp1*zffb*www/zuu/2.d0
      ztmp4a = ztmp4*zffb*www/zuu/2.d0

      zup   = fmi2 - 2.d0*(zebf*zemi + zqfix)
      zxb00 = zwf*(zemfi + 2.d0*zqfix) -  fmi2*zemf - fmf2*zemi
      zxb11 = - zwf* zqfqi
      zxff  = - zwf*zemf + fmf2
      zxfp  = - zwf*zemi + fmi2

      ztmp1 = zxa00 + zxb00 - zdi*zdf*(zxa11-zxb11)
     &       -zqi*zdi*(zxee + zxff) + zqf*zdf*(zxep - zxfp)
      ztmp4 = zxa11 + zxb11 - zdi*zdf*(zxa00-zxb00)
     &       -zqf*zdi*(zxep + zxfp) + zqi*zdf*(zxee - zxff)
      ztmp1b = ztmp1*zffb*www/zup/2.d0
      ztmp4b = ztmp4*zffb*www/zup/2.d0

      ztmp11 = ztmp1a + ztmp1b
      ztmp44 = ztmp4a + ztmp4b
c
c  rho meson
c
c------------------------------------------
      zqfi    = zemf*zemi - zqfix
      zqfin   = zebf*zebi - zqfix
      
      zenum2  = zebf**2 - zebi**2
      zfrap1  = zenup + xkrho*zopip + zenum*zenum2/frhos
      zfrap2  = - zopip/frhos
      zfram1  = (zenum*zopip-zenum2)/frhos
      zfrbp1  = -2.d0*fnuc-xkrho/2.d0/fnuc*zopip*zenup
      zfrbp2  = -xkrho/fnuc
      zfrbm1  = -xkrho/2.d0/fnuc*zenum
      zfrcp1  =  xkrho/fnuc*zqi*zqf
      zzr1   = 1.d0/(2.d0*fpio**2 - frhos - 2.d0*zqfi)/2.d0
      zzr2   = 1.d0/(2.d0*fnuc**2 - frhos - 2.d0*zqfin)/2.d0
      zzrp   = zzr1 + zzr2
      zzrm   = -zzr1*zopim + zzr2*zenum
      zhpot4a  = zzrp*(zfrap1 +zfrap2*zqx**2)+ zzrm*zfram1 +zfrap2
      zhpot4b  = zzrp*(zfrbp1 +zfrbp2*zqfix )+ zzrm*zfrbm1
      zhpot4c  = zzrp* zfrcp1

      zhpot41  = zffd*(zhpot4a+zhpot4b-zdi*zdf*zhpot4c)
      zhpot42  = zffd*(zdi*zdf*(zhpot4a-zhpot4b)+zhpot4c)
c
c    new delta cross
c
      zxp01        =  zebi - zemf
      zxp02        =  zebf - zemi
      zpiqi        =  zebi*zemi + zqi**2
      zpiqf        =  zebi*zemf + zqfix
      zpfqi        =  zebf*zemi + zqfix
      zpfqf        =  zebf*zemf + zqf**2
c
      zalf11      =  (zpiqi - zqfi    )/fdel - fnuc
      zalf12      =  (zpiqf - fpio**2)/fdel - fnuc
      zalf21      =  (zpfqi - fpio**2)/fdel - fnuc
      zalf22      =  (zpfqf - zqfi    )/fdel - fnuc
      zdd1        =  3.d0*(fnuc**2+fpio**2-fdel**2-2.d0*zpiqf)
      zdd2        =  3.d0*(fnuc**2+fpio**2-fdel**2-2.d0*zpfqi)
      zbpa1        = (zwi+zalf11)*(zwf+zalf12)*(zxp01-fdel)
     &             +(zwi+zalf11)*(zwf-zalf12)*(zebf-fnuc)
     &             +(zwi-zalf11)*(zwf+zalf12)*(zebi-fnuc)
     &             +(-zqfi+(zalf11+fnuc)*(zalf12+fnuc))*
     &              (zxp01-zebf-zebi+2.d0*fnuc+fdel)
      zbma1        = (zwi-zalf11)*(zwf-zalf12)*(zxp01+fdel)
     &             +(zwi-zalf11)*(zwf+zalf12)*(zebf+fnuc)
     &             +(zwi+zalf11)*(zwf-zalf12)*(zebi+fnuc)
     &             +(-zqfi+(zalf11+fnuc)*(zalf12+fnuc))*
     &              (zxp01-zebf-zebi-2.d0*fnuc-fdel)
      zbpa2        = (zwi+zalf21)*(zwf+zalf22)*(zxp02-fdel)
     &             +(zwi+zalf21)*(zwf-zalf22)*(zebf-fnuc)
     &             +(zwi-zalf21)*(zwf+zalf22)*(zebi-fnuc)
     &             +(-zqfi+(zalf21+fnuc)*(zalf22+fnuc))*
     &              (zxp02-zebf-zebi+2.d0*fnuc+fdel)
      zbma2        = (zwi-zalf21)*(zwf-zalf22)*(zxp02+fdel)
     &             +(zwi-zalf21)*(zwf+zalf22)*(zebf+fnuc)
     &             +(zwi+zalf21)*(zwf-zalf22)*(zebi+fnuc)
     &             +(-zqfi+(zalf21+fnuc)*(zalf22+fnuc))*
     &              (zxp02-zebf-zebi-2.d0*fnuc-fdel)
c
c    delta  cross
c
      zhpot31  =  zffc*(zbpa1/zdd1+zbpa2/zdd2)/2.d0
      zhpot32  =  zffc*(zbma1/zdd1+zbma2/zdd2)/2.d0*zdi*zdf

c
c   delta direct
c
      zhnewd   = 1.d0/6.d0*zqi*zqf*zdi*zdf
     &         *(1.d0/(fdel+zwi)+1.d0/(zwf+fdel))
      zhpot61  = zfff*(
     &    zemf*zemi/3.d0/fdel**2*(zwf+zwi+2.d0*fdel)
     &  - (zqi**2*zemf/(zebi+fnuc)+ zemi*zqf**2/(zebf+fnuc))/3.d0/fdel
     &  - zhnewd)
c
      zhpot62  = zfff*(
     &      zemf*zemi/3.d0/fdel**2*(zwf+zwi-2.d0*fdel)*zdi*zdf
     &    +(zemf/(zebf+fnuc)+zemi/(zebi+fnuc))*zqi*zqf/3.d0/fdel
     &    + zhnewd*3.d0*xxx)
c
c  sigma
c
      zzss1   = 1.d0/(2.d0*fpio**2 - fsigme**2 - 2.d0*zqfi )/2.d0
      zzss2   = 1.d0/(2.d0*fnuc**2 - fsigme**2 - 2.d0*zqfin)/2.d0
      zhpot51 = zffe*zqfi*(zzss1 + zzss2)
      zhpot52 =-zhpot51*zdi*zdf
c
      do il = 0,mxl
      flee  = fleg(il,ix)
      zvme(il,2,1) = zvme(il,2,1) + flee * ztmp11
      zvme(il,2,4) = zvme(il,2,4) + flee * ztmp44

      zvme(il,3,1) = zvme(il,3,1) + flee * zhpot31*www
      zvme(il,3,4) = zvme(il,3,4) + flee * zhpot32*www

      zvme(il,4,1) = zvme(il,4,1) + flee * zhpot41*www
      zvme(il,4,4) = zvme(il,4,4) + flee * zhpot42*www

      zvme(il,5,1) = zvme(il,5,1) + flee * zhpot51*www
      zvme(il,5,4) = zvme(il,5,4) + flee * zhpot52*www

      zvme(il,6,1) = zvme(il,6,1) + flee * zhpot61*www
      zvme(il,6,4) = zvme(il,6,4) + flee * zhpot62*www

      end do

      end do

c
c  lx = 1 l=l'=j-1/2  -> L1 = j - 1/2   L2 = j + 1/2
c  lx = 2 l=l'=j+1/2  -> L1 = j + 1/2   L2 = j - 1/2
c
c
      do jx = 1,mxj,2
      do lx = 1,2

       iss  = (-1)**lx
       l1   = (jx + iss)/2
       l2   = (jx - iss)/2

      
      do iso = 1,3,2
      zsum   = 0
      do ip  = 1,mxp
      ztmp   = (zvme(l1,ip,1)+zvme(l2,ip,4))*fiso(iso,ich,ip)
      zsum   = zsum + ztmp
      end do
      zpot(jx,lx,lx,iso) = zsum

      end do
      end do
      end do

      return
      end
c------------------------------------------------------------
c  potential 2 pi N -> eta N
c  
c  1 s- nucleon
c  2 u- nucleon
c
      subroutine vpn2en(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-----------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)
      dimension zvme(0:20,10,4),zpot(20,6,6,3)

      mxl    = 10
      ich    = 2      ! piN-enN
      mxp    = mxpot(ich)

      pi2    = 2.d0*pi
      fmi    = fpio
      fbi    = fnuc
      fmf    = feta
      fbf    = fnuc
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2

      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi

      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

      zvrta =  zvtx(zqf,vnnet,mnnet)*zvtx(zqi,vnnpi,mnnpi)
      zvrtb =  zvtx(zqf,vnnet,mnnet)*zvtx(zqi,vnnpi,mnnpi)
      zffa     = genn*gpin/fpio/feta*zfac*zvrta*swv(ich,1)
      zffb     = genn*gpin/fpio/feta*zfac*zvrtb*swv(ich,2)

c------------------------------------------------------


      zpot = 0
      zvme = 0

c-------------------------------------------------------
c   nucleon direct
c
      zvme(0,1,1) = zffa*(zwi - fnuc)*(zwf - fnuc)*
     &                   (1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))
      zvme(0,1,4) = zffa*(zwi + fnuc)*(zwf + fnuc)*zdi*zdf*
     &                   (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))

      zqfi  = zqf *zqi
      zemfi = zemf*zemi

      do ix = 1,mxx
      xxx   = xgau(ix)
      www   = wgau(ix)

      zqfix = zqfi*xxx
      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)
c
c   nucleon exchange
c
      zuu   = fmf2 - 2.d0*(zebi*zemf + zqfix)
      zxa00 = - fnuc*(zemfi - 2.d0*zqfix)
      zxa11 = - fnuc*zqfi
      zxb00 = zwi*(zemfi + 2.d0*zqfix) -  fmi2*zemf - fmf2*zemi
      zxb11 = - zwi* zqfi
      zxff  = - zwi*zemf + fmf2
      zxfp  = - zwi*zemi + fmi2
      zxee  = - fnuc*zemf
      zxep  =   fnuc*zemi

      ztmp1 = zxa00 + zxb00 - zdi*zdf*(zxa11-zxb11)
     &       -zqi*zdi*(zxee + zxff) + zqf*zdf*(zxep - zxfp)
      ztmp4 = zxa11 + zxb11 - zdi*zdf*(zxa00-zxb00)
     &       -zqf*zdi*(zxep + zxfp) + zqi*zdf*(zxee - zxff)
      ztmp1a = ztmp1*zffb*www/zuu/2.d0
      ztmp4a = ztmp4*zffb*www/zuu/2.d0

      zup   = fmi2 - 2.d0*(zebf*zemi + zqfix)
      zxb00 = zwf*(zemfi + 2.d0*zqfix) -  fmi2*zemf - fmf2*zemi
      zxb11 = - zwf* zqfi
      zxff  = - zwf*zemf + fmf2
      zxfp  = - zwf*zemi + fmi2

      ztmp1 = zxa00 + zxb00 - zdi*zdf*(zxa11-zxb11)
     &       -zqi*zdi*(zxee + zxff) + zqf*zdf*(zxep - zxfp)
      ztmp4 = zxa11 + zxb11 - zdi*zdf*(zxa00-zxb00)
     &       -zqf*zdi*(zxep + zxfp) + zqi*zdf*(zxee - zxff)
      ztmp1b = ztmp1*zffb*www/zup/2.d0
      ztmp4b = ztmp4*zffb*www/zup/2.d0

      ztmp11 = ztmp1a + ztmp1b
      ztmp44 = ztmp4a + ztmp4b

      do il = 0,mxl
      flee  = fleg(il,ix)
      zvme(il,2,1) = zvme(il,2,1) + flee * ztmp11
      zvme(il,2,4) = zvme(il,2,4) + flee * ztmp44
      end do

      end do

c

      do jx = 1,mxj,2
      do lx = 1,2
       iss  = (-1)**lx
       l1   = (jx + iss)/2
       l2   = (jx - iss)/2
      do iso = 1,3,2
      zsum   = 0
      do ip  = 1,mxp
      ztmp   = (zvme(l1,ip,1)+zvme(l2,ip,4))*fiso(iso,ich,ip)
      zsum   = zsum + ztmp
      end do
      zpot(jx,lx,lx,iso) = zsum
      end do
      end do
      end do

      return
      end
c------------------------------------------------------------
c  potential 3 eta N -> eta N
c  
c  1 s- nucleon
c  2 u- nucleon
c
      subroutine ven2en(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-----------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)
      dimension zvme(0:20,10,4),zpot(20,6,6,3)

      mxl    = 10
      ich    = 3      ! piN-enN
      mxp    = mxpot(ich)

      pi2    = 2.d0*pi
      fmi    = feta
      fbi    = fnuc
      fmf    = feta
      fbf    = fnuc
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2

      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi

      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

      zvrta =  zvtx(zqf,vnnet,mnnet)*zvtx(zqi,vnnet,mnnet)
      zvrtb =  zvtx(zqf,vnnet,mnnet)*zvtx(zqi,vnnet,mnnet)
      zffa     = (genn/feta)**2*zfac*zvrta*swv(ich,1)
      zffb     = (genn/feta)**2*zfac*zvrtb*swv(ich,2)
c------------------------------------------------------

      zvme = 0
      zpot = 0

c-------------------------------------------------------
c   nucleon direct
c
      zvme(0,1,1) = zffa*(zwi - fnuc)*(zwf - fnuc)*
     &                   (1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))
      zvme(0,1,4) = zffa*(zwi + fnuc)*(zwf + fnuc)*zdi*zdf*
     &                   (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))

      zqfi  = zqf *zqi
      zemfi = zemf*zemi

      do ix = 1,mxx
      xxx   = xgau(ix)
      www   = wgau(ix)

      zqfix = zqfi*xxx
      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)
c
c   nucleon exchange
c
      zuu   = fmf2 - 2.d0*(zebi*zemf + zqfix)
      zxa00 = - fnuc*(zemfi - 2.d0*zqfix)
      zxa11 = - fnuc*zqfi
      zxb00 = zwi*(zemfi + 2.d0*zqfix) -  fmi2*zemf - fmf2*zemi
      zxb11 = - zwi* zqfi
      zxff  = - zwi*zemf + fmf2
      zxfp  = - zwi*zemi + fmi2
      zxee  = - fnuc*zemf
      zxep  =   fnuc*zemi

      ztmp1 = zxa00 + zxb00 - zdi*zdf*(zxa11-zxb11)
     &       -zqi*zdi*(zxee + zxff) + zqf*zdf*(zxep - zxfp)
      ztmp4 = zxa11 + zxb11 - zdi*zdf*(zxa00-zxb00)
     &       -zqf*zdi*(zxep + zxfp) + zqi*zdf*(zxee - zxff)
      ztmp1a = ztmp1*zffb*www/zuu/2.d0
      ztmp4a = ztmp4*zffb*www/zuu/2.d0

      zup   = fmi2 - 2.d0*(zebf*zemi + zqfix)
      zxb00 = zwf*(zemfi + 2.d0*zqfix) -  fmi2*zemf - fmf2*zemi
      zxb11 = - zwf* zqfi
      zxff  = - zwf*zemf + fmf2
      zxfp  = - zwf*zemi + fmi2

      ztmp1 = zxa00 + zxb00 - zdi*zdf*(zxa11-zxb11)
     &       -zqi*zdi*(zxee + zxff) + zqf*zdf*(zxep - zxfp)
      ztmp4 = zxa11 + zxb11 - zdi*zdf*(zxa00-zxb00)
     &       -zqf*zdi*(zxep + zxfp) + zqi*zdf*(zxee - zxff)
      ztmp1b = ztmp1*zffb*www/zup/2.d0
      ztmp4b = ztmp4*zffb*www/zup/2.d0

      ztmp11 = ztmp1a + ztmp1b
      ztmp44 = ztmp4a + ztmp4b

      do il = 0,mxl
      flee  = fleg(il,ix)
      zvme(il,2,1) = zvme(il,2,1) + flee * ztmp11
      zvme(il,2,4) = zvme(il,2,4) + flee * ztmp44
      end do

      end do
c

      do jx = 1,mxj,2
      do lx = 1,2
       iss  = (-1)**lx
       l1   = (jx + iss)/2
       l2   = (jx - iss)/2
      do iso = 1,3,2
      zsum   = 0
      do ip  = 1,mxp
      ztmp   = (zvme(l1,ip,1)+zvme(l2,ip,4))*fiso(iso,ich,ip)
      zsum   = zsum + ztmp
      end do
      zpot(jx,lx,lx,iso) = zsum
      end do
      end do
      end do

      return
      end
c------------------------------------------------------------
c  potential 4 pi N -> sigma N
c  
c  1 s- nucleon
c  2 u- nucleon
c
      subroutine vpn2sn(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-----------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)
      dimension zvme(0:20,10,4),zpot(20,6,6,3)

      mxl    = 10
      ich    = 4      ! piN-sigamN
      mxp    = mxpot(ich)
      zi     = (0.d0,1.d0)
      pi2    = 2.d0*pi

      fmi    = fpio
      fbi    = fnuc
      fmf    = fsigm
      fbf    = fnuc
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2

      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi

      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

      zvrta =  zvtx(zqf,vnnsi,mnnsi)*zvtx(zqi,vnnpi,mnnpi)
      zvrtb =  zvtx(zqf,vnnsi,mnnsi)*zvtx(zqi,vnnpi,mnnpi)
      zffa  = zi*gsinn*gpin/fpio   *zfac*zvrta*swv(ich,1)
      zffb  = zi*gsinn*gpin/fpio   *zfac*zvrtb*swv(ich,2)

c------------------------------------------------------

      zvme = 0
      zpot = 0

c-------------------------------------------------------
c   nucleon direct   cc  2-> f01,   3-> f10
      zvme(0,1,2) =-zffa*(zwi + fnuc)*zdi*
     &                   (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))
      zvme(0,1,3) = zffa*(zwi - fnuc)*zdf*
     &                   (1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))

      zqfi  = zqf *zqi
      zemfi = zemf*zemi
      zxxx1 = (zebi-zebf)*zemi+zqi**2
      zxxx2 = (zebi-zebf)**2

      do ix = 1,mxx
      xxx   = xgau(ix)
      www   = wgau(ix)
      zqfix = zqfi*xxx
      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)

      zvrtc = zvtx(zqx,vnnpi,mnnpi)*zvtx(zqx,vsipp,msipp)       
      zffc  =-zi*gsipp*gpin/fpio**2*zfac*zvrtc
     &      *(zxxx1 - zqfix)/(zxxx2 - zqx**2 - fpio**2)
     &      * 2.d0*fnuc*swv(ich,3)
      ztmp2x= - zffc*zdi*www
      ztmp3x=   zffc*zdf*www
c
c   nucleon exchange
c
      zuu1   = fmf2 - 2.d0*(zebi*zemf + zqfix)
      zuu2   = fmi2 - 2.d0*(zebf*zemi + zqfix)
      zxu1   = 1.d0/zuu1
      zxu12  = 1.d0/zuu1 + 1.d0/zuu2
      zyyy   = 2.d0*(zemfi- zqfix) - zwf*zwi + fnuc**2
      zxc00 = (1.d0 + 2.d0*fnuc**2*zxu12 + zyyy*zxu1)/2.d0
      zxd00 = (2.d0*fnuc*zwi*zxu12 + fnuc*(zwi-zwf)*zxu1)/2.d0
      ztmp2 = - zdi*(zxc00 + zxd00)*www*zffb
      ztmp3 =   zdf*(zxc00 - zxd00)*www*zffb

c      if(ix.eq.3) then
c      write(*,9999)xxx,zxc00,zxd00
c      write(*,9999)zxu12*fm**2,zxu1*fm**2,zyyy/fm**2
c 9999 format(1h ,10e15.5)
c      end if
c      ztmp2 = 0

      do il = 0,mxl
      flee  = fleg(il,ix)
      zvme(il,2,2) = zvme(il,2,2) + flee * ztmp2
      zvme(il,2,3) = zvme(il,2,3) + flee * ztmp3
      zvme(il,3,2) = zvme(il,3,2) + flee * ztmp2x
      zvme(il,3,3) = zvme(il,3,3) + flee * ztmp3x
      end do

      end do

c

      do jx = 1,mxj,2
       lmin = (jx -1)/2
       lplu = (jx +1)/2
      do iso = 1,3,2
      zsum12 = 0
      zsum21 = 0
      do ip  = 1,mxp
      zsum12 = zsum12 
     & -(zvme(lplu,ip,3)+zvme(lmin,ip,2))*fiso(iso,ich,ip)
c                    f10              f01
      zsum21 = zsum21 
     & -(zvme(lmin,ip,3)+zvme(lplu,ip,2))*fiso(iso,ich,ip)
      end do
      zpot(jx,1,2,iso) = zsum12
      zpot(jx,2,1,iso) = zsum21
      end do
      end do

      return
      end
c------------------------------------------------------------
c  potential 5 eta N -> sigma N
c  
c  1 s- nucleon
c  2 u- nucleon
c
      subroutine ven2sn(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-----------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)
      dimension zvme(0:20,10,4),zpot(20,6,6,3)

      mxl    = 10
      ich    = 5      ! etaN-sigamN
      mxp    = mxpot(ich)
      zi     = (0.d0,1.d0)
      pi2    = 2.d0*pi

      fmi    = feta
      fbi    = fnuc
      fmf    = fsigm
      fbf    = fnuc
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2

      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi

      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

      zvrta =  zvtx(zqf,vnnsi,mnnsi)*zvtx(zqi,vnnet,mnnet)
      zvrtb =  zvtx(zqf,vnnsi,mnnsi)*zvtx(zqi,vnnet,mnnet)
      zffa     = zi*gsinn*genn/feta   *zfac*zvrta*swv(ich,1)
      zffb     = zi*gsinn*genn/feta   *zfac*zvrtb*swv(ich,2)
c------------------------------------------------------

      zvme = 0
      zpot = 0

c-------------------------------------------------------
c   nucleon direct
c
cc  2-> f01,   3-> f10
      zvme(0,1,2) =-zffa*(zwi + fnuc)*zdi*
     &                   (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))
      zvme(0,1,3) = zffa*(zwi - fnuc)*zdf*
     &                   (1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))

      zqfi  = zqf *zqi
      zemfi = zemf*zemi

      do ix = 1,mxx
      xxx   = xgau(ix)
      www   = wgau(ix)

      zqfix = zqfi*xxx
      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)
c
c   nucleon exchange
c
      zuu1   = fmf2 - 2.d0*(zebi*zemf + zqfix)
      zuu2   = fmi2 - 2.d0*(zebf*zemi + zqfix)
      zxu1   = 1.d0/zuu1
      zxu12  = 1.d0/zuu1 + 1.d0/zuu2
      zyyy   = 2.d0*(zemfi- zqfix) - zwf*zwi + fnuc**2
      zxc00 = (1.d0 + 2.d0*fnuc**2*zxu12 + zyyy*zxu1)/2.d0
      zxd00 = (2.d0*fnuc*zwi*zxu12 + fnuc*(zwi-zwf)*zxu1)/2.d0
      ztmp2 = - zdi*(zxc00 + zxd00)*www*zffb
      ztmp3 =   zdf*(zxc00 - zxd00)*www*zffb

      do il = 0,mxl
      flee  = fleg(il,ix)
      zvme(il,2,2) = zvme(il,2,2) + flee * ztmp2
      zvme(il,2,3) = zvme(il,2,3) + flee * ztmp3
      end do

      end do

c

      do jx = 1,mxj,2
       lmin = (jx -1)/2
       lplu = (jx +1)/2
      do iso = 1,3,2
      zsum12 = 0
      zsum21 = 0
      do ip  = 1,mxp
      zsum12 = zsum12 
     & -(zvme(lplu,ip,3)+zvme(lmin,ip,2))*fiso(iso,ich,ip)
c                    f10              f01
      zsum21 = zsum21 
     & -(zvme(lmin,ip,3)+zvme(lplu,ip,2))*fiso(iso,ich,ip)
      end do
      zpot(jx,1,2,iso) = zsum12
      zpot(jx,2,1,iso) = zsum21
      end do
      end do
      return
      end
c------------------------------------------------------------
c  potential 6 sigma N -> sigma N
c  
c  1 s- nucleon
c  2 u- nucleon
c  3 t- sigma
c
      subroutine vsn2sn(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-----------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)
      dimension zvme(0:20,10,4),zpot(20,6,6,3)

      mxl    = 10
      ich    = 6      ! sigmaN-sigmaN
      mxp    = mxpot(ich)
      zi     = (0.d0,1.d0)
      pi2    = 2.d0*pi

      fmi    = fsigm
      fbi    = fnuc
      fmf    = fsigm
      fbf    = fnuc
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2

      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi

      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

      zvrta =  zvtx(zqf,vnnsi,mnnsi)*zvtx(zqi,vnnsi,mnnsi)
      zvrtb =  zvtx(zqf,vnnsi,mnnsi)*zvtx(zqi,vnnsi,mnnsi)
      zffa     = gsinn*gsinn*zfac*zvrta*swv(ich,1)
      zffb     = gsinn*gsinn*zfac*zvrtb*swv(ich,2)
c------------------------------------------------------

      zvme = 0
      zpot = 0

c-------------------------------------------------------
c   nucleon direct
c
      zvme(0,1,1) = zffa*(1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))
      zvme(0,1,4) = zffa*(1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))*zdi*zdf

      zqfi  = zqf *zqi
      zemfi = zemf*zemi

      do ix = 1,mxx
      xxx   = xgau(ix)
      www   = wgau(ix)

      zqfix = zqfi*xxx
      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)

      zvrtc =  zvtx(zqx,vnnsi,mnnsi)*zvtx(zqx,vsisi,msisi)
      zffc     = 6.d0*gsinn*gsisi*zfac*zvrtc*swv(ich,3)
c
c   nucleon exchange
c
      zuu    = fmf2 - 2.d0*(zebi*zemf + zqfix)
      ztmp1a = (3.d0*fnuc-zwf)        /zuu
      ztmp4a =-(3.d0*fnuc+zwf)*zdi*zdf/zuu
      zup    = fmi2 - 2.d0*(zebf*zemi + zqfix)
      ztmp1b = (3.d0*fnuc-zwi)        /zup
      ztmp4b =-(3.d0*fnuc+zwi)*zdi*zdf/zup

      zxx    = zffb*www/2.d0
      ztmp11 = (ztmp1a + ztmp1b)*zxx
      ztmp44 = (ztmp4a + ztmp4b)*zxx

      do il = 0,mxl
      flee  = fleg(il,ix)
      zvme(il,2,1) = zvme(il,2,1) + flee * ztmp11
      zvme(il,2,4) = zvme(il,2,4) + flee * ztmp44
      end do

      end do

c

      do jx = 1,mxj,2
      do lx = 1,2
       iss  = (-1)**lx
       l1   = (jx + iss)/2
       l2   = (jx - iss)/2
      do iso = 1,3,2
      zsum   = 0
      do ip  = 1,mxp
      ztmp   = (zvme(l1,ip,1)+zvme(l2,ip,4))*fiso(iso,ich,ip)
      zsum   = zsum + ztmp
      end do
      zpot(jx,lx,lx,iso) = zsum
      end do
      end do
      end do

      return
      end
c------------------------------------------------------------
c  potential 7 pi N -> rho N
c  
c  1 s- nucleon
c  2 u- nucleon
c  3 t- pion
c  4 c  contact
c  5 A1
c  6 t- comega
c                             7
c-------------------------------------------------------------------
      subroutine vpn2rn(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-------------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)

      common / cefflo / cg1h(-2:2,-1:1,3)
     &     ,xef(20,6,-2:2,-3:3,10)
      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
     &     ,jss(20,6,10),jll(20,6,10)
     &     ,jip(20,6,10)


      dimension zpot(20,6,6,3)
      dimension zxfa(0:1,0:1,10),zxga(0:1,0:1,10),zxha(0:1,0:1,10)
      dimension zxfb(0:1,0:1,3),zxgb(0:1,0:1,3),zxhb(0:1,0:1,3)
      dimension zxia(0:1,0:1,10),zxib(0:1,0:1,3)
      dimension zvme(20,-2:2,-1:1,-1:1,3)
c--------------------------------------------------------------


      mxl    = 10
      ich    = 7
      mxp    = mxpot(ich)

      zi     = (0.d0,1.d0)
      pi2    = 2.d0*pi
      fmi    = fpio
      fbi    = fnuc
      fmf    = fmrho
      fbf    = fnuc
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2
      zqfi   = zqf*zqi
      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi

      zsip  = zwi + fnuc
      zsim  = zwi - fnuc
      zsfp  = zwf + fnuc
      zsfm  = zwf - fnuc
      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

c------------- use all same vertex function ----------------
      zvrta =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnpi,mnnpi)
      zvrtb =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnpi,mnnpi)
      zvrtd =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnpi,mnnpi)
      zffa     = zi*grnn*gpin/fpio/2.d0*zfac*zvrta*swv(ich,1)
      zffb     = zi*grnn*gpin/fpio/2.d0*zfac*zvrtb*swv(ich,2)
      zffd     =-zi*grnn*gpin/fpio     *zfac*zvrtd*swv(ich,4)
c------------------------------------------------------------

      zxfa = 0
      zxga = 0
      zxha = 0
      zxia = 0

      xkx   = xkrho/(2.d0*fnuc)
      xkxo  = xkomg/(4.d0*fnuc)
c
c s-channel nucleon exchange
c
      zalf = 1.d0 - xkx*zsfm
      zgam = 1.d0 + xkx*zsfp
      zbet = (zsip**2/(zwi**2-fnuc**2)+zsip*zsfp/(zwf**2-fnuc**2))/2.d0
      zdel = (zsim**2/(zwi**2-fnuc**2)+zsim*zsfm/(zwf**2-fnuc**2))/2.d0
      zxfa(0,1,1)=-zdi*zalf*zbet*zffa
      zxfa(1,0,1)=-zdf*zgam*zdel*zffa
      zxha(0,0,1)=-    zalf*zdel*zffa
      zxha(1,1,1)=-zdf*zdi*zgam*zbet*zffa
c
c u-channel nucleon exchange
c
      zalf1 = (1 - xkx*(fnuc-zwf))*zffb
      zbet1 = (1 - xkx*(fnuc+zwf))*zffb
      zf101 = zdi*zalf1
      zf110 = zdf*zbet1
      zh100 =         zalf1
      zh111 = zdi*zdf*zbet1

      zalf2 = (1 + xkx*(zebf-zemf-fnuc))*zffb
      zbet2 = (1 - xkx*(zebf-zemf+fnuc))*zffb
      zgam2a= zebi-zemi+fnuc
      zgam2b= zebi-zemi-fnuc
      zdel2a= zebi+zemi+fnuc
      zdel2b= zebi+zemi-fnuc
      zf201 = zdi*zgam2a*zalf2
      zf210 = zdf*zgam2b*zbet2
      zg201 = zdi*2.d0*zqi*zalf2
      zg210 = zdf*2.d0*zqi*zbet2
      zh200 =-zdel2b*zalf2
      zh211 =-zdi*zdf*zdel2a*zbet2

      zalf3 = (1.d0 -xkx*(zwf-fnuc))*zffb
      zbet3 = (1.d0 +xkx*(zwf+fnuc))*zffb
      zgam3a= zgam2a
      zgam3b= zgam2b
      zdel3a= zdel2b
      zdel3b= zdel2a
      zf301 = zdi*zgam3a*zalf3
      zf310 =-zdf*zgam3b*zbet3
      zg301 = zdi*2.d0*zqi*zalf3
      zg310 =-zdf*2.d0*zqi*zbet3
      zh300 =        -zdel3a*zalf3
      zh311 = zdi*zdf*zdel3b*zbet3
c  contact term
      zxfa(0,1,4) = -zdi*zffd
      zxfa(1,0,4) = -zdf*zffd
      zxha(0,0,4) = -    zffd
      zxha(1,1,4) = -zdi*zdf*zffd
      
      zxkxb = xkx*zffb

c
c  angular projection
c

      zvme = 0

      do ix = 1,mxx
      www   = wgau(ix)
      cc    = xgau(ix)
      ss    = sqrt(1.d0 -  cc**2)
      c2    = sqrt((1.d0 + cc)/2.d0)
      s2    = sqrt((1.d0 - cc)/2.d0)
      zqfix = zqfi*cc
      zkky  = zemf*zemi - zqfix

c------------------------------------------------------------
      zqx2  =  zqf2 + zqi2 - zqfix*2.d0
      zqx   =  sqrt(zqx2)
      zvrtc =  zvtx(zqx,vnnpi,mnnpi)*zvtx(zqx,vrpp ,mrpp )  *swv(ich,3)
      zvrte =  zvtx(zqx,vnna1 ,mnna1 )*zvtx(zqx,vnnpi,mnnpi)*swv(ich,5)
      zvrtf =  zvtx(zqx,vnnomg,mnnomg)*zvtx(zqx,vopr ,mopr )*swv(ich,6)
      zffc  =-zi*grpp*gpin/fpio     *zfac*zvrtc
      zffe  = zi*grnn*gpin/fpio     *zfac*zvrte
      zfff  =    gonn*gopr/fmomg    *zfac*zvrtf
c------------------------------------------------------------
c        u-channel nucleon exchange
      zkkx  = 2.d0*(zemf*zemi - zqfix)
      zuu   = 1.d0/(fmf2 - 2.d0*(zebi*zemf+zqfix))
      zuup  = 1.d0/(fmi2 - 2.d0*(zebf*zemi+zqfix))
      za1   = -(zwi-zwf)*zemi*zuu + (1.d0+zuu/zuup)/2.d0
      za2   = -(zwi-zwf)*zuu/2.d0
      za3   = -(zuu + zuup)*fnuc
      zxfa(0,1,2) = za1*zf101+za2*(zf201-zxkxb*zkkx*zdi)
     &                       +za3*(zf301-zxkxb*zkkx*zdi)
      zxfa(1,0,2) = za1*zf110+za2*(zf210+zxkxb*zkkx*zdf)
     &                       +za3*(zf310-zxkxb*zkkx*zdf)
      zxga(0,1,2) =           za2*zg201+za3*zg301
      zxga(1,0,2) =           za2*zg210+za3*zg310
      zxha(0,0,2) = za1*zh100+za2*(zh200-zxkxb*zkkx)
     &                       +za3*(zh300-zxkxb*zkkx)
      zxha(1,1,2) = za1*zh111+za2*(zh211+zxkxb*zkkx*zdi*zdf)
     &                       +za3*(zh311-zxkxb*zkkx*zdi*zdf)
c  t-channel pion exchange
      zpion       = -2.d0*fnuc/((zebi-zebf)**2 - zqx2 - fpio**2)*zffc
      ztmp        = zebi - zemi - zwf
      zxfa(0,1,3) =-zpion*ztmp*zdi
      zxfa(1,0,3) = zpion*ztmp*zdf
      zxga(0,1,3) =-zpion*zqi*zdi*2.d0
      zxga(1,0,3) = zpion*zqi*zdf*2.d0
c  t-channel omega exchange
      zomeg1= 1.d0/((zebi-zebf)**2 - zqx2 - fmomg**2)
      zomeg2= 1.d0/((zemi-zemf)**2 - zqx2 - fmomg**2)
      za1   =-xkxo*((zwi+zwf)*zomeg1+2.d0*zwi*zomeg2)
      za2   = (1.d0+xkomg)/2.d0*(zomeg1+zomeg2)
      za3   = xkxo*(zwi-zwf)*zomeg2
      za23p = (za2+za3)*zdi
      za23m = (za2-za3)*zdf
      zfx   = zfff*zi
      zxia(0,0,6)=-zfx*zqfi*(za1+za2+za3)
      zxia(1,1,6)=-zfx*zqfi*zdi*zdf*(za2-za3-za1)
      zxfa(0,1,6)=-zfx*(zkky*za23p+zqfi*za23m)
      zxfa(1,0,6)=-zfx*(zkky*za23m+zqfi*za23p)
      zxga(0,1,6)=-zfx*zqi*zemf*za23p
      zxga(1,0,6)=-zfx*zqi*zemf*za23m
      zxha(0,0,6)=+zfx*(zemf*zqi*za23p+zemi*zqf*za23m)
      zxha(1,1,6)=+zfx*(zemi*zqf*za23p+zemf*zqi*za23m)
c---------------------------------------------------------------

      do ix1= 0,1
      do ix2= 0,1
      do iso= 1,3,2
      iy1   = ih1(ix1,ix2)
      iy2   = ih2(ix1,ix2)
      zxfb(ix1,ix2,iso) = 0
      zxgb(ix1,ix2,iso) = 0
      zxhb(ix1,ix2,iso) = 0
      zxib(ix1,ix2,iso) = 0
      do ic = 1,mxp
      xiso  = fiso(iso,ich,ic)
      zxfb(ix1,ix2,iso)=zxfb(ix1,ix2,iso)+
     &(zqf*zxfa(ix1,ix2,ic)+zemf*(cc*zxga(ix1,ix2,ic)+zxha(iy1,iy2,ic)))
     & /fmrho*xiso
      zxgb(ix1,ix2,iso) = zxgb(ix1,ix2,iso)+
     &                    zxga(ix1,ix2,ic)*ss/sqrt(2.d0)*xiso
      zxhb(ix1,ix2,iso) = zxhb(ix1,ix2,iso)-
     &                    zxha(ix1,ix2,ic)*sqrt(2.d0)*xiso
      zxib(ix1,ix2,iso) = zxib(ix1,ix2,iso)+
     &                    zxia(ix1,ix2,ic)*ss/sqrt(2.d0)*xiso
      end do

      end do
      end do
      end do


      do imf = -1,1,2
      do imi = -1,1,2
      do iso = 1,3,2
      dhlf   = dfun(1,imi,imf,ix)*www
      zxf    =zxfb(0,0,iso)     +zxfb(0,1,iso)*imi
     &       +zxfb(1,0,iso)*imf+zxfb(1,1,iso)*imf*imi
      zxg    =zxgb(0,0,iso)    +zxgb(0,1,iso)*imi
     &       +zxgb(1,0,iso)*imf+zxgb(1,1,iso)*imf*imi
      zxh    =zxhb(0,0,iso)    +zxhb(0,1,iso)*imi
     &       +zxhb(1,0,iso)*imf+zxhb(1,1,iso)*imf*imi
      zxi    =zxib(0,0,iso)    +zxib(0,1,iso)*imi
     &       +zxib(1,0,iso)*imf+zxib(1,1,iso)*imf*imi
      zz0   = dhlf*zxf
      zzp   = dhlf*zxg
      zzm   =-zzp
      zzz   = dhlf*zxi
      if(imf.eq.-1) then
      zzp = zzp + dfun(1,imi,1,ix)*www*zxh
      else if(imf.eq.1) then
      zzm = zzm - dfun(1,imi,-1,ix)*www*zxh
      end if

      do jx = 1,mxj,2
      zvme(jx, 2,imf,imi,iso)=zvme(jx, 2,imf,imi,iso)
     &                        +dfun(jx,imi,imf+2,ix)*(zzp+zzz)
      zvme(jx, 0,imf,imi,iso)=zvme(jx, 0,imf,imi,iso)
     &                        +dfun(jx,imi,imf,ix)  *zz0
      zvme(jx,-2,imf,imi,iso)=zvme(jx,-2,imf,imi,iso)
     &                        +dfun(jx,imi,imf-2,ix)*(zzm+zzz)
      end do ! jx

      end do ! iso
      end do ! imf
      end do ! imi

      end do ! ix cos

c
c           lsj scheme
c
      icci = 1
      iccf = 5
      zpot = 0
      imiz = 0
      do 200 jjx = 1,mxj,2

      do 300 idxf= 1,6
      do 300 idxi= 1,6
      ipxi  = jip(jjx,idxi,icci)
      ipxf  = jip(jjx,idxf,iccf)
      iptest = ipxi*ipxf
      if(iptest.eq.1) then

      do 210 imfz= -2,2,2
      do 210 ibfz= -1,1,2
      xxf = xef(jjx,idxf,imfz,ibfz,iccf)

      if(abs(xxf).gt.1.d-20)then

      do 220 ibiz= -1,1,2
      www = xef(jjx,idxi,imiz,ibiz,icci)*xxf

      if(abs(www).gt.1.d-20) then
      do iso = 1,3
      zpot(jjx,idxf,idxi,iso) = zpot(jjx,idxf,idxi,iso)
     &         + www*zvme(jjx,imfz,ibfz,ibiz,iso)
      end do
      end if

 220  continue

      end if

 210  continue

      end if  ! parity test

 300  continue
 200  continue

      return
      end
c------------------------------------------------------------------
c  potential 8 eta N -> rho N
c  
c  1 s- nucleon
c  2 u- nucleon
c                             8
c-------------------------------------------------------------------
      subroutine ven2rn(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-------------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)

      common / cefflo / cg1h(-2:2,-1:1,3)
     &     ,xef(20,6,-2:2,-3:3,10)
      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
     &     ,jss(20,6,10),jll(20,6,10)
     &     ,jip(20,6,10)


      dimension zpot(20,6,6,3)
      dimension zxfa(0:1,0:1,10),zxga(0:1,0:1,10),zxha(0:1,0:1,10)
      dimension zxfb(0:1,0:1,3),zxgb(0:1,0:1,3),zxhb(0:1,0:1,3)
      dimension zvme(20,-2:2,-1:1,-1:1,3)
c--------------------------------------------------------------


      mxl    = 10
      ich    = 8
      mxp    = mxpot(ich)

      zi     = (0.d0,1.d0)
      pi2    = 2.d0*pi
      fmi    = feta
      fbi    = fnuc
      fmf    = fmrho
      fbf    = fnuc
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2
      zqfi   = zqf*zqi
      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi

      zsip  = zwi + fnuc
      zsim  = zwi - fnuc
      zsfp  = zwf + fnuc
      zsfm  = zwf - fnuc
      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

c------------- use all same vertex function ----------------
      zvrta =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnet,mnnet)
      zvrtb =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnet,mnnet)
      zffa     = zi*grnn*genn/feta/2.d0*zfac*zvrta*swv(ich,1)
      zffb     = zi*grnn*genn/feta/2.d0*zfac*zvrtb*swv(ich,2)
c------------------------------------------------------------

      zxfa = 0
      zxga = 0
      zxha = 0

      xkx   = xkrho/(2.d0*fnuc)
c
c s-channel nucleon exchange
c
      zalf = 1.d0 - xkrho/(2.d0*fnuc)*zsfm
      zgam = 1.d0 + xkrho/(2.d0*fnuc)*zsfp
      zbet = (zsip**2/(zwi**2-fnuc**2)+zsip*zsfp/(zwf**2-fnuc**2))/2.d0
      zdel = (zsim**2/(zwi**2-fnuc**2)+zsim*zsfm/(zwf**2-fnuc**2))/2.d0
      zxfa(0,1,1)=-zdi*zalf*zbet*zffa
      zxfa(1,0,1)=-zdf*zgam*zdel*zffa
      zxha(0,0,1)=-    zalf*zdel*zffa
      zxha(1,1,1)=-zdf*zdi*zgam*zbet*zffa
c
c u-channel nucleon exchange
c
      zalf1 = (1 - xkx*(fnuc-zwf))*zffb
      zbet1 = (1 - xkx*(fnuc+zwf))*zffb
      zf101 = zdi*zalf1
      zf110 = zdf*zbet1
      zh100 =         zalf1
      zh111 = zdi*zdf*zbet1

      zalf2 = (1 + xkx*(zebf-zemf-fnuc))*zffb
      zbet2 = (1 - xkx*(zebf-zemf+fnuc))*zffb
      zgam2a= zebi-zemi+fnuc
      zgam2b= zebi-zemi-fnuc
      zdel2a= zebi+zemi+fnuc
      zdel2b= zebi+zemi-fnuc
      zf201 = zdi*zgam2a*zalf2
      zf210 = zdf*zgam2b*zbet2
      zg201 = zdi*2.d0*zqi*zalf2
      zg210 = zdf*2.d0*zqi*zbet2
      zh200 =-zdel2b*zalf2
      zh211 =-zdi*zdf*zdel2a*zbet2

      zalf3 = (1.d0 -xkx*(zwf-fnuc))*zffb
      zbet3 = (1.d0 +xkx*(zwf+fnuc))*zffb
      zgam3a= zgam2a
      zgam3b= zgam2b
      zdel3a= zdel2b
      zdel3b= zdel2a
      zf301 = zdi*zgam3a*zalf3
      zf310 =-zdf*zgam3b*zbet3
      zg301 = zdi*2.d0*zqi*zalf3
      zg310 =-zdf*2.d0*zqi*zbet3
      zh300 =        -zdel3a*zalf3
      zh311 = zdi*zdf*zdel3b*zbet3

      zxkxb = xkx*zffb



c
c  angular projection
c

      zvme = 0

      do ix = 1,mxx
      www   = wgau(ix)
      cc    = xgau(ix)
      ss    = sqrt(1.d0 -  cc**2)
      c2    = sqrt((1.d0 + cc)/2.d0)
      s2    = sqrt((1.d0 - cc)/2.d0)
      zqfix = zqfi*cc
      zkkx  = 2.d0*(zemf*zemi - zqfix)
      zuu   = 1.d0/(fmf2 - 2.d0*(zebi*zemf+zqfix))
      zuup  = 1.d0/(fmi2 - 2.d0*(zebf*zemi+zqfix))
      za1   = -(zwi-zwf)*zemi*zuu + (1.d0+zuu/zuup)/2.d0
      za2   = -(zwi-zwf)*zuu/2.d0
      za3   = -(zuu + zuup)*fnuc
      zxfa(0,1,2) = za1*zf101+za2*(zf201-zxkxb*zkkx*zdi)
     &                       +za3*(zf301-zxkxb*zkkx*zdi)
      zxfa(1,0,2) = za1*zf110+za2*(zf210+zxkxb*zkkx*zdf)
     &                       +za3*(zf310-zxkxb*zkkx*zdf)
      zxga(0,1,2) =           za2*zg201+za3*zg301
      zxga(1,0,2) =           za2*zg210+za3*zg310
      zxha(0,0,2) = za1*zh100+za2*(zh200-zxkxb*zkkx)
     &                       +za3*(zh300-zxkxb*zkkx)
      zxha(1,1,2) = za1*zh111+za2*(zh211+zxkxb*zkkx*zdi*zdf)
     &                       +za3*(zh311-zxkxb*zkkx*zdi*zdf)



      do ix1= 0,1
      do ix2= 0,1
      do iso= 1,3,2
      iy1   = ih1(ix1,ix2)
      iy2   = ih2(ix1,ix2)
      zxfb(ix1,ix2,iso) = 0
      zxgb(ix1,ix2,iso) = 0
      zxhb(ix1,ix2,iso) = 0
      do ic = 1,mxp
      xiso  = fiso(iso,ich,ic)
      zxfb(ix1,ix2,iso)=zxfb(ix1,ix2,iso)+
     &(zqf*zxfa(ix1,ix2,ic)+zemf*(cc*zxga(ix1,ix2,ic)+zxha(iy1,iy2,ic)))
     & /fmrho*xiso
      zxgb(ix1,ix2,iso) = zxgb(ix1,ix2,iso)+
     &                    zxga(ix1,ix2,ic)*ss/sqrt(2.d0)*xiso
      zxhb(ix1,ix2,iso) = zxhb(ix1,ix2,iso)-
     &                    zxha(ix1,ix2,ic)*sqrt(2.d0)*xiso
      end do

      end do
      end do
      end do


      do imf = -1,1,2
      do imi = -1,1,2
      do iso = 1,3,2
      dhlf   = dfun(1,imi,imf,ix)*www
      zxf    =zxfb(0,0,iso)     +zxfb(0,1,iso)*imi
     &       +zxfb(1,0,iso)*imf+zxfb(1,1,iso)*imf*imi
      zxg    =zxgb(0,0,iso)    +zxgb(0,1,iso)*imi
     &       +zxgb(1,0,iso)*imf+zxgb(1,1,iso)*imf*imi
      zxh    =zxhb(0,0,iso)    +zxhb(0,1,iso)*imi
     &       +zxhb(1,0,iso)*imf+zxhb(1,1,iso)*imf*imi
      zz0   = dhlf*zxf
      zzp   = dhlf*zxg
      zzm   =-zzp
      if(imf.eq.-1) then
      zzp = zzp + dfun(1,imi,1,ix)*www*zxh
      else if(imf.eq.1) then
      zzm = zzm - dfun(1,imi,-1,ix)*www*zxh
      end if

      do jx = 1,mxj,2
      zvme(jx, 2,imf,imi,iso)=zvme(jx, 2,imf,imi,iso)
     &                        +dfun(jx,imi,imf+2,ix)*zzp
      zvme(jx, 0,imf,imi,iso)=zvme(jx, 0,imf,imi,iso)
     &                        +dfun(jx,imi,imf,ix)  *zz0
      zvme(jx,-2,imf,imi,iso)=zvme(jx,-2,imf,imi,iso)
     &                        +dfun(jx,imi,imf-2,ix)*zzm
      end do ! jx

      end do ! iso
      end do ! imf
      end do ! imi

      end do ! ix cos

c
c           lsj scheme
c
      icci = 2  ! eta
      iccf = 5  ! rho
      zpot = 0
      imiz = 0
      do 200 jjx = 1,mxj,2

      do 300 idxf= 1,6
      do 300 idxi= 1,6
      ipxi  = jip(jjx,idxi,icci)
      ipxf  = jip(jjx,idxf,iccf)
      iptest = ipxi*ipxf
      if(iptest.eq.1) then

      do 210 imfz= -2,2,2
      do 210 ibfz= -1,1,2
      xxf = xef(jjx,idxf,imfz,ibfz,iccf)

      if(abs(xxf).gt.1.d-20)then

      do 220 ibiz= -1,1,2
      www = xef(jjx,idxi,imiz,ibiz,icci)*xxf

      if(abs(www).gt.1.d-20) then
      do iso = 1,3
      zpot(jjx,idxf,idxi,iso) = zpot(jjx,idxf,idxi,iso)
     &         + www*zvme(jjx,imfz,ibfz,ibiz,iso)
      end do
      end if

 220  continue

      end if

 210  continue

      end if  ! parity test

 300  continue
 200  continue

      return
      end
c------------------------------------------------------------------
c  potential 9 sigma N -> rho N
c  
c  1 s- nucleon
c  2 u- nucleon
c                             9
c-------------------------------------------------------------------
      subroutine vsn2rn(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-------------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)

      common / cefflo / cg1h(-2:2,-1:1,3)
     &     ,xef(20,6,-2:2,-3:3,10)
      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
     &     ,jss(20,6,10),jll(20,6,10)
     &     ,jip(20,6,10)


      dimension zpot(20,6,6,3)
      dimension zxfa(0:1,0:1,10),zxga(0:1,0:1,10),zxha(0:1,0:1,10)
      dimension zxfb(0:1,0:1,3),zxgb(0:1,0:1,3),zxhb(0:1,0:1,3)
      dimension zvme(20,-2:2,-1:1,-1:1,3)
c--------------------------------------------------------------


      mxl    = 10
      ich    = 9
      mxp    = mxpot(ich)

      zi     = (0.d0,1.d0)
      pi2    = 2.d0*pi
      fmi    = fsigm
      fbi    = fnuc
      fmf    = fmrho
      fbf    = fnuc
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2
      zqfi   = zqf*zqi
      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi

      zsip  = zwi + fnuc
      zsim  = zwi - fnuc
      zsfp  = zwf + fnuc
      zsfm  = zwf - fnuc
      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

c------------- use all same vertex function ----------------
      zvrta =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnsi,mnnsi)*swv(ich,1)
      zvrtb =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnsi,mnnsi)*swv(ich,2)
      zffa     = grnn*gsinn/2.d0*zfac*zvrta
      zffb     = grnn*gsinn/2.d0*zfac*zvrtb
c------------------------------------------------------------

      zxfa = 0
      zxga = 0
      zxha = 0

c
c s-channel nucleon exchange
c
      xkx   = xkrho/(2.d0*fnuc)
      zalf = 1.d0 - xkx*zsfm
      zgam = 1.d0 + xkx*zsfp
      zbet = ( zsip/(zwi**2-fnuc**2)+zsfp/(zwf**2-fnuc**2))/2.d0
      zdel = (-zsim/(zwi**2-fnuc**2)-zsfm/(zwf**2-fnuc**2))/2.d0
      zxfa(0,0,1)=         zalf*zbet*zffa
      zxfa(1,1,1)= zdi*zdf*zgam*zdel*zffa
      zxha(0,1,1)= zdi*    zalf*zdel*zffa
      zxha(1,0,1)= zdf*    zgam*zbet*zffa
c
c u-channel nucleon exchange
c
      zalf1 = (1 + xkx*(fnuc-zwf))*zffb
      zbet1 = (1 + xkx*(fnuc+zwf))*zffb
      zf100 =         zalf1
      zf111 = zdi*zdf*zbet1
      zh101 = zdi*zalf1
      zh110 = zdf*zbet1

      zalf2 = (1 - xkx*(fnuc-zebf+zemf))*zffb
      zbet2 = (1 - xkx*(fnuc+zebf-zemf))*zffb
      zf200 =          zalf2
      zf211 = -zdi*zdf*zbet2
      zh201 =  zdi*    zalf2
      zh210 = -zdf*    zbet2

      zalf3 = (1.d0 +xkx*(zwf-fnuc))*zffb
      zbet3 = (1.d0 -xkx*(zwf+fnuc))*zffb
      zgam3 = zebi-zemi-fnuc
      zeta3 = zebi-zemi+fnuc
      zf300 =        -zgam3*zalf3
      zf311 = zdi*zdf*zeta3*zbet3

      zg300 = -2.d0*zqi        *zalf3
      zg311 =  2.d0*zqi*zdi*zdf*zbet3

      zh301 = zdi*(zwi+fnuc)*zalf3
      zh310 =-zdf*(zwi-fnuc)*zbet3

      zxkxb = xkx*zffb
c
c  angular projection
c

      zvme = 0

      do ix = 1,mxx
      www   = wgau(ix)
      cc    = xgau(ix)
      ss    = sqrt(1.d0 -  cc**2)
      c2    = sqrt((1.d0 + cc)/2.d0)
      s2    = sqrt((1.d0 - cc)/2.d0)
      zqfix = zqfi*cc
      zkkx  = 2.d0*(zemf*zemi - zqfix)
      zuu   = 1.d0/(fmf2 - 2.d0*(zebi*zemf+zqfix))
      zuup  = 1.d0/(fmi2 - 2.d0*(zebf*zemi+zqfix))
      za3   = - (zuu + zuup)/2.d0
      za1   = -2.d0*fnuc*za3
      za2   = (zwi-zwf)/2.d0*zuu

      zxfa(0,0,2) = za1*zf100+za2*zf200 + za3*(zf300-zxkxb*zkkx)
      zxfa(1,1,2) = za1*zf111+za2*zf211 + za3*(zf311-zxkxb*zkkx*zdi*zdf)
      zxga(0,0,2) = za3*zg300
      zxga(1,1,2) = za3*zg311
      zxha(0,1,2) = za1*zh101 + za2*zh201 + za3*(zh301-zxkxb*zkkx*zdi)
      zxha(1,0,2) = za1*zh110 + za2*zh210 + za3*(zh310-zxkxb*zkkx*zdf)

      do ix1= 0,1
      do ix2= 0,1
      do iso= 1,3,2
      iy1   = ih1(ix1,ix2)
      iy2   = ih2(ix1,ix2)
      zxfb(ix1,ix2,iso) = 0
      zxgb(ix1,ix2,iso) = 0
      zxhb(ix1,ix2,iso) = 0
      do ic = 1,mxp
      xiso  = fiso(iso,ich,ic)
      zxfb(ix1,ix2,iso)=zxfb(ix1,ix2,iso)+
     &(zqf*zxfa(ix1,ix2,ic)+zemf*(cc*zxga(ix1,ix2,ic)+zxha(iy1,iy2,ic)))
     & /fmrho*xiso
      zxgb(ix1,ix2,iso) = zxgb(ix1,ix2,iso)+
     &                    zxga(ix1,ix2,ic)*ss/sqrt(2.d0)*xiso
      zxhb(ix1,ix2,iso) = zxhb(ix1,ix2,iso)-
     &                    zxha(ix1,ix2,ic)*sqrt(2.d0)*xiso
      end do

      end do
      end do
      end do


      do imf = -1,1,2
      do imi = -1,1,2
      do iso = 1,3,2
      dhlf   = dfun(1,imi,imf,ix)*www
      zxf    =zxfb(0,0,iso)    +zxfb(0,1,iso)*imi
     &       +zxfb(1,0,iso)*imf+zxfb(1,1,iso)*imf*imi
      zxg    =zxgb(0,0,iso)    +zxgb(0,1,iso)*imi
     &       +zxgb(1,0,iso)*imf+zxgb(1,1,iso)*imf*imi
      zxh    =zxhb(0,0,iso)    +zxhb(0,1,iso)*imi
     &       +zxhb(1,0,iso)*imf+zxhb(1,1,iso)*imf*imi
      zz0   = dhlf*zxf
      zzp   = dhlf*zxg
      zzm   =-zzp
      if(imf.eq.-1) then
      zzp = zzp + dfun(1,imi,1,ix)*www*zxh
      else if(imf.eq.1) then
      zzm = zzm - dfun(1,imi,-1,ix)*www*zxh
      end if

      do jx = 1,mxj,2
      zvme(jx, 2,imf,imi,iso)=zvme(jx, 2,imf,imi,iso)
     &                        +dfun(jx,imi,imf+2,ix)*zzp
      zvme(jx, 0,imf,imi,iso)=zvme(jx, 0,imf,imi,iso)
     &                        +dfun(jx,imi,imf,ix)  *zz0
      zvme(jx,-2,imf,imi,iso)=zvme(jx,-2,imf,imi,iso)
     &                        +dfun(jx,imi,imf-2,ix)*zzm
      end do ! jx

      end do ! iso
      end do ! imf
      end do ! imi

      end do ! ix cos

c
c           lsj scheme
c
      icci = 4   ! sigma
      iccf = 5   ! rho
      zpot = 0
      imiz = 0
      do 200 jjx = 1,mxj,2

      do 300 idxf= 1,6
      do 300 idxi= 1,6
      ipxi  = jip(jjx,idxi,icci)
      ipxf  = jip(jjx,idxf,iccf)
      iptest = ipxi*ipxf
      if(iptest.eq.1) then

      do 210 imfz= -2,2,2
      do 210 ibfz= -1,1,2
      xxf = xef(jjx,idxf,imfz,ibfz,iccf)

      if(abs(xxf).gt.1.d-20)then

      do 220 ibiz= -1,1,2
      www = xef(jjx,idxi,imiz,ibiz,icci)*xxf

      if(abs(www).gt.1.d-20) then
      do iso = 1,3
      zpot(jjx,idxf,idxi,iso) = zpot(jjx,idxf,idxi,iso)
     &         + www*zvme(jjx,imfz,ibfz,ibiz,iso)
      end do
      end if

 220  continue

      end if

 210  continue

      end if  ! parity test

 300  continue
 200  continue

      return
      end
c------------------------------------------------------------------
c  potential 10 rho N -> rho N
c  
c  1 s- nucleon
c  2 u- nucleon
c  3   contact
c                             10
c-------------------------------------------------------------------
      subroutine vrn2rn(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-------------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)

      common / cefflo / cg1h(-2:2,-1:1,3)
     &     ,xef(20,6,-2:2,-3:3,10)
      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
     &     ,jss(20,6,10),jll(20,6,10)
     &     ,jip(20,6,10)
      dimension zpot(20,6,6,3)
      dimension zvme(20,-2:2,-1:1,-2:2,-1:1,3)
      dimension zans1(-2:2,-1:1,-2:2,-1:1)
      dimension zans2(-2:2,-1:1,-2:2,-1:1)
      dimension zans3(-2:2,-1:1,-2:2,-1:1)
c      dimension zux4(-2:2),zux5(-2:2)
      dimension zua(3,4),zub(3,4),zuc(3,4),zud(3,2),zuax(4),zuf(3)
      dimension zqfei(-2:2),zqief(-2:2)
      dimension zuoa(4),zuoap(4),zuob(4),zuoc(4),zuod(2)
      dimension zfff(-2:2),zffi(-2:2),fff(-2:2)
      dimension xdd(-2:2,-2:2)

      ich    = 10
      zi     = (0.d0,1.d0)
      pi2    = 2.d0*pi
      s2     = sqrt(2.d0)
      fmi    = fmrho
      fbi    = fnuc
      fmf    = fmrho
      fbf    = fnuc
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2

      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi
      zqfi   = zqf*zqi
      zwfm   = zwf - fnuc
      zwfp   = zwf + fnuc
      zwim   = zwi - fnuc
      zwip   = zwi + fnuc

      zfff(-2) = 1
      zfff( 2) = 1
      zfff( 0) = zemf/fmf
      zffi(-2) = 1
      zffi( 2) = 1
      zffi( 0) = zemi/fmi
      fff( 2)  = sqrt(2.d0)
      fff(-2)  = sqrt(2.d0)
      fff( 0)  = 1
c
c
c------------------------------------------------------------
      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
      zvrta =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnrho,mnnrho)
      zffa     = grnn**2/4.d0           *zfac*zvrta*swv(ich,1)
     &                                  *fiso(1,ich,1)
      zvrtb =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnrho,mnnrho)
      zffb     = grnn**2/4.d0           *zfac*zvrtb*swv(ich,2)
      zvrtc =  zvtx(zqf,vnnrho,mnnrho)*zvtx(zqi,vnnrho,mnnrho)
c  correction 02-13-2007
      zffc     = grnn**2*xkrho/8.d0/fnuc*zfac*zvrtc*swv(ich,3)  !10-17-2006
c      zffc     = grnn**2/xkrho/8.d0/fnuc*zfac*zvrtc*swv(ich,3)  !10-17-2006

c      write(*,*)zffa,swv(ich,1),zffb,swv(ich,2)
c-------------------------------------------------------------
      xkx   = xkrho/2.d0/fnuc
      zxaa  = (1.d0/zwim + 1.d0/zwfm)/2.d0*zffa
      zxbb  =-(1.d0/zwip + 1.d0/zwfp)/2.d0*zffa
      zyaa  =  1.d0-xkx*zwfm
      zybb  = (1.d0+xkx*zwfp)*zdf
      zycc  =  1.d0-xkx*zwim
      zydd  =-(1.d0+xkx*zwip)*zdi

      zza00 = zyaa*zycc*zxaa
      zza11 = zybb*zydd*zxbb
      zzb01 = zyaa*zydd*zxbb
      zzb10 = zybb*zycc*zxaa
      zzc01 =-zyaa*zydd*zxaa
      zzc10 =-zybb*zycc*zxbb
      zzd00 =-zyaa*zycc*zxbb
      zzd11 =-zybb*zydd*zxaa


      zzg00=(zqf*zqi*zza00+zqi*zemf*zzb10+zqf*zemi*zzc01
     &     +zemf*zemi*zzd11)/fmrho**2
      zzg11=(zqf*zqi*zza11+zqi*zemf*zzb01+zqf*zemi*zzc10
     &     +zemf*zemi*zzd00)/fmrho**2
      zzh00=(zqf*zzc01+zemf*zzd11)*sqrt(2.d0)/fmrho
      zzh11=(zqf*zzc10+zemf*zzd00)*sqrt(2.d0)/fmrho
      zzi00=(zqi*zzb10+zemi*zzd11)*sqrt(2.d0)/fmrho
      zzi11=(zqi*zzb01+zemi*zzd00)*sqrt(2.d0)/fmrho
c
c
c
      yy2    =  2.d0*xkx
      yy2s   =  2.d0*xkx**2
c  a
      zua(1,1)= (1.d0+xkx*zwfm)*(1.d0+xkx*zwim)+yy2s*(-zwfm*zwim)
      zua(1,2)=-(1.d0-xkx*zwfp)*(1.d0-xkx*zwip)+yy2s*(+zwfp*zwip)
      zua(1,3)=-(1.d0+xkx*zwfm)*(1.d0-xkx*zwip)+yy2s*(-zwfm*zwip)
      zua(1,4)= (1.d0-xkx*zwfp)*(1.d0+xkx*zwim)+yy2s*(+zwfp*zwim)
      zub(1,1)=-yy2*(1.d0-xkx*zwfm)
      zub(1,2)=-yy2*(1.d0+xkx*zwfp)
      zub(1,3)= zub(1,1)
      zub(1,4)= zub(1,2)
      zuc(1,1)=-yy2*(1.d0-xkx*zwim)
      zuc(1,2)=-yy2*(1.d0+xkx*zwip)
      zuc(1,3)= zuc(1,2)
      zuc(1,4)= zuc(1,1)
      zud(1,1)= -2.d0*yy2s
      zud(1,2)=  2.d0*yy2s
c b
      zuax(1)= (1.d0-xkx*zwfm)*(1.d0-xkx*zwim)-yy2s*(+zwfm*zwim)
      zuax(2)= (1.d0+xkx*zwfp)*(1.d0+xkx*zwip)-yy2s*(+zwfp*zwip)
      zuax(3)= (1.d0-xkx*zwfm)*(1.d0+xkx*zwip)-yy2s*(-zwfm*zwip)
      zuax(4)= (1.d0+xkx*zwfp)*(1.d0-xkx*zwim)-yy2s*(-zwfp*zwim)
      zua(2,1)=-yy2*zemi*( 1.d0+xkx*zwfm)-yy2*zemf*( 1.d0+xkx*zwim)
      zua(2,2)=-yy2*zemi*(-1.d0+xkx*zwfp)-yy2*zemf*(-1.d0+xkx*zwip)
      zua(2,3)=-yy2*zemi*(-1.d0-xkx*zwfm)-yy2*zemf*(-1.d0+xkx*zwip)
      zua(2,4)=-yy2*zemi*( 1.d0-xkx*zwfp)-yy2*zemf*( 1.d0+xkx*zwim)
      zub(2,1)= yy2*( 1.d0+xkx*zwfm + yy2*zemf)
      zub(2,2)= yy2*(-1.d0+xkx*zwfp + yy2*zemf)
      zub(2,3)= yy2*(-1.d0-xkx*zwfm + yy2*zemf)
      zub(2,4)= yy2*( 1.d0-xkx*zwfp + yy2*zemf)
      zuc(2,1)= yy2*( 1.d0+xkx*zwim + yy2*zemi)
      zuc(2,2)= yy2*(-1.d0+xkx*zwip + yy2*zemi)
      zuc(2,3)= yy2*( 1.d0-xkx*zwip + yy2*zemi)
      zuc(2,4)= yy2*(-1.d0-xkx*zwim + yy2*zemi)
      zud(2,1)= -2.d0*yy2s
      zud(2,2)= -2.d0*yy2s
c  c-------------------
      tmp1   = 1.d0 +yy2s*fmi**2/2.d0
      tmp2   = yy2*fmi**2
      zua(3,1)=-tmp1*( zwfm+zwim)-tmp2+yy2*zwfm*zwim
      zua(3,2)=-tmp1*( zwfp+zwip)+tmp2-yy2*zwfp*zwip
      zua(3,3)=-tmp1*(-zwfm+zwip)+tmp2+yy2*zwfm*zwip
      zua(3,4)=-tmp1*(-zwfp+zwim)-tmp2-yy2*zwfp*zwim
      zub(3,1)= 2.d0*(tmp1-yy2*zwfm)
      zub(3,2)= 2.d0*(tmp1+yy2*zwfp)
      zub(3,3)= 2.d0*(tmp1-yy2*zwfm)
      zub(3,4)= 2.d0*(tmp1+yy2*zwfp)
      zuc(3,1)= 2.d0*(tmp1-yy2*zwim)
      zuc(3,2)= 2.d0*(tmp1+yy2*zwip)
      zuc(3,3)= 2.d0*(tmp1+yy2*zwip)
      zuc(3,4)= 2.d0*(tmp1-yy2*zwim)
      zud(3,1)=  4.d0*yy2
      zud(3,2)= -4.d0*yy2


c      zua = 0
c      zub = 0
c      zuc = 0
c      zud = 0
c      zua(3,1)=-tmp2
c      zua(3,2)=+tmp2
c      zua(3,3)=+tmp2
c      zua(3,4)=-tmp2
c      zua(3,1)=  zwfm*zwim
c      zua(3,2)= -zwfp*zwip
c      zua(3,3)=  zwfm*zwip
c      zua(3,4)= -zwfp*zwim
c      zub(3,1)= -2.d0*zwfm
c      zub(3,2)=  2.d0*zwfp
c      zub(3,3)= -2.d0*zwfm
c      zub(3,4)=  2.d0*zwfp
c      zuc(3,1)= -2.d0*zwim
c      zuc(3,2)=  2.d0*zwip
c      zuc(3,3)=  2.d0*zwip
c      zuc(3,4)= -2.d0*zwim
c      zud(3,1)=  4.d0
c      zud(3,2)= -4.d0

      do k = 1,3
      zua(k,2) = zua(k,2)*zdi*zdf
      zua(k,3) = zua(k,3)*zdi
      zua(k,4) = zua(k,4)*zdf
      zub(k,2) = zub(k,2)*zdi*zdf
      zub(k,3) = zub(k,3)*zdi
      zub(k,4) = zub(k,4)*zdf
      zuc(k,2) = zuc(k,2)*zdi*zdf
      zuc(k,3) = zuc(k,3)*zdi
      zuc(k,4) = zuc(k,4)*zdf
      zud(k,2) = zud(k,2)*zdi*zdf
      end do
      zuax(2) = zuax(2)*zdi*zdf
      zuax(3) = zuax(3)*zdi
      zuax(4) = zuax(4)*zdf

      zefacxf  = zemf/fmf
      zefacxi  = zemi/fmi
      zefac0f  = zqf/fmf
      zefac0i  = zqi/fmi

      zvme = 0
      do ix = 1,mxx
      www   = wgau(ix)
      cc    = xgau(ix)
      ss    = sqrt(1.d0 -  cc**2)

      zkfi  = zemf*zemi - zqfi*cc    ! k'*k
      zaa1  = zqf2+zqi2+2.d0*zqfi*cc+fbi2
      zaa2  = 1.d0/((zebi-zemf)**2 -zaa1)  ! 1/(u-m^2)
      zaa3  = 1.d0/((zebf-zemi)**2 -zaa1)  ! 1/(u'-m^2)
      zuf(1)  = fnuc/2.d0*(zaa2+zaa3)
      zuf(2)  = (zwi*zaa2+zwf*zaa3)/2.d0
      zuf(3)  = -(zaa2+zaa3)/2.d0
      xdd(2, 2) = (1.d0+cc)/2.d0
      xdd(2, 0) = -ss/s2
      xdd(2,-2) = (1.d0-cc)/2.d0
      xdd(0, 2) = ss/s2
      xdd(0, 0) = cc
      xdd(0,-2) = -ss/s2
      xdd(-2, 2) = (1.d0-cc)/2.d0
      xdd(-2, 0) = ss/s2
      xdd(-2,-2) = (1.d0+cc)/2.d0
c
      zqfei(0) = (zqi*zemf-cc*zqf*zemi)/fmi    ! k'*e
      zqfei(2) =   ss*zqf/s2
      zqfei(-2)= - zqfei(2)
      zqief(0) = (zqf*zemi-cc*zqi*zemf)/fmi    ! k*e'
      zqief(2) = - ss*zqi/s2
      zqief(-2)= - zqief(2)

      do kx = 1,4
      zuoa(kx)= zuf(1)*zua(1,kx)+zuf(2)*zua(2,kx)+zuf(3)*zua(3,kx)
      end do
  
      zzz    =(zuf(1)*yy2s-zuf(3)*yy2*2.d0)*zkfi
      zuoa(1)=zuoa(1) + zzz
      zuoa(2)=zuoa(2) - zzz*zdi*zdf
      zuoa(3)=zuoa(3) - zzz*zdi
      zuoa(4)=zuoa(4) + zzz*zdf
      zuoap(1)=zuf(2)*(zuax(1)-yy2s*zkfi)
      zuoap(2)=zuf(2)*(zuax(2)-yy2s*zkfi*zdi*zdf)
      zuoap(3)=zuf(2)*(zuax(3)-yy2s*zkfi*zdi)
      zuoap(4)=zuf(2)*(zuax(4)-yy2s*zkfi*zdf)


      do kx = 1,4
      zuob(kx)=zuf(1)*zub(1,kx)+zuf(2)*zub(2,kx)+zuf(3)*zub(3,kx)
      zuoc(kx)=zuf(1)*zuc(1,kx)+zuf(2)*zuc(2,kx)+zuf(3)*zuc(3,kx)
      end do
      do kx = 1,2
      zuod(kx)=zuf(1)*zud(1,kx)+zuf(2)*zud(2,kx)+zuf(3)*zud(3,kx)
      end do

      zans2  = 0
      zans3  = 0

      do isf = -1,1,2
      do isi = -1,1,2
      xdfun  = dfun(1, isi, isf,ix)
      xdfuni = dfun(1,-isi, isf,ix)
      xdfunf = dfun(1, isi,-isf,ix)
      xdfunfi= dfun(1,-isi,-isf,ix)
      xfi   = isf*isi
c-----------------------------------------------------
      zzz9 =  zuod(1) + zuod(2)*xfi
      do imf = -2,2,2
      do imi = -2,2,2
      zans2(imf,isf,imi,isi) = zans2(imf,isf,imi,isi)
     &                      +  zqfei(imi)*zqief(imf)*zzz9*xdfun
      end do
      end do
c------------------------------------------------------
      zzz5=  (zuob(1) + zuob(2)*xfi)*zefac0i
      zzz7=  (zuoc(1) + zuoc(2)*xfi)*zefac0f
      do imx = -2,2,2
      zans2(0,isf,imx,isi)=zans2(0,isf,imx,isi)+zzz7*zqfei(imx)*xdfun
      zans2(imx,isf,0,isi)=zans2(imx,isf,0,isi)+zzz5*zqief(imx)*xdfun
      end do
c-------------------------------------------------------
      zzz6=  zuob(3)     + zuob(4)*xfi
      zzz8=  zuoc(3)*xfi + zuoc(4)
      zzz60= zzz6*zefacxi
      zzz80= zzz8*zefacxf
      do imx = -2,2,2
      zans2(0,isf,imx,isi)=zans2(0,isf,imx,isi)+zzz80*zqfei(imx)*xdfun
      zans2(imx,isf,0,isi)=zans2(imx,isf,0,isi)+zzz60*zqief(imx)*xdfun
      zans2(-2*isf,isf,imx,isi)=zans2(-2*isf,isf,imx,isi)
     &        +zzz8*zqfei(imx)*s2*xdfunf
      zans2(imx,isf,-2*isi,isi)=zans2(imx,isf,-2*isi,isi)
     &        +zzz6*zqief(imx)*s2*xdfuni
      end do
c-------------------------------------------------------
      zzz1 =( zuoa(1)+zuoap(1)+ ( zuoa(2)+zuoap(2))*xfi)*zefac0f*zefac0i
      zzz2 = -zuoa(1)+zuoap(1)+ (-zuoa(2)+zuoap(2))*xfi
      zzz3 =(  zuoa(3)+zuoap(3)      + ( zuoa(4)+zuoap(4))*xfi)*zefac0f
      zzz4 =((-zuoa(3)+zuoap(3))*xfi    -zuoa(4)+zuoap(4)     )*zefac0i

      zans2(0,isf,0,isi) = zans2(0,isf,0,isi)+zzz1*xdfun

      do imx = -2,2,2
      zimxi  = zffi(imx)*fff(imx)*dfun(1,isi+imx,isf,ix)
      zimxf  = zfff(imx)*fff(imx)*dfun(1,isi,isf+imx,ix)
      zans2(0,isf,imx,isi) = zans2(0,isf,imx,isi) + zzz3*zimxi
      zans2(imx,isf,0,isi) = zans2(imx,isf,0,isi) + zzz4*zimxf
      zans3(0,isf,imx,isi) = zans3(0,isf,imx,isi) + zimxi*zefac0f
     &                      *(-zdi + zdf*xfi)*2.d0
      zans3(imx,isf,0,isi) = zans3(imx,isf,0,isi) - zimxf*zefac0i
     &                      *(-zdi*xfi + zdf)*2.d0
      end do

      do imf = -2,2,2
      do imi = -2,2,2
      zfacx  = zfff(imf)*zffi(imi)
      faccx  = fff(imf)*fff(imi)*dfun(1,isi+imi,isf+imf,ix)*xfi
      faccy  = xdfun*xdd(imi,imf)
      zans2(imf,isf,imi,isi) = zans2(imf,isf,imi,isi)
     &  +   zzz2*zfacx*(2.d0*faccy - faccx)
      zans3(imf,isf,imi,isi) = zans3(imf,isf,imi,isi)
     &   +  2.d0*zfacx*(   - faccy + faccx)*(1.d0-zdi*zdf*xfi)
      end do
      end do
c--------------------------------------------------------------
      end do
      end do

      zans1  = 0
      do isf = -1,1,2
      do isi = -1,1,2
      zans1(     0,isf,     0,isi)=dfun(1, isi, isf,ix)
     &                             *(zzg00 + zzg11*isf*isi)
      zans1(     0,isf,-2*isi,isi)=dfun(1,-isi, isf,ix)
     &                             *(zzh00+zzh11*isf*isi)
      zans1(-2*isf,isf,     0,isi)=dfun(1, isi,-isf,ix)
     &                             *(zzi00 + zzi11*isf*isi)
      zans1(-2*isf,isf,-2*isi,isi)=dfun(1,-isi,-isf,ix)
     &                             *(zzd11 + zzd00*isf*isi)*2.d0
      end do
      end do

c
c  j-projection
c
      do imf  = -1,1,2
      do imi  = -1,1,2
      do immf = -2,2,2
      do immi = -2,2,2

c      if(ix.eq.1) then
c      write(1,1010)immf,imf,immi,imi
c     &  ,zans2(immf,imf,immi,imi)*zffb
c     &  ,zans1(immf,imf,immi,imi)/fiso(1,ich,1),cc
c 1010 format(1h ,'new ',4i3,5e15.5)
c      end if

      ihi = imi + immi
      ihf = imf + immf
      do jx = 1,mxj,2
      ddd   = dfun(jx,ihi,ihf,ix)*www
      zvme(jx,immf,imf,immi,imi,1)=zvme(jx,immf,imf,immi,imi,1)
     & +ddd*(zans1(immf,imf,immi,imi)
     &      +zans2(immf,imf,immi,imi)*fiso(1,ich,2)*zffb
     &      +zans3(immf,imf,immi,imi)*fiso(1,ich,3)*zffc)
      zvme(jx,immf,imf,immi,imi,3)=zvme(jx,immf,imf,immi,imi,3)
     & +ddd*(zans2(immf,imf,immi,imi)*fiso(3,ich,2)*zffb
     &      +zans3(immf,imf,immi,imi)*fiso(3,ich,3)*zffc)
      end do ! jx
      end do  ! immf
      end do  ! immi
      end do ! imf
      end do ! imi

c      write(1,1012)ix,cc
c 1012 format(1h ,'new ',i3,e15.5)
      end do


c
c           lsj scheme
c
      icci = 5
      iccf = 5
      jmf  = 2
      jmi  = 2
      jbf  = 1
      jbi  = 1

      zpot = 0
      do 200 jjx = 1,mxj,2

      do 300 idxf= 1,6
      do 300 idxi= 1,6
      ipxi  = jip(jjx,idxi,icci)
      ipxf  = jip(jjx,idxf,iccf)
      iptest = ipxi*ipxf
      if(iptest.eq.1) then

      do 210 imfz= -jmf,jmf,2
      do 210 ibfz= -jbf,jbf,2
      xxxf = xef(jjx,idxf,imfz,ibfz,iccf)

      if(abs(xxxf).gt.1.d-20)then

      do 220 imiz= -jmi,jmi,2
      do 220 ibiz= -jbi,jbi,2
      www = xef(jjx,idxi,imiz,ibiz,icci)*xxxf

      if(abs(www).gt.1.d-20) then
      do iso = 1,3
      zpot(jjx,idxf,idxi,iso) = zpot(jjx,idxf,idxi,iso)
     &         + www*zvme(jjx,imfz,ibfz,imiz,ibiz,iso)
      end do
      end if

 220  continue

      end if

 210  continue

      end if  ! parity test

 300  continue
 200  continue

      return
      end
c------------------------------------------------------------
c  potential 11 pi N -> pi D
c  
c  1 s- nucleon
c  2 u- nucleon
c  3 t- rho
c  4 s- Delta
c  5 u- Delta
c
      subroutine vpn2pd(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-----------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)
      common / cpidx / index(3,-3:3)

      dimension zvme(20,-3:3,-1:1,3),zpot(20,6,6,3)
      dimension zepsf(0:3,-2:2),zepsfkf(-2:2),zsnuc(-3:3)
      dimension                 zepsfki(-2:2),zunuc(-3:3)
      dimension zc10(-2:2),zc01(-2:2),zcrho(-3:3)
      dimension zsd01(-2:2),zsd10(-2:2),zsdel(-3:3)
      dimension zud01(-2:2),zud10(-2:2),zudel(-3:3)

      mxl    = 10
      ich    = 11
      mxp    = mxpot(ich)
      zi     = (0.d0,1.d0)
      pi2    = 2.d0*pi
      ss2    = 1.d0/sqrt(2.d0)
      ss3    = 1.d0/sqrt(3.d0)
      ss23   = sqrt(2.d0)/sqrt(3.d0)

      fmi    = fpio
      fbi    = fnuc
      fmf    = fpio
      fbf    = fdel
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2

      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi
      zwfip  = zwf + zwi
      zwfim  = zwf - zwi
      fmdnp  = fdel + fnuc
      fmdnm  = fdel - fnuc
      zqfi   = zqf *zqi
      zemfi  = zemf*zemi
      

      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

      zvrta =  zvtx(zqf,vndpi,mndpi)*zvtx(zqi,vnnpi,mnnpi)
      zvrtb =  zvtx(zqi,vndpi,mndpi)*zvtx(zqf,vnnpi,mnnpi)
      zvrtd =  zvtx(zqf,vddpi,mddpi)*zvtx(zqi,vndpi,mndpi)
      zvrte =  zvtx(zqi,vddpi,mddpi)*zvtx(zqf,vndpi,mndpi)
      zffa     = gpind*gpin/fpio**2*zfac*zvrta*swv(ich,1)
      zffb     = gpind*gpin/fpio**2*zfac*zvrtb*swv(ich,2)
c correct sign 6/2/2006 - 6/28/2006
      zffd     =-gpidd*gpind/fpio**2*zfac*zvrtd*swv(ich,4)
      zffe     =-gpidd*gpind/fpio**2*zfac*zvrte*swv(ich,5)
c----  s-channel nucleon only isospin 1/2 ---------------
      zs1101 = -((zwi+fnuc)*zqi /(zwi**2 - fnuc**2)
     &         + (zwf+fnuc)*zqi /(zwf**2 - fnuc**2))/2.d0
      zs1200 =  ((zwi+fnuc)*zemi/(zwi**2 - fnuc**2)
     &         + (zwf+fnuc)*zemi/(zwf**2 - fnuc**2))/2.d0
      zs2100 =  ((zwi-fnuc)*zemi/(zwi**2 - fnuc**2)
     &         + (zwf-fnuc)*zemi/(zwf**2 - fnuc**2))/2.d0
      zs2201 = -((zwi-fnuc)*zqi /(zwi**2 - fnuc**2)
     &         + (zwf-fnuc)*zqi /(zwf**2 - fnuc**2))/2.d0
      zs01x   = zs1101 - zs1200*zdi
      zs10x   =        + zs2100*zdf - zs2201*zdi*zdf 
      zs01    = zs01x*zffa*fiso(1,ich,1)
      zs10    = zs10x*zffa*fiso(1,ich,1)
c-----   u-channel nucleon exchange -----------------------
c
c   1/(u - m^2)[zu00a + zu01a*l + zu10a*l' + zu11a*l*l'
c             +(zu00b + zu01b*l + zu10b*l' + zu11b*l*l')*x]
c
      zemfqi= zemf*zqi
      zemfqf= zemf*zqf
      zdfi  = zdf*zdi
      zebip = zebi+fnuc
      zebim = zebi-fnuc
      zu01ax = -zemfqi+zebim*zqf*zdfi
     &         -zdi*(zemf*zebip-fpio**2)
     &         -zdf*zqfi
      zu10ax = -zebip*zqf+zemfqi*zdfi
     &         +zdi*zqfi
     &         +zdf*(zemf*zebim-fpio**2)
      zu01b =-2.d0*zqfi*zdi*zffb
      zu10b = 2.d0*zqfi*zdf*zffb
      zu01a = zu01ax*zffb
      zu10a = zu10ax*zffb
c--------------  s-channel delta  isospin 3/2 only--------------------
      zzzz   = zwf**2 - fdel**2
      zxxx   = zffd/zzzz*fiso(3,11,4)
      zxx1   = (zwf**2 - fdel**2)*fdel*zwi - 6.d0*fdel*zemi*zwf**2
      zxx2   = (zwf**2 - fdel**2)*fdel*fnuc
     &        +(4.d0*fdel**2+2.d0*zwf**2)*zemi*zwf
      zxaa   = zxx1 + zxx2
      zxbb   = zxx1 - zxx2
      zyaa   = (fdel+zwf)*( zemi*zwf-(fnuc+zwi)*fdel)
      zybb   = (fdel-zwf)*(-zemi*zwf+(fnuc-zwi)*fdel)
      zdalf1 = - (fdel - zwf)**2 * zxxx
      zdbet1 =   (fdel + zwf)**2 * zxxx
      zdalf2 = (zxaa/3.d0/fdel**2 + zyaa*2.d0/fdel/3.d0)*zxxx
      zdbet2 = (zxbb/3.d0/fdel**2 + zybb*2.d0/fdel/3.d0)*zxxx
c------------- u channel delta -----------------------------------
      zcefx105a=-fdel*zwf
      zcefx105b=0.0
      zcefx105c=fdel*zwf*(2.0*fdel**2+2.0*fdel*fnuc+fnuc**2-fpio**2)
      zcefx205a=0.0
      zcefx205b=0.0
      zcefx205c=3.0*fdel**2*(-2.0*fdel*zwi+fnuc*zwf-fnuc*zwi)
      zcefx305a=-2.0*fdel*zwi+fnuc*zwf-fnuc*zwi
      zcefx305b=0.0
      zcefx305c=-fdel**3*zwi+fdel**2*fnuc*zwf-fdel**2*fnuc*zwi+fdel*
     . fnuc**2*zwf+2.0*fdel*fnuc**2*zwi-3.0*fdel*fpio**2*zwi-fnuc**3*
     . zwf+fnuc**3*zwi+fnuc*fpio**2*zwf-fnuc*fpio**2*zwi
      zcefx15a=-fdel*fnuc
      zcefx15b=0.0
      zcefx15c=fdel*(-2.0*fdel**3-2.0*fdel**2*fnuc+2.0*fdel*fpio**2+
     . fnuc**3-fnuc*fpio**2)
      zcefx25a=0.0
      zcefx25b=-6.0*fdel**2
      zcefx25c=3.0*fdel**2*(-2.0*fdel*fnuc-fnuc**2+zwf*zwi)
      zcefx35a=-2.0*fdel*fnuc-fnuc**2+zwf*zwi
      zcefx35b=2.0*(-fdel**2-fdel*fnuc+fnuc**2-fpio**2)
      zcefx35ab=-2
      zcefx35c=-fdel**3*fnuc-fdel**2*fnuc**2+fdel**2*zwf*zwi+2.0*fdel*
     . fnuc**3-3.0*fdel*fnuc*fpio**2+fdel*fnuc*zwf*zwi+fnuc**4-fnuc**
     . 2*fpio**2-fnuc**2*zwf*zwi+fpio**2*zwf*zwi
c  part2
      zcef105a=0.0
      zcef105b=-2.0*fdel*zwi
      zcef105c=fdel*(4.0*fdel**2*zwf-3.0*fdel**2*zwi+2.0*fdel*fnuc*
     . zwf-fpio**2*zwi+zwf**2*zwi)
      zcef205a=0.0
      zcef205b=0.0
      zcef205c=-6.0*fdel**3*zwi
      zcef305a=fdel*zwf
      zcef305b=-6.0*fdel*zwi
      zcef305c=fdel*(-fdel**2*zwf-3.0*fdel**2*zwi-fpio**2*zwf-2.0*
     . fpio**2*zwi+3.0*zwf**2*zwi)
      zcef15a=0.0
      zcef15b=-2.0*fdel*fnuc
      zcef15c=fdel*(-2.0*fdel**3-3.0*fdel**2*fnuc+2.0*fdel*fpio**2-
     . 2.0*fdel*zwf**2+2.0*fdel*zwf*zwi-fnuc*fpio**2+fnuc*zwf**2)
      zcef25a=-3.0*fdel**2
      zcef25b=0.0
      zcef25c=3.0*fdel**3*(fdel-2.0*fnuc)
      zcef35a=-2.0*fdel**2-fpio**2+zwf**2
      zcef35b=2.0*fdel*(fdel-3.0*fnuc)
      zcef35ab=-2.d0
      zcef35c=fdel*(2.0*fdel**3-3.0*fdel**2*fnuc+2.0*fdel*fpio**2-
     . fdel*zwf**2-2.0*fnuc*fpio**2+3.0*fnuc*zwf**2)
c
c------------------------------------------------------

      zvme = 0
      zpot = 0

c-------------------------------------------------------


      do ix = 1,mxx
      www   = wgau(ix)
      c     = xgau(ix)
      s     = sqrt(1.d0 -  c**2)
      c2    = sqrt((1.d0 + c)/2.d0)
      s2    = sqrt((1.d0 - c)/2.d0)
      zqfix = zqfi*c

c----------------- u nucleon -------------------------------------
      zenn  = sqrt(fnuc**2 + zqi2 + zqf2 + 2.d0*zqfix)
      zenp  = zenn + fnuc
      zenm  = zenn - fnuc
      zu01cx = zemfqi+(-zemf+zenp)*zqf*zdfi
     &   -(zemf*zenm-zqf2-2.d0*zqfix)*zdi + zqfi*zdf
      zu10cx = zemfqf - zenm*zqf-zemfqi*zdfi
     &   +(zemf*zenp-zqf2-2.d0*zqfix)*zdf - zqfi*zdi
      zu10c  = zu10cx*zffb
      zu01c  = zu01cx*zffb

      zxxx  = 1.d0/(fpio**2 - 2.d0*(zebi*zemf+zqfix))
      zyyy  = (1.d0/(zebf-zemi+zenn)-1.d0/(zebi-zemf+zenn))/4.d0/zenn
      zu10  = (zu10a + zu10b*c)*zxxx + zu10c*zyyy
      zu01  = (zu01a + zu01b*c)*zxxx + zu01c*zyyy
c------------------t rho -------------------------------------------
      zqx   = sqrt(zqi2+zqf2 - 2.d0*zqfix)
      zvrtc =  zvtx(zqx,vndrh,mndrh)*zvtx(zqx,vrpp, mrpp )       
      zffc  = grnd*grpp/fmrho*zfac   *zvrtc*swv(ich,3)
      zdrho1= zffc/((zebi-zebf)**2 - zqx**2 - fmrho**2)
      zdrho2= zffc/((zemi-zemf)**2 - zqx**2 - fmrho**2)
c----------------- u delta -----------------------------------------
      xxx   = 3.d0*fdel**2
      zkfki = zemf*zemi - zqfix
      zuu   = (zebi - zemf)**2 - (zqi2+zqf2 + 2.d0*zqfix)
      zxxx  = zffe/(zuu - fdel**2)/2.d0/xxx
      zud1051=zxxx*(zcefx105a*zuu + zcefx105b*zkfki + zcefx105c)
      zud2051=zxxx*(zcefx205a*zuu + zcefx205b*zkfki + zcefx205c)
      zud3051=zxxx*(zcefx305a*zuu + zcefx305b*zkfki + zcefx305c)
      zud151=zxxx*(zcefx15a*zuu+zcefx15b*zkfki+zcefx15c)
      zud251=zxxx*(zcefx25a*zuu+zcefx25b*zkfki+zcefx25c)
      zud351=zxxx*(zcefx35a*zuu+zcefx35b*zkfki+zcefx35c-2.d0*zuu*zkfki)
c--------------------------------------------------------------------
      zuu   = (zebf - zemi)**2 - (zqi2+zqf2 + 2.d0*zqfix)
      zxxx  = zffe/(zuu - fdel**2)/2.d0/xxx
      zud1052=zxxx*(zcef105a*zuu + zcef105b*zkfki + zcef105c)
      zud2052=zxxx*(zcef205a*zuu + zcef205b*zkfki + zcef205c)
      zud3052=zxxx*(zcef305a*zuu + zcef305b*zkfki + zcef305c)
      zud152=zxxx*(zcef15a*zuu + zcef15b*zkfki + zcef15c)
      zud252=zxxx*(zcef25a*zuu + zcef25b*zkfki + zcef25c)
      zud352=zxxx*(zcef35a*zuu + zcef35b*zkfki + zcef35c-2.d0*zuu*zkfki)
c  test-----------------------------------
c      zud1051 = 0
c      zud2051 = 0
c      zud3051 = 0
c      zud151 = 0
c      zud251 = 0
c      zud351 = 0
c-------------------------------------------------------------------
      zepsf(0,2) = 0
      zepsf(1,2) = - ss2*c
      zepsf(2,2) =   ss2*zi
      zepsf(3,2) = + ss2*s

      zepsf(0,-2) = 0
      zepsf(1,-2) =  ss2*c
      zepsf(2,-2) =  ss2*zi
      zepsf(3,-2) = -ss2*s

      zepsf(0,0) = -  zqf/fbf
      zepsf(1,0) = s*zebf/fbf
      zepsf(2,0) = 0
      zepsf(3,0) = c*zebf/fbf

      do imx = -2,2,2
      zepsfkf(imx) = zepsf(0,imx)*zemf
     &             -(zepsf(3,imx)*c + zepsf(1,imx)*s)*zqf
      zepsfki(imx) = zepsf(0,imx)*zemi-zepsf(3,imx) *zqi

c for rho-exchange
      zxkfpi       = zepsf(0,imx)*zebi+zepsf(3,imx) *zqi
      zxkfim       = zepsfkf(imx)-zepsfki(imx)
      zxkfip       = zepsfkf(imx)+zepsfki(imx)
      zraa   =zxkfpi*zwfip*zdrho1+(zxkfim*zwfip-zxkfip*zwfim)*zdrho2
      zrbb   =(- zxkfpi*fmdnm+zxkfip*fmdnp)*zdrho1
     &       +(- zxkfim*fmdnm+zxkfip*fmdnp)*zdrho2
      zc01(imx)= zdi*(-zraa-zrbb)/2.d0
      zc10(imx)= zdf*(-zraa+zrbb)/2.d0
c for s-delta
      zalf     = zepsfki(imx)*zdalf1 + zepsfkf(imx)*zdalf2
      zbet     = zepsfki(imx)*zdbet1 + zepsfkf(imx)*zdbet2
      zsd01(imx) = zdi*zalf
      zsd10(imx) = zdf*zbet
c for u-delta
      zud05    = zepsfki(imx)*(zud1051+zud1052)
     &          +zepsfkf(imx)*(zud2051+zud2052)
     &          +(zxkfpi-zepsfkf(imx))*zud3051
     &          +(      -zepsfki(imx))*zud3052

      zud5     = zepsfki(imx)*(zud151+zud152)
     &          +zepsfkf(imx)*(zud251+zud252)
     &          +(zxkfpi-zepsfkf(imx))*zud351
     &          +(      -zepsfki(imx))*zud352
      zud01(imx)= zdi*(-zud05-zud5)
      zud10(imx)= zdf*(-zud05+zud5)

      end do

      do isn = -1,1,2
      xisn = isn

      zsu  = dfun(1,isn, 1,ix)*(zs01*xisn+zs10)
      zsd  = dfun(1,isn,-1,ix)*(zs01*xisn-zs10)
      zuu  = dfun(1,isn, 1,ix)*(zu01*xisn+zu10)
      zud  = dfun(1,isn,-1,ix)*(zu01*xisn-zu10)

      zsnuc( 3)  =     zepsfkf( 2)*zsu
      zsnuc( 1)  = ss3*zepsfkf( 2)*zsd + ss23*zepsfkf(0)*zsu
      zsnuc(-1)  = ss3*zepsfkf(-2)*zsu + ss23*zepsfkf(0)*zsd
      zsnuc(-3)  =     zepsfkf(-2)*zsd

      zunuc( 3)  =     zepsfki( 2)*zuu
      zunuc( 1)  = ss3*zepsfki( 2)*zud + ss23*zepsfki(0)*zuu
      zunuc(-1)  = ss3*zepsfki(-2)*zuu + ss23*zepsfki(0)*zud
      zunuc(-3)  =     zepsfki(-2)*zud
      

      zcrho( 3)  =    dfun(1,isn, 1,ix)*( zc10( 2) + zc01( 2)*xisn)
      zcrho( 1)  =ss3*dfun(1,isn,-1,ix)*(-zc10( 2) + zc01( 2)*xisn)
     &          +ss23*dfun(1,isn, 1,ix)*( zc10( 0) + zc01( 0)*xisn)
      zcrho(-1)  =ss3*dfun(1,isn, 1,ix)*( zc10(-2) + zc01(-2)*xisn)
     &          +ss23*dfun(1,isn,-1,ix)*(-zc10( 0) + zc01( 0)*xisn)
      zcrho(-3)  =    dfun(1,isn,-1,ix)*(-zc10(-2) + zc01(-2)*xisn)

      zsdel( 3)  =    dfun(1,isn, 1,ix)*( zsd10( 2) + zsd01( 2)*xisn)
      zsdel( 1)  =ss3*dfun(1,isn,-1,ix)*(-zsd10( 2) + zsd01( 2)*xisn)
     &          +ss23*dfun(1,isn, 1,ix)*( zsd10( 0) + zsd01( 0)*xisn)
      zsdel(-1)  =ss3*dfun(1,isn, 1,ix)*( zsd10(-2) + zsd01(-2)*xisn)
     &          +ss23*dfun(1,isn,-1,ix)*(-zsd10( 0) + zsd01( 0)*xisn)
      zsdel(-3)  =    dfun(1,isn,-1,ix)*(-zsd10(-2) + zsd01(-2)*xisn)

      zudel( 3)  =    dfun(1,isn, 1,ix)*( zud10( 2) + zud01( 2)*xisn)
      zudel( 1)  =ss3*dfun(1,isn,-1,ix)*(-zud10( 2) + zud01( 2)*xisn)
     &          +ss23*dfun(1,isn, 1,ix)*( zud10( 0) + zud01( 0)*xisn)
      zudel(-1)  =ss3*dfun(1,isn, 1,ix)*( zud10(-2) + zud01(-2)*xisn)
     &          +ss23*dfun(1,isn,-1,ix)*(-zud10( 0) + zud01( 0)*xisn)
      zudel(-3)  =    dfun(1,isn,-1,ix)*(-zud10(-2) + zud01(-2)*xisn)

      do jx = 1,mxj,2
      do isd= -3,3,2
      zvme(jx,isd,isn,1) = zvme(jx,isd,isn,1)+www*dfun(jx,isn,isd,ix)
     &   *(zsnuc(isd)+zunuc(isd)*fiso(1,ich,2)+zcrho(isd)*fiso(1,ich,3)
     &    +zudel(isd)*fiso(1,ich,5))
      zvme(jx,isd,isn,3) = zvme(jx,isd,isn,3)+www*dfun(jx,isn,isd,ix)
     &   *(zsdel(isd)+zunuc(isd)*fiso(3,ich,2)+zcrho(isd)*fiso(3,ich,3)
     &    +zudel(isd)*fiso(3,ich,5))
      end do  !ise
      end do  !jx

      end do  !isn

      end do  ! ix loop cos

c
c           lsj scheme
c
      jsf    = 3
      jsi    = 1
      ipi    = -1  ! pion parity
      ipf    = -1  ! pion parity

      do jjx = 1,mxj,2
      do lfx = abs(jjx-jsf),jjx+jsf,2
      do lix = abs(jjx-jsi),jjx+jsi,2
      lix2   = lix/2
      lfx2   = lfx/2
      ipxi   = (-1)**lix2*ipi
      ipxf   = (-1)**lfx2*ipf
      iptest = ipxi*ipxf

      if(iptest.eq.1) then

      lff  = lfx - jjx
      lii  = lix - jjx
      idxf = index(jsf,lff)
      idxi = index(jsi,lii)

      do iso = 1,3
      zsum   = 0
      do isf  = -jsf,jsf,2
      do isi  = -jsi,jsi,2
      ww  = flsjxh(lfx,jsf,isf,jjx)*flsjxh(lix,jsi,isi,jjx)
      zsum= zsum+ww*zvme(jjx,isf,isi,iso)
      end do
      end do
      zpot(jjx,idxf,idxi,iso) = zsum
      end do  !iso

      end if

      end do  !lf
      end do  !li
      end do  !j

      return
      end

c------------------------------------------------------------
c  potential 12 eta N -> pi D
c  
c  1 s- nucleon
c
      subroutine ven2pd(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-----------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)
      common / cpidx / index(3,-3:3)

      dimension zvme(20,-3:3,-1:1,3),zpot(20,6,6,3)
      dimension zepsf(0:3,-2:2),zepsfk(-2:2),zsnuc(-3:3)

      ich    = 12
      mxp    = mxpot(ich)
      zi     = (0.d0,1.d0)
      pi2    = 2.d0*pi
      ss2    = 1.d0/sqrt(2.d0)
      ss3    = 1.d0/sqrt(3.d0)
      ss23   = sqrt(2.d0)/sqrt(3.d0)

      fmi    = feta
      fbi    = fnuc
      fmf    = fpio
      fbf    = fdel
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2

      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi
      
      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

      zvrta =  zvtx(zqf,vndpi,mndpi)*zvtx(zqi,vnnet,mnnet)
      zffa     = gpind*genn/fpio/feta*zfac*zvrta*swv(ich,1)

      zs1101 = -((zwi+fnuc)*zqi /(zwi**2 - fnuc**2)
     &         + (zwf+fnuc)*zqi /(zwf**2 - fnuc**2))/2.d0
      zs1200 =  ((zwi+fnuc)*zemi/(zwi**2 - fnuc**2)
     &         + (zwf+fnuc)*zemi/(zwf**2 - fnuc**2))/2.d0
      zs2100 =  ((zwi-fnuc)*zemi/(zwi**2 - fnuc**2)
     &         + (zwf-fnuc)*zemi/(zwf**2 - fnuc**2))/2.d0
      zs2201 = -((zwi-fnuc)*zqi /(zwi**2 - fnuc**2)
     &         + (zwf-fnuc)*zqi /(zwf**2 - fnuc**2))/2.d0
c
c include vertex, coupling constant.isospin
c
c  s-channel nucleon only isospin 1/2
c
      zs01x   = zs1101 - zs1200*zdi
      zs10x   =        + zs2100*zdf - zs2201*zdi*zdf 
      zs01    = zs01x*zffa*fiso(1,ich,1)
      zs10    = zs10x*zffa*fiso(1,ich,1)

c------------------------------------------------------

      zvme = 0
      zpot = 0


      do jx  = 1,mxj,2
      do idf = 1,6
      do idi = 1,6
      do iso = 1,3
      zpot(jx,idf,idi,iso)=0
      end do
      end do
      end do
      end do

      do  jx     = 1,mxj,2
      do  md     = -3,3,2
      do  mn     = -1,1,2
      do  iso    = 1,3,2
      zvme(jx,md,mn,iso)  = 0
      end do
      end do
      end do
      end do
c-------------------------------------------------------

      zqfi  = zqf *zqi
      zemfi = zemf*zemi

      do ix = 1,mxx
      www   = wgau(ix)
      c     = xgau(ix)
      s     = sqrt(1.d0 -  c**2)
      c2    = sqrt((1.d0 + c)/2.d0)
      s2    = sqrt((1.d0 - c)/2.d0)

      zepsf(0,2) = 0
      zepsf(1,2) = - ss2*c
      zepsf(2,2) =   ss2*zi
      zepsf(3,2) = + ss2*s

      zepsf(0,-2) = 0
      zepsf(1,-2) =  ss2*c
      zepsf(2,-2) =  ss2*zi
      zepsf(3,-2) = -ss2*s

      zepsf(0,0) = -  zqf/fbf
      zepsf(1,0) = s*zebf/fbf
      zepsf(2,0) = 0
      zepsf(3,0) = c*zebf/fbf

      do imx = -2,2,2
      zepsfk(imx) = zepsf(0,imx)*zemf
     &            -(zepsf(3,imx)*c + zepsf(1,imx)*s)*zqf
      end do

      do isn = -1,1,2
      xisn = isn

      zsu  = dfun(1,isn,1,ix)*(  zs01*xisn + zs10)
      zsd  = dfun(1,isn,-1,ix)*( zs01*xisn - zs10)

      zsnuc( 3)  = zsu*zepsfk(2)
      zsnuc( 1)  = ss3*zepsfk( 2)*zsd + ss23*zepsfk(0)*zsu
      zsnuc(-1)  = ss3*zepsfk(-2)*zsu + ss23*zepsfk(0)*zsd
      zsnuc(-3)  = zsd*zepsfk(-2)
      
      do jx = 1,mxj,2
      do isd= -3,3,2
      zvme(jx,isd,isn,1) = zvme(jx,isd,isn,1)+www*dfun(jx,isn,isd,ix)
     &                                   *zsnuc(isd)
      end do  !ise
      end do  !jx

      end do  !isn

      end do  ! ix loop cos

c
c           lsj scheme
c
      jsf    = 3
      jsi    = 1
      ipi    = -1  ! eta parity
      ipf    = -1  ! pion parity

      do jjx = 1,mxj,2
      do lfx = abs(jjx-jsf),jjx+jsf,2
      do lix = abs(jjx-jsi),jjx+jsi,2
      lix2   = lix/2
      lfx2   = lfx/2
      ipxi   = (-1)**lix2*ipi
      ipxf   = (-1)**lfx2*ipf
      iptest = ipxi*ipxf

      if(iptest.eq.1) then

      lff  = lfx - jjx
      lii  = lix - jjx
      idxf = index(jsf,lff)
      idxi = index(jsi,lii)

      do iso = 1,3
      zsum   = 0
      do isf  = -jsf,jsf,2
      do isi  = -jsi,jsi,2
      ww  = flsjxh(lfx,jsf,isf,jjx)*flsjxh(lix,jsi,isi,jjx)
      zsum= zsum+ww*zvme(jjx,isf,isi,iso)
      end do
      end do
      zpot(jjx,idxf,idxi,iso) = zsum
      end do  !iso

      end if

      end do  !lf
      end do  !li
      end do  !j

      return
      end
c------------------------------------------------------------
c  potential 13 sigma N -> pi D
c  
c  1 s- nucleon
c
      subroutine vsn2pd(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-----------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)
      common / cpidx / index(3,-3:3)

      dimension zvme(20,-3:3,-1:1,3),zpot(20,6,6,3)
      dimension zepsf(0:3,-2:2),zepsfk(-2:2),zsnuc(-3:3)

      ich    = 13
      mxp    = mxpot(ich)
      zi     = (0.d0,1.d0)
      pi2    = 2.d0*pi
      ss2    = 1.d0/sqrt(2.d0)
      ss3    = 1.d0/sqrt(3.d0)
      ss23   = sqrt(2.d0)/sqrt(3.d0)

      fmi    = fsigm
      fbi    = fnuc
      fmf    = fpio
      fbf    = fdel
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2

      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi
      
      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

      zvrta =  zvtx(zqf,vndpi,mndpi)*zvtx(zqi,vnnsi,mnnsi)
      zffa     = -zi*gpind*gsinn/fpio*zfac*zvrta*swv(ich,1)

      zs1100 =  (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))/2.d0
      zs2200 =  (1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))/2.d0
c
c include vertex, coupling constant.isospin
c
c  s-channel nucleon only isospin 1/2
c
      zs00    = zs1100*zffa*fiso(1,ich,1)
      zs11    = zs2200*zffa*fiso(1,ich,1)*zdi*zdf

c------------------------------------------------------

      zvme = 0
      zpot = 0


c-------------------------------------------------------

      zqfi  = zqf *zqi
      zemfi = zemf*zemi

      do ix = 1,mxx
      www   = wgau(ix)
      c     = xgau(ix)
      s     = sqrt(1.d0 -  c**2)
      c2    = sqrt((1.d0 + c)/2.d0)
      s2    = sqrt((1.d0 - c)/2.d0)

      zepsf(0,2) = 0
      zepsf(1,2) = - ss2*c
      zepsf(2,2) =   ss2*zi
      zepsf(3,2) = + ss2*s

      zepsf(0,-2) = 0
      zepsf(1,-2) =  ss2*c
      zepsf(2,-2) =  ss2*zi
      zepsf(3,-2) = -ss2*s

      zepsf(0,0) = -  zqf/fbf
      zepsf(1,0) = s*zebf/fbf
      zepsf(2,0) = 0
      zepsf(3,0) = c*zebf/fbf

      do imx = -2,2,2
      zepsfk(imx) = zepsf(0,imx)*zemf
     &            -(zepsf(3,imx)*c + zepsf(1,imx)*s)*zqf
      end do

      do isn = -1,1,2
      xisn = isn

      zsu  = dfun(1,isn,1,ix)*(  zs00 + zs11*isn)
      zsd  = dfun(1,isn,-1,ix)*( zs00 - zs11*isn)

      zsnuc( 3)  = zsu*zepsfk(2)
      zsnuc( 1)  = ss3*zepsfk( 2)*zsd + ss23*zepsfk(0)*zsu
      zsnuc(-1)  = ss3*zepsfk(-2)*zsu + ss23*zepsfk(0)*zsd
      zsnuc(-3)  = zsd*zepsfk(-2)
      
      do jx = 1,mxj,2
      do isd= -3,3,2
      zvme(jx,isd,isn,1) = zvme(jx,isd,isn,1)+www*dfun(jx,isn,isd,ix)
     &                                   *zsnuc(isd)
      end do  !ise
      end do  !jx

      end do  !isn

      end do  ! ix loop cos

c
c           lsj scheme
c
      jsf    = 3
      jsi    = 1
      ipi    =  1  ! sigma parity
      ipf    = -1  ! pion  parity

      do jjx = 1,mxj,2
      do lfx = abs(jjx-jsf),jjx+jsf,2
      do lix = abs(jjx-jsi),jjx+jsi,2
      lix2   = lix/2
      lfx2   = lfx/2
      ipxi   = (-1)**lix2*ipi
      ipxf   = (-1)**lfx2*ipf
      iptest = ipxi*ipxf

      if(iptest.eq.1) then

      lff  = lfx - jjx
      lii  = lix - jjx
      idxf = index(jsf,lff)
      idxi = index(jsi,lii)

      do iso = 1,3
      zsum   = 0
      do isf  = -jsf,jsf,2
      do isi  = -jsi,jsi,2
      ww  = flsjxh(lfx,jsf,isf,jjx)*flsjxh(lix,jsi,isi,jjx)
      zsum= zsum+ww*zvme(jjx,isf,isi,iso)
      end do
      end do
      zpot(jjx,idxf,idxi,iso) = zsum
      end do  !iso

      end if

      end do  !lf
      end do  !li
      end do  !j

      return
      end
c------------------------------------------------------------------
c  potential 14 rho N -> pi D
c  
c  1 s- nucleon
c  2 u- nucleon
c                             14
c-------------------------------------------------------------------
      subroutine vrn2pd(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-------------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)

      common / cefflo / cg1h(-2:2,-1:1,3)
     &     ,xef(20,6,-2:2,-3:3,10)
      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
     &     ,jss(20,6,10),jll(20,6,10)
     &     ,jip(20,6,10)

      dimension zpot(20,6,6,3)
      dimension zvme(20,-3:3,-2:2,-1:1,3)
      dimension zans1(-3:3,-2:2,-1:1),zans2(-3:3,-2:2,-1:1)
      dimension zpolfk(-2:2),zpolik(-2:2),zpolfi(-2:2,-2:2)

c      fdel = fnuc
      ich    = 14
      mxp    = mxpot(ich)
      zi     = (0.d0,1.d0)
      pi2    = 2.d0*pi

      fmi    = fmrho
      fbi    = fnuc
      fmf    = fpio
      fbf    = fdel
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2

      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi
      zqfi   = zqf*zqi
      zwfm   = zwf - fdel
      zwfp   = zwf + fdel
      zwim   = zwi - fnuc
      zwip   = zwi + fnuc

c------------------------------------------------------------
      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3
      zvrta =  zvtx(zqi,vnnrho,mnnrho)*zvtx(zqf,vndpi,mndpi)
      zvrtb =  zvtx(zqi,vndrh ,mndrh )*zvtx(zqf,vnnpi,mnnpi)
      zffa     =-zi*grnn*gpind/fpio/2.d0      *zfac*zvrta*swv(ich,1)
      zffb     = zi*grnd*gpin /fpio/fmrho     *zfac*zvrtb*swv(ich,2)

c
c  s-channel
c
      zgam =  (1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))/2.d0
      zdel = -(1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))/2.d0*zdf
      zalf =  1.d0 - xkrho/(2.d0*fnuc)*( zwi-fnuc)
      zbet =-(1.d0 - xkrho/(2.d0*fnuc)*(-zwi-fnuc))*zdi

      zffaxx  = zffa*zwf*fiso(1,14,1)
      zxfa001 = zffaxx*zgam*zalf
      zxfa111 =-zffaxx*zdel*zbet
      zxfb011 =-zffaxx*zgam*zbet
      zxfb101 = zffaxx*zdel*zalf
      zacef0 = -zqf*zqi/fdel/fmrho
      zbcef0 = -zqf*zemi/fdel/fmrho
      zbcef1 = -zqf/fdel*sqrt(2.d0)

      fnxx1     = 2.d0*fnuc
      fnxx2     = fnuc+fdel
c-----------------------------------------------------------


      zvme = 0

      do ix = 1,mxx
      www   = wgau(ix)
      cc    = xgau(ix)
      ss    = sqrt(1.d0 -  cc**2)
      c2x   = (1.d0 + cc)/2.d0
      s2x   = (1.d0 - cc)/2.d0
      zqfix = zqfi*cc
      ss22  = ss/sqrt(2.d0)
c
c  u-channel nucleon
c
      zqx2  =  zqf2 + zqi2 + zqfix*2.d0 ! for u
      zqfqi1= (zemf*zemi - zqfix)*2.d0*fnxx1
      zqfqi2= (zemf*zemi - zqfix)*2.d0*fnxx2

      zux1  = (zebi-zemf)**2 - zqx2
      zux2  = (zebf-zemi)**2 - zqx2
      zu1   = 1.d0/(zux1 - fnuc**2)/2.d0
      zu2   = 1.d0/(zux2 - fnuc**2)/2.d0
      zaa1  = (zwf-zwi)*zu2*zffb
      zaa2  =-(zu1+zu2)*(zux1-fnuc**2)*zffb
      zaa3  =-(zu1+zu2)*2.d0*fnuc*zffb
      zaab  =  zaa1*2.d0*zemf + zaa2
      zxga00=  zaab + zwfm*(zaa1-zaa3)            ! epsilon 0
      zxga11= (zaab + zwfp*(zaa1+zaa3))*zdf*zdi
      zxgb01= (zaab - zwfm*(zaa1+zaa3))*zdi       ! e.s
      zxgb10= (zaab - zwfp*(zaa1-zaa3))*zdf
      zxgc00= 2.d0*(zaa3-zaa1)                    ! k'.e
      zxgc11=-2.d0*(zaa3+zaa1)*zdi*zdf
c
      zxgd00=-(zu1*(zwim*(-zux1+fnuc**2+fnxx1*zwfm)-zqfqi1)
     &      +  zu2*(zwfm*(-zux2+fdel**2+fnxx2*zwim)-zqfqi2))*zffb
      zxgd11=-(zu1*(zwip*(-zux1+fnuc**2-fnxx1*zwfp)+zqfqi1)
     &      +  zu2*(zwfp*(-zux2+fdel**2-fnxx2*zwip)+zqfqi2))*zffb
     &      *zdi*zdf
      zpolfk(0) = -(zqf*zemi+zebf*zqi*cc)/fdel
      zpolfk(2) = - zqi*ss22
      zpolfk(-2)= - zpolfk(2)

      zpolik(0) =  (zqi*zemf-zemi*zqf*cc)/fmrho
      zpolik(2) =   zqf*ss22
      zpolik(-2)= - zpolik(2)

      zpolfi( 0, 0) = - (zqf*zqi+zebf*zemi*cc)/fmrho/fdel
      zpolfi( 0, 2) = zebf*ss22/fdel
      zpolfi( 0,-2) =-zpolfi(0,2)
      zpolfi( 2, 0) =-zemi*ss22/fmrho
      zpolfi(-2, 0) =-zpolfi(2,0)
      zpolfi( 2, 2) = - c2x
      zpolfi( 2,-2) = - s2x
      zpolfi(-2, 2) = zpolfi(2,-2)
      zpolfi(-2,-2) = zpolfi(2,2)

      zans1 = 0
      zans2 = 0

      do isf = -1,1,2
      do isi = -1,1,2
      zxxd   = zxgd00 + isf*isi*zxgd11
      zxxc   = zxgc00 + isf*isi*zxgc11
      zxxa   = zxga00 + isf*isi*zxga11
      zxxb   =(zxgb01*isi + zxgb10*isf)*isi
      do imf = -2,2,2
      izf    = isf + imf
      weix   = cg1h(imf,isf,3)*www
      zaaa   = weix*zpolfk(imf)*sqrt(2.d0)*zxxb
      zbbb   = weix*zpolfk(imf)*(zqi*zxxa+zemi*zxxb)/fmrho
     &                         *dfun(1,isi,isf,ix)
      zans2(izf, 0,isi)=zans2(izf, 0,isi) + zbbb
      if(isi.eq.-1) then
      zans2(izf, 2,isi)=zans2(izf, 2,isi)+dfun(1, 1,isf,ix)*zaaa
      else if(isi.eq.1) then
      zans2(izf,-2,isi)=zans2(izf,-2,isi)+dfun(1,-1,isf,ix)*zaaa
      end if

      do imi = -2,2,2
      zzz    = zxxd*zpolfi(imf,imi)+zxxc*zpolfk(imf)*zpolik(imi)
      zans2(izf,imi,isi) =zans2(izf,imi,isi)+zzz*dfun(1,isi,isf,ix)*weix
      end do

      end do
      end do
      end do

      do isf = -1,1,2
      do isi = -1,1,2
      zxxa   = (zxfa001    +zxfa111*isf*isi)*cg1h(0,isf,3)
      zxxb   = (zxfb011*isi+zxfb101*isf    )*cg1h(0,isf,3)
      zans1(isf,0,isi)=(zacef0*zxxa+zbcef0*zxxb*isi)
     &                 *dfun(1,isi,isf,ix)*www

      if(isi.eq.-1) then
      zans1(isf, 2,isi)=zbcef1*zxxb*isi*dfun(1,1,isf,ix)*www
      else if(isi.eq.1) then
      zans1(isf,-2,isi)=zbcef1*zxxb*isi*dfun(1,-1,isf,ix)*www
      end if

      end do
      end do
c
c  j-projection
c

      do imf = -3,3,2
      do imi = -1,1,2
      do immi = -2,2,2

      ihi = imi + immi
      do jx = 1,mxj,2
      zvme(jx,imf,immi,imi,1)=zvme(jx,imf,immi,imi,1)
     &   +dfun(jx,ihi,imf,ix)*(zans1(imf,immi,imi)
     &                        +zans2(imf,immi,imi)*fiso(1,14,2))
      zvme(jx,imf,immi,imi,3)=zvme(jx,imf,immi,imi,3)
     &   +dfun(jx,ihi,imf,ix)*zans2(imf,immi,imi)*fiso(3,14,2)
      end do ! jx

      end do  ! immi
      end do ! imf
      end do ! imi

      end do

c
c           lsj scheme
c
      iccf = 3
      jmf  = 0
      jbf  = 3

      icci = 5
      jmi  = 2
      jbi  = 1

      zpot = 0
      do 200 jjx = 1,mxj,2

      do 300 idxf= 1,6
      do 300 idxi= 1,6
      ipxi  = jip(jjx,idxi,icci)
      ipxf  = jip(jjx,idxf,iccf)
      iptest = ipxi*ipxf
      if(iptest.eq.1) then

      imfz = 0
      do 210 ibfz= -jbf,jbf,2
      xxf = xef(jjx,idxf,imfz,ibfz,iccf)

      if(abs(xxf).gt.1.d-20)then

      do 220 imiz= -jmi,jmi,2
      do 220 ibiz= -jbi,jbi,2
      www = xef(jjx,idxi,imiz,ibiz,icci)*xxf

      if(abs(www).gt.1.d-20) then
      do iso = 1,3
      zpot(jjx,idxf,idxi,iso) = zpot(jjx,idxf,idxi,iso)
     &         + www*zvme(jjx,ibfz,imiz,ibiz,iso)
      end do
      end if

 220  continue

      end if

 210  continue

      end if  ! parity test

 300  continue
 200  continue

      return
      end
c------------------------------------------------------------------
c  potential 15 pi D -> pi D
c  
c  1 s- nucleon
c  4 t- rho
c                             15
c-------------------------------------------------------------------
      subroutine vpd2pd(zqf,zqi,zpot)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
c-------------------------------------------------------------------
      parameter(njmx=11,maxl=10)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / coupl / gpin,gpind,grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,
     &                            xkrho,xkomg,genn,grnp,gsinn
      common / cvert / vnnpi,vndpi,vnnrho,vnnomg,vrpp,vnnet,vnnsi,
     &                 mnnpi,mndpi,mnnrho,mnnomg,mrpp,mnnet,mnnsi
      common / cmass2 / fma0,fmf0,fma1,fsigme,frhoe
      common / coupl2 / grnd,gpidd,grpp,ga0nn,ga0pe,gf0nn,gf0ee,gsipp,
     &                  gsisi,gopr,grdd,xkrdd,ga1nn,ga1pr
      common / cvert2 / vndrh,vddpi,vnna0,va0pe,vnnf0,vf0ee,vsipp,
     &                  vsisi,vopr,vnna1,va1pr,vddrh,
     &                  mndrh,mddpi,mnna0,ma0pe,mnnf0,mf0ee,msipp,
     &                  msisi,mopr,mnna1,ma1pr,mddrh
      common / cdfi / meshx,mxx,mxj,mxm
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / fisos  / fiso(3,20,20),mxpot(20)
      common / cswv  / swv(20,20)

      common / cefflo / cg1h(-2:2,-1:1,3)
     &     ,xef(20,6,-2:2,-3:3,10)
      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
     &     ,jss(20,6,10),jll(20,6,10)
     &     ,jip(20,6,10)


      dimension zpot(20,6,6,3)
      dimension zvme(20,-3:3,-3:3,3)
      dimension zedd(-2:2,-2:2)

      ich    = 15
      mxp    = mxpot(ich)
      zi     = (0.d0,1.d0)
      pi2    = 2.d0*pi
      ss2    = 1.d0/sqrt(2.d0)
      ss3    = 1.d0/sqrt(3.d0)
      ss23   = sqrt(2.d0)/sqrt(3.d0)

      fmi    = fpio
      fbi    = fdel
      fmf    = fpio
      fbf    = fdel
      fmi2   = fmi**2
      fmf2   = fmf**2
      fbi2   = fbi**2
      fbf2   = fbf**2

      zqf2   = zqf**2
      zqi2   = zqi**2
      zemf   = sqrt(fmf2+zqf2)
      zemi   = sqrt(fmi2+zqi2)
      zebf   = sqrt(fbf2+zqf2)
      zebi   = sqrt(fbi2+zqi2)
      zdf    = zqf/(zebf+fbf)
      zdi    = zqi/(zebi+fbi)
      zwf    = zemf + zebf
      zwi    = zemi + zebi
      zqfi   = zqf*zqi

c------------------------------------------------------------
      zfac   = sqrt( (zebf+fbf)*(zebi+fbi)/4.d0/zebi/zebf)
     &        /sqrt(4.d0*zemi*zemf)*pi2/pi2**3

      zvrta =  zvtx(zqi,vndpi,mndpi)*zvtx(zqf,vndpi,mndpi)
      zffa     = gpind**2/fpio**2*zfac*zvrta*swv(ich,1)
      zxxx     = zffa*zwi*zwf*fiso(1,15,1)
      zxfa00 = zxxx*(1.d0/(zwi-fnuc)+1.d0/(zwf-fnuc))/2.d0
      zxfa11 = zxxx*(1.d0/(zwi+fnuc)+1.d0/(zwf+fnuc))/2.d0
     &             *zdi*zdf
      zpol00   = zqf*zqi/fdel**2
      xfdel2   = 1.d0/sqrt(2.d0)/fdel
      zxxa     = (1.d0+xkrdd)*(zwi+zwf)
      zxxc     = -xkrdd/2.d0/fdel*(zwf-zwi)*(zebf-zebi)
      zxxb1    = -2.d0*fdel
      zxxb2    = xkrdd/fdel
      zxxb3    = - fdel**2 - zwi*zwf + zemf*zemi
c-----------------------------------------------------------
      zvme = 0

      do ix = 1,mxx
      www   = wgau(ix)
      cc    = xgau(ix)
      ss    = sqrt(1.d0 -  cc**2)
      c2    = sqrt((1.d0 + cc)/2.d0)
      s2    = sqrt((1.d0 - cc)/2.d0)
      zqfix = zqfi*cc

      zqx2  =  zqf2 + zqi2 - zqfix*2.d0
      zqx   =  sqrt(zqx2)
      zvrtd =  zvtx(zqx,vddrh,mddrh)*zvtx(zqx,vrpp ,mrpp )
      zffd  = -grdd*grpp*zfac*zvrtd*swv(ich,4)

      zrho1 = 1.d0/((zebi-zebf)**2 - zqx2 - frhoe**2)*zffd/2.d0
      zrho2 = 1.d0/((zemi-zemf)**2 - zqx2 - frhoe**2)*zffd/2.d0
      zaa   = (zrho1+zrho2)*zxxa
      zbb   = (zrho1+zrho2)*(zxxb1 + zxxb2*(zxxb3-zqfix))
      zcc   =  zrho2*zxxc
      zxfb00 =  zaa+zbb+zcc
      zxfb11 = (zaa-zbb-zcc)*zdi*zdf

      zedd( 0, 0) = (zqfi-cc*zebf*zebi)/fdel**2

      zedd( 2, 0) =-zebi*ss*xfdel2
      zedd(-2, 0) =-zedd(2,0)

      zedd( 0, 2) = zebf*ss*xfdel2
      zedd( 0,-2) =-zedd(0,2)

      zedd( 2, 2) =-(1.d0 + cc)/2.d0
      zedd(-2,-2) = zedd(2,2)
      zedd( 2,-2) =-(1.d0 - cc)/2.d0
      zedd(-2, 2) = zedd(2,-2)

      do imf = -1,1,2
      do imi = -1,1,2
      
      dhlf   = dfun(1,imi,imf,ix)*www
      zxfa   = (zxfa00+zxfa11*imf*imi)*dhlf
      zxfb   = (zxfb00+zxfb11*imf*imi)*dhlf
      do immf = -2,2,2
      do immi = -2,2,2

c      if(imf.eq.1.and.imi.eq.1) then
c      write(52,1111)immf,immi,cc,zedd(immf,immi)
c 1111 format(1h ,2i3,4e15.5)
c      end if

      ibmf = imf+immf
      ibmi = imi+immi
      cefxx = cg1h(immf,imf,3)*cg1h(immi,imi,3)

      if(abs(cefxx).gt.1.d-15) then
      zsuma  = 0
      if(immf.eq.0.and.immi.eq.0) then
      zsuma  = zxfa*zpol00*cefxx
      end if
      zsumb  = zxfb*zedd(immf,immi)*cefxx
      zsum1  = zsuma+zsumb*fiso(1,15,4)
      zsum3  =       zsumb*fiso(3,15,4)
      do jx = 1,mxj,2
      zvme(jx,ibmf,ibmi,1)=zvme(jx,ibmf,ibmi,1)
     &                     +dfun(jx,ibmi,ibmf,ix)*zsum1
      zvme(jx,ibmf,ibmi,3)=zvme(jx,ibmf,ibmi,3)
     &                     +dfun(jx,ibmi,ibmf,ix)*zsum3
      end do ! jx
      end if

      end do  ! immi
      end do  ! immf

      end do ! imf
      end do ! imi

      end do

c--------------------------------------------------------------
c           lsj scheme
c
      icci = 3
      iccf = 3
      zpot = 0
      imiz = 0
      imfz = 0
      do 200 jjx = 1,mxj,2

      do 300 idxf= 1,6
      do 300 idxi= 1,6
      ipxi  = jip(jjx,idxi,icci)
      ipxf  = jip(jjx,idxf,iccf)
      iptest = ipxi*ipxf
      if(iptest.eq.1) then

      do 210 ibfz= -3,3,2
      xxf = xef(jjx,idxf,imfz,ibfz,iccf)

      if(abs(xxf).gt.1.d-20)then

      do 220 ibiz= -3,3,2
      www = xef(jjx,idxi,imiz,ibiz,icci)*xxf

      if(abs(www).gt.1.d-20) then
      do iso = 1,3
      zpot(jjx,idxf,idxi,iso) = zpot(jjx,idxf,idxi,iso)
     &         + www*zvme(jjx,ibfz,ibiz,iso)
      end do
      end if

 220  continue

      end if

 210  continue

      end if  ! parity test

 300  continue
 200  continue


      return
      end
c==========================================================
c---------------------------------------------------------------------
c  sqrt(2l+1/2j+1)(l,0,js,s|j,s)
c
c   l2 = 2*l, js2 = js*2, is2= 2*s, j2=2*j
c
      real*8 function flsjxh(l2,js2,is2,j2)
      implicit real*8(a-h,o-z)

      flsjxh = 0

      if(j2.gt.l2+js2.or.j2.lt.abs(l2-js2))return
      if(abs(is2).gt.js2) return

         jj  = (j2 + 1)/2
         ll  = l2/2
         is  = is2
      if(js2.eq.1) then
         flsjxh = flsj1h(jj,ll,is)
      else if(js2.eq.3) then
         flsjxh = flsj3h(jj,ll,is)
      end if
      return
      end
c----------------------------------------------------------------------
c
c  sqrt((2l + 1)/(2j + 1)) (l 0 1/2 s | j s)
c
c  ll = l, jj = j+1/2, is = 2* s
c
      real*8 function flsj1h(jj,ll,is)
      implicit real*8(a-h,o-z)
c
      flsj1h = 0
      if(abs(is).ne.1) return
c
      isx  = (is + 1)/2
      if(jj.gt.ll) then
      flsj1h = 1.d0/sqrt(2.d0)
      else 
      flsj1h = dble((-1)**isx)/sqrt(2.d0)
      end if
c
      return
      end
c
c  sqrt((2l + 1)/(2j + 1)) (l 0 3/2 s | j s)
c
c  ll = l, jj = j+1/2, is = 2*s
c
      real*8 function flsj3h(jj,ll,is)
      implicit real*8(a-h,o-z)

      j2     = 2*jj - 1
      l2     = 2*ll
      flsj3h = 0

      if(abs(j2-l2).gt.3.or.j2+l2.lt.3) return
      if(abs(is).gt.j2) return

      isx  = (is + 1)/2
      if(jj.eq.ll+2) then
         if(isx.eq.2) then
            flsj3h = sqrt(dble(ll+3)/dble(2*ll+3))/2.d0
         else if(isx.eq.1) then
            flsj3h = sqrt(dble(3*ll+3)/dble(2*ll+3))/2.d0
         else if(isx.eq.0) then
            flsj3h = sqrt(dble(3*ll+3)/dble(2*ll+3))/2.d0
         else if(isx.eq.-1) then
            flsj3h = sqrt(dble(ll+3)/dble(2*ll+3))/2.d0
         end if
      else if(jj.eq.ll+1) then
         if(isx.eq.2) then
            flsj3h = -sqrt(dble(3*ll+6)/dble(2*ll+3))/2.d0
         else if(isx.eq.1) then
            flsj3h = -sqrt(dble(ll)/dble(2*ll+3))/2.d0
         else if(isx.eq.0) then
            flsj3h = sqrt(dble(ll)/dble(2*ll+3))/2.d0
         else if(isx.eq.-1) then
            flsj3h = sqrt(dble(3*ll+6)/dble(2*ll+3))/2.d0
         end if
      else if(jj.eq.ll) then
         if(isx.eq.2) then
            flsj3h = sqrt(dble(3*ll-3)/dble(2*ll-1))/2.d0
         else if(isx.eq.1) then
            flsj3h = -sqrt(dble(ll+1)/dble(2*ll-1))/2.d0
         else if(isx.eq.0) then
            flsj3h = -sqrt(dble(ll+1)/dble(2*ll-1))/2.d0
         else if(isx.eq.-1) then
            flsj3h = sqrt(dble(3*ll-3)/dble(2*ll-1))/2.d0
         end if
      else if(jj.eq.ll-1) then
         if(isx.eq.2) then
            flsj3h = -sqrt(dble(ll-2)/dble(2*ll-1))/2.d0
         else if(isx.eq.1) then
            flsj3h =  sqrt(dble(3*ll)/dble(2*ll-1))/2.d0
         else if(isx.eq.0) then
            flsj3h = -sqrt(dble(3*ll)/dble(2*ll-1))/2.d0
         else if(isx.eq.-1) then
            flsj3h = sqrt(dble(ll-2)/dble(2*ll-1))/2.d0
         end if
      end if
      return
      end
c-------------------- initialize necessary parameters---------
      subroutine setsato
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
      common / const / pi, fm, scale
      common / cdfi  / meshx,mxx,mxj,mxm          
      common / cmxj / jjmx                        ! used in elemag
      common / cpidx / index(3,-3:3)
c------------------------------------------------------------
c
      index(1,-1) = 1
      index(1, 1) = 2
      index(3,-3) = 3
      index(3,-1) = 4
      index(3, 1) = 5
      index(3, 3) = 6

      meshx   = 30   ! angular projection mesh 20 x 3
      mxm     = 5    ! 2 x m_max  for d-function
c      mxj     = 11   ! 2j pin  2 l + 1
c      mxj     = 9
c      jjmx    = (mxj + 1)/2
      pi      = acos(-1.d0)
      fm      = 197.32858d0

      iscale  = 1  ! Harry's code is in MeV

      if(iscale.eq.1) then
      scale   = 1                ! calculation with MeV unit
      else if(iscale.eq.2) then
      scale   = fm               ! calculation with fm unit
      end if

      scalefm = scale/fm

      call setcpl         !set mass,coupling constant,vertex function
      call bifc           ! setup N!, etc.
      call setdfun        ! d-function
      call setconst

c----------- added for electromagnetic routine -------------------
      call setieps        ! epsilon
      call setdirac       ! dirac matrix
      call subgaus        ! gauss weight
      call setcmat
c-----------------------------------------------------------------
      return
      end
c
      subroutine setconst
      implicit real*8(a-h,o-z)
      common / cefflo / cg1h(-2:2,-1:1,3)
     &     ,xef(20,6,-2:2,-3:3,10)
      common / cefint / ih1(0:1,0:1),ih2(0:1,0:1),icpot(10,10)
     &     ,jss(20,6,10),jll(20,6,10)
     &     ,jip(20,6,10)
      common / cdfi / meshx,mxx,mxj,mxm
      common / cpidx / index(3,-3:3)
      dimension jsb(5),jsm(5),ipm(5)
      data jsb/1,1,3,1,1/
      data jsm/0,0,0,0,2/
      data ipm/-1,-1,-1,1,-1/
c
c   cg1h = (1,m1/2,1/2,mh/2;jj/2,m1/2+mh/2)
c
      x3   = 1.d0/sqrt(3.d0)
      x2   = 1.d0/sqrt(2.d0)
      x23  = sqrt(2.d0/3.d0)
      cg1h( 0, 1,1) = -x3
      cg1h( 2,-1,1) =  x23
      cg1h( 0,-1,1) =  x3
      cg1h(-2, 1,1) = -x23

      cg1h( 0, 1,3) = x23
      cg1h( 2,-1,3) = x3
      cg1h( 0,-1,3) = x23
      cg1h(-2, 1,3) = x3
      cg1h( 2, 1,3) = 1
      cg1h(-2,-1,3) = 1
c
c   table of lsj <-> m scheme
c
c   jj = 2j,  ip= parity
c
c   xef(jj,idx,mm,mb,ix)= sqrt(2l+1/2j+1)*(sm,sb|s)(l,s|j)
c   ss (jj,idx,ix)= 2s
c   ll (jj,idx,ix)= 2l
c   ip (jj,idx,ix)= parity
c

      xef = 0
      jss = 0
      jll = 0
      jip = 0

      do 110 ix = 1,5

      ipmx = ipm(ix)
      jsbx = jsb(ix)
      jsmx = jsm(ix)
      do 100 jx = 1,mxj,2
      do 200 ijs= abs(jsbx-jsmx),jsbx+jsmx,2
      do 210 ijl= abs(jx-ijs),jx+ijs,2
      ipp   = ipmx*(-1)**(ijl/2)
      lll   = ijl - jx
      idx   = index(ijs,lll)
      do 220 mbz = -jsbx,jsbx,2
      do 220 mmz = -jsmx,jsmx,2
        mtotz = mbz+mmz
      if(abs(mtotz).le.ijs) then
      if(jsmx.eq.0) then
         xspin = 1
      else if(jsmx.eq.2) then
         xspin = cg1h(mmz,mbz,ijs)
      end if
      xef(jx,idx,mmz,mbz,ix)= xspin*flsjxh(ijl,ijs,mtotz,jx)
      jss(jx,idx,ix)= ijs
      jll(jx,idx,ix)= ijl
      jip(jx,idx,ix)= ipp
      end if

 220  continue
 210  continue
 200  continue
 100  continue
 110  continue
c
c
c
      ih1(0,0) = 1
      ih2(0,0) = 0
      ih1(0,1) = 1
      ih2(0,1) = 1
      ih1(1,0) = 0
      ih2(1,0) = 0
      ih1(1,1) = 0
      ih2(1,1) = 1
c
c  ignore this number!!!!!!!!!!!!!!!!!
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
c
c
c
      return
      end
c
c      half integer d^j_{m',m)
c
c  dfun(2j,2m,2n) = d^{j}_{m,n}
c

      subroutine setdfun
      implicit real*8(a-h,o-z)
      parameter(maxl=10,njmx=11)
      common / cdff / xgau(100),wgau(100),dfun(2*njmx-1,-5:5,-5:5,100)
     & ,fleg(0:maxl,100)
      common / cdfi / meshx,mxx,mxj,mxm
      dimension fle(0:maxl),fled(-1:maxl),fledd(-1:maxl)
      dimension xg(3),wg(3)
      data xg/ -0.7745966692D0,0.d0,0.7745966692D0/
      data wg/ 0.5555555555d0,0.8888888888d0,0.5555555555d0/
      data ngaus/3/

      dfun = 0
      fleg = 0
c
      mxx       = meshx * ngaus
      dgux      = 2.d0/dble(2*meshx)
      idx       = 1
      do 110 nx = 1,meshx
      do 110 ng = 1,ngaus
      xgau(idx)   = dgux*(xg(ng)+dble(2*nx-1))-1.d0
      wgau(idx)   = wg(ng)*dgux
      x         = xgau(idx)

      call legen(x,fle)
      do 111 lx = 0,maxl
 111  fleg(lx,idx) = fle(lx)

      ss        = sqrt((1.d0 - x)/2.d0)
      cc        = sqrt((1.d0 + x)/2.d0)
c
      do 120 lx = 1,2*njmx-1,2
      maxm      = min(lx,mxm)
      do 130 mf = -maxm,maxm,2
      do 140 mi = -maxm,maxm,2
      jj        = (lx - 1)/2
      mfm       = (mf - 1)/2
      mfp       = (mf + 1)/2
      mim       = (mi - 1)/2
      mip       = (mi + 1)/2
      df1       = 0
      df2       = 0
      df3       = 0
      df4       = 0
      if(jj.ge.abs(mfm).and.jj.ge.abs(mim))then
      fac       = sqrt(dble((lx + mf)*(lx + mi)))/dble(2*lx)
      df1       = fblmmx(jj,mfm,mim,cc,ss)*fac*cc
      end if

      if(jj.ge.abs(mfp).and.jj.ge.abs(mip))then
      fac       = sqrt(dble((lx - mf)*(lx - mi)))/dble(2*lx)
      df2       = fblmmx(jj,mfp,mip,cc,ss)*fac*cc
      end if

      if(jj.ge.abs(mfm).and.jj.ge.abs(mip))then
      fac       = sqrt(dble((lx + mf)*(lx - mi)))/dble(2*lx)
      df3       =-fblmmx(jj,mfm,mip,cc,ss)*fac*ss
      end if

      if(jj.ge.abs(mfp).and.jj.ge.abs(mim))then
      fac       = sqrt(dble((lx - mf)*(lx + mi)))/dble(2*lx)
      df4       = fblmmx(jj,mfp,mim,cc,ss)*fac*ss
      end if

      dfun(lx,mf,mi,idx) = df1 + df2 + df3 + df4

c      write(*,*)'setd',lx,mf,mi,idx,df1+df2+df3+df4
c      read(*,*)ixxx

 140  continue
 130  continue
 120  continue

      idx       = idx + 1
 110  continue
c
      return
      end
c
c
c      <l,mf|exp(-iJ_y*theta)|l,mi>
c
      real*8 function fblmmx(l,mf,mi,cc,ss)
      implicit real*8(a-h,o-z)
      parameter (n=100,m=50)
      common / fdbn / h(0:n),dh(-1:m),bb(0:n,0:n)
c
      fblmmx  = 0
      if(l.lt.0.or.abs(mf).gt.l.or.abs(mi).gt.l)return
c
c      t2     = theta/2.d0
c      cc     = cos(t2)
c      ss     = sin(t2)
      jmip   = l  + mi
      jmim   = l  - mi
      jmfp   = l  + mf
      jmfm   = l  - mf
      mfmim  = mf - mi
      iicos  = 2*l - mfmim
      iisin  = mfmim
      kmax   = min(jmip,jmfm)
      kmin   = max(0,-mfmim)
      if(kmax.lt.kmin)return
      sum    = 0
      do 100 kx = kmin,kmax
      phase     = (-1)**(mfmim+kx)
      factor    = h(jmip)*h(jmim)*h(jmfp)*h(jmfm)
     &           /(h(jmip-kx)*h(kx)*h(jmfm-kx)*h(kx+mfmim))**2
      sum       = phase*factor*cc**(iicos-2*kx)*ss**(iisin+2*kx)
     &           + sum
 100  continue
      fblmmx     = sum
      return
      end
c
c
c
c   P_L(z), d P_L/dx, d^2 P_L/d^2 x
c    fle     fled       fledd
c
      subroutine legend(z,fle,fled,fledd)
      implicit real*8(a-h,o-z)
      parameter(maxl=10)
      dimension fle(0:maxl),fled(-1:maxl),fledd(-1:maxl)
c
      fle(0)   = 1
      fle(1)   = z
c
      fled(-1) = 0
      fled(0)  = 0
      fled(1)  = 1
c
      fledd(-1)= 0
      fledd(0) = 0
      fledd(1) = 0
c
      do 100 i = 2,maxl
         xi = dble(i)
         fle(i)  = ( (2.d0*xi-1.d0)*z*fle(i-1)
     &                     -(xi-1.d0)*fle(i-2) )/xi
         fled(i) = xi*fle(i-1) + z*fled(i-1)
         fledd(i)= (xi+1.d0)*fled(i-1) + z*fledd(i-1)
  100 continue
c
      return
      end
c
c
      subroutine legen(z,fle)
      implicit real*8(a-h,o-z)
      parameter(maxl=10)
      dimension fle(0:maxl)
c
      fle(0)   = 1
      fle(1)   = z
      do 100 i = 2,maxl
      fle(i)   = (dble(2*i-1)*z*fle(i-1)-dble(i-1)*fle(i-2))/dble(i)
  100 continue
      return
      end
      subroutine bifc
c------------------------------------------------------------
      implicit real*8(a-h,o-z)
      parameter (n=100,m=50)
      common / fdbn / h(0:n),dh(-1:m),bb(0:n,0:n)
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      e=1.0d0 
      h(0)=e 
      dh(-1)=e 
      bb(0,0)=e 
      sd=e 
      do 10 i=1,n 
      s=i 
      sd=sd*sqrt(s) 
   10 h(i)=sd 
      sd=e 
      do 20 i=0,m 
      s=i+i+1 
      sd=sd*sqrt(s) 
   20 dh(i)=sd
      do 30 i=1,n 
      bb(i,0)=e 
      bb(0,i)=e 
      bb(i,i)=e 
      s=e
      do 30 j=1,i/2 
      l=i-j 
      s=s*dble(l+1)/dble(j)
      sd=sqrt(s) 
      bb(j,i)=sd 
      bb(l,i)=sd 
      bb(i,j)=s 
   30 bb(i,l)=s
      return 
      end
c-----------------------------------------------------------------
c-----------------------------------------------------------------
c
c  map [-1;1] -> [0,infty]
c
c    dfmap(x) = d fmap(x)/dx
c
c   lmap = 1 f = (1+x)/(1-x)     df/dx = 2/(1-x)**2
c          2 f = tan(pi/4*(1+x)) df/dx = pi/4*(1+tan^2(pi/4*(1+x)))
c
      real*8 function dfmap(x,lmap)
      implicit real*8(a-b,d-h,o-z)
      parameter(pi=3.14159265d0)
c
      if(lmap.eq.1) then
        dfmap = 2.d0/(1.d0-x)**2
        else if(lmap.eq.2) then 
        dfmap = pi/4.d0*(1.d0+tan(pi/4.d0*(1.d0+x))**2)
      end if
      return
      end
c
      real*8 function fmap(x,lmap)
      implicit real*8(a-b,d-h,o-z)
      parameter(pi=3.14159265d0)
c
      if(lmap.eq.1) then
        fmap = (1.d0+x)/(1.d0-x)
        else if(lmap.eq.2) then 
        fmap = tan(pi/4.d0*(1.d0+x))
      end if
      return
      end
c===================================================================
c
c  calculate T-matrix via matrix inversion
c
c===================================================================
      subroutine subtmat(maxmat)
      implicit real*8(a-h,o-w)
      implicit complex*16(z)
      parameter(ndim=400,ndima=400)
      common / cpotme / zvme(ndim,ndim),zwei(ndim),ztmat(ndim,ndim)
      dimension zamat(ndima,ndima),zbmat(ndima,ndima),
     &         zytmat(ndima),itmat(ndima)
c
c
      do 210 k1 = 1,maxmat
      do 210 k2 = 1,maxmat
        if(k2.eq.k1)then
          zamat(k2,k1) = 1.d0 - zvme(k2,k1)*zwei(k1)
        else
          zamat(k2,k1) =      - zvme(k2,k1)*zwei(k1)
        end if
  210 continue
c
c   matrix inversion
c
      mmax   = maxmat
      nmax   = maxmat
      iddim  = ndima
      call mtxinv(zamat,nmax,iddim,zbmat,mmax,zytmat,itmat)
c
      do 310 k2  = 1,maxmat
      do 310 k1  = 1,maxmat
        zsum = (0.d0,0.d0)
      do 320 kn  = 1,maxmat
        zsum = zsum + zbmat(k2,kn)*zvme(kn,k1)
  320 continue
      ztmat(k2,k1)= zsum
  310 continue
c
      return
      end
c===============================================================
c  relativistic kinematics
c===============================================================
      subroutine kinrel(lmap,meshp,fm1,fm2,ngaus,ion,zcm,zon,
     &                  zrho,zpint,zint,mxp,ikt)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)
      common / cgauss / xg(32,32),wg(32,32)
      dimension zpint(300),zint(300)
c      data nallowed/4,8,12,16,20,24,32,48/
c
      pi     = acos(-1.d0)
      ngaus  = 4
      scale  = 197.3d0
c
      zsum  = 0
      dgu   = 1.d0/dble(meshp)
      ig    = 0
      do 100 nx  = 1,meshp
      do 100 ng  = 1,ngaus
        ig       = ig + 1
        x        = dgu*(xg(ngaus,ng)+dble(2*nx-1))-1.d0
        zpint(ig) = fmap(x,lmap)*scale
        zpss      = zpint(ig)**2
        zeee      = sqrt(fm1**2+zpss)+sqrt(fm2**2+zpss)
        wwww      = wg(ngaus,ng)*dgu*dfmap(x,lmap)*scale
        zsum      = zsum + wwww/(zon**2 - zpss)
        zint(ig)  = zpss*wwww/(zcm-zeee)
  100 continue
      mxp        = ngaus*meshp

      if(abs(ion).eq.1) then
      mxp        = ngaus*meshp+1
      zee1       = sqrt(zon**2+fm1**2)
      zee2       = sqrt(zon**2+fm2**2)
      zrho       = zon*zee1*zee2/zcm
      zpint(mxp) = zon
      if(ikt.eq.1) then
      zww        = -(0.d0,1.d0)*pi*zrho
      else if(ikt.eq.2) then
      zww        = 0
      end if
      zint(mxp)  = -zsum*2.d0*zon*zrho + zww

c      else if(ion.eq.-1) then
c      mxp        = ngaus*meshp+1
c      zee1       = sqrt(zon**2+fm1**2)
c      zee2       = sqrt(zon**2+fm2**2)
c      zrho       = zon*zee1*zee2/zcm
c      zpint(mxp) = zon
c      zint(mxp)  = -(0.d0,2.d0)*pi*zrho
      end if

c     
      return
      end
c-----------------------------------------------------------------------
      subroutine mtxinv(a,n,ndim,b,m,t,it)
c-----------------------------------------------------------------------
      implicit real*8(a-h,o-z)
      complex*16 a,b,t,stor,divx,f,sum
      dimension a(ndim,ndim),b(ndim,ndim),t(ndim),it(ndim)

c   dimension number
      if(n.eq.1) then
        b(1,1) = (1.d0, 0.d0)/a(1,1)
        go to 9999
      endif
      
c   matrix clear
      do 1 i = 1, ndim
        it(i) = 0
        t(i) = (0.d0, 0.d0)
        do 2 j = 1, ndim
          b(i,j) = (0.d0, 0.d0)
          if(i.eq.j) b(i,j) = (1.d0, 0.d0)
    2   continue
    1 continue

c
      do 10 i=1,n
   10 it(i)=i
      do 80 i1=1,n
c
        mxi=i1
        mxj=i1
        bigx=abs(a(i1,i1))
        do 16 i3=i1,n
          do 16 i2=i1,n
            bigx1=abs(a(i2,i3))
            if(bigx.ge.bigx1) go to 16
            bigx=bigx1
            mxi=i2
            mxj=i3
   16     continue
c
          if(mxi.eq.i1) go to 22
          do 20 i2=i1,n
            stor=a(i1,i2)
            a(i1,i2)=a(mxi,i2)
   20     a(mxi,i2)=stor
          do 21 i2=1,m
            stor=b(i1,i2)
            b(i1,i2)=b(mxi,i2)
   21     b(mxi,i2)=stor
c
   22     if(mxj.eq.i1) go to 30
          do 28 i3=1,n
            stor=a(i3,i1)
            a(i3,i1)=a(i3,mxj)
   28     a(i3,mxj)=stor
          istor=it(i1)
          it(i1)=it(mxj)
          it(mxj)=istor
c
   30 divx=a(i1,i1)
      if(abs(divx)) 40,35,40
   35 write(6,1000)
 1000 format(/////3x,'matrix *a* which is used in subroutine simeq
     &  is irregular ,and so the eq. ax = b   can not be solved')
      return
   40 divx=1.d0/divx
      do 50 i2=i1,n
   50 a(i1,i2)=divx*a(i1,i2)
      do 55 i2=1,m
   55 b(i1,i2)=divx*b(i1,i2)
      if(i1.eq.n) go to 90
      j1=i1+1
      do 75 i2=j1,n
        f=a(i2,i1)
        do 70 i3=i1,n
   70   a(i2,i3)=a(i2,i3)-f*a(i1,i3)
        do 75 i3=1,m
   75   b(i2,i3)=b(i2,i3)-f*b(i1,i3)
   80 continue
c
   90 do 120 i2=1,m
        t(n)=b(n,i2)
      im=n
   92 sum=0.d0
      k=im-1
      do 94 i1=im,n
   94 sum=sum+a(k,i1)*t(i1)
      t(k)=b(k,i2)-sum
      im=k
      if(im.ne.1) go to 92
      do 120 i1=1,n
        k1=it(i1)
  120 b(k1,i2)=t(i1)


c   return
 9999 continue
      
      return
      end
c
c   Note N=4,8,12,16,20,24,28,32 only!
c      
      subroutine subgaus
      implicit real*8(a-h,o-z)
      common / cgauss / xg(32,32),wg(32,32)
       xg( 4, 1) =   -.8611363115940525725d+00
       wg( 4, 1) =    .3478548451374632311d+00
       xg( 4, 2) =   -.3399810435848562573d+00
       wg( 4, 2) =    .6521451548625636363d+00
       xg( 4, 3) =    .3399810435848562573d+00
       wg( 4, 3) =    .6521451548625636363d+00
       xg( 4, 4) =    .8611363115940525725d+00
       wg( 4, 4) =    .3478548451374632311d+00
       xg( 8, 1) =   -.9602898564975361761d+00
       wg( 8, 1) =    .1012285362903781599d+00
       xg( 8, 2) =   -.7966664774136267280d+00
       wg( 8, 2) =    .2223810344533806993d+00
       xg( 8, 3) =   -.5255324099163289908d+00
       wg( 8, 3) =    .3137066458778958178d+00
       xg( 8, 4) =   -.1834346424956498078d+00
       wg( 8, 4) =    .3626837833783718157d+00
       xg( 8, 5) =    .1834346424956498078d+00
       wg( 8, 5) =    .3626837833783718157d+00
       xg( 8, 6) =    .5255324099163289908d+00
       wg( 8, 6) =    .3137066458778958178d+00
       xg( 8, 7) =    .7966664774136267280d+00
       wg( 8, 7) =    .2223810344533806993d+00
       xg( 8, 8) =    .9602898564975361761d+00
       wg( 8, 8) =    .1012285362903781599d+00
       xg(12, 1) =   -.9815606342467192436d+00
       wg(12, 1) =    .4717533638651335431d-01
       xg(12, 2) =   -.9041172563704747978d+00
       wg(12, 2) =    .1069393259953215908d+00
       xg(12, 3) =   -.7699026741943046925d+00
       wg(12, 3) =    .1600783285433506342d+00
       xg(12, 4) =   -.5873179542866174829d+00
       wg(12, 4) =    .2031674267230713093d+00
       xg(12, 5) =   -.3678314989981801841d+00
       wg(12, 5) =    .2334925365383612172d+00
       xg(12, 6) =   -.1252334085114688855d+00
       wg(12, 6) =    .2491470458134096011d+00
       xg(12, 7) =    .1252334085114688855d+00
       wg(12, 7) =    .2491470458134096011d+00
       xg(12, 8) =    .3678314989981801841d+00
       wg(12, 8) =    .2334925365383612172d+00
       xg(12, 9) =    .5873179542866174829d+00
       wg(12, 9) =    .2031674267230713093d+00
       xg(12,10) =    .7699026741943046925d+00
       wg(12,10) =    .1600783285433506342d+00
       xg(12,11) =    .9041172563704747978d+00
       wg(12,11) =    .1069393259953215908d+00
       xg(12,12) =    .9815606342467192436d+00
       wg(12,12) =    .4717533638651335431d-01
       xg(16, 1) =   -.9894009349916499385d+00
       wg(16, 1) =    .2715245941175453703d-01
       xg(16, 2) =   -.9445750230732326003d+00
       wg(16, 2) =    .6225352393864926753d-01
       xg(16, 3) =   -.8656312023878317552d+00
       wg(16, 3) =    .9515851168249524206d-01
       xg(16, 4) =   -.7554044083550029987d+00
       wg(16, 4) =    .1246289712555374851d+00
       xg(16, 5) =   -.6178762444026437706d+00
       wg(16, 5) =    .1495959888165806495d+00
       xg(16, 6) =   -.4580167776572274252d+00
       wg(16, 6) =    .1691565193950071988d+00
       xg(16, 7) =   -.2816035507792589154d+00
       wg(16, 7) =    .1826034150449286075d+00
       xg(16, 8) =   -.9501250983763744051d-01
       wg(16, 8) =    .1894506104550737202d+00
       xg(16, 9) =    .9501250983763744051d-01
       wg(16, 9) =    .1894506104550737202d+00
       xg(16,10) =    .2816035507792589154d+00
       wg(16,10) =    .1826034150449286075d+00
       xg(16,11) =    .4580167776572274252d+00
       wg(16,11) =    .1691565193950071988d+00
       xg(16,12) =    .6178762444026437706d+00
       wg(16,12) =    .1495959888165806495d+00
       xg(16,13) =    .7554044083550029987d+00
       wg(16,13) =    .1246289712555374851d+00
       xg(16,14) =    .8656312023878317552d+00
       wg(16,14) =    .9515851168249524206d-01
       xg(16,15) =    .9445750230732326003d+00
       wg(16,15) =    .6225352393864926753d-01
       xg(16,16) =    .9894009349916499385d+00
       wg(16,16) =    .2715245941175453703d-01
       xg(20, 1) =   -.9931285991850948847d+00
       wg(20, 1) =    .1761400713915369301d-01
       xg(20, 2) =   -.9639719272779138093d+00
       wg(20, 2) =    .4060142980038770194d-01
       xg(20, 3) =   -.9122344282513258351d+00
       wg(20, 3) =    .6267204833411117726d-01
       xg(20, 4) =   -.8391169718222187823d+00
       wg(20, 4) =    .8327674157670722499d-01
       xg(20, 5) =   -.7463319064601507957d+00
       wg(20, 5) =    .1019301198172431894d+00
       xg(20, 6) =   -.6360536807265150250d+00
       wg(20, 6) =    .1181945319615215761d+00
       xg(20, 7) =   -.5108670019508271265d+00
       wg(20, 7) =    .1316886384491801343d+00
       xg(20, 8) =   -.3737060887154195488d+00
       wg(20, 8) =    .1420961093183858992d+00
       xg(20, 9) =   -.2277858511416450682d+00
       wg(20, 9) =    .1491729864726078492d+00
       xg(20,10) =   -.7652652113349732443d-01
       wg(20,10) =    .1527533871307300561d+00
       xg(20,11) =    .7652652113349732443d-01
       wg(20,11) =    .1527533871307300561d+00
       xg(20,12) =    .2277858511416450682d+00
       wg(20,12) =    .1491729864726078492d+00
       xg(20,13) =    .3737060887154195488d+00
       wg(20,13) =    .1420961093183858992d+00
       xg(20,14) =    .5108670019508271265d+00
       wg(20,14) =    .1316886384491801343d+00
       xg(20,15) =    .6360536807265150250d+00
       wg(20,15) =    .1181945319615215761d+00
       xg(20,16) =    .7463319064601507957d+00
       wg(20,16) =    .1019301198172431894d+00
       xg(20,17) =    .8391169718222187823d+00
       wg(20,17) =    .8327674157670722499d-01
       xg(20,18) =    .9122344282513258351d+00
       wg(20,18) =    .6267204833411117726d-01
       xg(20,19) =    .9639719272779138093d+00
       wg(20,19) =    .4060142980038770194d-01
       xg(20,20) =    .9931285991850948847d+00
       wg(20,20) =    .1761400713915369301d-01
       xg(24, 1) =   -.9951872199970213106d+00
       wg(24, 1) =    .1234122979998539607d-01
       xg(24, 2) =   -.9747285559713094738d+00
       wg(24, 2) =    .2853138862893439542d-01
       xg(24, 3) =   -.9382745520027327979d+00
       wg(24, 3) =    .4427743881742059184d-01
       xg(24, 4) =   -.8864155270044010715d+00
       wg(24, 4) =    .5929858491543809479d-01
       xg(24, 5) =   -.8200019859739029471d+00
       wg(24, 5) =    .7334648141108221497d-01
       xg(24, 6) =   -.7401241915785543579d+00
       wg(24, 6) =    .8619016153195555030d-01
       xg(24, 7) =   -.6480936519369755455d+00
       wg(24, 7) =    .9761865210411652116d-01
       xg(24, 8) =   -.5454214713888395627d+00
       wg(24, 8) =    .1074442701159684099d+00
       xg(24, 9) =   -.4337935076260451273d+00
       wg(24, 9) =    .1155056680537287217d+00
       xg(24,10) =   -.3150426796961633413d+00
       wg(24,10) =    .1216704729278066527d+00
       xg(24,11) =   -.1911188674736162829d+00
       wg(24,11) =    .1258374563468317997d+00
       xg(24,12) =   -.6405689286260561610d-01
       wg(24,12) =    .1279381953467556010d+00
       xg(24,13) =    .6405689286260561610d-01
       wg(24,13) =    .1279381953467556010d+00
       xg(24,14) =    .1911188674736162829d+00
       wg(24,14) =    .1258374563468317997d+00
       xg(24,15) =    .3150426796961633413d+00
       wg(24,15) =    .1216704729278066527d+00
       xg(24,16) =    .4337935076260451273d+00
       wg(24,16) =    .1155056680537287217d+00
       xg(24,17) =    .5454214713888395627d+00
       wg(24,17) =    .1074442701159684099d+00
       xg(24,18) =    .6480936519369755455d+00
       wg(24,18) =    .9761865210411652116d-01
       xg(24,19) =    .7401241915785543579d+00
       wg(24,19) =    .8619016153195555030d-01
       xg(24,20) =    .8200019859739029471d+00
       wg(24,20) =    .7334648141108221497d-01
       xg(24,21) =    .8864155270044010715d+00
       wg(24,21) =    .5929858491543809479d-01
       xg(24,22) =    .9382745520027327979d+00
       wg(24,22) =    .4427743881742059184d-01
       xg(24,23) =    .9747285559713094738d+00
       wg(24,23) =    .2853138862893439542d-01
       xg(24,24) =    .9951872199970213106d+00
       wg(24,24) =    .1234122979998539607d-01
       xg(28, 1) =   -.9964424975739544221d+00
       wg(28, 1) =    .9124282593095001115d-02
       xg(28, 2) =   -.9813031653708726987d+00
       wg(28, 2) =    .2113211259277241599d-01
       xg(28, 3) =   -.9542592806289381668d+00
       wg(28, 3) =    .3290142778230553677d-01
       xg(28, 4) =   -.9156330263921320656d+00
       wg(28, 4) =    .4427293475900564296d-01
       xg(28, 5) =   -.8658925225743950849d+00
       wg(28, 5) =    .5510734567571793502d-01
       xg(28, 6) =   -.8056413709171791337d+00
       wg(28, 6) =    .6527292396700153099d-01
       xg(28, 7) =   -.7356108780136317860d+00
       wg(28, 7) =    .7464621423457072635d-01
       xg(28, 8) =   -.6566510940388649020d+00
       wg(28, 8) =    .8311341722890354389d-01
       xg(28, 9) =   -.5697204718114017297d+00
       wg(28, 9) =    .9057174439303525282d-01
       xg(28,10) =   -.4758742249551182746d+00
       wg(28,10) =    .9693065799793250392d-01
       xg(28,11) =   -.3762515160890786969d+00
       wg(28,11) =    .1021129675780635265d+00
       xg(28,12) =   -.2720616276351780494d+00
       wg(28,12) =    .1060557659228492655d+00
       xg(28,13) =   -.1645692821333807621d+00
       wg(28,13) =    .1087111922582971735d+00
       xg(28,14) =   -.5507928988403425902d-01
       wg(28,14) =    .1100470130164782068d+00
       xg(28,15) =    .5507928988403425902d-01
       wg(28,15) =    .1100470130164782068d+00
       xg(28,16) =    .1645692821333807621d+00
       wg(28,16) =    .1087111922582971735d+00
       xg(28,17) =    .2720616276351780494d+00
       wg(28,17) =    .1060557659228492655d+00
       xg(28,18) =    .3762515160890786969d+00
       wg(28,18) =    .1021129675780635265d+00
       xg(28,19) =    .4758742249551182746d+00
       wg(28,19) =    .9693065799793250392d-01
       xg(28,20) =    .5697204718114017297d+00
       wg(28,20) =    .9057174439303525282d-01
       xg(28,21) =    .6566510940388649020d+00
       wg(28,21) =    .8311341722890354389d-01
       xg(28,22) =    .7356108780136317860d+00
       wg(28,22) =    .7464621423457072635d-01
       xg(28,23) =    .8056413709171791337d+00
       wg(28,23) =    .6527292396700153099d-01
       xg(28,24) =    .8658925225743950849d+00
       wg(28,24) =    .5510734567571793502d-01
       xg(28,25) =    .9156330263921320656d+00
       wg(28,25) =    .4427293475900564296d-01
       xg(28,26) =    .9542592806289381668d+00
       wg(28,26) =    .3290142778230553677d-01
       xg(28,27) =    .9813031653708726987d+00
       wg(28,27) =    .2113211259277241599d-01
       xg(28,28) =    .9964424975739544221d+00
       wg(28,28) =    .9124282593095001115d-02
       xg(32, 1) =   -.9972638618494815699d+00
       wg(32, 1) =    .7018610009469284511d-02
       xg(32, 2) =   -.9856115115452682707d+00
       wg(32, 2) =    .1627439473090690897d-01
       xg(32, 3) =   -.9647622555875063899d+00
       wg(32, 3) =    .2539206530926331126d-01
       xg(32, 4) =   -.9349060759377396668d+00
       wg(32, 4) =    .3427386291302263188d-01
       xg(32, 5) =   -.8963211557660520912d+00
       wg(32, 5) =    .4283589802222791815d-01
       xg(32, 6) =   -.8493676137325699704d+00
       wg(32, 6) =    .5099805926237756942d-01
       xg(32, 7) =   -.7944837959679423856d+00
       wg(32, 7) =    .5868409347853724423d-01
       xg(32, 8) =   -.7321821187402897113d+00
       wg(32, 8) =    .6582222277636355645d-01
       xg(32, 9) =   -.6630442669302152314d+00
       wg(32, 9) =    .7234579410885036421d-01
       xg(32,10) =   -.5877157572407623043d+00
       wg(32,10) =    .7819389578707240662d-01
       xg(32,11) =   -.5068999089322293594d+00
       wg(32,11) =    .8331192422694910782d-01
       xg(32,12) =   -.4213512761306353327d+00
       wg(32,12) =    .8765209300440622575d-01
       xg(32,13) =   -.3318686022821276671d+00
       wg(32,13) =    .9117387869576631942d-01
       xg(32,14) =   -.2392873622521370924d+00
       wg(32,14) =    .9384439908080713377d-01
       xg(32,15) =   -.1444719615827964876d+00
       wg(32,15) =    .9563872007927740060d-01
       xg(32,16) =   -.4830766568773831038d-01
       wg(32,16) =    .9654008851473035169d-01
       xg(32,17) =    .4830766568773831038d-01
       wg(32,17) =    .9654008851473035169d-01
       xg(32,18) =    .1444719615827964876d+00
       wg(32,18) =    .9563872007927740060d-01
       xg(32,19) =    .2392873622521370924d+00
       wg(32,19) =    .9384439908080713377d-01
       xg(32,20) =    .3318686022821276671d+00
       wg(32,20) =    .9117387869576631942d-01
       xg(32,21) =    .4213512761306353327d+00
       wg(32,21) =    .8765209300440622575d-01
       xg(32,22) =    .5068999089322293594d+00
       wg(32,22) =    .8331192422694910782d-01
       xg(32,23) =    .5877157572407623043d+00
       wg(32,23) =    .7819389578707240662d-01
       xg(32,24) =    .6630442669302152314d+00
       wg(32,24) =    .7234579410885036421d-01
       xg(32,25) =    .7321821187402897113d+00
       wg(32,25) =    .6582222277636355645d-01
       xg(32,26) =    .7944837959679423856d+00
       wg(32,26) =    .5868409347853724423d-01
       xg(32,27) =    .8493676137325699704d+00
       wg(32,27) =    .5099805926237756942d-01
       xg(32,28) =    .8963211557660520912d+00
       wg(32,28) =    .4283589802222791815d-01
       xg(32,29) =    .9349060759377396668d+00
       wg(32,29) =    .3427386291302263188d-01
       xg(32,30) =    .9647622555875063899d+00
       wg(32,30) =    .2539206530926331126d-01
       xg(32,31) =    .9856115115452682707d+00
       wg(32,31) =    .1627439473090690897d-01
       xg(32,32) =    .9972638618494815699d+00
       wg(32,32) =    .7018610009469284511d-02
       return
       end
 
