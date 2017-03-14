c=============================================================
c  non-resonant electromagnetic current
c=============================================================
c !!!!!! following variables are not set for isw2= 2 !!!!!!!!
c  mesh00 /  zgreen
c  ELEM2  /  zf (only on-shell <pi N|\Gamma |N*>)
c
c------------------------------------------------------------
      subroutine caljme(nchx)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)
      parameter (maxlsj2=50)
      parameter (njmx=11)

      character chn(5)*2,chnx*2

c  from ts 
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / cdfi  / meshx,mxx,mxj,mxm
      common / cpidx / index(3,-3:3)
      common / input1 / mxq,q2(maxq2)
      common / csw   / gsw(20,20)

c    ts 10-26-2007
      common /cntrlchn / iiichn(maxmb)
c
      common /onlybare/ isw4

c   from bruno

      common/pot1/nchh,Ldt(5,maxmb,maxlsj),isdt(5,maxmb,maxlsj)
     &                                    ,nLsdt(maxmb,maxlsj)
      common/chdat1/njLs,jpind(maxlsj) ,Lpind(maxlsj)
     &                  ,ispind(maxlsj),itpind(maxlsj)
      common/chdat2/nch,ich0(maxmb)

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

      common / cres / nres(2*maxl+1,0:maxl,3)
     &               ,xmres(2*maxl+1,0:maxl,3,maxres)
     &               ,cfac(2*maxl+1,0:maxl,3,maxres)


      common / elmamp/ zxmbrn(maxwcm,maxq2,8,0:maxl,5)
     3                ,zxmnres(maxwcm,maxq2,8,0:maxl,5)
     4                ,zxmclo(maxwcm,maxq2,8,0:maxl,5)
     5                ,zxmbare(maxwcm,maxq2,8,0:maxl,5)

c ts 11-3-2007
      common/chdat3/njLs2,jpind2(maxlsj2) ,Lpind2(maxlsj2)
     &                  ,ispind2(maxlsj2),itpind2(maxlsj2),lmx2
     &                  ,ljt1(0:maxl*2,2*maxl+1,3)

      dimension zampjm(2*njmx-1,6,6,3)
     &          ,zampj(5,40,2*njmx-1,6,6,3)
      dimension fiso(3)

      dimension zout1(8),zout2(8)

      dimension znres1(6),znres2(6)
      dimension zbrn1(6),zbrn2(6)
      dimension zbare1(6),zbare2(6)
      dimension zcloud1(6),zcloud2(6)

      dimension zresgx(maxres,6,maxmb)
      dimension zxmclox(maxwcm,maxq2,8,0:maxl,5,maxmb)
      dimension zxmnresx(maxwcm,maxq2,8,0:maxl,5,maxmb)

      dimension bareamp(5,6)

!> bjulia change order
      data chn/'pn','en','pd','sn','rn'/
!< bjulia

c  !! be careful !!
      if (ifirst.ne.7) then 
      gsw  = 1
      endif

         zxmnres = 0
         zxmclo  = 0

      if (ifirst.ne.7.or.isw4.ne.1) then 
         zxmbrn = 0
         zxmbare = 0
         zxmclox = 0
         zxmnresx= 0
      end if
         zxmbare = 0

c
c conversion factor from ours to VPI
c
      fiso(1)  = sqrt(3.d0/2.d0) ! for 3/2
      fiso(2)  = -1.d0/sqrt(3.d0)! for 1/2p
      fiso(3)  = -1.d0/sqrt(3.d0)! for 1/2n

c ts

      do 100 ie  = 1,ne0

      wcm        = ze0(ie)
      qpio   = zp(np1,1,ie)
      qeta   = zp(np1,2,ie)


      do 200 iq  = 1,mxq   ! in general run for electroproduction

      q2x    = q2(iq)
      egam0  = (wcm**2 - q2x - fnuc**2)/2.d0/wcm
      egam   = sqrt(q2x + egam0**2)
      enuc   = sqrt(fnuc**2 + egam**2)

      ebi  = enuc

      emf  = sqrt(fpio**2 + qpio**2)
      ebf  = sqrt(fnuc**2 + qpio**2)
      xfacp= 4.d0*pi**2/wcm*sqrt(ebf*emf*abs(egam0)*ebi)/2.d0/pi

      emf  = sqrt(feta**2 + qeta**2)
      ebf  = sqrt(fnuc**2 + qeta**2)
      xface= 4.d0*pi**2/wcm*sqrt(ebf*emf*abs(egam0)*ebi)/2.d0/pi

      call gamfrm(egam0,egam)

c---------------------------------------------------------------
c  calculate non-resonant gamma N -> MB for all lsj and store
c

c bjulia 10 10 2007 only recomputes if isw4.eq.0
      if (ifirst.ne.7.or.isw4.ne.1) then 

      do 110 imb = 1,nch
      do 120 ipx = 1,np1
      zpf        = zp(ipx,imb,ie)
      chnx       = chn(ich0(imb))     ! transformed !

      call subjme(chnx,zpf,egam,egam0,zampjm)

c ts 11-3-2007 prepapre brn for piN and etaN

      if(ipx.eq.np1.and.imb.le.2) then

      do 700 iii = 1,njls2
      jjj2   = jpind2(iii)
      lll2   = lpind2(iii)
      iii2   = itpind2(iii)
      iss2   = ispind2(iii)
      idxts  = index(iss2,lll2-jjj2)

      if(iii2.eq.3) then 
        ismin = 1
        ismax = 1
      else if(iii2.eq.1) then
        ismin = 2
        ismax = 3
      end if

      do 710 iso = ismin,ismax

      do 720 idx = 1,6
      zbrn1(idx) =  zampjm(jjj2,idxts,idx,iso)
 720  continue

      zout1 = 0
      call xmulnew(zbrn1,jjj2,lll2,zout1)

      do idx1 = 1,8

      if(imb.eq.1) then

       zxmbrn(ie,iq,idx1,lll2/2,iso  ) = 
     & zxmbrn(ie,iq,idx1,lll2/2,iso  ) + zout1(idx1)*xfacp

      else if(imb.eq.2.and.iso.ne.1) then

       zxmbrn(ie,iq,idx1,lll2/2,iso+2) =
     & zxmbrn(ie,iq,idx1,lll2/2,iso+2) + zout1(idx1)*xface

      end if
      end do

 710  continue
 700  continue

      end if


      do 130 jjx = 1,mxj
      do 130 id1 = 1,6
      do 130 id2 = 1,6
      do 130 id3 = 1,3
      zampj(imb,ipx,jjx,id1,id2,id3) = zampjm(jjx,id1,id2,id3)
 130  continue
 120  continue
 110  continue

      endif  ! ifirst
c----------------------------------------------------------------

      do 300 i = 1,njLS

      jpin   =jpind(i)
      Lpin   =Lpind(i)
      itpin  =itpind(i)
      kres   = nstar0(i)
      lpin2  = lpin/2

      if(itpin.eq.3) then 
        ismin = 1
        ismax = 1
      else if(itpin.eq.1) then
        ismin = 2
        ismax = 3
      end if

      do 400 iso = ismin,ismax

c---------------------------------------------------------------------
c  calculate amplitudes
c---------------------------------------------------------------------
c
c  our helicity amplitudes   :  CGLN amplitudes
c
c   zbrn                       zmbrn             Born 
c   znres                      zmnres            Non-resonant
c   zcloud                     zmclo             Meson Cloud
c   zbare                      zmbare            Bare
c
c  Following 'K-matrix' type separation is valid only
c  piN intermediate state in this current code
c
c   zkful              zmkres     Non-resonant=(1 - i pi rho T_piN) v_gam-pi
c   zkcloud            zmkclo     zmclo - [on-shell pole part],
c                                                         principal value int
c
c
c  to convert VPI's isospin separation
c     VPI = zmxxx(above CGLN amplitude) x fiso(iso)
c
c  to convert charge base
c   for gamma + p
c    p pi0 = (sqrt(2)*amp(iso=1) - amp(iso=2))/sqrt(3)
c    n pi+ = (sqrt(2)*amp(iso=2) + amp(iso=1))/sqrt(3)
c   for gamma + n
c
c  to convert VPI normalization 10^-3 fm
c   VPI = amp*197.32*1000
c
c  to convert 10^-3/m_pi unit
c         amp*fpio*1000
c
c-----------------------------------------------------
c


c
c  non-resonant amplitude
c
c     zbres = (1 - V G_0)^{-1} |MB><MB| v_MB-GammaN
c
c  meson-cloud for resonant helicity amplitude
c
c     zresg = <N*|\bar{Gamma}|MB><MB|G_0 v_MB-GammaN
c
c

      if (ifirst.ne.7.or.isw4.ne.1) then 

      zresgx = 0
      zbrn1  = 0
      zbrn2  = 0
c
c  loop of |MB><MB|
c
      do 316 imb = 1,nchx

      znres1 = 0
      znres2 = 0

      do 311 lsj = 1,nLsdt(imb,i)   !
      kkki   = (imb - 1)*5 + lsj
      isss   = isdt(lsj,imb,i)      !
      llll   = ldt (lsj,imb,i)      !
      idxts  = index(isss,llll-jpin)
      do 313 ip  = 1,np1
      do 314 idx = 1,6
c
c  loop integral of non-resonant part
c
      zzzz        = zampj(imb,ip,jpin,idxts,idx,iso)
      znres1(idx) = znres1(idx) + zwf(ie,i,ip,kkki,1)*zzzz
      znres2(idx) = znres2(idx) + zwf(ie,i,ip,kkki,2)*zzzz

c      if(imb.eq.1.and.ip.eq.np1) then
c      zbrn1(idx)  = zzzz
c      else if(imb.eq.2.and.ip.eq.np1) then
c      zbrn2(idx)  = zzzz
c      end if

c
c  loop integral of meson cloud
c
      if(kres.ne.0) then
      do 315 ires = 1,kres
      zresgx(ires,idx,imb)=zresgx(ires,idx,imb)
     &              + zfmb(ie,i,ip,imb,lsj,ires)*zzzz
 315  continue
      end if

 314  continue
 313  continue
 311  continue

      zout2 = 0
      zout1 = 0

      call xmulnew(znres1,jpin,lpin,zout1)
      if(iso.ne.1) then
      call xmulnew(znres2,jpin,lpin,zout2)
      end if

      do idx1 = 1,8
       zxmnresx(ie,iq,idx1,lpin2,iso  ,imb) = 
     & zxmnresx(ie,iq,idx1,lpin2,iso  ,imb) + zout1(idx1)*xfacp
      if(iso.ne.1) then
       zxmnresx(ie,iq,idx1,lpin2,iso+2,imb) = 
     & zxmnresx(ie,iq,idx1,lpin2,iso+2,imb) + zout2(idx1)*xface
      end if
      end do

c-----------------------------------------------------
c      if(iso.ne.1) then
c      write(998,8998)wcm,iq,i,lpin2,iso+2,imb
c 8998 format(1h ,e13.5,10i3)
c      do idx = 1,6
c      write(998,8997)znres1(idx)
c      write(998,8997)znres2(idx),zbrn2(idx)
c      end do

c      do idx = 1,8
c      write(998,8997)zout2(idx)*xfacp,
c     & zxmnresx(ie,iq,idx,lpin2,iso+2,imb)
c 8997 format(1h ,20e13.5)
c      end do
c      end if

c------------------------------------------------------
c      stop

 316  continue
c
c  Born amplitude
c
c      zout2 = 0
c      zout1 = 0
c
c      call xmulnew(zbrn1,jpin,lpin,zout1)
c      if(iso.ne.1) then
c      call xmulnew(zbrn2,jpin,lpin,zout2)
c      end if
c
c      do idx1 = 1,8
c       zxmbrn(ie,iq,idx1,lpin2,iso  ) = 
c     & zxmbrn(ie,iq,idx1,lpin2,iso  ) + zout1(idx1)*xfacp
c      if(iso.ne.1) then
c       zxmbrn(ie,iq,idx1,lpin2,iso+2) =
c     & zxmbrn(ie,iq,idx1,lpin2,iso+2) + zout2(idx1)*xface
c      end if
c      end do

      end if ! ifirst isw4
c-----------------------------------------------------------------------
c
c  helicity amplitude A and S, bareamp, meson cloud for A,S
c
c  bareamp(1) = <N^*|-J.e_+1|N,s= 1/2>
c  bareamp(2) = <N^*|-J.e_+1|N,s=-1/2>
c  bareamp(3) = <N^*| J_0   |N,s= 1/2>
c
c  Helicity amplitude
c
c  A_3/2 = bareamp(1) x facb
c  A_1/2 = bareamp(2) x facb
c  S_1/2 = bareamp(3) x facb
c
c  facb = sqrt(E_N/m_N)/sqrt(2 K_gamma)
c  let simply use  K_gamma = (W^2 - m_N^2)/(2 W)
c
c  dressed amplitude zresg calculated above
c
c   zresg = <N*|\bar{Gamma}|MB><MB|G_0^{P} v_MB-GammaN
c
c  Normalization of zresg is related to bereamp from fxx
c
c   fxx = sqrt(4 pi/(2j+1))/sqrt( (2 pi)^3 2|q_0| )
c
c   'zresg' = fxx * bareamp
c   
c  therefore effecitve A,S can be calculated from zresg as
c
c   A_3/2 = zresg(ire,1) /fxx * fxb * sqrt(1000.d0)*1000
c
c  last factor is for conventional 10^-3 GeV^{-1/2}
c



      if(kres.ne.0) then


      aKgamma= (wcm**2-fnuc**2)/(2.d0*wcm)
      fxb    = sqrt(enuc/fnuc)/sqrt(2.d0*aKgamma)

c
c  cloud [g G g G_0 v] and bare  [g G g_gamma]
c


      call calbare(egam,egam0,qpio,wcm,jpin,lpin,itpin,bareamp)
c
c  next factor fxx is needed to get correct normalization
c  of amplitude when combined with bareamp x sigma x zf
c
      fxx = sqrt((4.d0*pi)/(dble(jpin)+1))
     &     /sqrt((2.d0*pi)**3*2.d0*abs(egam0))
c
c  below we sum all contributions of resonances
c  separate contirbution of each resonance can
c  be obtained rewriting following loops for reosnance
c
c   zcloud = <pi N|\bar{Gamma}|N*> G_N* <N*|Cloud |gamma N>
c   zbare  = <pi N|\bar{Gamma}|N*> G_N* <N*|Bare  |gamma N>
c
c------------------------------------------------------------------
c
c   bare 
c
c-------------------------------------------------------------------
c
      zbare1  = 0
      zbare2  = 0
      do ire1 = 1,kres
      do ire2 = 1,kres
      zxx1 = zf(ie,i,1,1,np1,ire1)*zsigma(ie,i,ire1,ire2)
      zxx2 = zf(ie,i,1,2,np1,ire1)*zsigma(ie,i,ire1,ire2)
           
      do idx  = 1,6
      zbare1(idx)  = zbare1(idx) +zxx1*bareamp(ire2,idx)*fxx
      zbare2(idx)  = zbare2(idx) +zxx2*bareamp(ire2,idx)*fxx
      end do

      end do
      end do

      zout2 = 0
      zout1 = 0

      call xmulnew(zbare1,jpin,lpin,zout1)
      if(iso.ne.1) then
      call xmulnew(zbare2,jpin,lpin,zout2)
      end if

      do idx1 = 1,8
       zxmbare(ie,iq,idx1,lpin2,iso  ) = 
     & zxmbare(ie,iq,idx1,lpin2,iso  ) + zout1(idx1)*xfacp
      if(iso.ne.1) then
       zxmbare(ie,iq,idx1,lpin2,iso+2) = 
     & zxmbare(ie,iq,idx1,lpin2,iso+2) + zout2(idx1)*xface
      end if
      end do

c----------------------------------------------------
c
c  meson cloud
c
c----------------------------------------------------
      if (ifirst.ne.7.or.isw4.ne.1) then 

      do imbx = 1,nchx

      zcloud1 = 0
      zcloud2 = 0

      do ire1 = 1,kres
      do ire2 = 1,kres
      zxx1 = zf(ie,i,1,1,np1,ire1)*zsigma(ie,i,ire1,ire2)
      zxx2 = zf(ie,i,1,2,np1,ire1)*zsigma(ie,i,ire1,ire2)
      do idx  = 1,6
      zcloud1(idx) = zcloud1(idx) +zxx1*zresgx(ire2,idx,imbx)
      zcloud2(idx) = zcloud2(idx) +zxx2*zresgx(ire2,idx,imbx)
      end do
      end do
      end do

      zout2 = 0
      zout1 = 0

      call xmulnew(zcloud1,jpin,lpin,zout1)
      if(iso.ne.1) then
      call xmulnew(zcloud2,jpin,lpin,zout2)
      end if

      do idx1 = 1,8
       zxmclox(ie,iq,idx1,lpin2,iso  ,imbx) = 
     & zxmclox(ie,iq,idx1,lpin2,iso  ,imbx) + zout1(idx1)*xfacp
      if(iso.ne.1) then
       zxmclox(ie,iq,idx1,lpin2,iso+2,imbx) = 
     & zxmclox(ie,iq,idx1,lpin2,iso+2,imbx) + zout2(idx1)*xface
      end if
      end do

      end do


      end if  ! ifirst isw4

      end if  ! kres
c----------------------------------------------------------------

 400  continue  ! iso


 300  continue  ! jtp loop



      do idx = 1,8
      do iso = 1,5
      do lx  = 0,maxl

      zaaa   = 0
      zbbb   = 0
      do imbx = 1,nchx
      zaaa = zaaa +zxmnresx(ie,iq,idx,lx,iso,imbx)*dble(iiichn(imbx))
      zbbb = zbbb +zxmclox (ie,iq,idx,lx,iso,imbx)*dble(iiichn(imbx))
      end do

      zxmnres(ie,iq,idx,lx,iso) =  zaaa
      zxmclo (ie,iq,idx,lx,iso) =  zbbb

      end do
      end do
      end do

c---------------------------------------------------------------
c      write(998,9998)wcm,iq
c      do idx = 1,8
c      do lx  = 0,2
c      write(998,9997)idx,lx,(zxmnres(ie,iq,idx,lx,iso),iso=4,5)
c      write(998,9997)idx,lx,(zxmclo (ie,iq,idx,lx,iso),iso=4,5)
c 9997 format(1h ,2i3,20e13.5)
c      end do
c      end do
c 9998 format(1h ,e15.5,2i4)
c 9999 format(1h ,i3,10e14.5)
c-------------------------------------------------------------------

 200  continue  ! Q^2 loop 
 100  continue  ! energy loop
      ifirst=7

       return
       end

