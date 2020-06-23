      subroutine allkqw(win,qin,nx)

      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      character tag*15
      parameter(ndim=20,maxpot=8,nedim=50,npdim=20,nbmas=10,
     &          ladim=10)
      common / cmass  / pi,fm,fnuc,fpio,fdel,frho(nbmas),fsigm(nbmas)
      common / ckine1 / wcm,fmass1,fmass2,pp(ndim),ww(ndim),rhoon,maxp
      common / ckine3 / ww0(ndim)
      common / ctmat2 / vert0(ndim),cvert1(ndim),cvert2(ndim)
     &                 ,cself,ctmat(ndim,ndim,10),ct33(ndim,ndim)
     &                 ,fres0,iiout(10),jjout(10),llout(10)
      common / ctmat1 / vpot(ndim,ndim),xkmat(ndim,ndim)
     &                 ,facpot(maxpot,nbmas)
      common / cchan /imaxchn
      common / cgampi1 /grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,swgm(8)
     &  ,fdelgm,fmrho,fmomg,xkrho,xkomg,phpi,vnnrho,vnnomg
     &  ,mnnrho,mnnomg,lambda,meshgm,lmaxgm
      common / c2001 / wcmmev(1000),qsgev(1000),fact(1000),
     &ebeam,qmin,wmin,delq,delw,meshq,meshw,imaxdt
      common / ckmat1 / ikmat
      common / ckmat2 / cdenomi,cvdelpi
      common / cinteg/ ngaus,mesh,lmap,ngang,meshang,nbmaxs,nbmaxr
      
      common / coutpt / vpoton(nedim,5,0:4,3,maxpot),ioutpt,ipoton
      dimension phase(nedim,5,2),phaset(nedim,5,2)
      common /mult/ cdmul(nedim,4,0:ladim,8),cbmul(nedim,4,0:ladim,8)
     & ,cbrmul(nedim,4,0:ladim,8),cbdmul(nedim,4,0:ladim,8)
     & ,ctmul (nedim,4,0:ladim,8)
     & ,cxb(4,0:ladim,8),cxbr(4,0:ladim,8),cxbd(4,0:ladim,8)
     & ,cxd(4,0:ladim,8),cxt(4,0:ladim,8)
     
c  input Wcm for photo and electro reaction

      wcm    = win/fm
      qsfm   = qin*(1000.d0/fm)**2

c  egam0 = time  component of virtual photon in CM
c  egam  = space component of virtual photon in CM

      egam0  = (wcm**2 - qsfm - fnuc**2)/2.d0/wcm
      egam   = sqrt(qsfm + egam0**2)

c  kinematics pi-n

      fmass1 = fpio
      fmass2 = fnuc

      call kinem 

c     fcc  = T to F of asymptotic state and unit 10^3/m_pi

      enui0  = sqrt(egam**2 + fnuc**2)
      enuf0  = sqrt(pp(maxp)**2 + fnuc**2)
      epi0   = sqrt(pp(maxp)**2 + fpio**2)
      fccx   = - 4.d0*pi**2/wcm*sqrt(enuf0*enui0*abs(egam0)*epi0)

      call setpotme(ngang,meshang,nbmaxs,nbmaxr)
      
      call pintmat(nx,ioutpt,nbmaxs,nbmaxr,phase,phaset)

      wcmm  = wcm *fm      
      tlab = (wcmm**2-938.28**2-139.57**2)/(2*938.28)-139.57
      call outph(nx,tlab,phaset,wcmm,imaxchn)

c     calculate electromagnetic form factor
      
      call gamfrm(egam0,egam)
      
      call mulamp(nx,egam,egam0)
            
      fact(nx) = fccx*1000.d0*fpio	! Units 10^-3 / m_pi
      
      return
      end
c------------------------------------------------------------------------
c     Write out multipole files
c------------------------------------------------------------------------      
      subroutine mulwrite(lm,tag)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      parameter(ndim=20,maxpot=8,nedim=50,npdim=20,nbmas=10,
     &          ladim=10)
      common / c2001 / wcmmev(1000),qsgev(1000),fact(1000),
     &ebeam,qmin,wmin,delq,delw,meshq,meshw,imaxdt
      common /mult/ cdmul(nedim,4,0:ladim,8),cbmul(nedim,4,0:ladim,8)
     & ,cbrmul(nedim,4,0:ladim,8),cbdmul(nedim,4,0:ladim,8)
     & ,ctmul (nedim,4,0:ladim,8)
     & ,cxb(4,0:ladim,8),cxbr(4,0:ladim,8),cxbd(4,0:ladim,8)
     & ,cxd(4,0:ladim,8),cxt(4,0:ladim,8)
     
      integer mulout(6),nopt1(18),nopt2(18)
      real rm1(100),re1(100),rs1(100)
      character*3 cmulout(6),vecout(18)
      character tag*30
      data mulout/7,8,1,2,3,4/
      data cmulout/'SL+','SL-','EL+','EL-','ML+','ML-'/
      data vecout/'e0p','e1p','e1m','m1p','m1m','e2p','e2m','m2p','m2m',
     1'e3p','e3m','m3p','m3m','s0p','s1p','s1m','s2p','s2m'/
      data nopt1/0,1,1,1,1,2,2,2,2,3,3,3,3,0,1,1,2,2/
      data nopt2/1,1,2,3,4,1,2,3,4,1,2,3,4,7,7,8,7,8/
      
      open(unit=1,file='hlee-PPpi'//tag,form='formatted',status='UNKNOWN')
      open(unit=2,file='hlee-PNpi'//tag,form='formatted',status='UNKNOWN')

      do nxx=1,imaxdt
c      c12=(ctmul(nxx,1,1,4)-sqrt(2.)*ctmul(nxx,2,1,4))/3.
c      c32=(ctmul(nxx,1,1,4)+ctmul(nxx,2,1,4)/sqrt(2.))
c      c32=c32*1.2
c      ctmul(nxx,1,1,4)=(c12+2*c32/3.) 
      do ich=1,2
      write(ich,1101)wcmmev(nxx)/1000.,qsgev(nxx)
      do iimu=1,6
      imu = mulout(iimu)
      write(ich,*) cmulout(iimu)
      do lx=0,lm
      cxb(ich,lx,imu)  = cbmul(nxx,ich,lx,imu)*fact(nxx)
      cxbr(ich,lx,imu) = cbrmul(nxx,ich,lx,imu)*fact(nxx)
      cxbd(ich,lx,imu) = cbdmul(nxx,ich,lx,imu)*fact(nxx)
      cxd(ich,lx,imu)  = cdmul(nxx,ich,lx,imu)*fact(nxx)
      cxt(ich,lx,imu)  = ctmul(nxx,ich,lx,imu)*fact(nxx)
      enddo
      write(ich,1234)cxt(ich,0,imu),cxt(ich,1,imu),cxt(ich,2,imu)
      write(ich,1234)cxt(ich,3,imu),cxt(ich,4,imu),cxt(ich,5,imu)
      write(20+ich,333)cxt(ich,0,imu),cxt(ich,1,imu),cxt(ich,2,imu),
     1                 cxt(ich,3,imu),cxt(ich,4,imu),cxt(ich,5,imu)
      enddo
      enddo
      enddo
      
1101  format('W= ',f5.3,1x,'Q2= ',f5.3)
1234  format(6e15.5)
 333  format(6f9.4,6f9.4)

      do i=1,2
        close(i)
      enddo

      do i=1,18
      do nxx=1,imaxdt
      open(unit=1,file=vecout(i)//'.vec',form='formatted',status='UNKNOWN')
      cpi0=ctmul(nxx,1,nopt1(i),nopt2(i))*fact(nxx)
      cpip=ctmul(nxx,2,nopt1(i),nopt2(i))*fact(nxx)
      c12=(cpi0-sqrt(2.)*cpip)/3.
      c32=(cpi0+cpip/sqrt(2.))
      write(1,1235) qsgev(nxx),wcmmev(nxx),c12,dum,dum,c32,dum,dum
      if (i.eq.2)  re1(nxx)=imag(c32)
      if (i.eq.4)  rm1(nxx)=imag(c32)
      if (i.eq.15) rs1(nxx)=imag(c32)
      enddo
      close(1)
      enddo
      
      open(unit=1,file='mrat.dat',form='formatted',status='UNKNOWN')
      do nxx=1,imaxdt
        write(1,1236) qsgev(nxx),rm1(nxx),re1(nxx)/rm1(nxx),rs1(nxx)/rm1(nxx)
      enddo
      close(1)
        
1235  format(10f9.3)
1236  format(4f9.4)
      return
      end
c--------------------------------------------------------------      
c    output phase shift
c--------------------------------------------------------------
      subroutine outph(ke,tpi0,phaset,wcmm,imaxchn)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      parameter(ndim=20,maxpot=8,nedim=50,npdim=20,nbmas=10,
     &          ladim=10)
      dimension phout(nedim,10),phaset(nedim,5,2)
      
      phout(ke,1) = phaset(ke,1,1)
      phout(ke,2) = phaset(ke,1,2)
      phout(ke,3) = phaset(ke,2,1)
      phout(ke,4) = phaset(ke,2,2)
      phout(ke,5) = phaset(ke,3,1)
      phout(ke,6) = phaset(ke,3,2)
      phout(ke,7) = phaset(ke,4,1)
      phout(ke,8) = phaset(ke,4,2)
      phout(ke,9) = phaset(ke,5,1)
      phout(ke,10) = phaset(ke,5,2)
      
      write(*,1000)wcmm,tpi0,(phout(ke,kc),kc=1,imaxchn)
 1000 format(1h ,12f10.5)
      return
      end
c--------------------------------------------------------------
c     read input data file
c--------------------------------------------------------------
      subroutine setpara(qinput,pinput,pfile)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      parameter(ndim=20,maxpot=8,nedim=50,npdim=20,nbmas=10)
      real pval,pstp,pmin,pmax,qinput 
      real*8 xval(37)
      integer plist,pstat,pinput
      character cdum*20, pname*20, pfile*30
      common / vrtfrm / vfpar(maxpot,nbmas),vgpar(maxpot,nbmas)
     &                 ,ivert(maxpot,nbmas),iverg(maxpot,nbmas)
      common / vrtcpl / gpin,gpind,grnp(nbmas),vkrnn(nbmas),gspn(nbmas)
      common / cmass  / pi,fm,fnuc,fpio,fdel,frho(nbmas),fsigm(nbmas)
      common / coutpt / vpoton(nedim,5,0:4,3,maxpot),ioutpt,ipoton
      common / ctmat1 / vpot(ndim,ndim),xkmat(ndim,ndim)
     &                 ,facpot(maxpot,nbmas)
      common / ctmat2 / vert0(ndim),cvert1(ndim),cvert2(ndim)
     &                 ,cself,ctmat(ndim,ndim,10),ct33(ndim,ndim)
     &                 ,fres0,iiout(10),jjout(10),llout(10)
      common / cnewver / facnew,iswsigm
      common / cchan /imaxchn
      common / cgampi1 /grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,swgm(8)
     &  ,fdelgm,fmrho,fmomg,xkrho,xkomg,phpi,vnnrho,vnnomg
     &  ,mnnrho,mnnomg,lambda,meshgm,lmaxgm
      common / c2001 / wcmmev(1000),qsgev(1000),fact(1000),
     &ebeam,qmin,wmin,delq,delw,meshq,meshw,imaxdt
      common / c2002 / igfrm,idfrm, betd, gamd
      common / ccpion / lmaxpi
      common / ckmat1 / ikmat
      common / fit /pval(37),pstp(37),pmin(37),pmax(37),
     &              plist(37),pstat(37),pname(37)
      common / cinteg/ ngaus,mesh,lmap,ngang,meshang,nbmaxs,nbmaxr
      
      dimension swpot(npdim)

      nbmaxs = 1
      nbmaxr = 1
      nxx    = 1
      mesh   = 1
      lmap   = 1
      
      call setdirac
      call setieps
      
      imaxchn    = 10
      fm         = 197.3289d0
      pi         = 3.1415926d0
      gpin       = sqrt(0.08d0*4.d0*pi)
      
c     Control parameters      

      open(unit=10,file='inp.dat',form='formatted',status='old')
      
      read(10,1121)cdum
      read(10,*)ikmat				! 1=T-matrix 2=K-matrix
      
      read(10,1121)cdum
      read(10,*)fmprom,fmneum,fmpi0m,fmpicm	! Hadron masses
      
      fmpro    = fmprom/fm
      fmneu    = fmneum/fm
      fmpi0    = fmpi0m/fm
      fmpic    = fmpicm/fm
      
      fnuc     = fmpro
      fpio     = fmpic
      
      read(10,1121)cdum				! delta(1232) mass
      read(10,*)fdelgmx
      
      fdelgm   = fdelgmx/fm
      
      read(10,1121)cdum  			! rho,omega masses
      read(10,*)fmrhox,fmomgx
      
      fmrho    = fmrhox/fm
      fmomg    = fmomgx/fm
      
c     delta-exchange potential    ipoton  = 2 (new) 1,0 (old)
c     sigma coupling              iswsigm = 1 (non-deriv.) 2 (deriv.)
c     direct p-wave anti-delta    facnew  = 1 (on) 0 (off)

      read(10,1121)cdum				! Control switches
      read(10,*)ipoton,ioutpt,iswsigm,facnew

      read(10,1121)cdum				! Scale factors for pi-n potential
      read(10,*) (swpot(i), i=1,6) 
                
      read(10,1121)cdum				! # points in gaussian integration
      read(10,*)ngaus
         
      read(10,1121)cdum				! Misc. parameters
      read(10,*)lambda,phpi,meshgm,lmaxgm
      
      read(10,1121)cdum				! Born diagram scale factors
      read(10,*) (swgm(i), i=1,8)
      
      read(10,1121)cdum				! Max L for calculated multipoles
      read(10,*) lmaxpi 
      
      read(10,1121)cdum				! Choose N,D f.f. parameterization
      read(10,*)igfrm, idfrm
      
      read(10,1121)cdum				! W mesh parameters
      read(10,*   )wmin,delw,meshw
      
      read(10,1121)cdum				! Q2 mesh parameters
      read(10,*   )qmin,delq,meshq
      
      read(10,1121)cdum				! Beam energy
      read(10,*   )ebeam
      
      close(10) 

C     Fit parameters

      print *, 'ALLK: Reading ',pfile
      open(unit=10,file=pfile,form='formatted',status='old')
      
      do i=1,37
        read(10,*) plist(i),pname(i),pval(i),pstp(i),
     &             pstat(i),pmin(i),pmax(i)
        write(6,1120) plist(i),pname(i),pval(i),pstp(i),
     &                pstat(i),pmin(i),pmax(i)
        xval(i)=dble(pval(i))
      enddo
      
      close(10)
      
1120  format(i2,1x,a10,2f10.4,i10,2f10.4)
1121  format(a20)
           
      if (qinput.ge.0.and.qinput.lt.1000) qmin = qinput
      if (qinput.ge.1000) wmin = qinput
      
      pinp = abs(pinput)
      if (pinput.gt.0) pval(pinp)=pval(pinp)+pstp(pinp)
      if (pinput.lt.0) pval(pinp)=pval(pinp)-pstp(pinp)
      
      call parput(xval)
      
c   settings for pi-N scattering

      mesh   = 1
      lmap   = 1

c   mesh for angular projection

      ngang = 4
      meshang = 10

c   factor for pi-n potential

      facpot(1,1) = (gpin/fpio)**2*swpot(1)
      facpot(2,1) = (gpin/fpio)**2*swpot(2)
      facpot(5,1) = (gpind/fpio)**2*swpot(5)
      facpot(6,1) = (gpind/fpio)**2*swpot(6)

      do kx = 1,nbmaxs
      facpot(3,kx) = gspn(kx)*swpot(3)
      enddo
  
      do kx = 1,nbmaxr
      facpot(4,kx) = grnp(kx)*swpot(4)
      enddo

      return
      end
      
c-------------------------------------------------------------------
c
c-------------------------------------------------------------------
      
      subroutine parput(pval)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      double precision pval(37)
      parameter(ndim=20,maxpot=8,nedim=50,npdim=20,nbmas=10)
      real*8 grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,swgm
     &  ,fdelgm,fmrho,fmomg,xkrho,xkomg,phpi,vnnrho,vnnomg
      common / vrtcpl / gpin,gpind,grnp(nbmas),vkrnn(nbmas),gspn(nbmas)
      common / cmass  / pi,fm,fnuc,fpio,fdel,frho(nbmas),fsigm(nbmas)
      common / cgampi1 /grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,swgm(8)
     &  ,fdelgm,fmrho,fmomg,xkrho,xkomg,phpi,vnnrho,vnnomg
     &  ,mnnrho,mnnomg,lambda,meshgm,lmaxgm
      common / vrtfrm / vfpar(maxpot,nbmas),vgpar(maxpot,nbmas)
     &                 ,ivert(maxpot,nbmas),iverg(maxpot,nbmas)
      common / ctmat2 / vert0(ndim),cvert1(ndim),cvert2(ndim)
     &                 ,cself,ctmat(ndim,ndim,10),ct33(ndim,ndim)
     &                 ,fres0,iiout(10),jjout(10),llout(10)
      common / c2002 / igfrm,idfrm,betd,gamd
      
      gpindx     = sqrt(72.d0/25.d0)*gpin
      
c     Variable     Fit            Description			SL01 (SL96)  Origin 
      
      gdm1       = pval(1)	! Delta(1232) gm coupling	1.85         Fit p(g,pi)
      gde2       = pval(2)	! Delta(1232) ge coupling	0.025        Fit p(g,pi)
      gdc2       = pval(3)	! Delta(1232) gc coupling      -0.238        Theory
      betd       = pval(4)	! Delta(1232) f.f. parameter    0.154        Fit ee'p
      gamd       = pval(5)	! Delta(1232) f.f. parameter    0.166        Fit ee'p
      fdel       = pval(6)/fm	! Delta(1232) mass           1236.           Fixed
      gpind      = pval(7)*gpindx	! piND coupling         1.204        Fit piN phases
      fres0      = pval(8)/fm	! Bare Delta(1232) mass      1299.07         Fit piN phases
      vfpar(5,1) = pval(9)	! piND vertex cutoff            3.29         Fit piN phases
      ivert(5,1) = pval(10)	! piND vertex power             2            Fixed
      vfpar(1,1) = pval(11)	! piNN vertex cutoff            3.2551       Fit piN phases
      ivert(1,1) = pval(12)	! piNN vertex power             2            Fixed
      fsigm(1)   = pval(13)/fm	! sigma meson mass            478.           Fixed
      gspn(1)    = pval(14)	! sigma meson coupling          0            Fixed
      vfpar(3,1) = pval(15)	! sigma f vertex cutoff         3.           Fixed
      ivert(3,1) = pval(16)	! sigma f vertex power          0            Fixed
      vgpar(3,1) = pval(17)	! sigma g vertex cutoff         3.           Fixed
      iverg(3,1) = pval(18)	! sigma g vertex power          4            Fixed
      frho(1)    = pval(19)/fm	! rho meson mass              765.           Fixed
      grnp(1)    = pval(20)	! rho meson coupling           38.4329       Fit piN phases
      vkrnn(1)   = pval(21)	! rho tensor coupling           1.825        Fit piN phases
      vfpar(4,1) = pval(22)	! rho f vertex cutoff           6.2305       Fixed
      ivert(4,1) = pval(23)	! rho f vertex power            0            Fixed
      vgpar(4,1) = pval(24)	! rho g vertex cutoff           6.2305       Fit piN phases
      iverg(4,1) = pval(25)	! rho g vertex power            4            Fixed
      xkrho      = vkrnn(1)	! rho tensor coupling           1.825        From piN fit
      xkomg      = pval(27)	! omega tensor coupling         0.           Fixed
      grpg       = pval(28)	! rho photon coupling           0.1027       PDG
      gopg       = pval(29)	! omega photon coupling         0.3247       PDG
      grnn       = pval(30)	! rho NN coupling               6.1994       universality->sqrt(grnp(1))
      gonn       = pval(31)	! omega NN coupling            11.5 (10.5)   Fit p(g,pi)
      vnnrho     = vgpar(4,1)	! rho vertex cutoff             6.2305       From piN fit
      mnnrho     = pval(33)	! rho vertex power              2            Fixed
      vnnomg     = pval(34)	! omega vertex cutoff           6.2305       From piN fit
      mnnomg     = pval(35)     ! omega vertex power            2            Fixed
      vfpar(1,2) = pval(36)	! piNN u-channel vertex cutoff
      vfpar(5,2) = pval(37)	! piND u-channel vertex cutoff
      
      return
      end
      
c-------------------------------------------------------------------
c
c-------------------------------------------------------------------

      subroutine tabmesh
      implicit real*8(a-b,d-h,o-z)
      common / c2001 / wcmmev(1000),qsgev(1000),fact(1000),
     &ebeam,qmin,wmin,delq,delw,meshq,meshw,imaxdt
     
      mxdim = 1000
      incnt = 0
      
      print *, 'ALLK: Kinematics table at Q2=',qmin
      
      do nx = 0,meshq
        qqx = qmin + delq*dble(nx)
      do ny = 0,meshw
        wcmx = wmin + delw*dble(ny)
        incnt = incnt + 1
        wcmmev(incnt) = wcmx
        qsgev (incnt) = qqx

        if(incnt.gt.mxdim) then
          write(*,*)' loop exceeds dim'
          stop
        end if
        
      enddo
      enddo
      imaxdt = incnt
      return
      end      
      
c-------------------------------------------------------------------
c
c-------------------------------------------------------------------
      subroutine pintmat(nx,ioutpt,nbmaxs,nbmaxr,phase,phaset)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      parameter(ndim=20,maxpot=8,nedim=50,npdim=20,nbmas=10,
     &          ladim=10)
      common / cmass  / pi,fm,fnuc,fpio,fdel,frho(nbmas),fsigm(nbmas)
      common / ckine1 / wcm,fmass1,fmass2,pp(ndim),ww(ndim),rhoon,maxp
      common / ckine3 / ww0(ndim)
      common / ctmat2 / vert0(ndim),cvert1(ndim),cvert2(ndim)
     &                 ,cself,ctmat(ndim,ndim,10),ct33(ndim,ndim)
     &                 ,fres0,iiout(10),jjout(10),llout(10)
      common / ctmat1 / vpot(ndim,ndim),xkmat(ndim,ndim)
     &                 ,facpot(maxpot,nbmas)
      common / cchan /imaxchn
      dimension jjin(5),llin(5)
     &  ,phase(nedim,5,2),phaset(nedim,5,2)
c      
      data jjin/1,1,3,3,5/
      data llin/0,2,2,4,4/
c
c   loop of channel
c
       ichn       = 0      
       do 200 jlx = 1,imaxchn/2
       do 200 itx = 1,2
c
         ichn     = ichn + 1
         jj       = jjin(jlx)
         ll       = llin(jlx)
         ii       = (itx-1)*2+1
         call tmat1(jj,ll,ii,nx,nbmaxs,nbmaxr)
c
         if(ioutpt.eq.1) go to 200
c
c    include direct isobar in 33 channel
c
         if(jlx.eq.3.and.itx.eq.2) then
           ires = 1
         else
           ires = 0
         end if
c         
         call tmat2(ires,ichn,ii,jj,ll)
c
c   tan(del)  from K-matrix
c
      tdel = -pi*rhoon*xkmat(maxp,maxp)
      del  = atan(tdel)*180.d0/pi
      if(ires.eq.1) then
         cxxx = ct33(maxp,maxp)
      tdelt = imag(cxxx)/dble(cxxx)
      else
         cxxx = ctmat(maxp,maxp,ichn)
      tdelt = imag(cxxx)/dble(cxxx)
      end if
      delt  = atan(tdelt)*180.d0/pi
c
      if(del.le.0.d0.and.ires.eq.1) del = del+180.d0
      if(delt.le.0.d0.and.ires.eq.1) delt = delt+180.d0
c
      phase(nx,jlx,itx) = del
      phaset(nx,jlx,itx)= delt
c
  200 continue
      return
      end
c------------------------------------------------------------------
c modify to k-matrix
c------------------------------------------------------------------
      subroutine tmat2(ires,ichn,ii,jj,ll)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      parameter(ndim=20,maxpot=8,nbmas=10)
      common / ckine1 / wcm,fmass1,fmass2,pp(ndim),ww(ndim),rhoon,maxp
      common / ctmat1/ vpot(ndim,ndim),xkmat(ndim,ndim)
     &                ,facpot(maxpot,nbmas)
      common / cmass  / pi,fm,fnuc,fpio,fdel,frho(nbmas),fsigm(nbmas)
      common / ctmat2 / vert0(ndim),cvert1(ndim),cvert2(ndim)
     &                 ,cself,ctmat(ndim,ndim,10),ct33(ndim,ndim)
     &                 ,fres0,iiout(10),jjout(10),llout(10)
      common / ckmat1 / ikmat
      common / ckmat2 / cdenomi, cvdelpi
c
      iiout(ichn) = ii
      jjout(ichn) = jj
      llout(ichn) = ll
c
      if(ires.eq.0) go to 1234
c
c     t-matrix with s-channel resonance
c
      if(ikmat.eq.1) then
      cfac1     = 1.d0 + (0.d0,1.d0)*pi*rhoon*xkmat(maxp,maxp)
      cfac2     = (0.d0,1.d0)*pi*rhoon
      else if(ikmat.eq.2) then
      cfac1     = 1.d0
      cfac2     = (0.d0,0.d0)
      end if
c
      do 100 kx = 1,maxp
      ppx       = pp(kx)
      vert0(kx) = fvert(ppx)
  100 continue
c
      do 200 kx = 1,maxp
      px    = pp(kx)
      csum1 = vert0(kx) - cfac2*vert0(maxp)*xkmat(kx,maxp)/cfac1
      csum2 = vert0(kx) - cfac2*vert0(maxp)*xkmat(maxp,kx)/cfac1
      do 210 ky = 1,maxp
      cttt  = (xkmat(kx,ky)+cfac2*(xkmat(kx,ky)*xkmat(maxp,maxp)
     &                            -xkmat(kx,maxp)*xkmat(maxp,ky)))
     &        /cfac1
      csum1 = csum1 + ww(ky)*vert0(ky)*cttt
      cttt  = (xkmat(ky,kx)+cfac2*(xkmat(ky,kx)*xkmat(maxp,maxp)
     &                            -xkmat(ky,maxp)*xkmat(maxp,kx)))
     &        /cfac1
      csum2 = csum2 + ww(ky)*vert0(ky)*cttt
  210 continue
      cvert1(kx) = csum1
      cvert2(kx) = csum2
  200 continue
c
      cself     = -cfac2*vert0(maxp)*cvert1(maxp)
      do 300 kx =1,maxp
      cself     = cself + ww(kx)*vert0(kx)*cvert1(kx)
  300 continue
c
      cfac3     = wcm - fres0 - cself
      do 400 kx = 1,maxp
      do 400 ky = 1,maxp
      cttt      = (xkmat(kx,ky)+cfac2*(xkmat(kx,ky)*xkmat(maxp,maxp)
     &                            -xkmat(kx,maxp)*xkmat(maxp,ky)))
     &        /cfac1
      ct33(kx,ky) = cttt + cvert1(kx)*cvert2(ky)/cfac3
  400 continue
      cdenomi    = cfac3
      cvdelpi    = cvert1(maxp)
c
 1234 continue
c
c   t-matrix without resonance
c
      if(ikmat.eq.1) then
      cfac2     = (0.d0,1.d0)*pi*rhoon
      cfac1     = 1.d0 + (0.d0,1.d0)*pi*rhoon*xkmat(maxp,maxp)
      else if(ikmat.eq.2) then
      cfac2     = (0.d0,0.d0)
      cfac1     = 1.d0
      end if
c
      do 500 kx = 1,maxp
      do 500 ky = 1,maxp
      cttt      = (xkmat(kx,ky)+cfac2*(xkmat(kx,ky)*xkmat(maxp,maxp)
     &                            -xkmat(kx,maxp)*xkmat(maxp,ky)))
     &            /cfac1
      ctmat(kx,ky,ichn) = cttt
  500 continue
c
      return
      end
c-------------------------------------------------------------------
c
c-------------------------------------------------------------------
      real*8 function fvert(p)
      implicit real*8(a-h,o-z)
      parameter(maxpot=8,nbmas=10)
      common / vrtfrm / vfpar(maxpot,nbmas),vgpar(maxpot,nbmas)
     &                 ,ivert(maxpot,nbmas),iverg(maxpot,nbmas)
      common / vrtcpl / gpin,gpind,grnp(nbmas),vkrnn(nbmas),gspn(nbmas)
      common / cmass  / pi,fm,fnuc,fpio,fdel,frho(nbmas),fsigm(nbmas)
c
      enuc  = sqrt(p**2 + fnuc**2)
      epi   = sqrt(p**2 + fpio**2)
      fvert = gpind/fpio*sqrt(fnuc/12.d0/pi**2/enuc/epi)*p*
     &                   sqrt((enuc+fnuc)/2.d0/fnuc)*
     &  vrtfun(vfpar(5,1),fpio,p,ivert(5,1))
      return
      end
c--------------------------------------------------------------------------
c   1 nuclon uncross   2 nucleon cross
c   3 sigma            4 rho
c
c  jj = 2*J,  ll = 2*L,  ii = 2*t
c      
c
c     K-matrix of back ground potential
c--------------------------------------------------------------------------
      subroutine tmat1(jj,ll,ii,nengy,nbmaxs,nbmaxr)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      parameter(maxl=4,maxpot=8,ndim=20,nedim=50,nbmas=10)
      common / vrtfrm / vfpar(maxpot,nbmas),vgpar(maxpot,nbmas)
     &                 ,ivert(maxpot,nbmas),iverg(maxpot,nbmas)
      common / cmass  / pi,fm,fnuc,fpio,fdel,frho(nbmas),fsigm(nbmas)
      common / ckine1 / wcm,fmass1,fmass2,pp(ndim),ww(ndim),rhoon,maxp
      common / ctmat1 / vpot(ndim,ndim),xkmat(ndim,ndim),
     &                  facpot(maxpot,nbmas)
      common / cpotme / vme(ndim,ndim,0:maxl,maxpot,2,nedim)
      common / coutpt / vpoton(nedim,5,0:4,3,maxpot),ioutpt,ipoton
      dimension yymat(ndim,ndim),ybmat(ndim+2),faciso(maxpot,3)
c
c  t_j*t_i
      faciso(1,1) = 3
      faciso(1,3) = 0
c  t_i*t_j
      faciso(2,1) = -1
      faciso(2,3) = 2
c  d_(j,i)
      faciso(3,1) = 1
      faciso(3,3) = 1
c  i e_(jik)t_k/2
      faciso(4,1) = 1
      faciso(4,3) =-0.5d0
c   T_i*T_j
      faciso(5,1) = 4.d0/3.d0
      faciso(5,3) = 1.d0/3.d0
c   T_j*T_i
      faciso(6,1) = 0
      faciso(6,3) = 1
c
c   pion-nucleon potential angular momentum decomposition
c
      l1        = ll/2
      l2        = jj - l1
      do 100 k1 = 1,maxp
      do 110 k2 = k1,maxp
        ppf     = pp(k2)
        ppi     = pp(k1)
        potnu   = (vme(k1,k2,l1,1,1,1)+vme(k1,k2,l2,1,2,1))
     &            *faciso(1,ii)*facpot(1,1)
        potnc   = (vme(k1,k2,l1,2,1,1)+vme(k1,k2,l2,2,2,1))
     &            *faciso(2,ii)*facpot(2,1)
        potsi   = 0
        do 210 kx =1,nbmaxs
        potsi   = (vme(k1,k2,l1,3,1,kx)+vme(k1,k2,l2,3,2,kx))
     &            *faciso(3,ii)*facpot(3,kx)+potsi
  210   continue
        potrh   = 0
        do 220 kx = 1,nbmaxr
        potrh   = (vme(k1,k2,l1,4,1,kx)+vme(k1,k2,l2,4,2,kx))
     &            *faciso(4,ii)*facpot(4,kx)+potrh
  220   continue
        potde   = (vme(k1,k2,l1,5,1,1)+vme(k1,k2,l2,5,2,1))
     &            *faciso(5,ii)*facpot(5,1)
        potded  = (vme(k1,k2,l1,6,1,1)+vme(k1,k2,l2,6,2,1))
     &            *faciso(6,ii)*facpot(6,1)
c
        vpot(k2,k1) = potnu + potnc + potsi + potrh + potde+potded
        vpot(k1,k2) = potnu + potnc + potsi + potrh + potde+potded
c
      if(ioutpt.eq.1.and.k1.eq.k2.and.k1.eq.maxp) then
        vpoton(nengy,jj,ll,ii,1)=potnu
        vpoton(nengy,jj,ll,ii,2)=potnc
        vpoton(nengy,jj,ll,ii,3)=potsi
        vpoton(nengy,jj,ll,ii,4)=potrh
        vpoton(nengy,jj,ll,ii,5)=potde
        vpoton(nengy,jj,ll,ii,6)=potded
        vpoton(nengy,jj,ll,ii,7)=vpot(k1,k2)
      end if
c
  110 continue
  100 continue
c
      if(ioutpt.eq.1) return
c
      do 200 k1 = 1,maxp
      do 200 k2 = 1,maxp
        fsum = 0
        if(k1.eq.k2)then
        fsum = 1
        end if
        yymat(k2,k1) = fsum - vpot(k2,k1)*ww(k1)
  200 continue
c
c  matrix inversion
c
c   kmat = yymat^(-1)*vpot
c
       id = ndim
       is = -1
       call crout(yymat,ybmat,maxp,id,is)
c
       do 300 k1=1,maxp
       do 300 k2=1,maxp
         sum = 0
         do 310 kn = 1,maxp
         sum = sum + yymat(k1,kn)*vpot(kn,k2)
  310    continue
         xkmat(k1,k2)= sum
  300 continue
      return
      end
c-----------------------------------------------------------------
c    hpot(i,j)  = m/sqrt(e,e')u(p') o u(p) = h00 + l*l' h11
c
c        j=1 h00  j=2  h11
c        i=1 n-s  i=2  n-u  i=3 d-u
c-----------------------------------------------------------------
      subroutine setpotme(meshg,maxg,nbmaxs,nbmaxr)
      implicit real*8(a-h,o-z)
      parameter(maxl=4,maxpot=8,ndim=20,nedim=50,nbmas=10)
      common / vrtfrm / vfpar(maxpot,nbmas),vgpar(maxpot,nbmas)
     &                 ,ivert(maxpot,nbmas),iverg(maxpot,nbmas)
      common / vrtcpl / gpin,gpind,grnp(nbmas),vkrnn(nbmas),gspn(nbmas)
      common / cmass  / pi,fm,fnuc,fpio,fdel,frho(nbmas),fsigm(nbmas)
      common / cpotme / vme(ndim,ndim,0:maxl,maxpot,2,nedim)
      common / ckine1 / wcm,fmass1,fmass2,pp(ndim),ww(ndim),rhoon,maxp
      common / coutpt / vpoton(nedim,5,0:4,3,maxpot),ioutpt,ipoton
      common / cpotmx / xgaus(50),wgaus(50),flest(0:10,100)
     &                 ,zzst(100),wwst(100),ichxx,maxm
      common / cnewver / facnew,iswsigm
c
      dimension fle(0:10)
     & ,hpot31(nbmas),hpot32(nbmas),hpot41(nbmas),hpot42(nbmas)
c
      if(ichxx.ne.1007)then
      call gaussl(meshg,xgaus,wgaus)
      maxm      = meshg*maxg
      dgu       = 1.d0/dble(maxg)
      ic        = 1
      do 11 nx  = 1,maxg
      do 11 ng  = 1,meshg
      zz        = dgu*(xgaus(ng)+dble(2*nx-1))-1.d0
      www       = dgu*wgaus(ng)*2.d0*pi
c
      call legen(zz,fle)
c
      zzst(ic)  = zz
      wwst(ic)  = www
      do 12 lx  = 0,10
   12 flest(lx,ic)= fle(lx)
      ic        = ic + 1
   11 continue
      ichxx     = 1007
      end if
c
      do 100 n1     = 1,maxp
      do 100 n2     = n1,maxp
      pcf           = pp(n1)
      pci           = pp(n2)
c
      do 200 l      = 0,maxl
      do 200 k1     = 1,maxpot
      do 200 k2     = 1,2
      do 200 k3     = 1,nbmas
  200 vme(n1,n2,l,k1,k2,k3)  = 0
c
c   z-independent part
c
      ecf  = sqrt(fnuc**2+pcf**2)
      eci  = sqrt(fnuc**2+pci**2)
      ocf  = sqrt(fpio**2+pcf**2)
      oci  = sqrt(fpio**2+pci**2)
      ssf  = ecf+ocf
      ssi  = eci+oci
      sip  = ssi + fnuc
      sim  = ssi - fnuc
      sfp  = ssf + fnuc
      sfm  = ssf - fnuc
      enum   = ecf    - eci
      enup   = ecf    + eci
      opim   = ocf    - oci
      opip   = ocf    + oci
c
c   common factor
c
      fac00 = 1.d0/(2.d0*pi)**3/sqrt(4.d0*oci*ocf)*
     &       sqrt((ecf+fnuc)*(eci+fnuc)/(4.d0*eci*ecf))
      fac01 = pci*pcf/(eci+fnuc)/(ecf+fnuc)
      fac11 = fac00*fac01
c
c    angular integration
c
      maxm      = meshg*maxg
c
      do 110 nx = 1,maxm
      zz        = zzst(nx)
      www       = wwst(nx)
c
      do 115 nnx = 0,10
  115 fle(nnx)   = flest(nnx,nx)
c
c   z-dependent part
c
      pfiz   = pcf*pci*zz
      qfi    = ocf*oci - pfiz
      qfin   = ecf*eci - pfiz
      qspacs = pcf**2 + pci**2 - 2.d0*pfiz
      pspacs = pcf**2 + pci**2 + 2.d0*pfiz
      qspac  = sqrt(qspacs)
      pspac  = sqrt(pspacs)
c
c
c    s-channel nucleon
c
      xnbrn   = vrtfun(vfpar(1,1),fpio,pcf,ivert(1,1))
     &         *vrtfun(vfpar(1,1),fpio,pci,ivert(1,1))
      f00ns   = sim*sfm/2.d0*(1.d0/sip + 1.d0/sfp)
      f11ns   = sip*sfp/2.d0*(1.d0/sim + 1.d0/sfm)
      hpot11  = f00ns*fac00*xnbrn
      hpot12  = f11ns*fac11*xnbrn
c
c    u-channel nucleon
c
      if (vfpar(1,2).ne.0) then
      xnbrn   = vrtfun(vfpar(1,2),fpio,qspac,ivert(1,1))
      xnbrn   = xnbrn**2
      endif
      
      f00nu0  = (-sim-sfm)/2.d0
      f00nu1  = sim*sfm*fnuc
      f00nu2  = -2.d0*fnuc
      f11nu0  = (-sip-sfp)/2.d0
      f11nu1  = -sip*sfp*fnuc
      f11nu2  = 2.d0*fnuc
      zz1     = 1.d0/(fpio**2 -2.d0*eci*ocf-2.d0*pfiz)
     &         + 1.d0/(fpio**2 -2.d0*ecf*oci-2.d0*pfiz)
      zz2     = qfi*zz1
      hpot21  = (f00nu0+f00nu1*zz1+f00nu2*zz2)*fac00*xnbrn
      hpot22  = (f11nu0+f11nu1*zz1+f11nu2*zz2)*fac11*xnbrn
c
c    sigma meson
c
      if(iswsigm.eq.1) then
        facsigm   = 1
      else if(iswsigm.eq.2) then
        facsigm   = qfi
      else if(iswsigm.eq.3) then
        facsigm  = 0.0
      end if
      do 910 kx = 1,nbmaxs
      xsigm  = vrtfun(vfpar(3,kx),fpio,pcf,ivert(3,kx))
     &        *vrtfun(vfpar(3,kx),fpio,pci,ivert(3,kx))
     &        *vrtfun(vgpar(3,kx),fsigm(kx),qspac,iverg(3,kx))
      zzs    = 1.d0/(2.d0*fpio**2 - fsigm(kx)**2 - 2.d0*qfi)/2.d0
     &       + 1.d0/(2.d0*fnuc**2 - fsigm(kx)**2 - 2.d0*qfin)/2.d0
      hpot31(kx)  =  fac00*xsigm*zzs*2.d0*facsigm
      hpot32(kx)  = -fac11*xsigm*zzs*2.d0*facsigm
  910 continue
c
c    rho meson
c
      enum2  = ecf**2 - eci**2
      do 911 kx =1,nbmaxr
c      xrhoo  = vrtfun(vfpar(4,kx),fpio,pcf,ivert(4,kx))
c     &        *vrtfun(vfpar(4,kx),fpio,pci,ivert(4,kx))
c     &        *vrtfun(vgpar(4,kx),frho(kx),qspac,iverg(4,kx))
      xrhoo  = vrtfun(vfpar(4,kx),fpio,qspac,ivert(4,kx))
     &        *vrtfun(vgpar(4,kx),frho(kx),qspac,iverg(4,kx))
      frhos  = frho(kx)**2
      frap1  = enup + vkrnn(kx)*opip + enum*enum2/frhos
      frap2  = - opip/frhos
      fram1  = (enum*opip-enum2)/frhos
      frbp1  = -2.d0*fnuc-vkrnn(kx)/2.d0/fnuc*opip*enup
      frbp2  = -vkrnn(kx)/fnuc
      frbm1  = -vkrnn(kx)/2.d0/fnuc*enum
      frcp1  =  vkrnn(kx)/fnuc*pci*pcf
      zzr1   = 1.d0/(2.d0*fpio**2 - frhos - 2.d0*qfi)/2.d0
      zzr2   = 1.d0/(2.d0*fnuc**2 - frhos - 2.d0*qfin)/2.d0
      zzrp   = zzr1 + zzr2
      zzrm   = -zzr1*opim + zzr2*enum
      hpot4a  = zzrp*(frap1 + frap2*qspacs)+ zzrm*fram1 +frap2
      hpot4b  = zzrp*(frbp1 + frbp2*pfiz  )+ zzrm*frbm1
      hpot4c  = zzrp* frcp1
      hpot41(kx)  = fac00*xrhoo*(hpot4a+hpot4b-fac01*hpot4c)
      hpot42(kx)  = fac00*xrhoo*(fac01*(hpot4a-hpot4b)+hpot4c)
  911 continue
c
c    new delta cross
c
      if (vfpar(5,2).ne.0) then
        xdelt = vrtfun(vfpar(5,2),fpio,qspac,ivert(5,1))
        xdelt = xdelt**2
      else
        xdelt = vrtfun(vfpar(5,1),fpio,pcf,ivert(5,1))
     &         *vrtfun(vfpar(5,1),fpio,pci,ivert(5,1))
      endif
c
      xp01        =  eci - ocf
      xp02        =  ecf - oci
      xp00        =  sqrt(fdel**2+pspacs)
      piqi        =  eci*oci + pci**2
      piqf        =  eci*ocf + pfiz
      pfqi        =  ecf*oci + pfiz
      pfqf        =  ecf*ocf + pcf**2
      p0qi        =  xp00*oci + pci**2 + pfiz
      p0qf        =  xp00*ocf + pcf**2 + pfiz
c
      xalf11      =  (piqi - qfi    )/fdel - fnuc
      xalf12      =  (piqf - fpio**2)/fdel - fnuc
      xalf21      =  (pfqi - fpio**2)/fdel - fnuc
      xalf22      =  (pfqf - qfi    )/fdel - fnuc
      xalf01      =  p0qi/fdel - fnuc
      xalf02      =  p0qf/fdel - fnuc
      xdd1        =  3.d0*(fnuc**2+fpio**2-fdel**2-2.d0*piqf)
      xdd2        =  3.d0*(fnuc**2+fpio**2-fdel**2-2.d0*pfqi)
      xdd0        = (1.d0/(eci-xp00-ocf)+1.d0/(ecf-xp00-oci))/12.d0/xp00
      bpa1        = (ssi+xalf11)*(ssf+xalf12)*(xp01-fdel)
     &             +(ssi+xalf11)*(ssf-xalf12)*(ecf-fnuc)
     &             +(ssi-xalf11)*(ssf+xalf12)*(eci-fnuc)
     &             +(-qfi+(xalf11+fnuc)*(xalf12+fnuc))*
     &              (xp01-ecf-eci+2.d0*fnuc+fdel)
      bma1        = (ssi-xalf11)*(ssf-xalf12)*(xp01+fdel)
     &             +(ssi-xalf11)*(ssf+xalf12)*(ecf+fnuc)
     &             +(ssi+xalf11)*(ssf-xalf12)*(eci+fnuc)
     &             +(-qfi+(xalf11+fnuc)*(xalf12+fnuc))*
     &              (xp01-ecf-eci-2.d0*fnuc-fdel)
      bpa2        = (ssi+xalf21)*(ssf+xalf22)*(xp02-fdel)
     &             +(ssi+xalf21)*(ssf-xalf22)*(ecf-fnuc)
     &             +(ssi-xalf21)*(ssf+xalf22)*(eci-fnuc)
     &             +(-qfi+(xalf21+fnuc)*(xalf22+fnuc))*
     &              (xp02-ecf-eci+2.d0*fnuc+fdel)
      bma2        = (ssi-xalf21)*(ssf-xalf22)*(xp02+fdel)
     &             +(ssi-xalf21)*(ssf+xalf22)*(ecf+fnuc)
     &             +(ssi+xalf21)*(ssf-xalf22)*(eci+fnuc)
     &             +(-qfi+(xalf21+fnuc)*(xalf22+fnuc))*
     &              (xp02-ecf-eci-2.d0*fnuc-fdel)
      bpa0        = (ssi+xalf01)*(ssf+xalf02)*(xp00-fdel)
     &             +(ssi+xalf01)*(ssf-xalf02)*(ecf-fnuc)
     &             +(ssi-xalf01)*(ssf+xalf02)*(eci-fnuc)
     &             +(-qfi+(xalf01+fnuc)*(xalf02+fnuc))*
     &              (xp00-ecf-eci+2.d0*fnuc+fdel)
      bma0        = (ssi-xalf01)*(ssf-xalf02)*(xp00+fdel)
     &             +(ssi-xalf01)*(ssf+xalf02)*(ecf+fnuc)
     &             +(ssi+xalf01)*(ssf-xalf02)*(eci+fnuc)
     &             +(-qfi+(xalf01+fnuc)*(xalf02+fnuc))*
     &              (xp00-ecf-eci-2.d0*fnuc-fdel)
c
      xnew0       = xdelt*(bpa1/xdd1+bpa2/xdd2)/2.d0*fac00
      xnew1       = xdelt*(bma1/xdd1+bma2/xdd2)/2.d0*fac00*fac01
      xold0       = xdelt*bpa0*xdd0*fac00
      xold1       = xdelt*bma0*xdd0*fac00*fac01
c
c
c    delta  cross
c
      fd2ndp  = 2.d0*fnuc+fdel
      fd2ndt  = 2.d0*fnuc*fdel
      fddels  = fdel**2
      edels   = fddels+pspacs
      edel    = sqrt(edels)
      fdalf   = (edel+opip)**2*(edel**2-opim**2)+fddels*
     &          (4.d0*fpio**2-fddels-2.d0*opip*(edel+opip))
      fdbeta  = (edel-enup)*(enum**2-edels+fddels+fd2ndt)
     &            - 2.d0*fnuc*edel*fd2ndp
      fdgamm  =-(edel-enup)*(2.d0*edel*fnuc+(edel+enup)*fdel)
     &            +fddels*fd2ndp
      ddelta  = (1.d0/(eci-edel-ocf)+1.d0/(ecf-edel-oci))/2.d0
      delfac  = fac00*xdelt*ddelta/6.d0/fdel/edel
c
      hdel1   = fd2ndp/2.d0/fdel*fdalf     + (edel+opip)/2.d0*fdbeta
      hdel2   = (edel-enup)/2.d0/fdel*fdalf+ (edel+opip)/2.d0*fdgamm
      if(ipoton.eq.0) then
      hpot51  = delfac*( hdel1+hdel2)
      hpot52  = delfac*(-hdel1+hdel2)*fac01
      else if(ipoton.eq.1) then
      hpot51  = xold0 
      hpot52  = xold1
      else if(ipoton.eq.2) then
      hpot51  = xnew0 
      hpot52  = xnew1
      end if
c
c   delta direct
c
      xdelt = vrtfun(vfpar(5,1),fpio,pcf,ivert(5,1))
     &       *vrtfun(vfpar(5,1),fpio,pci,ivert(5,1))
     
      hdel3x  = fac00*xdelt
      hnewd   = facnew/6.d0*pci*pcf*fac01
     &         *(1.d0/(fdel+ssi)+1.d0/(ssf+fdel))
      hpot61  = hdel3x*(
     &    ocf*oci/3.d0/fdel**2*(ssf+ssi+2.d0*fdel)
     &  - (pci**2*ocf/(eci+fnuc)+ oci*pcf**2/(ecf+fnuc))/3.d0/fdel
     &  - hnewd)
c
      hpot62  = hdel3x*(
     &      ocf*oci/3.d0/fdel**2*(ssf+ssi-2.d0*fdel)*fac01
     &    +(ocf/(ecf+fnuc)+oci/(eci+fnuc))*pci*pcf/3.d0/fdel
     &    + hnewd*3.d0*zz)
c
      do 120 lx          = 0,maxl
        wwl              = www*fle(lx)
        vme(n1,n2,lx,1,1,1)= 
     &  vme(n1,n2,lx,1,1,1)+wwl*hpot11
        vme(n1,n2,lx,1,2,1)=
     &  vme(n1,n2,lx,1,2,1)+wwl*hpot12
c
        vme(n1,n2,lx,2,1,1)=
     &  vme(n1,n2,lx,2,1,1)+wwl*hpot21
        vme(n1,n2,lx,2,2,1)=
     &  vme(n1,n2,lx,2,2,1)+wwl*hpot22
c sigma
        do 920 kx = 1,nbmaxs
        vme(n1,n2,lx,3,1,kx)=
     &  vme(n1,n2,lx,3,1,kx)+wwl*hpot31(kx)
        vme(n1,n2,lx,3,2,kx)=
     &  vme(n1,n2,lx,3,2,kx)+wwl*hpot32(kx)
  920   continue
c  rho
        do 921 kx = 1,nbmaxr
        vme(n1,n2,lx,4,1,kx)=
     &  vme(n1,n2,lx,4,1,kx)+wwl*hpot41(kx)
        vme(n1,n2,lx,4,2,kx)=
     &  vme(n1,n2,lx,4,2,kx)+wwl*hpot42(kx)
  921   continue
c   delta-exchange
        vme(n1,n2,lx,5,1,1)=
     &  vme(n1,n2,lx,5,1,1)+wwl*hpot51
        vme(n1,n2,lx,5,2,1)=
     &  vme(n1,n2,lx,5,2,1)+wwl*hpot52
c    delta-direct
        vme(n1,n2,lx,6,1,1)=
     &  vme(n1,n2,lx,6,1,1)+wwl*hpot61
        vme(n1,n2,lx,6,2,1)=
     &  vme(n1,n2,lx,6,2,1)+wwl*hpot62
  120 continue
c
  110 continue
  100 continue
      return
      end
c-----------------------------------------------------
c
c-----------------------------------------------------
      real*8 function vrtfun(fcut,fmass,q,itype)
      implicit real*8(a-h,o-z)
      vrtfun= ((fcut**2)/(fcut**2+q**2))**itype
      return
      end
c---------------------------------------------------
c
c---------------------------------------------------
      subroutine legen(z,fle)
      implicit real*8(a-h,o-z)
      parameter(maxl=10)
      dimension fle(0:maxl)

      fle(0)   = 1
      fle(1)   = z
      do 100 i = 2,maxl
      fle(i)   = (dble(2*i-1)*z*fle(i-1)-dble(i-1)*fle(i-2))/dble(i)
  100 continue
      return
      end
c--------------------------------------------------------------------------
c
c--------------------------------------------------------------------------
      subroutine kinem 
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      parameter(ndim=20)
      common / ckine1 / wcm,fmass1,fmass2,pp(ndim),ww(ndim),rhoon,maxp
      common / ckine2 / xgaus(50),wgaus(50),ichgg
      common / ckine3 / ww0(ndim)
      common / cinteg/ ngaus,mesh,lmap,ngang,meshang,nbmaxs,nbmaxr

      if(ichgg.ne.1104) then
      call gaussl(ngaus,xgaus,wgaus)
      ichgg   = 1104
      end if

      maxp = ngaus*mesh+1

      pon  = sqrt( (wcm**2 - (fmass1+fmass2)**2)
     &            *(wcm**2 - (fmass1-fmass2)**2) )/2.d0/wcm

      wsum = 0
      dgu = 1.d0/dble(mesh)
      ig = 0
      do 100 nx = 1,mesh
      do 100 ng =1,ngaus
        ig      = ig + 1
        x       = dgu*(xgaus(ng)+dble(2*nx-1))-1.d0
        pp(ig)  = fmap(x,lmap)
        pps     = pp(ig)**2
        ecm     = sqrt(fmass1**2+pps)+sqrt(fmass2**2+pps)
        wwww    = wgaus(ng)*dgu*dfmap(x,lmap)
        wsum    = wsum + wwww/(pon**2 - pp(ig)**2)
        ww(ig)  = pp(ig)**2*wwww/(wcm-ecm)
        ww0(ig) = wwww
  100 continue
      ee1       = sqrt(pon**2+fmass1**2)
      ee2       = sqrt(pon**2+fmass2**2)
      rhoon     = pon*ee1*ee2/wcm
      pp(maxp)  = pon
      ww(maxp)  = -wsum*2.d0*pon*rhoon
      ww0(maxp) = 0

      return
      end
c-------------------------------------------------------------------
c   cbmul   b. g. born
c          v_bg 
c   cbrmul  b. g. rescatt by back ground pi-n scatt
c          T_V G_0 v_bg
c   cbdmul  b.g.  rescatt by delta
c          \Gamma_{\pi}G_{\Delta}\Gamma_{\pi}G_{\Delta}G_0 v_bg
c   cdmul   delta-direct
c          \Gamma_{\pi} G_{\Delta}\Gamma_{\gamma}   
c
c       1  eL+   2 eL-  3 mL+  4 mL- 5 sL+' 6 sL-' 7 sL+ 8 sL-
c       L
c       iso 1/2  3/2  0
c--------------------------------------------------------------------      
      subroutine mulamp(nxin,egam,egam0)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      parameter(ndim=20,maxpot=8,nedim=50,npdim=20,nbmas=10,
     &          nadim=50,ladim=10)
      common / cmass  / pi,fm,fnuc,fpio,fdel,frho(nbmas),fsigm(nbmas)
      common / ckine1 / wcm,fmass1,fmass2,pp(ndim),ww(ndim),rhoon,maxp
      common / ct2eml / icheck
      common / ctmat2 / vert0(ndim),cvert1(ndim),cvert2(ndim)
     &                 ,cself,ctmat(ndim,ndim,10),ct33(ndim,ndim)
     &                 ,fres0,iiout(10),jjout(10),llout(10)
      common / cchan /imaxchn
      common / cgampi1 /grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,swgm(8)
     &  ,fdelgm,fmrho,fmomg,xkrho,xkomg,phpi,vnnrho,vnnomg
     &  ,mnnrho,mnnomg,lambda,meshgm,lmaxgm
      common / ccpion / lmaxpi
      common / ckmat1 / ikmat
       dimension
     &   pl(-2:ladim),fap(4,4,0:ladim,nadim),fep(2,2,0:ladim,nadim)
     &  ,xgu(50),wgu(50),fle(0:10)
       dimension camp(4,8),campd(4,8)
     &   ,ctmp(4,0:ladim,8),ctmpd(4,0:ladim,8)
     &   ,cbgt(3),c33t(3),cbgtmat(0:ladim,2,2,2),c33tmat(0:ladim,2,2,2)
     
      common /mult/ cdmul(nedim,4,0:ladim,8),cbmul(nedim,4,0:ladim,8)
     & ,cbrmul(nedim,4,0:ladim,8),cbdmul(nedim,4,0:ladim,8)
     & ,ctmul (nedim,4,0:ladim,8)
     & ,cxb(4,0:ladim,8),cxbr(4,0:ladim,8),cxbd(4,0:ladim,8)
     & ,cxd(4,0:ladim,8),cxt(4,0:ladim,8)
c

      if(icheck.eq.1234) go to 1234
      icheck        = 1234
      s2            = sqrt(2.d0)
c
      call gaussl(meshgm,xgu,wgu)
c
      do 100 nx     = 1,meshgm
        z           = xgu(nx)
        call legen(z,fle)
        do 110 lx = 0,ladim
  110   pl(lx)    = fle(lx)
        
      pl(-2)        = 0
      pl(-1)        = 0
c
      do 120 lpi = 0,ladim-2
      ple        = pl(lpi)
      plep1      = pl(lpi+1)  
      plem1      = pl(lpi-1)
      ple1m      = plep1  - plem1
      ple2m      = pl(lpi)-pl(lpi-2)
      ple2p      = pl(lpi)-pl(lpi+2)
      fac1       = wgu(nx)/dble(2*lpi+2)
      fap(1,1,lpi,nx) =  ple*fac1
      fap(1,2,lpi,nx) = -plep1*fac1
      fap(1,3,lpi,nx) = -dble(lpi)  /dble(2*lpi+1)*ple1m*fac1
      fap(1,4,lpi,nx) =  dble(lpi+1)/dble(2*lpi+3)*ple2p*fac1
      fep(1,1,lpi,nx) = plep1*fac1
      fep(1,2,lpi,nx) = ple*fac1
      if(lpi.ge.1) then
      fac2            = wgu(nx)/dble(2*lpi)
      fap(2,1,lpi,nx) =  ple*fac2
      fap(2,2,lpi,nx) = -plem1*fac2
      fap(2,3,lpi,nx) =  dble(lpi+1)/dble(2*lpi+1)*ple1m*fac2
      fap(2,4,lpi,nx) =  dble(lpi)/dble(2*lpi-1)*ple2m*fac2
      fap(3,1,lpi,nx) =  ple*fac1
      fap(3,2,lpi,nx) = -plep1*fac1
      fap(3,3,lpi,nx) =  1.d0/dble(2*lpi+1)*ple1m*fac1
      fap(3,4,lpi,nx) =  0
      fap(4,1,lpi,nx) = -ple*fac2
      fap(4,2,lpi,nx) =  plem1*fac2
      fap(4,3,lpi,nx) = -1.d0/dble(2*lpi+1)*ple1m*fac2
      fap(4,4,lpi,nx) = 0
      fep(2,1,lpi,nx) = plem1*fac2
      fep(2,2,lpi,nx) = ple*fac2
      end if
 120  continue
 100  continue
 1234 continue
c
      do 402 ich = 1,4
      do 402 lx  = 0,ladim
      do 402 kx  = 1,8
        cbmul(nxin,ich,lx,kx) = (0.d0,0.d0)
        cbrmul(nxin,ich,lx,kx)= (0.d0,0.d0)
        cbdmul(nxin,ich,lx,kx)= (0.d0,0.d0)
        cdmul(nxin,ich,lx,kx) = (0.d0,0.d0)
        ctmul(nxin,ich,lx,kx) = (0.d0,0.d0)
  402 continue
c
      if(ikmat.eq.1) then
         fkmat = 1
      else if(ikmat.eq.2) then
         fkmat = 0
      end if
c
      cdenom =  wcm - fres0 - cself
c
      do 400 npx = 1,maxp
c
      ppion     = pp(npx)
      cwwxx     = ww(npx)
      if(npx.eq.maxp) then
        cwwxx   = ww(npx) - (0.d0,1.d0)*pi*rhoon*fkmat
      end if
c
c  direct delta vertex
c
      eee    = sqrt((fnuc**2 + ppion**2)*(fpio**2+ppion**2))
      yfact  = sqrt((sqrt(fnuc**2 + ppion**2)+fnuc)/(2.d0*fnuc)) !! corrected
      xfact  = sqrt(4.d0*pi/3.d0/(2.d0*pi)**3*
     &             fnuc/2.d0/eee)*ppion
      cvrtd     = cvert1(npx)/cdenom/xfact/yfact
c
      do 200 kx = 1,8
      do 200 lx = 0,lmaxgm
      do 200 ich = 1,4
      ctmpd(ich,lx,kx) = (0.d0,0.d0)    
  200 ctmp(ich,lx,kx)  = (0.d0,0.d0)
c
c   construct t-matrix for elastic and charge exchange
c
      do 201 lx = 0,lmaxgm
      do 201 jx = -1,1,2
        lchk      = 2*lx
        jchk      = 2*lx + jx
        do 202 ix = 1,3,2
c
        do 203 ichn = 1,imaxchn
        ixxx      = abs(lchk-llout(ichn))+abs(jchk-jjout(ichn))+
     &              abs(ix  -iiout(ichn))
        if(ixxx.eq.0) then
          cbgt(ix) = ctmat(maxp,npx,ichn)
          if(lchk.eq.2.and.jchk.eq.3.and.ix.eq.3) then
            c33t(ix)= ct33(maxp,npx)- cbgt(ix)
          else
            c33t(ix)= (0.d0,0.d0)
          end if
        end if
  203 continue
  202 continue
      jxx                   = (jx+3)/2
      lxx                   = lx
      cbgtmat(lxx,jxx,1,1) = (cbgt(1)+2.d0*cbgt(3))/3.d0
      cbgtmat(lxx,jxx,2,2) = (2.d0*cbgt(1)+cbgt(3))/3.d0
      cbgtmat(lxx,jxx,1,2) = -s2*(cbgt(1)-cbgt(3))/3.d0
      cbgtmat(lxx,jxx,2,1) = cbgtmat(lxx,jxx,1,2)
      c33tmat(lxx,jxx,1,1) = (c33t(1)+2.d0*c33t(3))/3.d0
      c33tmat(lxx,jxx,2,2) = (2.d0*c33t(1)+c33t(3))/3.d0
      c33tmat(lxx,jxx,1,2) = -s2*(c33t(1)-c33t(3))/3.d0
      c33tmat(lxx,jxx,2,1) = c33tmat(lxx,jxx,1,2)
  201 continue
c
      xfac  = egam/egam0
c
      do 210 nx = 1,meshgm
      zz        = xgu(nx)      
      
      call ampl(camp,campd,ppion,egam,egam0,zz,cvrtd,nx,npx)
c              (camp,campd,ppion,egam,zz,cphot,cvrtd)
c
      
      do 220 ich = 1,4
c
      do 221 lx = 0,lmaxgm
      do 221 jx = 1,2    
c   sl+
      ctmp(ich,lx,5) =ctmp(ich,lx,5) +fep(1,jx,lx,nx)*camp (ich,4+jx)
     &                                *xfac
      ctmpd(ich,lx,5)=ctmpd(ich,lx,5)+fep(1,jx,lx,nx)*campd(ich,4+jx) 
     &                                *xfac
      ctmp(ich,lx,7) =ctmp(ich,lx,7) +fep(1,jx,lx,nx)*camp (ich,6+jx)
      ctmpd(ich,lx,7)=ctmpd(ich,lx,7)+fep(1,jx,lx,nx)*campd(ich,6+jx)
      if(lx.ne.0)then
c   sl-         
      ctmp(ich,lx,6) =ctmp(ich,lx,6) +fep(2,jx,lx,nx)*camp (ich,4+jx) 
     &                                *xfac
      ctmpd(ich,lx,6)=ctmpd(ich,lx,6)+fep(2,jx,lx,nx)*campd(ich,4+jx)
     &                                *xfac
      ctmp(ich,lx,8) =ctmp(ich,lx,8) +fep(2,jx,lx,nx)*camp (ich,6+jx)
      ctmpd(ich,lx,8)=ctmpd(ich,lx,8)+fep(2,jx,lx,nx)*campd(ich,6+jx)
        end if
  221 continue
c
      do 240 kx = 1,4
      ctmp(ich,0,1) = ctmp(ich,0,1) + fap(1,kx,0,nx)*camp(ich,kx)
      ctmp(ich,1,1) = ctmp(ich,1,1) + fap(1,kx,1,nx)*camp(ich,kx)
      ctmp(ich,1,3) = ctmp(ich,1,3) + fap(3,kx,1,nx)*camp(ich,kx)
      ctmp(ich,1,4) = ctmp(ich,1,4) + fap(4,kx,1,nx)*camp(ich,kx)
      ctmpd(ich,0,1)= ctmpd(ich,0,1)+ fap(1,kx,0,nx)*campd(ich,kx)
      ctmpd(ich,1,1)= ctmpd(ich,1,1)+ fap(1,kx,1,nx)*campd(ich,kx)
      ctmpd(ich,1,3)= ctmpd(ich,1,3)+ fap(3,kx,1,nx)*campd(ich,kx)
      ctmpd(ich,1,4)= ctmpd(ich,1,4)+ fap(4,kx,1,nx)*campd(ich,kx)
      if(lmaxgm.ge.2) then
        do 250 lx = 2,lmaxgm
        do 250 ky = 1,4
      ctmpd(ich,lx,ky)= ctmpd(ich,lx,ky)+fap(ky,kx,lx,nx)*campd(ich,kx)
  250 ctmp(ich,lx,ky) = ctmp(ich,lx,ky)+fap(ky,kx,lx,nx)*camp(ich,kx)
      end if
  240 continue


  220 continue
  210 continue
c
c   construct on-shell multipole amplitude
c
c
c   born back ground and delta direct
c
      do 301 lx = 0,lmaxgm
      if(npx.eq.maxp) then
        do 310 ich= 1,4
        do 310 ky = 1,8
          cbmul(nxin,ich,lx,ky) = ctmp(ich,lx,ky)
          cdmul(nxin,ich,lx,ky) = ctmpd(ich,lx,ky)
  310   continue
      end if
 301  continue

      do 300 lx = 0,lmaxpi
c
c    bg(g-pi)*bg(pi-n)  bg(g-pi)*delta(pi-n)
c      
      do 330 kn  = 0,2,2
      do 330 kf  = 1,2
      do 330 ki  = 1,2
      kfn        = kn+kf
      kin        = kn+ki  
c  el+
        cbrmul(nxin,kfn,lx,1) = cbrmul(nxin,kfn,lx,1) +
     &                    cbgtmat(lx,2,kf,ki)*ctmp(kin,lx,1)*cwwxx
        cbdmul(nxin,kfn,lx,1) = cbdmul(nxin,kfn,lx,1) +
     &                    c33tmat(lx,2,kf,ki)*ctmp(kin,lx,1)*cwwxx
c  el-
        cbrmul(nxin,kfn,lx,2) = cbrmul(nxin,kfn,lx,2) +
     &                    cbgtmat(lx,1,kf,ki)*ctmp(kin,lx,2)*cwwxx
        cbdmul(nxin,kfn,lx,2) = cbdmul(nxin,kfn,lx,2) +
     &                    c33tmat(lx,1,kf,ki)*ctmp(kin,lx,2)*cwwxx
c  ml+
        cbrmul(nxin,kfn,lx,3) = cbrmul(nxin,kfn,lx,3) +
     &                    cbgtmat(lx,2,kf,ki)*ctmp(kin,lx,3)*cwwxx
        cbdmul(nxin,kfn,lx,3) = cbdmul(nxin,kfn,lx,3) +
     &                    c33tmat(lx,2,kf,ki)*ctmp(kin,lx,3)*cwwxx
c  ml-
        cbrmul(nxin,kfn,lx,4) = cbrmul(nxin,kfn,lx,4) +
     &                    cbgtmat(lx,1,kf,ki)*ctmp(kin,lx,4)*cwwxx
        cbdmul(nxin,kfn,lx,4) = cbdmul(nxin,kfn,lx,4) +
     &                    c33tmat(lx,1,kf,ki)*ctmp(kin,lx,4)*cwwxx
c  sl+
        cbrmul(nxin,kfn,lx,5) = cbrmul(nxin,kfn,lx,5) +
     &                    cbgtmat(lx,2,kf,ki)*ctmp(kin,lx,5)*cwwxx
        cbdmul(nxin,kfn,lx,5) = cbdmul(nxin,kfn,lx,5) +
     &                    c33tmat(lx,2,kf,ki)*ctmp(kin,lx,5)*cwwxx
        cbrmul(nxin,kfn,lx,7) = cbrmul(nxin,kfn,lx,7) +
     &                    cbgtmat(lx,2,kf,ki)*ctmp(kin,lx,7)*cwwxx
        cbdmul(nxin,kfn,lx,7) = cbdmul(nxin,kfn,lx,7) +
     &                    c33tmat(lx,2,kf,ki)*ctmp(kin,lx,7)*cwwxx
c  sl-
        cbrmul(nxin,kfn,lx,6) = cbrmul(nxin,kfn,lx,6) +
     &                    cbgtmat(lx,1,kf,ki)*ctmp(kin,lx,6)*cwwxx
        cbdmul(nxin,kfn,lx,6) = cbdmul(nxin,kfn,lx,6) +
     &                    c33tmat(lx,1,kf,ki)*ctmp(kin,lx,6)*cwwxx
        cbrmul(nxin,kfn,lx,8) = cbrmul(nxin,kfn,lx,8) +
     &                    cbgtmat(lx,1,kf,ki)*ctmp(kin,lx,8)*cwwxx
        cbdmul(nxin,kfn,lx,8) = cbdmul(nxin,kfn,lx,8) +
     &                    c33tmat(lx,1,kf,ki)*ctmp(kin,lx,8)*cwwxx
c
  330 continue
  300 continue
c
  400 continue
c
      do 500 ich = 1,4
      do 500 lx  = 0,lmaxgm
      do 500 imul= 1,8
        ctmul(nxin,ich,lx,imul) =
     &       cbmul (nxin,ich,lx,imul)+cdmul (nxin,ich,lx,imul)
     &    +  cbrmul(nxin,ich,lx,imul)+cbdmul(nxin,ich,lx,imul)
  500 continue
      return
      end
c------------------------------------------------------------------------
c   input 1 p-pi0, 2 p-pi+, 3 n-pi0, 4 n-pi-
c   out   1 1/2    2 3/2    3 0
c------------------------------------------------------------------------
      subroutine ch2iso(cin,cout)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      dimension cin(4),cout(3)
      s2  = sqrt(2.d0)
       cout(1) = (cin(1)+cin(3)-(cin(2)+cin(4))*s2)/2.d0
       cout(2) = (cin(1)+cin(3)+(cin(2)+cin(4))/s2)/2.d0
       cout(3) = (cin(1)-cin(3))/2.d0
      return
      end
c------------------------------------------------------------------------
c  out camp, campd
c  camp(ich,if)
c  if=1 f1, if=2 f2, if=3 f3, if=4 f4
c  if=5 f4 x + f6
c  if=6 f1 + f3 x + f5
c  if=7 f7
c  if=8 f8
c    cmbrn 4x4x4  4x4 dirac
c                   4 (p,pi0),(p,pi+),(n,p0),(n,pi-)
c-------------------------------------------------------------------------      
      subroutine ampl(camp,campd,qpio,egam,egam0,zz,cvrtd,ntx,nttx)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      parameter(ndim=20,maxpot=8,nedim=50,npdim=20,nbmas=10
     &         ,ladim=10)
      character charge(4)*5
      common / vrtfrm / vfpar(maxpot,nbmas),vgpar(maxpot,nbmas)
     &                 ,ivert(maxpot,nbmas),iverg(maxpot,nbmas)
      common / vrtcpl / gpin,gpind,grnp(nbmas),vkrnn(nbmas),gspn(nbmas)
      common / cmass  / pi,fm,fnuc,fpio,fdel,frho(nbmas),fsigm(nbmas)
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      common / cgampi1 /grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,swgm(8)
     &  ,fdelgm,fmrho,fmomg,xkrho,xkomg,phpi,vnnrho,vnnomg
     &  ,mnnrho,mnnomg,lambda,meshgm,lmaxgm      
      common / ckine1 / wcm,fmass1,fmass2,pp(ndim),ww(ndim),rhoon,maxp
      dimension
     &  pnui(0:3),pnuf(0:3),pgam(0:3),ppio(0:3),cphot(0:3)
     & ,camp(4,8),campd(4,8),fmass(4)
      dimension
     &   cdd(4,4,4),cde(4,4,4),ctdd(4,4),ctde(4,4)
     &  ,cnd(4,4,4),cne(4,4,4),ctnd(4,4),ctne(4,4)
     &  ,cvr(4,4,4),cvo(4,4,4),ctrh(4,4),ctom(4,4)
     &  ,cpip(4,4,4),ccon(4,4,4),ctpi(4,4),ctco(4,4)
     &  ,cound(2,2),coune(2,2),coudd(2,2),coude(2,2)
     &  ,coupi(2,2),couco(2,2),courh(2,2),couom(2,2)
     &  ,ctmp1(2,2),ctmp2(2,2),ctmpo1(4),ctmpo2(4)
c
      fmass(1) = 0
      fmass(2) = fnuc
      fmass(3) = fpio
      fmass(4) = fnuc
      pgam(0)  = egam0
      pgam(1)  = 0
      pgam(2)  = 0
      pgam(3)  = egam
      pnui(0)  = sqrt(fnuc**2 + egam**2)
      pnui(1)  = 0
      pnui(2)  = 0
      pnui(3)  = -egam
      ppio(0)  = sqrt(fpio**2+qpio**2)
      ppio(1)  = qpio*sqrt(1.d0-zz**2)*cos(phpi)
      ppio(2)  = qpio*sqrt(1.d0-zz**2)*sin(phpi)
      ppio(3)  = qpio*zz
      pnuf(1)  = pgam(1)+pnui(1)-ppio(1)
      pnuf(2)  = pgam(2)+pnui(2)-ppio(2)
      pnuf(3)  = pgam(3)+pnui(3)-ppio(3)
      pnuc2    = pnuf(1)**2 + pnuf(2)**2+pnuf(3)**2
      pnuf(0)  = sqrt(fnuc**2 + pnuc2)
      qspio    = qpio**2
      qsnuc    = (pgam(1)-ppio(1))**2+(pgam(2)-ppio(2))**2
     &  +(pgam(3)-ppio(3))**2
      qsnuc    = sqrt(qsnuc)
c
      charge(1) = 'p-pi0'
      charge(2) = 'p-pi+'
      charge(3) = 'n-pi0'
      charge(4) = 'n-pi-'
c
c   constants
c
      fopi   = 4.d0*pi
      alpha  = 1.d0/137.03604d0
      ep     = sqrt(alpha*fopi)
      ff     = gpin
      ef     = ep*ff/fpio
c      fpmom  = 1.79287d0/2.d0/fnuc
c      fnmom  = -1.91315d0/2.d0/fnuc
      fpmom = 1            ! included in form factor
      fnmom = 1            ! included in form factor
c   delta
c            direct
      cefdd  =  ep*cvrtd
c            exchange
      efde   = ep*gpind/fpio
c  rho omega
      fomgx  = fmomg
      frhox  = fmrho
      grho   = grnn*grpg*ep/fpio
      gomg   = gonn*gopg*ep/fpio
      s2     = sqrt(2.d0)
c
c    vertex function
c

      vf1   = vrtfun(vfpar(1,1),fpio,qpio,ivert(1,1))
      
      if (vfpar(5,2).ne.0) then
        vf1c   = vrtfun(vfpar(1,2),fpio,qsnuc,ivert(1,1))
        vf5c   = vrtfun(vfpar(5,2),fpio,qsnuc,ivert(5,1))
      else
        vf1c   = vf1
        vf5c   = vrtfun(vfpar(5,1),fpio,qpio,ivert(5,1))
      endif
      
c!    vf2   = vrtfun(vfpar(1,1),fpio,qsnuc,ivert(1,1))
      vf2   = vrtfun(vfpar(1,1),fpio,qpio,ivert(1,1))
      
c !begin-test
c      vcut  = 674.8/fm
c      ivv   = 2
c      vf1   = vrtfun(vcut,fpio,qpio,ivv)
c      vf2   = vf1
c      vf5   = vf1
c !end-test

      vf3   = vrtfun(vnnrho,fmrho,qsnuc,mnnrho)
      vf4   = vrtfun(vnnomg,fmomg,qsnuc,mnnomg)

c     factor  fcc  = M to T 

      fcc  = 1.d0/(2.d0*pi)**3*sqrt(fnuc**2/pnuf(0)/pnui(0))
     &          /sqrt(2.d0*ppio(0)*2.d0*abs(pgam(0)))
c! for testr
c      fcc  = 1          !!!! test only
c
      lambda = 1
c
c  photon polarization
c
      do 400 ipol = 1,3
      if(ipol.eq.1) then
      cphot(0)  = (0.d0,0.d0)
      cphot(1)  = -dble(lambda)/sqrt(2.d0)
      cphot(2)  = -1.d0/sqrt(2.d0)*(0.d0,1.d0)
      cphot(3)  = (0.d0,0.d0)
      else if(ipol.eq.2) then
      cphot(0)  = (0.d0,0.d0)
      cphot(1)  = (0.d0,0.d0)
      cphot(2)  = (0.d0,0.d0)
      cphot(3)  = (1.d0,0.d0)
      else 
      cphot(0)  = (1.d0,0.d0)
      cphot(1)  = (0.d0,0.d0)
      cphot(2)  = (0.d0,0.d0)
      cphot(3)  = (0.d0,0.d0)
      end if
c
c  nucleon BORN
c
      call nucamp(cnd,cne,cphot,pgam,pnui,ppio,pnuf,fmass,
     &                  fpmom,fnmom)
c
c  pion pole and contact
c
      call pipamp(cpip,ccon,cphot,pgam,pnui,ppio,pnuf,fmass)
c
c   Delta
c      
      call delamp(cdd,cde,cphot,pgam,pnui,ppio,pnuf,fmass,
     &           fdelgm,gdm1,gde2,gdc2)
c
c    vector meson
c
      call vecamp(cvr,cvo,cphot,pgam,pnui,ppio,pnuf,fmass,
     &           frhox,xkrho,fomgx,xkomg)
c
      do 200 ich = 1,4
c
      do 210 kf = 1,4
      do 210 ki = 1,4
      ctnd(kf,ki) =  cnd(kf,ki,ich)*ef*vf1*swgm(1)*fcc
      ctne(kf,ki) =  cne(kf,ki,ich)*ef*vf1c*swgm(2)*fcc
      ctpi(kf,ki) =  cpip(kf,ki,ich)*ef*vf2*swgm(3)*fcc
      ctco(kf,ki) =  ccon(kf,ki,ich)*ef*vf1*swgm(4)*fcc
      ctdd(kf,ki) =  cdd(kf,ki,ich)*cefdd*swgm(7)*fcc
      ctde(kf,ki) =  cde(kf,ki,ich)*efde*vf5c*swgm(8)*fcc
      ctrh(kf,ki) =  cvr(kf,ki,ich)*grho*vf3*swgm(5)*fcc
      ctom(kf,ki) =  cvo(kf,ki,ich)*gomg*vf4*swgm(6)*fcc
  210 continue
c
c   take \bar_{s'}(p') M u(p)_{s}  1 = 1/2, 2 = -1/2
c
      call ubau(ctnd,cound,fnuc,pnuf,fnuc,pnui)
      call ubau(ctne,coune,fnuc,pnuf,fnuc,pnui)
      call ubau(ctpi,coupi,fnuc,pnuf,fnuc,pnui)
      call ubau(ctco,couco,fnuc,pnuf,fnuc,pnui)
      call ubau(ctde,coude,fnuc,pnuf,fnuc,pnui)
      call ubau(ctdd,coudd,fnuc,pnuf,fnuc,pnui)
      call ubau(ctrh,courh,fnuc,pnuf,fnuc,pnui)
      call ubau(ctom,couom,fnuc,pnuf,fnuc,pnui)
c
c      do 300 kf = 1,2
c      do 300 ki = 1,2
      do 300 kf = 2,1,-1
      do 300 ki = 2,1,-1
        ctmp1(kf,ki) =
     &   cound(kf,ki)+coune(kf,ki)+couco(kf,ki)+coupi(kf,ki)
     &  +courh(kf,ki)+couom(kf,ki)+coude(kf,ki)
        ctmp2(kf,ki) = coudd(kf,ki)
c
c        if(abs(lambda).ne.1)write(*,3333)ich,kf,ki,ctmp2(kf,ki)
c 3333   format(1h ,3i3,2e15.3)
c        if(ich.le.2) then
c        if(kf.eq.2.and.ki.eq.2) write(9,3334)ipol
c 3334   format(1h ,' pol = ',i5)
c        write(9,3333)ich,kf,ki,cound(kf,ki),coune(kf,ki),
c     &    coupi(kf,ki),couco(kf,ki),
c     &    coudd(kf,ki),coude(kf,ki),courh(kf,ki),couom(kf,ki)
c 3333   format(1h ,3i3,8e12.4,/,1h ,9x,8e12.4)
c        end if
  300 continue
c
c   transform  T -> T_i i = 1,4 (T = i \epsilon \cdot \sigma T_1 ...)
c
      if(ipol.eq.1) then
      call trans(ctmp1,ctmpo1,phpi,zz,lambda)
      call trans(ctmp2,ctmpo2,phpi,zz,lambda)
      do 310 kk = 1,4
        camp(ich,kk) = ctmpo1(kk)
        campd(ich,kk)= ctmpo2(kk)
  310 continue
      else 
      call transl(camp ,ctmp1,phpi,zz,ipol,ich)
      call transl(campd,ctmp2,phpi,zz,ipol,ich)
      end if      
c
  200 continue
  400 continue
c
c      charge(1) = 'p-pi0'
c      charge(2) = 'p-pi+'
c      charge(3) = 'n-pi0'
c      charge(4) = 'n-pi-'
c 1 + 2 0  3 -     
c      if(ntx.eq.3.and.nttx.eq.maxp) then
c         write(*,1998)zz,pnuf(0),pnui(0),ppio(0),pgam(0)
c 1998    format(1h ,' z Ef Ei Ep Eg ',5e15.5)
c      do 1997 iii = 1,8
c      x11  = (camp(1,iii) + camp(3,iii))/2.d0
c      x12  = (camp(1,iii) - camp(3,iii))/2.d0
c      x13  =-(camp(2,iii) + camp(4,iii))/2.d0/sqrt(2.d0)
c      write(*,1999)iii,x11,x12,x13
c 1999 format(1h ,i5,3e15.5)
c 1997 continue
c      end if
      return
      end
c
c  ipol= 2 (0,0,0,1)
c  ipol= 3 (1,0,0,0)
c
      subroutine transl(cout,cin,phi,zz,ipol,ich)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      dimension cin(2,2),cout(4,8)
      cei   = (0.d0,1.d0)
      phil  = phi
      fcos  = zz
      xsin  = sqrt(1.d0-zz**2)
      gcos  = cos(phil)
      gsin  = sin(phil)
      cqlx   = (gcos - cei*gsin)*xsin
      if(ipol.eq.2) then
        cout(ich,5) = cin(1,2)/cei/cqlx
        cout(ich,6) = cin(1,1)/cei - fcos* cin(1,2)/cei/cqlx
      else if(ipol.eq.3) then
        cout(ich,7) =  -cin(1,2)/cei/cqlx
        cout(ich,8) =  -(cin(1,1)/cei - fcos*cin(1,2)/cei/cqlx)
      end if
      return
      end
c -------------------------------------------------------------------
c     vector meson
c--------------------------------------------------------------------
      subroutine vecamp(coutr,couto,cphot,pgam,pnui,ppio,pnuf,fmass,
     &                frho,xkrho,fomg,xkomg)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cgmfrm / fg1p,fg1n,fg2p,fg2n,fg1v,fg1s,fg2v,fg2s
     &                 ,fgpr,fgpo,fgdm,fgde,fgdc
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      common / ceps / imax,ie1(24),ie2(24),ie3(24),ie4(24),iph(24)
      dimension coutr(4,4,4),couto(4,4,4)
     &  ,pnui(0:3),pnuf(0:3),pgam(0:3),ppio(0:3),cphot(0:3)
     &  ,fmass(4)
      dimension
     &  qvec(0:3),cqq(4,4),cvec(0:3),cvecs(4,4),cg1(4,4),cg2(4,4)
     & ,ctmpr(4,4),ctmpo(4,4)
c
      s2   = sqrt(2.d0)
      fnuc = fmass(2)
c
      do 130 k1 = 0,3
        qvec(k1)= pnui(k1) - pnuf(k1)
  130 continue
      qqs       = sprod(qvec,qvec)
      ddrho     = qqs - frho**2
      ddomg     = qqs - fomg**2
c
c   e_(abcd)q^a e^b Q^c g^{d d'}
c
      do 100 k1 = 0,3
        cvec(k1)= (0.d0,0.d0)
  100 continue
      do 110 ix = 1,imax
        k1      = ie1(ix)
        k2      = ie2(ix)
        k3      = ie3(ix)
        k4      = ie4(ix)
        xeps    = iph(ix)
        cvec(k4)= cvec(k4) - xeps*pgam(k1)*cphot(k2)*qvec(k3)
  110 continue
      do 120 k1 = 0,3
      cvec(k1)  = cvec(k1)*gg(k1)
  120 continue
c
      call pslash(qvec,cqq)
      call cpslash(cvec,cvecs)
      call mul2(cg1,cvecs,cqq)
      call mul2(cg2,cqq,cvecs)
c
      do 200 k1 = 1,4
      do 200 k2 = 1,4
        cmom    =  cg1(k1,k2) - cg2(k1,k2) 
        ctmpr(k1,k2) = cvecs(k1,k2) + cmom*xkrho/(4.d0*fnuc)
        ctmpo(k1,k2) = cvecs(k1,k2) + cmom*xkomg/(4.d0*fnuc)
  200 continue
c
c   multiply isospin factor and propagator
c
c   rho \tau^i/2   omega \delta_{i,3}
c
c     (p,pi0),(p,pi+),(n,p0),(n,pi-)
c
      do 220 k1 = 1,4
      do 220 k2 = 1,4
        ccrho   = ctmpr(k1,k2)/ddrho*fgpr
        ccomg   = ctmpo(k1,k2)/ddomg*fgpo
        coutr(k1,k2,1) = ccrho/2.d0
        coutr(k1,k2,2) =-ccrho/s2
        coutr(k1,k2,3) =-ccrho/2.d0
        coutr(k1,k2,4) = ccrho/s2
        couto(k1,k2,1) = ccomg
        couto(k1,k2,2) = (0.d0,0.d0)
        couto(k1,k2,3) = ccomg
        couto(k1,k2,4) = (0.d0,0.d0)
  220 continue
      return
      end
c---------------------------------------------------
c  pion pole and contact term
c---------------------------------------------------
      subroutine pipamp(coutp,coutc,cphot,pgam,pnui,ppio,pnuf,fmass)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cgmfrm / fg1p,fg1n,fg2p,fg2n,fg1v,fg1s,fg2v,fg2s
     &                 ,fgpr,fgpo,fgdm,fgde,fgdc
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      dimension coutp(4,4,4),coutc(4,4,4)
     &  ,pnui(0:3),pnuf(0:3),pgam(0:3),ppio(0:3),cphot(0:3)
     &  ,fmass(4)
      dimension
     &   ceps(4,4),c5pip(4,4),c5eps(4,4),cpip(4,4),ppip(0:3)
c
      cei2 = (0.d0,1.d0)*sqrt(2.d0)
      fpio = fmass(3)
c
      cpipf    = (0.d0,0.d0)
      t        = 0
      do 100 k = 0,3
        ppip(k) = pnui(k) - pnuf(k)
        cpipf   = cpipf + (ppio(k) + ppip(k))*cphot(k)*gg(k)
        t       = t + ppip(k)**2*gg(k)
  100 continue
      cpipf      = cpipf/(t - fpio**2)
c
      call pslash(ppip,cpip)
      call mul2(c5pip,cg5,cpip)
c
      call cpslash(cphot,ceps)
      call mul2(c5eps,cg5,ceps)
c
c     (p,pi0),(p,pi+),(n,p0),(n,pi-)
c
      do 120 k1 = 1,4
      do 120 k2 = 1,4
        coutp(k1,k2,1) = (0.d0,0.d0)
        coutc(k1,k2,1) = (0.d0,0.d0)
        coutp(k1,k2,2) = -cei2*c5pip(k1,k2)*cpipf*fg1v
        coutc(k1,k2,2) =  cei2*c5eps(k1,k2)*fg1v
        coutp(k1,k2,3) = (0.d0,0.d0)
        coutc(k1,k2,3) = (0.d0,0.d0)
        coutp(k1,k2,4) =  coutp(k1,k2,2)
        coutc(k1,k2,4) =  coutc(k1,k2,2)
  120 continue
      return
      end
c-------------------------------------------------------
c   Delta internmediate state
c-------------------------------------------------------
      subroutine delamp(coutd,coute,cphot,pgam,pnui,ppio,pnuf,fmass,
     &                  fdel,gmd,ged,gcd)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cgmfrm / fg1p,fg1n,fg2p,fg2n,fg1v,fg1s,fg2v,fg2s
     &                 ,fgpr,fgpo,fgdm,fgde,fgdc
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      common / ceps / imax,ie1(24),ie2(24),ie3(24),ie4(24),iph(24)
      dimension coutd(4,4,4),coute(4,4,4)
     &  ,pnui(0:3),pnuf(0:3),pgam(0:3),ppio(0:3),cphot(0:3)
     &  ,fmass(4)
      dimension
     &  pdele(0:3),pppe(0:3),ceme(0:3),cdele(0:3,0:3,4,4)
     & ,pdeld(0:3),pppd(0:3),cemd(0:3),cdeld(0:3,0:3,4,4)
     & ,cexd(0:3),cexe(0:3),ceee(0:3),ceed(0:3)
c
      fnuc     = fmass(2)
      cei      = (0.d0,1.d0)
c
c     direct delta (without energy denminator)
c
      do 100 k = 0,3
        pdeld(k)  = pnuf(k) + ppio(k)
c        pdeld(k)  = pnui(k) + pgam(k)
        pppd(k)   = (pnui(k) + pdeld(k))/2.d0
  100 continue
c      
      call prond(pdeld,fdel,cdeld)
c
c     cross delta
c      
      do 200 k = 0,3
        pdele(k) = pnui(k) - ppio(k)
c        pdele(k) = pnuf(k) - pgam(k)
        pppe(k)  = (pnuf(k) + pdele(k))/2.d0
c! tsato static delta
c        if(k.gt.0) then
c          pdele(k) = 0
c          pppe(k)  = 0
c        end if
c! tsato        
  200 continue
c
      call propd(pdele,fdel,cdele)
c
c   Magnetic only Gamma vertex.  Note Gamma_{\mu}
c
c   e^{ijkl} = - e_{ijkl}
c
      qqq      = sprod(pgam,pgam)
      qppd     = sprod(pgam,pppd)
      qppe     = sprod(pgam,pppe)
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
c!tsato large component
      do 320 k1 = 1,4
      do 320 k2 = 1,4
      ctmpd = (0.d0,0.d0)
      ctmpe = (0.d0,0.d0)
c
c!tsato  space componet of propagator
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
c   isospin factor
c
      coutd(k1,k2,1) = 2.d0/3.d0*ctmpd
      coutd(k1,k2,2) = sqrt(2.d0)/3.d0*ctmpd
      coutd(k1,k2,3) =  coutd(k1,k2,1)
      coutd(k1,k2,4) =  coutd(k1,k2,2)
      coute(k1,k2,1) = 2.d0/3.d0*ctmpe
      coute(k1,k2,2) = -sqrt(2.d0)/3.d0*ctmpe
      coute(k1,k2,3) =  coute(k1,k2,1)
      coute(k1,k2,4) =  coute(k1,k2,2)
  320 continue
c
      return
      end
c------------------------------------------------------
c
c------------------------------------------------------
      subroutine nucamp(coutd,coute,cphot,pgam,pnui,ppio,pnuf,fmass,
     &                  fpmom,fnmom)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      common / cgmfrm / fg1p,fg1n,fg2p,fg2n,fg1v,fg1s,fg2v,fg2s
     &                 ,fgpr,fgpo,fgdm,fgde,fgdc
      dimension coutd(4,4,4),coute(4,4,4)
     &  ,pnui(0:3),pnuf(0:3),pgam(0:3),ppio(0:3),cphot(0:3)
     &  ,fmass(4)
      dimension
     &   cpi(4,4),cgam(4,4),cpro(4,4),ceps(4,4),ctmp1(4,4),ctmp2(4,4)
     &  ,ctmp3(4,4),ctmp11(4,4),ctmp33(4,4)
     &  ,ptmp(0:3),ctmp2x(4,4),ctmp2y(4,4)
c
      s2   = sqrt(2.d0)
      cei  = (0.d0,1.d0)
      fnuc = fmass(2)
      call pslash(ppio,cpi)
      call pslash(pgam,cgam)
      call cpslash(cphot,ceps)
       call mul2(ctmp2x,cgam,ceps)
c  modify 99
      call mul2(ctmp2y,ceps,cgam)
      do 9999 i1 = 1,4
      do 9999 i2 = 1,4
 9999 ctmp2(i1,i2) = (ctmp2x(i1,i2) - ctmp2y(i1,i2))/2.d0
c
c    nucleon direct
c      
c i[e g^5 sla(k) S(p'+k) sla(e) + mu  g^5 sla(k) S(p'+k) sla(k) sla(e)]
c
      do 100 k = 0,3
        ptmp(k) = pnuf(k) + ppio(k)
  100 continue
      call propn(ptmp,fnuc,cpro)
      call mul4(ctmp1,cg5,cpi,cpro,ceps)
      call mul4(ctmp3,cg5,cpi,cpro,ctmp2)
c
c   nucleon exchange
c
c i[e sla(e)  S(p'+k) g^5 sla(k) + mu sla(k) sla(e) S(p'+k) g^5 sla(k)]
c
      do 110 k = 0,3
        ptmp(k) = pnui(k) - ppio(k)
  110 continue
      call propn(ptmp,fnuc,cpro)
      call mul4(ctmp11,ceps,cpro,cg5,cpi)
      call mul4(ctmp33,ctmp2,cpro,cg5,cpi)
c
c     isospin factor
c
c     (p,pi0),(p,pi+),(n,p0),(n,pi-)
c
c      if(ichxx.ne.1234)then
c      write(*,*)fg1p,fg1n
c      write(*,*)fg2p/2.d0/fnuc,fg2n/2.d0/fnuc
c      write(*,*)fg2p,fg2n
c      ichxx = 1234
c      end if
c
      do 120 k1 = 1,4
      do 120 k2 = 1,4
        cdpro          =
     & (fg1p*ctmp1 (k1,k2) + fg2p*ctmp3 (k1,k2)/2.d0/fnuc)*cei
        cepro          =
     & (fg1p*ctmp11(k1,k2) + fg2p*ctmp33(k1,k2)/2.d0/fnuc)*cei
        cdneu          =
     & (fg1n*ctmp1 (k1,k2) + fg2n*ctmp3 (k1,k2)/2.d0/fnuc)*cei
        ceneu          =
     & (fg1n*ctmp11(k1,k2) + fg2n*ctmp33(k1,k2)/2.d0/fnuc)*cei
c
c        cdpro          = (ctmp1 (k1,k2) + fpmom*ctmp3 (k1,k2))*cei
c        cepro          = (ctmp11(k1,k2) + fpmom*ctmp33(k1,k2))*cei
c        cdneu          =  fnmom*ctmp3 (k1,k2)*cei
c        ceneu          =  fnmom*ctmp33(k1,k2)*cei
        coutd(k1,k2,1) = cdpro
        coute(k1,k2,1) = cepro
        coutd(k1,k2,3) = cdneu*(-1.d0)
        coute(k1,k2,3) = ceneu*(-1.d0)
        coutd(k1,k2,2) = cdpro*(-s2)
        coute(k1,k2,2) = ceneu*(-s2)
        coutd(k1,k2,4) = cdneu*s2
        coute(k1,k2,4) = cepro*s2
  120 continue
      return
      end
c-------------------------------------------------------
c     set of i1,i2,i3,i4   ieps
c-------------------------------------------------------
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
c----------------------------------------------------------      
c    ieps = epsilon^{i1,i2,i3,i4}
c    epsilon^{0123} = 1
c----------------------------------------------------------
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
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine ubau(cin,cout,fmassf,ppf,fmassi,ppi)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      dimension cin(4,4),cout(2,2),cci(2,2),ccf(2,2),ppf(0:3),ppi(0:3)
c
      faci    = fmassi + ppi(0)
      facf    = fmassf + ppf(0)
      fac     = sqrt(faci*facf/4.d0/fmassf/fmassi)
c
      do 100 kf = 1,2
      do 100 ki = 1,2
        cci(kf,ki) = (0.d0,0.d0)
        ccf(kf,ki) = (0.d0,0.d0)
        ctmp1=cci(kf,ki)
        ctmp2=ccf(kf,ki)
        do 110 k = 1,3
          ctmp1 = ctmp1 + cpauli(kf,ki,k)*ppi(k)/faci
          ctmp2 = ctmp2 + cpauli(kf,ki,k)*ppf(k)/facf
  110   continue
        cci(kf,ki) = ctmp1
        ccf(kf,ki) = ctmp2
  100 continue
c
      do 200 kf = 1,2
      do 200 ki = 1,2
      csum      = cin(kf,ki)
      do 210 kn = 1,2
      csum      = csum + cin(kf,kn+2)*cci(kn,ki)
     &                 - ccf(kf,kn)  *cin(kn+2,ki)
      do 220 km = 1,2
  220 csum      = csum - ccf(kf,kn)*cin(kn+2,km+2)*cci(km,ki)
  210 continue
      cout(kf,ki) = csum*fac
  200 continue
c
      return
      end
c-------------------------------------------------------------
c
c-------------------------------------------------------------
      subroutine mul4(cout,c1,c2,c3,c4)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      dimension cout(4,4),c1(4,4),c2(4,4),c3(4,4),c4(4,4),
     &  ct12(4,4),ct34(4,4)
      do 100 k1 = 1,4
      do 100 k2 = 1,4
        ct12(k1,k2) = (0.d0,0.d0)
        ct34(k1,k2) = (0.d0,0.d0)
        do 200 k3 = 1,4
          ct12(k1,k2) = ct12(k1,k2) + c1(k1,k3)*c2(k3,k2)
          ct34(k1,k2) = ct34(k1,k2) + c3(k1,k3)*c4(k3,k2)
  200   continue
  100 continue
      do 300 k1 = 1,4
      do 300 k2 = 1,4
        cout(k1,k2) = (0.d0,0.d0)
        do 400 k3 = 1,4
          cout(k1,k2) = cout(k1,k2) + ct12(k1,k3)*ct34(k3,k2)
  400   continue
  300 continue
      return
      end
c-------------------------------------------------------------
c
c-------------------------------------------------------------
      subroutine mul2(cout,c1,c2)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      dimension cout(4,4),c1(4,4),c2(4,4)
      do 300 k1 = 1,4
      do 300 k2 = 1,4
        cout(k1,k2) = (0.d0,0.d0)
        ctmp = cout(k1,k2)
        do 400 k3 = 1,4
          ctmp = ctmp + c1(k1,k3)*c2(k3,k2)
  400   continue
        cout(k1,k2) = ctmp
  300 continue
      return
      end
c-------------------------------------------------------------      
c    set metrix and dirac matrix
c-------------------------------------------------------------
      subroutine setdirac
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &,gg(0:3)
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
      return
      end
c---------------------------------------------------------------------
c     p*q
c---------------------------------------------------------------------
      real*8 function sprod(p,q)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      dimension p(0:3),q(0:3)
      sprod    = 0
      do 100 k = 0,3
        sprod  = sprod + p(k)*q(k)*gg(k)
  100 continue
      return
      end
c---------------------------------------------------------------------
c      p_mu gamm^mu -> cout
c---------------------------------------------------------------------
      subroutine pslash(p,cout)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &,gg(0:3)
      dimension cout(4,4),p(0:3)
c
      do 100 kf = 1,4
      do 100 ki = 1,4
        cout(kf,ki) = (0.d0,0.d0)
        do 110 kmu = 0,3
        cout(kf,ki) = cout(kf,ki) + cgv(kf,ki,kmu)*gg(kmu)*p(kmu)
  110 continue
  100 continue
      return
      end
c---------------------------------------------------------------------
c      gamma_5 p_mu gamm^mu -> cout
c---------------------------------------------------------------------
      subroutine pslash5(p,cout)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &,gg(0:3)
      dimension cout(4,4),p(0:3)
c
      do 100 kf = 1,4
      do 100 ki = 1,4
        cout(kf,ki) = (0.d0,0.d0)
        do 120 km  = 1,4
        do 110 kmu = 0,3
          cout(kf,ki) = cout(kf,ki) +
     &        cg5(kf,km)*cgv(km,ki,kmu)*gg(kmu)*p(kmu)
  110 continue
  120 continue
  100 continue
      return
      end
c---------------------------------------------------------------------
c      p_mu gamm^mu -> cout  for complex momentum
c---------------------------------------------------------------------
      subroutine cpslash(cp,cout)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &,gg(0:3)
      dimension cout(4,4),cp(0:3)
c
      do 100 kf = 1,4
      do 100 ki = 1,4
        cout(kf,ki) = (0.d0,0.d0)
        do 110 kmu = 0,3
        cout(kf,ki) = cout(kf,ki) + cgv(kf,ki,kmu)*gg(kmu)*cp(kmu)
  110 continue
  100 continue
      return
      end
c---------------------------------------------------------------------
c     propagator of spin 1/2  = 1/(p*g -m)
c---------------------------------------------------------------------
      subroutine propn(p,fmas,cout)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      dimension p(0:3),cout(4,4),cgam(4,4)
c
      call pslash(p,cgam)
c
      ss     = sprod(p,p)
      fact   = ss - fmas**2
c
      do 100 kf = 1,4
      do 110 ki = 1,4
        cout(kf,ki) = cgam(kf,ki)/fact
  110 continue
        cout(kf,kf) = cout(kf,kf) + fmas/fact
  100 continue
      return
      end
c---------------------------------------------------------------------
c    p = (p^0 , p)
c     propagator of spin 3/2  = 1/(p*g -m)
c   ( -g^{mn}+gam^mgam^u/3 + (p^m gam^n - p^n gam^m)/3M
c         + 2p^m p^n /(3M^2) )
c---------------------------------------------------------------------
      subroutine propd(p,fmas,cout)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      dimension p(0:3),cout(0:3,0:3,4,4),cgam(4,4)
     &         ,ctmp1(4,4),ctmp2(4,4),ctmp3(4,4),ctmp4(4,4)
c
      call pslash(p,cgam)
      ss     = sprod(p,p)
      fact   = ss - fmas**2
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
      call mul2(ctmp3,ctmp1,ctmp2)
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
      call mul2(ctmp2,ctmp1,ctmp4)
      do 220 k1 = 1,4
      do 220 k2 = 1,4
      cout(m1,m2,k1,k2) = ctmp2(k1,k2)
  220 continue
  200 continue
      return
      end
c---------------------------------------------------------------------
c    p = (sqrt(p^2+M^2) , p)
c
c     M/E sum_mu U^(mu)(p)U^(mu)(p)
c
c   (ssla(p) + M) /(2E)
c   ( -g^{mn}+gam^mgam^u/3 + (p^m gam^n - p^n gam^m)/3M
c         + 2p^m p^n /(3M^2) )
c----------------------------------------------------------------------
      subroutine prond(pin,fmas,cout)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      common / cdirac / ce(4,4),cg5(4,4),cgv(4,4,0:3),cpauli(2,2,3)
     &  ,gg(0:3)
      dimension pin(0:3),cout(0:3,0:3,4,4),cgam(4,4)
     &  ,ctmp1(4,4),ctmp2(4,4),ctmp3(4,4),ctmp4(4,4)
     &  ,p(0:3)
c
      pdel  = pin(1)**2 + pin(2)**2 + pin(3)**2
      p(0)  = sqrt(fmas**2 + pdel)
      p(1)  = pin(1)
      p(2)  = pin(2)
      p(3)  = pin(3)
      fnrm  = 1.d0/2.d0/p(0)
c
      call pslash(p,cgam)
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
      call mul2(ctmp3,ctmp1,ctmp2)
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
      call mul2(ctmp2,ctmp1,ctmp4)
      do 220 k1 = 1,4
      do 220 k2 = 1,4
      cout(m1,m2,k1,k2) = ctmp2(k1,k2)*fnrm
  220 continue
  200 continue
      return
      end
c------------------------------------------------------------------------
c    T(2,2) -> f(1),.. f(4)
c------------------------------------------------------------------------
      subroutine trans(cin,cout,phi,zz,lambda)
      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      dimension cin(2,2),cout(4)
      s2    = sqrt(2.d0)
      cei   = (0.d0,1.d0)
      phil  = dble(lambda)*phi
      fcos  = zz
      fsin  = sqrt(1.d0-zz**2)/s2
      fsin2 = fsin**2
      gcos  = cos(phil)
      gsin  = sin(phil)
      cql   = (gcos + cei*gsin)*fsin
      cql2  = cql*cql
      caa   = (cin(1,1)+cin(2,2))/2.d0
      cbb0  = -cei*(cin(1,1)-cin(2,2))/2.d0
      cbbp  =  cei*cin(2,1)/s2
      cbbm  = -cei*cin(1,2)/s2
      if(lambda.eq.1) then
        cout(4) = cbbp/cql2
        cout(2) = -cei*caa/cql
        cout(3) = -dble(lambda)*cbb0/cql-cout(4)*fcos-cout(2)
        cout(1) = -cbbm+fcos*cout(2)-fsin2*cout(4)
      else
        cout(4) = cbbm/cql2
        cout(2) = -cei*caa/cql
        cout(3) = -dble(lambda)*cbb0/cql-cout(4)*fcos-cout(2)
        cout(1) = -cbbp+fcos*cout(2)-fsin2*cout(4)
      end if
      return
      end
c--------------------------------------------------------------------------
c     Nucleon form factor from PRC51 409(95)
c--------------------------------------------------------------------------
      subroutine gamfrm(egam0,egam)
      implicit real*8(a-h,o-z)
      parameter(ndim=20,maxpot=8,nedim=50,npdim=20,nbmas=10,
     &          ladim=10)
      common / cmass  / pi,fm,fnuc,fpio,fdel,frho(nbmas),fsigm(nbmas)
      common / cgmfrm / fg1p,fg1n,fg2p,fg2n,fg1v,fg1s,fg2v,fg2s
     &                 ,fgpr,fgpo,fgdm,fgde,fgdc
      common / c2002 / igfrm,idfrm, betd, gamd
      
      qs     = egam0**2 - egam**2

c     Nucleon form factor

      tau    = -qs/(4.d0*fnuc**2)
      cut    = 0.71d0/fm**2*1.d6
      gdd    = 1.d0/(1.d0 - qs/cut)**2
      fpmom  = 2.79285d0
      fnmom  =  -1.91315d0

      qsgev  = sqrt(abs(qs)/(1000.d0/fm)**2)
      
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

      fg1n = (gen + tau*gmn) / (1.d0+tau)
      fg2n = (gmn -     gen) / (1.d0+tau)
      fg1p = (gep + tau*gmp) / (1.d0+tau)
      fg2p = (gmp -     gep) / (1.d0+tau)
      fg1v = fg1p - fg1n
      fg1s = fg1p + fg1n
      fg2v = fg2p - fg2n
      fg2s = fg2p + fg2n

c     Delta form factor

      taud   = -qs/(938.93d0+1238.d0)**2*fm**2
      tauv   = -qs*4.d0/(769.d0+782.6d0)**2*fm**2
      gdelt1 = sqrt(1.d0+taud)/(1.d0+tau)*gep
      delt   = 1200.d0/fm
      gdelt2 = sqrt(1.d0+taud)/(1.d0 - qs/4.d0/delt**2)*gep
      
c  our parametrization Mar-24

      betyn  = betd/1.d6*fm**2
      gamyn  = gamd/1.d6*fm**2

c  delta bare electromagnetic form factor

      if(idfrm.eq.1) then
        gdelt3 = gdd
      else if(idfrm.eq.2) then
        gdelt3 = gdd*(1.d0 + betyn*(-qs))*exp(-gamyn*(-qs))
      else if(idfrm.eq.3) then
        gdelt3 = 1.d0
      endif
      
      fgdm = gdelt3
      fgde = gdelt3
      fgdc = gdelt3
      
      fgpr = 1.d0/(1.d0+tauv)
      fgpo = 1.d0/(1.d0+tauv)

      return
      end
