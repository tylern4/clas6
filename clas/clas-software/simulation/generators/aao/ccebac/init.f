      subroutine init(par,npar,isw0)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)
      parameter (maxlsj2=50)

      character cdum*5,cval*10
      character numbers*10,wave*9
      character wpw*3,allwaves*3

c ts 11-3-2007
      common/chdat3/njLs2,jpind2(maxlsj2) ,Lpind2(maxlsj2)
     &                  ,ispind2(maxlsj2),itpind2(maxlsj2),lmx2
     &                  ,ljt1(0:maxl*2,2*maxl+1,3)
      common / cmxj / jjmx                        ! used in elemag
      common / cdfi  / meshx,mxx,mxj,mxm          

      common / input1 / mxq,q2(maxq2)
      common / input2 / np,icres,icpot
      common / input3 / itmx,theta(0:maxtheta),wtheta(0:maxtheta)
c ts 10-26-2007
      common /cntrlchn / iiichn(maxmb)

c
c bjulia 9-10-2007
      dimension equi(maxtheta+1),omeg(maxtheta+1)
c bjulia 9-10-2007

      common/chdat1/njLs,jpind(maxlsj) ,Lpind(maxlsj)
     &                  ,ispind(maxlsj),itpind(maxlsj)
      common/chdat2/nch,ich0(maxmb)
      common/pot1/nchh,Ldt(5,maxmb,maxlsj),isdt(5,maxmb,maxlsj)
     &                                    ,nLsdt(maxmb,maxlsj)

      common/nstardt/nstar0(maxlsj),amstar(maxres,maxlsj)
     &    ,coup(5,maxmb,maxres,maxlsj),cut(5,maxmb,maxres,maxlsj)

      common/imat/itmat
      common/quark/iquark
      common/waves/allwaves(maxlsj)

       common/mesh00/nc,ne0,np1,ich(maxmb),
     1   ze0(maxwcm),am1(maxmb),am2(maxmb),istab(maxmb)
     1  ,zp(maxmom,maxmb,maxwcm),zwp(maxmom,maxmb,maxwcm)
     1  ,zp0(maxmb,maxwcm),zgreen(maxmom,maxmb,maxwcm)

       common/masses/am1dt(maxmb),am2dt(maxmb)

       dimension is1dt(maxmb),is2dt(maxmb),is1d(maxmb),is2d(maxmb)
       dimension it1dt(maxmb),it2dt(maxmb),it1d(maxmb),it2d(maxmb)
       dimension istabdt(maxmb)
c bjulia 23 10 2007
      common/eyeball/icontrolnres,icontrolclo,icontrolbar,icontl(0:maxl)

      common/threseta/ifwe

      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm

c bjulia 28-07-2007
c experimental data from SAID
       character*4 ce
       character*4 cnd
       character*1 longc
       character*9 non
       character*11 expname,saidpwf(2)

      common/saidpw/ said(10,400), nsaid1, nsaid, saidpwf
      
      common/alldata/ numexpdat(0:2,3,3000,0:maxtheta)
     1                 ,expdata(0:2,3,3000,0:maxtheta,50)
     1                  ,experr(0:2,3,3000,0:maxtheta,50)
     1                  ,expang(0:2,3,3000,0:maxtheta,50)
     1                 ,   expw(0:2,3,3000,0:maxtheta,50)
     1                 ,  obser(0:2,3,3000,0:maxtheta)

       dimension nset(0:2,3)
c bjulia 28-07-2007

c bjulia 9-10-2007

      common/buildchi/iob(3),irea(0:2)

c bjulia 9-10-2007


      data is1dt/0,0,3,0,2/,is2dt/1,1,0,1,1/
      data it1dt/2,0,3,0,2/,it2dt/1,1,2,1,1/
      data istabdt/0,0,1,1,1/

      dimension opt(4),par(maxpar)

      itmx=24
      numbers="0123456789"
      WAVE   ="SPDFGHIJK"

c ts 10-26-2007
      do ix = 1,maxmb
      iiichn(ix) = 1
      end do

c------------------------------------------------------------
c    define masses for channels
c
c      1 :pi-N      2 :eta-N      3 :pi-Delta
c      4 :sigma-N   5 :rho-N
c
       amn   = 938.5
       api   = 138.5
       aeta  = 547.45
       amdel = 1300.
       asigma= 896.8
       arho  = 811.7

       if(isw0.eq.1) then
c matsuyamas
       amn   = 940.0
       api   = 140.0
       else if(isw0.eq.2) then
c SL
       amn   = 938.5
       api   = 138.5
       end if

       aeta  = 550.00
       amdel = 1299.
       asigma= 897.0
       arho  = 812.0
c
c      Lee's isobar model for Delta
c     
       am1dt(1) = api
       am2dt(1) = amn
       am1dt(2) = aeta
       am2dt(2) = amn
       am1dt(3) = amdel
       am2dt(3) = api
       am1dt(4) = asigma
       am2dt(4) = amn
       am1dt(5) = arho 
       am2dt(5) = amn

c lcsmith 14-10-2007

      if (saidpwf(1).ne.'NONE') then
      nsaid=0
      print *, 'Opening ',saidpwf(1)
      open(98,file='said/'//saidpwf(1))
      do i=1,2000
        read(98,*,end=403) (said(k,i),k=1,10)
        nsaid=nsaid+1
      enddo
403   close(98)
      endif
      
      nsaid1 = nsaid
      
      if (saidpwf(2).ne.'NONE') then
      print *, 'Opening ',saidpwf(2)
      open(98,file='said/'//saidpwf(2))
      do i=nsaid+1,2000
        read(98,*,end=404) (said(k,i),k=1,10)
        nsaid=nsaid+1
      enddo
404   close(98)
      endif
      
      print *, 'Npoints 1,2 =',nsaid1,nsaid
      
c cc.inp2      

      read(16,*)lmx2  ! max L not 2L -- ts 11-1-2007
      jjmx = 2*lmx2 + 1
      read(16,*)mxe,mxq
      read(16,*)eminr,emini,emaxr,emaxi,qmin,qmax
      
      if (mxe.lt.0) then
        mxe=abs(mxe) 
        do ie=1,mxe
          read(16,*) wr,wi
          ze0(ie) = wr + (0.d0,1.d0)*wi
          if (wr.gt.aeta+amn.and.ifirstwe.ne.7) then 
            ifwe=ie
            ifirstwe=7
            write(*,*) 'Eta Threshold:',ifwe," W=",wr
          endif
        enddo
      else
        zmin = eminr + (0.d0,1.d0)*emini
        zmax = emaxr + (0.d0,1.d0)*emaxi
        do ie     = 1,mxe
          if(mxe.eq.1) then
           ze0(ie)  = zmin
          else
           ze0(ie)  = zmin + (zmax-zmin)*dble(ie-1)/dble(mxe-1)
          end if
        end do
      endif
      
      ne0      = mxe

      if(mxq.lt.0) then
        mxq=abs(mxq)
        read(16,*) (q2(iq),iq=1,mxq)
      else
        if (mxq.ne.1) then
          do iq=1,mxq
            q2(iq) = qmin + (qmax-qmin)*dble(iq-1)/dble(mxq-1)
          end do
        else
          q2(1)   = qmin
        endif
      endif
       
c angles bjulia 30-07-2007

      do it=0,itmx
        theta(it) = cos(dble(it)/real(itmx)*3.14159265)
      enddo

c       call gauleg(-1.,1.,equi,omeg,itmx+1)

      write(*,*) 'ANGLES'
      
      do it=0,itmx
       write(*,*) theta(it),wtheta(it)      
      enddo

c bjulia 17 10 2007

      write(668,1113) 'ENER',mxe
      do iprinte=1,mxe
        write(668,1116) ze0(iprinte) 
      enddo
      
      write(668,1113) 'Q2  ',mxq
      do iprinte=1,mxq
        write(668,1116) q2(iprinte)
      enddo
      
      write(668,1113) 'ANGL',itmx
      do iprinte=0,itmx
        write(668,1116) theta(iprinte)
      enddo
      
 1113  format(A4,2x,I4)
 1116  format(4(E14.6,2x))

c bjulia

c----------------finds relevant data---------------------
c bjulia 28-07-2007

c  ichpi = 0   g p -> p pi0
c          1       -> n pi+
c          2       -> e p  

c included in chi2  bjulia 9 Oct 2007

c iobs = 1 diff cross section
c        2 Sigma polarization
c        3 total cross section

      do i=1,3
        iob(i)=1
      enddo
      do j=0,1
        irea(j)=1
      enddo
      
      irea(0)=1
      irea(1)=1
      irea(2)=1
      iob(1) =1
      iob(2) =1
      iob(3) =1

c sets all to zero

      numexpdat=0.
       
      do ireac=0,2

      dw = 2.5
      da = 1.5
      if (wcm.lt.1300.) dw=1.5
      if (wcm.gt.1700.) dw=4.5
        
c cross section data

      iobs=1

      if (ireac.eq.0) open(98,file="all.pi0p")
      if (ireac.eq.1) open(98,file="all.pipn")
      if (ireac.eq.2) open(98,file="all.etan")

      nset(ireac,iobs)=1
      ntotdata=0
        
 301  read(98,*) 
      READ(98,399,IOSTAT=JEOF) ce,xe,cnd,ieend,non,expname
 399  format(A4,F9.3,A3,I6,A9,A11)
 
      eew = sqrt(xe*939.*2+939.**2)
      
      if (jeof.eq.0) then 
        
      do ind=1,ieend
      ntotdata=ntotdata+1
      read(98,*,IOSTAT=IEOF) eang, edcs, eedcs

      if (ieof.ne.0) then 
      print*,"number mixmatch"
      print*,"in ichpi",ireac," at elab",xe
      stop
      endif
        
      do iene=1,mxe
      do iang=0,itmx
      wcm  = ze0(iene)
      the  = acos(theta(iang))*180./3.14159265
      wexp = eew
      aexp = eang

      if (wexp.gt.wcm-dw.and.wexp.lt.wcm+dw) then 
      if (aexp.gt.the-da.and.aexp.lt.the+da) then 

      numexpdat(ireac,iobs,iene,iang)=numexpdat(ireac,iobs,iene,iang)+1    
      indexito=numexpdat(ireac,iobs,iene,iang)
      expdata(ireac,iobs,iene,iang,indexito)=edcs
      experr(ireac,iobs,iene,iang,indexito)=eedcs
      expang(ireac,iobs,iene,iang,indexito)=eang
      expw(ireac,iobs,iene,iang,indexito)=eew

      goto 44

      endif ! aexp
      endif ! wexp

      enddo ! iang
      enddo ! iene

 44   continue

      enddo ! ind
       
      nset(ireac,iobs)=nset(ireac,iobs)+1
       
      else 
       
      print*,"SETS=",nset(ireac,iobs),"N of data",ntotdata
      goto 302 
       
      endif ! jeof
       
      goto 301
       
 300  format(f10.2,2x,e11.1,2x,e11.1)
 302  close(98)
    
cc----------------------------
ccc Polarization data, Sigma
cc----------------------------

      iobs=2

      if (ireac.eq.0)  open(98,file="all.pi0p.S")
      if (ireac.eq.1)  open(98,file="all.pipn.S")
      if (ireac.eq.2)  open(98,file="all.etan.S")
       
      nset(ireac,iobs)=1
      ntotdata=0
       
 401  read(98,*) 
      READ (98, 399, IOSTAT=JEOF) ce,xe,cnd,ieend,non,expname
 
      eew = sqrt(xe*939.*2+939.**2)

      if (jeof.eq.0) then 
       
      do ind=1,ieend
      ntotdata=ntotdata+1
      read(98,*,IOSTAT=IEOF) eang,edcs,eedcs

      if (ieof.ne.0) then 
      print*,"number mixmatch"
      print*,"in ichpi",ireac," at elab",xe
      stop
      endif

      do iene=1,mxe
      do iang=0,itmx
      wcm  = ze0(iene)
      the  = acos(theta(iang))*180./3.14159265
      wexp = eew
      aexp = eang

      if (wexp.gt.wcm-dw.and.wexp.lt.wcm+dw) then 
      if (aexp.gt.the-da.and.aexp.lt.the+da) then 
    
      numexpdat(ireac,iobs,iene,iang)=numexpdat(ireac,iobs,iene,iang)+1    
      indexito=numexpdat(ireac,iobs,iene,iang)
      expdata(ireac,iobs,iene,iang,indexito) = edcs
      experr(ireac,iobs,iene,iang,indexito)  = eedcs
      expang(ireac,iobs,iene,iang,indexito)  = eang
      expw(ireac,iobs,iene,iang,indexito)    = eew

      endif ! aexp
      endif ! wexp
      
      enddo ! iang
      enddo ! iene

      enddo ! ind
      
      nset(ireac,iobs)=nset(ireac,iobs)+1

      else 
      
      print*,"SETS=",nset(ireac,iobs),"N of data",ntotdata      
      goto 402 
      
      endif ! jeof
      
      goto 401

 402  close(98)

c     Total cross section data, tcs

      iobs=3
      
      if (ireac.eq.0)  open(98,file="all.pi0.said.tcs")
      if (ireac.eq.1)  open(98,file="all.pip.said.tcs")
      if (ireac.eq.2)  open(98,file="all.etan.said.tcs")

      nset(ireac,iobs)=1
      ntotdata=0
      
 501  READ (98, *, IOSTAT=JEOF) ieend
      
      if (jeof.eq.0) then
        
      do ind=1,ieend
      ntotdata=ntotdata+1
      read(98,*,IOSTAT=IEOF) eew,edcs,eedcs
      
      nset(ireac,iobs)=ind
      
      do iene=1,mxe
      wcm  = ze0(iene)
      iang = 0
      dw   = 6
      if (iobs.eq.3.and.wcm.lt.1300.) dw=3

      wexp=eew

      if (wexp.gt.wcm-dw.and.wexp.lt.wcm+dw) then 

      numexpdat(ireac,iobs,iene,0)=numexpdat(ireac,iobs,iene,0)+1    
      indexito=numexpdat(ireac,iobs,iene,0)
      expdata(ireac,iobs,iene,0,indexito) = edcs
      experr(ireac,iobs,iene,0,indexito)  = eedcs
      expw(ireac,iobs,iene,0,indexito)    = eew

      endif ! wexp
      enddo ! iene

      if (ieof.ne.0) then 
      print*,"number mixmatch"
      print*,"in ichpi",ireac," at elab",xe
      stop
      endif

      enddo ! ind
      
      nset(ireac,iobs)=nset(ireac,iobs)+1

      else 
      
      print*,"SETS=",nset(ireac,iobs),"N of data",ntotdata
      goto 502 
      
      endif
      
      goto 501

 502  enddo ! ireac
      close(98)

c all data are now already loaded

      write(668,3669) 'ireac=',2,'iobs=',3
3669  format(A6,I2,2x,A5,I2,2x)
 
      do ire=0,1
      do iobs=1,3
      itm=itmx
      if (iobs.eq.3) itm=0
      do iprite=1,mxe
      do iang=0,itm
      write(668,3668) ire,iobs,iprite,iang,
     1   numexpdat(ire,iobs,iprite,iang)
 3668  format(i2,2x,i2,2x,i4,2x,i4,2x,i4)
      enddo
      enddo
      enddo
      enddo
      
c bjulia 28-07-2007

      par = 0
      read(79,1) cdum ! cc_minput
 1    format(a5)

      ipar  = 0
 2    continue
      read(79,*,ERR=100,END=200)ixx,cval,xpar
      ipar      = ipar + 1
      par(ipar) = xpar
      write(*,*)IPAR,XPAR
      write(*,*)ixx,cval,xpar
      go to 2
 200  continue
      npar      = ipar
      write(*,*)'end 200'
      go to 101
 100  continue
      npar = ipar
      write(*,*)'error 100'
 101  continue
c      close(unit=79)

c----------------------------------------------------

c ts 11-3-2007
      mxj = 1

      read(15,*)njLs
      do i=1,njLs
        read(15,*)jpind(i),Lpind(i),ispind(i),itpind(i)
        read(15,*)nstar0(i)
c ts 11-3-2007
        mxj = max(mxj,jpind(i))
      end do

      read(15,*)nch
      read(15,*)(ich0(ic),ic=1,nch)

      read(15,*)ipipin,iofft,nmesh
      read(15,*)(opt(i),i=1,4)

      read(15,*)np
      read(15,*)icres,icpot,itmat,iquark

      np1 = np + 1

c-----------------------------------------------------
c  setup LSJ  11-3-2007
c-----------------------------------------------------

c  LSJ for full amplitude

      ljt1 = 0
      
      do i = 1,nljs
        ljt1(lpind(i),jpind(i),itpind(i)) = 1
      end do

c  LSJ for full + Born

      idx    = 0
      
      do it  = 1,3,2
      do lx  = 0,lmx2
      lx2    = lx*2
      do jjx = abs(lx2-1),lx2+1,2

      idx    = idx + 1
      itpind2(idx) = it
      ispind2(idx) = 1
      lpind2 (idx) = lx2
      jpind2 (idx) = jjx
      end do
      end do
      end do

      njls2 = idx

c-------------------------------------------------------
c  meson baryon channel
c------------------------------------------------------
      nc   = nch
      nchh = nc
      do ic  = 1,nch
        ich(ic)= ich0(ic)
        icc    = ich0(ic)
        is1d(ic)=is1dt(icc)
        is2d(ic)=is2dt(icc)
        it1d(ic)=it1dt(icc)
        it2d(ic)=it2dt(icc)
      end do

      do ic    = 1,nch
        icc      = ich0(ic)
        am1(ic)  = am1dt(icc)
        am2(ic)  = am2dt(icc)
        istab(ic)=istabdt(icc)
      end do

      fpio=api
      feta=aeta
      fnuc=amn

c------------------------------------------------------------
c      define L S of each channel
c------------------------------------------------------------

      do iwav=1,20
        allwaves(iwav)="aaa"
      enddo

      do i  =1,njLs
        jpin  =jpind(i)
        Lpin  =Lpind(i)
        ispin =ispind(i)
        itpin =itpind(i)
        ipar00=(-1)**(Lpin/2 +2+1)

        wpw=wave(Lpin/2+1:Lpin/2+1)//numbers(itpin+1:itpin+1)
     1//numbers(jpin+1:jpin+1)
        allwaves(i)=wpw

        do 50 ic=1,nc
          is1=is1d(ic)
          is2=is2d(ic)
          ismin=abs(is1-is2)
          ismax=is1+is2
          ni=0

          do 51 is=ismin,ismax,2
            Lmin=abs(jpin-is)
            Lmax=jpin+is
          do 51 L=Lmin,Lmax,2
            ipa=(-1)**(L/2+2+1)
            if(ich(ic).eq.4)ipa=(-1)**(L/2+2)
            if(ipa.eq.ipar00)then
              ni=ni+1
              isdt(ni,ic,i)= is
              Ldt(ni,ic,i) = L
            end if
  51      continue
          nLsdt(ic,i)  = ni
  50    continue
         
      end do         ! end loop i

c eyeball set 

      icontrolnres=1
      icontrolclo=1
      icontrolbar=1

      do i=0,maxl
        icontl(i)=1
      enddo
      
      return
      end

