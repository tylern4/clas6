      subroutine calcrs(ichpi,xpdsgchi,iflag)

      implicit real*8(a-h,o-y)
      implicit complex*16(z)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)
      parameter (maxlsj2=50)

      character cl(0:4)*1,cj(7)*1
      character cfname*3
      character numbers*10,aen*2
      
      character*11 saidpwf(2)
      character*7 nam

c bjulia 17 10 2007
      common /noprintout/ isw5
      common /pgplotwrite/iwriteplot
c bjulia 17 10 2007

      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common / input1 / mxq,q2(maxq2)
      common / ctest / ieout,iqout

      common/chdat1/njLs,jpind(maxlsj) ,Lpind(maxlsj)
     &                  ,ispind(maxlsj),itpind(maxlsj)

c ts 11-3-2007
      common/chdat3/njLs2,jpind2(maxlsj2) ,Lpind2(maxlsj2)
     &                  ,ispind2(maxlsj2),itpind2(maxlsj2),lmx2
     &                  ,ljt1(0:maxl*2,2*maxl+1,3)

       common/mesh00/nc,ne0,np1,ich(maxmb),
     1   ze0(maxwcm),am1(maxmb),am2(maxmb),istab(maxmb)
     1  ,zp(maxmom,maxmb,maxwcm),zwp(maxmom,maxmb,maxwcm)
     1  ,zp0(maxmb,maxwcm),zgreen(maxmom,maxmb,maxwcm)

       common /ELEM3/ ztpin(maxwcm,maxlsj)
     1                ,ztmx(maxwcm,maxlsj)
     2               ,ztres(maxwcm,maxlsj)
     3            ,zsigsato(maxwcm,maxlsj,maxres,maxres)

      common / elmamp/ zxmbrn(maxwcm,maxq2,8,0:maxl,5)
     3                ,zxmnres(maxwcm,maxq2,8,0:maxl,5)
     4                ,zxmclo(maxwcm,maxq2,8,0:maxl,5)
     5                ,zxmbare(maxwcm,maxq2,8,0:maxl,5)
      common/totalcrs/xrs0,xrsp,xrse

      common/saidexp/ eang(0:1,2,3000,40),edcs(0:1,2,3000,40)
     1 ,eedcs(0:1,2,3000,40)
     1 ,eew(0:1,2,3000),ieend(0:1,2,3000),nset(0:1,2)
     1 ,expname(0:1,2,3000)
 
      common/saidpw/ said(10,400), nsaid1, nsaid, saidpwf
      
c bjulia 23 10 2007
      common/eyeball/icontrolnres,icontrolclo,icontrolbar,icontl(0:maxl)
c bjulia

c ts 10-30-2007
      common /etapi / jetapi
c ts
      dimension zmul(0:maxl,8),zmule(0:maxl,8),zm(0:maxl,8,6)
      dimension xrs0(10),xrsp(10),xrse(10)
      
      dimension fiso(3)
      dimension fr(2),fi(2),ft(2)
      
      fiso(1)  = sqrt(3.d0/2.d0) ! for 3/2
      fiso(2)  = -1.d0/sqrt(3.d0)! for 1/2p
      fiso(3)  = -1.d0/sqrt(3.d0)! for 1/2n
      
      numbers="0123456789"
     
      xsll = fpio/scale*1000          ! 10^3 /m_pi 
      fac1 = 1./1.414383

      lmx  = 0
      nexp = 0

      do i = 1,njls
       lmx = max(lmx,lpind(i)/2)
      end do
      
      if (lmx.gt.maxl) then 
         write(*,*)'maxl overruled'
         stop
      endif

      xchic=0.
      xchi =0.
      
      if (ichpi.eq.0) then
        nexc=0
        nexm=0
        xchim=0.
        do if=1,2
          if (saidpwf(if).ne.'NONE') then
            nam=saidpwf(if)
            nam=nam(1:3)//"-141"
            open(unit=776+if,file=nam,status='unknown')
          endif
        enddo
      endif
      
      do ie = 1,ne0
      do iq = 1,mxq

       wcm = ze0(ie)
        qq = q2(iq)

c bjulia 2 11 2007
       etacheck=wcm-feta-fnuc
       ietaopen=0
       if (etacheck.gt.0.) ietaopen=1
c bjulia

c       if (iflag.eq.3.and.isw5.ne.1) then 

c          i1en=ie/10
c          i2en=ie-i1en*10

c          aen=numbers(i1en+1:i1en+1)//numbers(i2en:i2en)
        
c       do iobs=1,3
c          write(135+ichpi+2*(iobs-1),444) wcm,iobs,ichpi
c          write(135+ichpi+2*(iobs-1),445) 
c          write(135+ichpi+2*(iobs-1),*) "@target G"//aen//".S1"
c          write(135+ichpi+2*(iobs-1),*) "@type xydy"
 444      format('& WCM=',F8.2,'  iobs (1 DSG, 2 Sigma, 3 tcs)=',I1,
     1     ', reaction (0 pi0,1 pi+)=',I1)
 445      format('& ANGLE     OBS     ERROR    WCM      Q2')
c       enddo
c          write(22,*) "&"
c          write(22,*) "@target G"//aen//".S2"
c          write(22,*) "@type xy"
    
c          write(23,*) "&"
c          write(23,*) "@target G"//aen//".S2"
c          write(23,*) "@type xy"
c       endif

c------------------------------------------------------------------
c ts 11-3-2007
      lmxx = max(lmx,lmx2)
      
c  test --------------
c      lmxx = lmx
c----------------------
       
      do lx = 0,lmxx
      do idx= 1,8

      if(lx.le.lmx) then

      do i=1,6
      zm(lx,idx,i)=zxmnres(ie,iq,idx,lx,i)*icontrolnres
     &            + zxmclo(ie,iq,idx,lx,i)*icontrolclo
     &            +zxmbare(ie,iq,idx,lx,i)*icontrolbar
      zm(lx,idx,i)=zm(lx,idx,i)*icontl(lx)
      enddo
      
      else
      
      do i=1,6
      zm(lx,idx,i)=zxmbrn(ie,iq,idx,lx,i)*icontrolnres
      zm(lx,idx,i)=zm(lx,idx,i)*icontl(lx)
      enddo
      
      endif
                  
      if (iflag.eq.3.and.isw5.ne.1) then 
      if (idx.eq.1.and.lx.eq.1) then 
      write(233,1555) wcm,zm(lx,idx,1)*xsll
      endif
      if (idx.eq.3.and.lx.eq.1) then 
      write(234,1555) wcm,zm(lx,idx,1)*xsll
      endif
      endif

1555   format(7(E14.6,2x))
 
      zm1=zm(lx,idx,1)
      zm2=zm(lx,idx,2)
      zm2e=zm(lx,idx,4)
      
      if(ichpi.eq.0) then	! p-pi0 p
         zmul(lx,idx) =(sqrt(2.d0)*zm1 - zm2)/sqrt(3.d0)*xsll
      else if(ichpi.eq.1) then	! p-pi+ n
         zmul(lx,idx) =(sqrt(2.d0)*zm2 + zm1)/sqrt(3.d0)*xsll
      else if(ichpi.eq.2) then
         zmule(lx,idx) =zm2e*xsll
      end if

      end do	! idx
      end do	! lx
      
      
c computes total cross section

      xrs   = 0
      xre   = 0
c      xrse   = 0
c      xrsp   = 0

      do lx = 0,lmxx

      xrs   = xrs + (lx+1)**2*
     &  ( (lx+2)*(abs(zmul(lx,1))**2 + abs(zmul(lx+1,4))**2)
     &       +lx*(abs(zmul(lx,3))**2 + abs(zmul(lx+1,2))**2))

      xre   = xre + (lx+1)**2*
     &  ( (lx+2)*(abs(zmule(lx,1))**2 + abs(zmule(lx+1,4))**2)
     &       +lx*(abs(zmule(lx,3))**2 + abs(zmule(lx+1,2))**2))
      end do

      wcm   = ze0(ie) 
      egamc = (wcm**2 - fnuc**2)/(2.d0*wcm)
      xtrns = 1.d-03/(fpio/197.3d0) ! fm

      if(ichpi.lt.2) then

      xx    = (wcm**2 - fpio**2 + fnuc**2)/(2.d0*wcm)
      if(xx.le.0.d0) then
      facrsp = 0
      else
      qpio   = sqrt(xx**2 - fnuc**2)
      facrsp = 2.d0*pi*qpio/egamc*xtrns**2*10000.d0
      end if

      if(ichpi.eq.0) then
      xrs0(4) = facrsp * xrs
      else if(ichpi.eq.1) then
      xrsp(4) = facrsp * xrs
      end if

      else if(ichpi.eq.2) then

      xx    = (wcm**2 - feta**2 + fnuc**2)/(2.d0*wcm)
      if(xx.le.0.d0) then
      facrse = 0
      else
      qeta   = sqrt(xx**2 - fnuc**2)
      facrse = 2.d0*pi*qeta/egamc*xtrns**2*10000.d0
      end if

      if (facrse.ne.0)  then 
         xrse(4) =  facrse *  xre
        else 
         xrse(4)=0
      endif

      end if
      
c---------------------------------------------------------------------

c lcsmith 14-10-2007
      
      if (saidpwf(1).ne.'NONE'.and.ichpi.eq.0) then
      
      do if=1,2
      if (saidpwf(if).ne.'NONE'.and.ichpi.eq.0) then
      
      ind=(if-1)*nsaid1+1
      lpin = 2*(said(6,ind)-1)
      lx   = lpin/2.
      if (said(7,ind).eq.2) iso=1
      if (said(7,ind).eq.1) iso=2      
      jpin = 2*said(8,ind)-1
      idx  = said(10,ind)
      
      if(jpin.lt.lpin) then
        ims = 8
        ime = 2
        imm = 4
        if(idx.eq.1) imp=4
        if(idx.eq.2) imp=2
        if(idx.eq.3) imp=8
      else if(jpin.gt.lpin) then
        ims = 7
        ime = 1
        imm = 3
        if(idx.eq.1) imp=3
        if(idx.eq.2) imp=1
        if(idx.eq.3) imp=7
      endif
      
      fr(if)= real(zm(lx,imp,iso))*xsll*fiso(iso)
      fi(if)=aimag(zm(lx,imp,iso))*xsll*fiso(iso)
      ft(if)=sqrt(fr(if)**2+fi(if)**2)
      
      xsllx=xsll*fiso(iso)
      
      write(776+if,1111)wcm/1000,q2x/1000000,
     & zxmbare(ie,iq,ime,lx,iso)*xsllx,zxmnres(ie,iq,ime,lx,iso)*xsllx,
     & zxmclo(ie,iq,ime,lx,iso)*xsllx,
     & zxmbare(ie,iq,imm,lx,iso)*xsllx,zxmnres(ie,iq,imm,lx,iso)*xsllx,
     & zxmclo(ie,iq,imm,lx,iso)*xsllx,
     & zxmbare(ie,iq,ims,lx,iso)*xsllx,zxmnres(ie,iq,ims,lx,iso)*xsllx,
     & zxmclo(ie,iq,ims,lx,iso)*xsllx
      
      endif
      enddo
      
 1111 format(1h ,40e15.6)
      
      xchir=0
      xchii=0
      xchit=0
      
      do i=1,nsaid
        esaid=said(1,i)
        if (esaid.gt.wcm-10.and.esaid.lt.wcm+10) then
          if (i.le.nsaid1) then
            ffr=fr(1)
            ffi=fi(1)
            fft=ft(1)
          else
            ffr=fr(2)
            ffi=fi(2)
            fft=ft(2)
          endif
          dr =said(2,i)*fac1
          dre=max(0.05,said(3,i)*fac1)
          dre=0.1
          di =said(4,i)*fac1
          die=max(0.05,said(5,i)*fac1)
          die=0.1
          dt=sqrt(dr*dr+di*di)
          dte=sqrt(dre*dre+dri*dri)
          xchir=xchir+(dr-ffr)**2/dre**2
          xchii=xchii+(di-ffi)**2/die**2
          xchit=xchit+(dt-fft)**2/dte**2
          nexm=nexm+1
        endif
      enddo
      
      xchim=xchim+xchii		! loop over imag only
      
      endif
      
      do iobs=1,3
      
        if (ichpi.lt.2) then 
          jetapi = 1 
          call calexc(zmul,ie,iq,lmxx,ichpi,xchi,iobs,iflag,nexp)
          xchic = xchic+xchi
        endif 
   
        if (ichpi.eq.2.and.ietaopen.eq.1) then 
          jetapi = 2
          call calexc(zmule,ie,iq,lmxx,ichpi,xchi,iobs,iflag,nexp)
          xchic = xchic+xchi
        endif
           
        write(*,777) iobs,ichpi,wcm,xchi,xchic,xchir,xchii,xchit,xchim,
     1               nexp,nexm
777   format(1x,2i3,7f10.2,2i5) 

        if (iwriteplot.eq.1) then 
        write(668,3334) ichpi,iobs,xchi,wcm
 3334   format("partial ichpi=",I3,"iobs=",I3,
     1  " xchi=", f12.3," wcm=",f12.3)
        endif
        
      enddo     ! iobs 

      end do	! mxq
      end do	! ne0

      print*,"Reduced xsect chi2 ",xchic/float(nexp)
      print*,"Reduced mpole chi2 ",xchim/float(nexm)
      
      xpdsgchi = xchic
      
      if (saidpwf(1).ne.'NONE') xpdsgchi = xchim
       
      if (ichpi.eq.0) close(777)
      if (ichpi.eq.0.and.saidpwf(1).ne.'NONE') close(778)
      
      return
      
c ----------------------------------------------------------------------
c  only check
c
      xxx  = xsll
      ie = 1
      do iq = 1,mxq
      wcm    = ze0(ie)
      q2x    = q2(iq)
      write(114,9192)wcm/1000,q2x/1000000
 9192 format(1h ,2e15.5)
      do lx  = 0,1
      do idx = 1,8

      iss = 1

      if(iss.eq.0) then
      write(114,9191)lx,idx
 9191 format(1h ,5i3)
      do iso = 1,3
      zobrn= zxmbrn  (ie,iq,idx,lx,iso)*xxx
      zonre= zxmnres (ie,iq,idx,lx,iso)*xxx
      zomc = zxmclo  (ie,iq,idx,lx,iso)*xxx
      zobar= zxmbare (ie,iq,idx,lx,iso)*xxx
      ztot = zonre+zomc+zobar
      write(114,9193)iso,ztot,zobrn,zonre,zomc,zobar
 9193 format(1h ,i3,10e15.5)
      end do

      else if(iss.eq.1) then
      zobrn1= zxmbrn  (ie,iq,idx,lx,1)*xxx
      zonre1= zxmnres (ie,iq,idx,lx,1)*xxx
      zomc1 = zxmclo  (ie,iq,idx,lx,1)*xxx
      zobar1= zxmbare (ie,iq,idx,lx,1)*xxx
      ztot1 = zonre1+zomc1+zobar1

      zobrn2= zxmbrn  (ie,iq,idx,lx,2)*xxx
      zonre2= zxmnres (ie,iq,idx,lx,2)*xxx
      zomc2 = zxmclo  (ie,iq,idx,lx,2)*xxx
      zobar2= zxmbare (ie,iq,idx,lx,2)*xxx
      ztot2 = zonre2+zomc2+zobar2

      zpi0 = (sqrt(2.d0)*ztot1 - ztot2)/sqrt(3.d0)
      zpip = (sqrt(2.d0)*ztot2 + ztot1)/sqrt(3.d0)

      write(114,9194)lx,idx,zpi0,zpip
 9194 format(1h ,2i3,10e15.5)

      end if

      end do
      end do
      end do
      stop

      end
