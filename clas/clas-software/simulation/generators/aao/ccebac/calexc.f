c------------------------------------------------------------------
c
c    file sent june 2007
c
c  input zmul,wcm,q2,eps
c         q_0     egam0
c         |q|     egam
c         q_gamma egamc
c         k       qpio
c         nldim   l-max
c
c
      subroutine calexc(zmul,iwcm,iq,nldim,ichpi,xchi2,iobs,iflag,nexp)

      implicit real*8(a-h,o-y)
      implicit complex*16(z)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)

c      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm

       common/mesh00/nc,ne0,np1,ich(maxmb),
     1   ze0(maxwcm),am1(maxmb),am2(maxmb),istab(maxmb)
     1  ,zp(maxmom,maxmb,maxwcm),zwp(maxmom,maxmb,maxwcm)
     1  ,zp0(maxmb,maxwcm),zgreen(maxmom,maxmb,maxwcm)

      common/alldata/ numexpdat(0:2,3,3000,0:maxtheta)
     1                 ,expdata(0:2,3,3000,0:maxtheta,50)
     1                  ,experr(0:2,3,3000,0:maxtheta,50)
     1                  ,expang(0:2,3,3000,0:maxtheta,50)
     1                 ,   expw(0:2,3,3000,0:maxtheta,50)
     1                 ,  obser(0:2,3,3000,0:maxtheta)


c ts 10-30-2007
      common /etapi / jetapi
c ts

      real*4 xpl, ypl, yple, eyple, radiu,esi
      common / input1 / mxq,q2(maxq2)
c bjulia 28-07-2007

        dimension outexp(3,0:2000),outexpe(3,0:2000)
     1 ,outw(3,0:2000),outhe(3,0:2000),iexp(3)
c bjulia
      dimension zmul(0:maxl,8),zf(8)
      dimension rt(0:3),rl(0:3)
     &        ,rttc(0:4),rtts(0:4),rtlc(0:4),rtls(0:4)
     &        ,rttp(0:4),rtlpc(0:4),rtlps(0:4)

      common / input3 / itmx,theta(0:maxtheta),wtheta(0:maxtheta)

c tsato 24-08-2007
      data iunit0/400/
      data iunitp/450/
      data iunite/500/
c tsato
 
c bjulia 9-10-2007
      common/buildchi/iob(3),irea(0:2)
c bjulia

c bjulia 17 10 2007
      common /pgplotwrite/iwriteplot
      common /noprintout/ isw5
c bjulia

c bjulia 31 10 2007
       common/totalcrs/xrs0(10),xrsp(10),xrse(10)
c bjulia
c

      wcm   = ze0(iwcm)
      qq    = q2(iq)

c ts 10-30-2007
      fmeson = am1(jetapi)
c

c      write(998,997)jetapi,ichpi,am1(1),am2(1),fpio,feta,fnuc,fmeson
c 997  format(1h ,2i3,6e15.5)
c bjulia 31 10 2007

      fpio=am1(1)
      fnuc=am2(1)

      xx    = (wcm**2 - fmeson**2 + fnuc**2)/(2.d0*wcm)
      qpio  = sqrt(xx**2 - fnuc**2)
      egam0 = (wcm**2 - qq - fnuc**2)/(2.d0*wcm)
      egam  = sqrt((wcm-egam0)**2 - fnuc**2)
      egamc = (wcm**2 - fnuc**2)/(2.d0*wcm)

      pi    = acos(-1.d0)

c     fpio in MeV

      xtrns =1.d-03/(fpio/197.3d0) ! fm
      facrs = xtrns**2*10000.d0

      xlond = sqrt(qq/egam**2)
      factt = sqrt(facrs)
      facll = sqrt(facrs)*xlond
      xkine = qpio/egamc
       
      xchi2=0.
      
      do 300 it = 0,itmx
       the    = acos(theta(it))*180./pi
       xxc    = theta(it)
       xxs    = sqrt(1.d0-xxc**2)

       call mul2f(zmul,xxc,zf,nldim)

       zf(1) = zf(1)*factt
       zf(2) = zf(2)*factt
       zf(3) = zf(3)*factt
       zf(4) = zf(4)*factt
       zf(7) = zf(7)*facll
       zf(8) = zf(8)*facll

c-------------------------------------------
       zaa = - xxs*zf(2)
       zdd = zf(1) - xxc*zf(2)
       zbb = zdd + xxs**2 * zf(4)
       zcc = (zf(2)+zf(3)+xxc*zf(4))*xxs
       zal = xxs*zf(7)
       zbe = xxc*zf(7) + zf(8)

       zcaa = dconjg(zaa)
       zcbb = dconjg(zbb)
       zccc = dconjg(zcc)
       zcdd = dconjg(zdd)
       zcal = dconjg(zal)
       zcbe = dconjg(zbe)
c-------------------------------------------
       yy1   = zcaa*zaa + zcdd*zdd
       yy2   = zcbb*zbb + zccc*zcc


c R_T
       rt(0)   = (yy1+yy2)/2.d0
       rt(1)   = 0
       rt(2)   = dimag(-zccc*zbb - zcaa*zdd)
       rt(3)   = 0
c R_L
       rl(0)   = zcal*zal + zcbe*zbe
       rl(1)   = 0
       rl(2)   = 2.d0*dimag(zcal*zbe)
       rl(3)   = 0
c R_TT^c,s
       rttc(0) = (yy2-yy1)/2.d0
       rttc(1) = 0
       rttc(2) = dimag(-zccc*zbb + zcaa*zdd)
       rttc(3) = 0
       rtts(0) = 0
       rtts(1) = dimag(zcdd*zcc + zcaa*zbb)
       rtts(2) = 0
       rtts(3) = dimag(-zcdd*zbb + zcaa*zcc)
c R_TL^c,s
       rtlc(0) = - dble(zcal*zbb+zcbe*zcc)
       rtlc(1) = 0
       rtlc(2) = dimag(-zcal*zcc+zcbe*zbb)
       rtlc(3) = 0 
       rtls(0) = 0
       rtls(1) = dimag(zcbe*zdd+zcal*zaa)
       rtls(2) = 0
       rtls(3) = dimag(-zcal*zdd+zcbe*zaa)
c R_TT'
       rttp(0) = 0
       rttp(1) = dble(zcdd*zcc+zcaa*zbb)
       rttp(2) = 0
       rttp(3) = dble(-zcdd*zbb+zcaa*zcc)
c R_TL'^c,s
       rtlpc(0) = 0
       rtlpc(1) = dble(-zcbe*zdd-zcal*zaa)
       rtlpc(2) = 0
       rtlpc(3) = dble(zcal*zdd-zcbe*zaa)
       rtlps(0) = dimag(zcal*zbb+zcbe*zcc)
       rtlps(1) = 0
       rtlps(2) = dble(-zcal*zcc+zcbe*zbb)
       rtlps(3) = 0

       xst   = rt(0)
       xsl   = rl(0)
       xsp   = rttc(0)
       xsi   = rtlc(0)
       xse   = rtlps(0)


c       xst   = (yy1+yy2)/2.d0
c       xsl   =  zcal*zal + zcbe*zbe
c       xsp   =    (yy2-yy1)/2.d0
c       xsi   =  - dble(zcal*zbb+zcbe*zcc)
c       xse   = dimag(zcal*zbb+zcbe*zcc)
c
       cst   = xst*xkine
       csl   = xsl*xkine
       csp   = xsp*xkine
       csi   = xsi*xkine
       cse   = xse*xkine 

c  xps = Sigma,  rr = (1 - Sigma)/(1 + Sigma) = R

       xps  = -xsp/xst                                   
       rr   = ( 1.d0-xps)/(1.d0+xps)

       if (iobs.eq.1.or.iobs.eq.2) then 

       obs=0
       if (iobs.eq.1) obs=cst	! cross-section
       if (iobs.eq.2) obs=xps	! polarization asymmetry

       obser(ichpi,iobs,iwcm,it)=obs

       if (iwriteplot.eq.1.and.isw5.ne.1) then 
       write(668,6669) ichpi,iobs,itg,obs
 6669  format(3(I3,2x),e10.4)
       endif

      do iout=1,numexpdat(ichpi,iobs,iwcm,it)
      
       if (iwriteplot.eq.1.and.iobs.lt.3) then 
       write(668,6668) ichpi,iobs
     1 ,  wcm
     1 ,  expw(ichpi,iobs,iwcm,it,iout)
     1 ,  the
     1 ,  expang( ichpi,iobs,iwcm,it,iout)
     1 , obser(ichpi,iobs,iwcm,it)
     1 ,expdata(ichpi,iobs,iwcm,it,iout)
     1 ,experr(ichpi,iobs,iwcm,it,iout)
 6668  format(i3,2x,i3,2x,7(e10.4,2x))
       endif


c bjulia 9-10-2007
      if (irea(ichpi)*iob(iobs).eq.1) then 
c bjulia 9-10-2007

         xchi2=   xchi2   + 
     1     (obs-expdata(ichpi,iobs,iwcm,it,iout))**2 
     1          /experr(ichpi,iobs,iwcm,it,iout)**2
c    1      *(ichpi+1+(1-ichpi)*5)
      nexp=nexp+1

      endif
      enddo
      endif   ! iobs 1 and 2

      if (iobs.eq.1.and.iflag.eq.3.and.isw5.ne.1) then 
        if(ichpi.eq.0) then
          write(iunit0,1010)wcm/1000,the,cst,xps 
        elseif(ichpi.eq.1) then
          write(iunitp,1010)wcm/1000,the,cst,xps 
        elseif(ichpi.eq.2) then
          write(iunite,1010)wcm/1000,the,cst,xps 
        end if
      endif
      
 300  continue

 1010  format(2f10.5,f10.3,9e15.5)
 1011  format(f10.3,9e15.5,2f10.5)
 6010  format(f10.3,2f10.5,2e15.5)


c bjulia 9-10-2007

      if (iobs.eq.3.and.wcm.lt.1930.) then 
      obs=0.          
      if (ichpi.eq.0) obs=xrs0(4)
      if (ichpi.eq.1) obs=xrsp(4)
      if (ichpi.eq.2) obs=xrse(4)

      obser(ichpi,iobs,iwcm,0)=obs

      do iout=1,numexpdat(ichpi,iobs,iwcm,0)
      
      if (irea(ichpi)*iob(iobs).eq.1) then 
         xchi2=   xchi2   + 
     1     (obs-expdata(ichpi,iobs,iwcm,0,iout))**2
     1     /experr(ichpi,iobs,iwcm,0,iout)**2
      endif

      if(iflag.eq.3.and.isw5.ne.1) then
       write(300+ichpi,431) wcm,tot,expdata(ichpi,iobs,iwcm,0,iout),
     1experr(ichpi,iobs,iwcm,0,iout)
 431   format(4(E12.6,2x))
      end if

      enddo
      endif
      
c bjulia 9-10-2007

      if(iflag.eq.3.and.iobs.eq.1) then
        if(ichpi.eq.0) then
          iunit0 = iunit0 + 1
        elseif(ichpi.eq.1) then
          iunitp = iunitp + 1
        elseif(ichpi.eq.2) then
          iunite = iunite + 1
        end if
      end if
      
      return
      end
