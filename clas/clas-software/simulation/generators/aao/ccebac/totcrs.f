      subroutine totcrs(iflag,xsll,ie,iq,lmx)
      implicit complex*16(z)
      implicit real*8(a-h,o-y)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)

      common /noprintout/ isw5

      common / elmamp/ zxmbrn(maxwcm,maxq2,8,0:maxl,5)
     3                ,zxmnres(maxwcm,maxq2,8,0:maxl,5)
     4                ,zxmclo(maxwcm,maxq2,8,0:maxl,5)
     5                ,zxmbare(maxwcm,maxq2,8,0:maxl,5)

       common/mesh00/nc,ne0,np1,ich(maxmb),
     1   ze0(maxwcm),am1(maxmb),am2(maxmb),istab(maxmb)
     1  ,zp(maxmom,maxmb,maxwcm),zwp(maxmom,maxmb,maxwcm)
     1  ,zp0(maxmb,maxwcm),zgreen(maxmom,maxmb,maxwcm)

      common/totalcrs/xrs0,xrsp,xrse
c bjulia 23 10 2007
      common/eyeball/icontrolnres,icontrolclo,icontrolbar,icontl(0:maxl)
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
c bjulia
      common /etapi / jetapi

      dimension zamp3(0:maxl,8,10),zamp1(0:maxl,8,10)
      dimension zamp0(0:maxl,8,10),zampp(0:maxl,8,10)
      dimension zampe(0:maxl,8,10)

      dimension crs3(0:maxl,10),crs1(0:maxl,10)
      dimension crs0(0:maxl,10),crsp(0:maxl,10)
      dimension crse(0:maxl,10)

      dimension xrs1(10),xrs3(10),xrs0(10),xrsp(10)
      dimension xrse(10)

c bjulia 31 10 2007

      wcm=ze0(ie)

      egamc = (wcm**2 - fnuc**2)/(2.d0*wcm)
      pi    = acos(-1.d0)
      xtrns =1.d-03/(fpio/197.3d0) ! fm

      xx    =(wcm**2 - fpio**2 + fnuc**2)/(2.d0*wcm)
      if(xx.le.0.d0) then
      facrsp = 0
      else
      qpio  = sqrt(xx**2 - fnuc**2)
      facrsp = 2.d0*pi*qpio/egamc*xtrns**2*10000.d0
     &        *xsll**2
      end if

      xx    =(wcm**2 - feta**2 + fnuc**2)/(2.d0*wcm)
      if(xx.le.0.d0) then
      facrse = 0
      else
      qeta   = sqrt(xx**2 - fnuc**2)
      facrse = 2.d0*pi*qeta/egamc*xtrns**2*10000.d0
     &        *xsll**2
      end if

c------------------------------------------------------------------
      do lx = 0,lmx
      do idx= 1,4
c
      ztmp1 = zxmnres(ie,iq,idx,lx,1)
      ztmp2 = zxmnres(ie,iq,idx,lx,2)
      ztmp0 = (sqrt(2.d0)*ztmp1 - ztmp2)/sqrt(3.d0)
      ztmpp = (sqrt(2.d0)*ztmp2 + ztmp1)/sqrt(3.d0)

      zamp3(lx,idx,1) = ztmp1
      zamp1(lx,idx,1) = ztmp2
      zamp0(lx,idx,1) = ztmp0
      zampp(lx,idx,1) = ztmpp
      zampe(lx,idx,1) = zxmnres(ie,iq,idx,lx,4)
c----------------------------------------------------
      ztmp1 = zxmclo(ie,iq,idx,lx,1)
      ztmp2 = zxmclo(ie,iq,idx,lx,2)
      ztmp0 = (sqrt(2.d0)*ztmp1 - ztmp2)/sqrt(3.d0)
      ztmpp = (sqrt(2.d0)*ztmp2 + ztmp1)/sqrt(3.d0)

      zamp3(lx,idx,2) = ztmp1
      zamp1(lx,idx,2) = ztmp2
      zamp0(lx,idx,2) = ztmp0
      zampp(lx,idx,2) = ztmpp
      zampe(lx,idx,2) = zxmclo(ie,iq,idx,lx,4)
c-----------------------------------------------------
      ztmp1 = zxmbare(ie,iq,idx,lx,1)
      ztmp2 = zxmbare(ie,iq,idx,lx,2)
      ztmp0 = (sqrt(2.d0)*ztmp1 - ztmp2)/sqrt(3.d0)
      ztmpp = (sqrt(2.d0)*ztmp2 + ztmp1)/sqrt(3.d0)

      zamp3(lx,idx,3) = ztmp1
      zamp1(lx,idx,3) = ztmp2
      zamp0(lx,idx,3) = ztmp0
      zampp(lx,idx,3) = ztmpp
      zampe(lx,idx,3) = zxmbare(ie,iq,idx,lx,4)
c-----------------------------------------------------

c bjulia eyeball controls which term to include
c bjulia 31 10 2007
      zamp3(lx,idx,4)=(icontrolnres   * zamp3(lx,idx,1)
     1                +icontrolclo    * zamp3(lx,idx,2)
     2                +icontrolbar    * zamp3(lx,idx,3))
     3                *icontl(lx)
      zamp1(lx,idx,4)=(icontrolnres   * zamp1(lx,idx,1)
     1                +icontrolclo    * zamp1(lx,idx,2)
     2                +icontrolbar    * zamp1(lx,idx,3))
     3                *icontl(lx)
      zamp0(lx,idx,4)=(icontrolnres   * zamp0(lx,idx,1)
     1                +icontrolclo    * zamp0(lx,idx,2)
     2                +icontrolbar    * zamp0(lx,idx,3))
     3                *icontl(lx)
      zampp(lx,idx,4)=(icontrolnres   * zampp(lx,idx,1)
     1                +icontrolclo    * zampp(lx,idx,2)
     2                +icontrolbar    * zampp(lx,idx,3))
     3                *icontl(lx)
      zampe(lx,idx,4)=(icontrolnres   * zampe(lx,idx,1)
     1                +icontrolclo    * zampe(lx,idx,2)
     2                +icontrolbar    * zampe(lx,idx,3))
     3                *icontl(lx)
c bjulia
      end do
      end do

      xrs1  = 0
      xrs3  = 0
      xrs0  = 0
      xrsp  = 0
      xrse  = 0

      do lx = 0,lmx
      do ix = 1,4

      crs3(lx,ix)= facrsp*(lx+1)**2*
     &  ( (lx+2)*(abs(zamp3(lx,1,ix))**2 + abs(zamp3(lx+1,4,ix))**2)
     &       +lx*(abs(zamp3(lx,3,ix))**2 + abs(zamp3(lx+1,2,ix))**2))
      crs1(lx,ix)= facrsp*(lx+1)**2*
     &  ( (lx+2)*(abs(zamp1(lx,1,ix))**2 + abs(zamp1(lx+1,4,ix))**2)
     &       +lx*(abs(zamp1(lx,3,ix))**2 + abs(zamp1(lx+1,2,ix))**2))

      crs0(lx,ix)= facrsp*(lx+1)**2*
     &  ( (lx+2)*(abs(zamp0(lx,1,ix))**2 + abs(zamp0(lx+1,4,ix))**2)
     &       +lx*(abs(zamp0(lx,3,ix))**2 + abs(zamp0(lx+1,2,ix))**2))

      crsp(lx,ix)= facrsp*(lx+1)**2*
     &  ( (lx+2)*(abs(zampp(lx,1,ix))**2 + abs(zampp(lx+1,4,ix))**2)
     &       +lx*(abs(zampp(lx,3,ix))**2 + abs(zampp(lx+1,2,ix))**2))

      crse(lx,ix)= facrse*(lx+1)**2*
     &  ( (lx+2)*(abs(zampe(lx,1,ix))**2 + abs(zampe(lx+1,4,ix))**2)
     &       +lx*(abs(zampe(lx,3,ix))**2 + abs(zampe(lx+1,2,ix))**2))

      end do

      do ix = 1,4
      xrs1(ix) = xrs1(ix) + crs1(lx,ix)
      xrs3(ix) = xrs3(ix) + crs3(lx,ix)
      xrs0(ix) = xrs0(ix) + crs0(lx,ix)
      xrsp(ix) = xrsp(ix) + crsp(lx,ix)
      if (facrse.ne.0) xrse(ix) = xrse(ix) + crse(lx,ix)
      end do
      end do


c  503 ispspin 3/2   501 1/2p
c  511 pi0           513  pi+
c  520/521 energy dependence


      if (isw5.ne.1) then 
      write(520,1010)wcm,qq,(xrs1(ix),xrs3(ix),ix=1,4)
      write(521,1010)wcm,qq,(xrs0(ix),xrsp(ix),xrse(ix),ix=1,4)

   
      write(503,1010)wcm,qq,
     &  crs3(0,1),crs3(0,2),crs3(0,3),crs3(0,4),
     &  crs3(1,1),crs3(1,2),crs3(1,3),crs3(1,4),
     &  crs3(2,1),crs3(2,2),crs3(2,3),crs3(2,4),
     &  crs3(3,1),crs3(3,2),crs3(3,3),crs3(3,4)
      write(501,1010)wcm,qq,
     &  crs1(0,1),crs1(0,2),crs1(0,3),crs1(0,4),
     &  crs1(1,1),crs1(1,2),crs1(1,3),crs1(1,4),
     &  crs1(2,1),crs1(2,2),crs1(2,3),crs1(2,4),
     &  crs1(3,1),crs1(3,2),crs1(3,3),crs1(3,4)
      write(511,1010)wcm,qq,
     &  crs0(0,1),crs0(0,2),crs0(0,3),crs0(0,4),
     &  crs0(1,1),crs0(1,2),crs0(1,3),crs0(1,4),
     &  crs0(2,1),crs0(2,2),crs0(2,3),crs0(2,4),
     &  crs0(3,1),crs0(3,2),crs0(3,3),crs0(3,4)
      write(513,1010)wcm,qq,
     &  crsp(0,1),crsp(0,2),crsp(0,3),crsp(0,4),
     &  crsp(1,1),crsp(1,2),crsp(1,3),crsp(1,4),
     &  crsp(2,1),crsp(2,2),crsp(2,3),crsp(2,4),
     &  crsp(3,1),crsp(3,2),crsp(3,3),crsp(3,4)
      write(514,1010)wcm,qq,
     &  crse(0,1),crse(0,2),crse(0,3),crse(0,4),
     &  crse(1,1),crse(1,2),crse(1,3),crse(1,4),
     &  crse(2,1),crse(2,2),crse(2,3),crse(2,4),
     &  crse(3,1),crse(3,2),crse(3,3),crse(3,4)
 1010 format(1h ,20e15.5)
      endif

      return
      end

