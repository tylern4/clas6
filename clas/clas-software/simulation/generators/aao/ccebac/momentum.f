      subroutine momentum(isw)
      IMPLICIT REAL*8 (A-H,O-Y),integer(i-n)
      implicit complex*16(z)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)

      common/chdat2/nch,ich0(maxmb)

       common/mesh00/nc,ne0,np1,ich(maxmb),
     1   ze0(maxwcm),am1(maxmb),am2(maxmb),istab(maxmb)
     1  ,zp(maxmom,maxmb,maxwcm),zwp(maxmom,maxmb,maxwcm)
     1  ,zp0(maxmb,maxwcm),zgreen(maxmom,maxmb,maxwcm)

       common/masses/am1dt(maxmb),am2dt(maxmb)

      dimension xx(100),wx(100)

      pi  = acos(-1.d0)
      api  = am1dt(1)
      amn  = am2dt(1)
      np   = np1 -1

       call gaussl(np,xx,wx)



       do 1 ie= 1,ne0
       zw0    = ze0(ie)

       do 2 ic= 1,nc
       a1     = am1(ic)
       a2     = am2(ic)

c===========================================================
       if(isw.eq.1) then   ! original mesh

       call onshell(zw0,a1,a2,zp00)
       p00  = zp00
       w0   = zw0

c selects mapping
      imap=1

c------------------------------------------
c MAP 0, original 
c
      if (imap.eq.0) then 
c original mapping
      do ip=1,np
      p00m=p00
      zp(ip,ic,ie)=p00m*(1.+xx(ip))/(1.-xx(ip))
      zwp(ip,ic,ie)=p00m*2./(1.-xx(ip))**2*wx(ip)
      end do
c-------------------------------------------
c MAP 1
c
      else if (imap.eq.1) then
      p00m=p00
      aex=1.

      if (ich0(ic).eq.3) then
      aex=0.4
      p00m=p00
      if (p00m.lt.100.) p00m=200.
      if (p00m.lt.100.) aex=1.1
      endif

      if (ich0(ic).eq.5) then 
         aex=0.4
      if (p00m.lt.500.) p00m=500.
      if (p00m.lt.500.) aex=0.4
      endif

      if (ich0(ic).eq.4) then
      aex = 1
      xxx = ((w0**2 - amn**2 + (2.d0*api)**2)/2.d0/w0)**2
     &     - (2.d0*api)**2
      if(xxx.lt.20.d0) then
        p00m = 500
      else
        p00m = sqrt(xxx)
      end if

      do ip=1,np
       zp(ip,ic,ie) = p00m*((1.+xx(ip))/(1.-xx(ip)))**aex
       zwp(ip,ic,ie)= p00m*2./(1.-xx(ip))**2*wx(ip)
     1              *((1.+xx(ip))/(1.-xx(ip)))**(aex-1.)*aex
      end do

      endif

      do ip=1,np
      zp(ip,ic,ie)=p00m*((1.+xx(ip))/(1.-xx(ip)))**aex
      zwp(ip,ic,ie)=p00m*2./(1.-xx(ip))**2*wx(ip)
     1 *((1.+xx(ip))/(1.-xx(ip)))**(aex-1.)*aex
      end do
c-----------------------------------------------
c MAP 2
c 
      else if (imap.eq.2) then 
      POF=atan(1.0D0)
      SCL=w0/2.
      check=0.
      DO ip=1,NP
       zp(ip,ic,ie)=tan(POF*(1.+XX(ip)))
       zwp(ip,ic,ie)=SCL*POF*(1.+zp(ip,ic,ie)**2)*WX(ip)
       zp(ip,ic,ie)=zp(ip,ic,ie)*SCL
      ENDDO
      endif

c----------------------------------------------------

      check=0.
      do ip=1,np
       uch=100.
      check=check+2.*(zp(ip,ic,ie)/uch)*
     1       (zwp(ip,ic,ie)/uch)*exp(-(zp(ip,ic,ie)/uch)**2)
      end do


       zp0(ic,ie)=p00
       zp(np1,ic,ie)=p00
       zwp(np1,ic,ie)=0.

       if(istab(ic).eq.0)then
       if(w0.gt.(a1+a2))then
       e1=sqrt(a1**2+p00**2)
       e2=sqrt(a2**2+p00**2)
       cc=-pi*e1*e2/(e1+e2)/p00
       sum=0.         ! zsum
       do ip=1,np
       px=zp(ip,ic,ie)
       ww=zwp(ip,ic,ie)
       sum=sum-ww/(p00**2-px**2)
       end do
       sum=sum*2.*(e1*e2)/(e1+e2)
       
       zwp(np1,ic,ie)=sum + (0.d0,1.d0)*cc
       end if
       end if
c================================================================
       end if

    2  continue
    1  continue

       return
       end
c-----------------------------------------------------

