c bjulia 
c 1 Dec 2006, now computes
c             as in equations 59-61 of Matsuyama, Sato, Lee 
c
c 31 Dec 2006
c             jacobian included

      subroutine sigma(zmass)
      implicit real*8(a-h,o-y)
      implicit complex*16(z)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)

       complex*16 e0,wp,green,p,p0,pxx
       complex*16 w,px,espec,omega2,omega,sk0
       complex*16 e1,e2,cc,sum,ww

       common/mesh00/nc,ne0,np1,ich(maxmb),
     1   e0(maxwcm),am1(maxmb),am2(maxmb),istab(maxmb)
     1  ,p(maxmom,maxmb,maxwcm),wp(maxmom,maxmb,maxwcm)
     1  ,p0(maxmb,maxwcm),green(maxmom,maxmb,maxwcm)

       dimension zmass(maxmom,maxmb,maxwcm)
       dimension aam1(5),aam2(5)
       data aam1/0.0d0,0.0d0,138.5d0,138.5d0,138.5d0/
       data aam2/0.0d0,0.0d0,938.5d0,138.5d0,138.5d0/
       dimension xx(100),wxx(100),zwsk(100)
       complex sk(100)
       data ical/1/
c
c     specified channels are:
c     ich(i) =1 --- pi N
c             2 --- eta N
c             3 --- delta pi
c             4 --- sigma N
c             5 --- rho N
c
c       stop
       if(ical.eq.1)then
       nx=24
       pi=4.*atan(1.)
       call gaussl(nx,xx,wxx)
       ical=0
       end if
c
       zmass = 0
cbjulia

       do 1 ie=1,ne0
       w=e0(ie)
       do 2 ic=1,nc
       ichanl=ich(ic)
       if(ichanl.le.2)go to 2
       do 3 ip=1,np1
       px=p(ip,ic,ie)
       espec=sqrt(am2(ic)**2+px**2)
       omega2=(w-espec)**2-px**2
       if(dble(omega2).lt.1.d0)omega2=1.d0
       omega=sqrt(omega2)
       a1=aam1(ichanl)
       a2=aam2(ichanl)
       call onshell(omega,a1,a2,sk0)


ccc ---------------------------------------------
ccc several mappings, one of them involving 
ccc complex rotation
ccc ---------------------------------------------

       imap=3
       if (imap.eq.0) then 
c original mapping
      aex=1.
        do ix=1,nx
        sk(ix)=sk0*((1.+xx(ix))/(1.-xx(ix)))**aex
        zwsk(ix)=sk0*2./(1.-xx(ix))**2*wxx(ix)
     1  *( (1.+xx(ix))/(1.-xx(ix)) )**(aex-1.)*aex
        end do

       else if (imap.eq.1) then 
c tangent mapping
      POF=atan(1.0D0)
      SCL=100.
c      check=0.
      DO ix=1,NX
       sk(ix)=tan(POF*(1.+XX(ix)))
       zwsk(ix)=SCL*POF*(1.+sk(ix)**2)*WXX(ix)
       sk(ix)=sk(ix)*SCL
c      uch=100.
c      check=check+2.*(p(ip,ic,ie)/uch)*
c    1       (wp(ip,ic,ie)/uch)*exp(-(p(ip,ic,ie)/uch)**2)
      ENDDO

      else if (imap.eq.2) then 
c AM mapping
      aex=1.
        do ix=1,nx
        sk(ix)=sk0*((1.+xx(ix))/(1.-xx(ix)))**aex
        zwsk(ix)=sk0*2./(1.-xx(ix))**2*wxx(ix)
     1  *( (1.+xx(ix))/(1.-xx(ix)) )**(aex-1.)*aex
        end do
      else if (imap.eq.3) then 
      aex=1.
      rot=0.2
      zr=dcmplx(cos(rot),-sin(rot))
        do i=1,nx
        ssc=500.
        sk(i)=ssc*((1.0d0+xx(i))/(1.0d0-xx(i)))**aex *zr
        zwsk(i)=sk(i)*2.0d0*aex/(1.0d0-xx(i)**2)*wxx(i)
      enddo
      endif

c      print*,"mesh check ie,ic",ie,ic, check," should be 1"

c       do ix=1,nx
c       sk(ix)=sk0*(1.+xx(ix))/(1.-xx(ix))
c       zwsk(ix)=sk0*2./(1.-xx(ix))**2*wxx(ix)
c       end do
       nx1=nx+1
       sk(nx1)=sk0
       zwsk(nx1)=0.

       if(dble(omega).gt.(a1+a2))then
       e1=sqrt(a1**2+sk0**2)
       e2=sqrt(a2**2+sk0**2)

       cc=-pi* sqrt((e1+e2)**2+px**2) *e1*e2/(e1+e2)**2/sk0

       sum=0.
       do ix=1,nx
       pxx=sk(ix)
       ww=zwsk(ix)
       sum=sum-ww/(sk0**2-pxx**2)
       end do
       sum=sum*2.*sqrt((e1+e2)**2+px**2)*e1*e2/(e1+e2)**2
       zwsk(nx1)=sum + (0.d0,1.d0)*cc
       end if

c no substraction for complex rotated
       if (imap.eq.3) zwsk(nx1)=0.


       zsum=0.
       do ix=1,nx1
       zskk=sk(ix)
       ze1=sqrt(a1**2+zskk**2)
       ze2=sqrt(a2**2+zskk**2)
       zg=zvertex(zskk,ichanl)
c        print*,zg*zg
       zgreen=1.


       if(ix.ne.nx1)zgreen=1./(w-espec-sqrt((ze1+ze2)**2+px**2))


       isato=1
       if(isato.eq.1) then
c sigma N modification, Eq 1.11 of trans-lee
       if(ichanl.eq.4) then
        zmpp=ze1+ze1
        zesig=sqrt(am1(ic)**2+px**2)
        zxx=am1(ic)/zesig * zmpp /sqrt( zmpp**2+px**2)
c pi D modification Eq 1.18 of trans-lee
       else if (ichanl.eq.3) then 
        zmpn=ze1+ze2
        zedel=sqrt(am1(ic)**2+px**2)
        zxx=am1(ic)/zedel * zmpn /sqrt( zmpn**2+px**2)
c pi D modification Eq 1.18 of trans-lee
       else if (ichanl.eq.5) then 
        zmpp=ze1+ze2
        zerho=sqrt(am1(ic)**2+px**2)
        zxx=am1(ic)/zerho * zmpp /sqrt( zmpp**2+px**2)
       end if
       else
       zxx = 1
       end if

c         print*,zxx
       zsum=zsum+zg**2*zskk**2*zgreen*zwsk(ix)*zxx

       end do
       zmass(ip,ic,ie)=zsum
c       if (imag(zsum).gt.0) print*,"p,ie:",ip,ie,"chan:",ic,zsum
c       write(40,3333) px,w,zsum,sum
c 3333  format(5E14.6)
    3  continue
    2  continue
    1  continue
c       stop
       return
       end

