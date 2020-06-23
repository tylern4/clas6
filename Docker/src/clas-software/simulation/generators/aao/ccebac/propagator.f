       subroutine propagator

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)

       implicit real*8(a-h,o-y)
       implicit complex*16(z)
       complex*16 wp,green,p,p0,e0
       complex*16 w,px,e1,e2

       common/mesh00/nc,ne0,np1,ich(maxmb),
     1   e0(maxwcm),am1(maxmb),am2(maxmb),istab(maxmb)
     1  ,p(maxmom,maxmb,maxwcm),wp(maxmom,maxmb,maxwcm)
     1  ,p0(maxmb,maxwcm),green(maxmom,maxmb,maxwcm)

       dimension zmass(maxmom,maxmb,maxwcm)
c-------------------------------------------------
c       data ical/1/
c       if(ical.ne.0)then


       call sigma(zmass)
c

       do 300 ie=1,ne0
c      write(*,*)' w=',e0(ie)
       do 300 ic=1,nc
       ichanl=ich(ic)
c      write(*,*)' p zmass for ic=',ichanl
c      write(*,1244)(p(ip,ic,ie),zmass(ip,ic,ie),ip=1,np1)
 1244  format(10e12.4)
  300  continue
c       ical=0
c       end if

       do 1 ie=1,ne0
       w=e0(ie)
       do 1 ic=1,nc
       ichanl=ich(ic)
       a1=am1(ic)
       a2=am2(ic)
       do 2 ip=1,np1
       px=p(ip,ic,ie)
       e1=sqrt(a1**2+px**2)
       e2=sqrt(a2**2+px**2)
       green(ip,ic,ie)=1./(w-e1-e2-zmass(ip,ic,ie))
    2  continue
    1  continue

c
c checks possible numerical problems due miscalculation of zmass
c
       do ie=1,ne0
       do ic=1,nc
       if (istab(ic).eq.1) then 
        firs=real(green(1,ic,ie))
        do ip=2,np1
        val=real(green(ip,ic,ie))
c checks sign change
        if (firs*val.lt.0.) then 
          if (abs(imag(zmass(ip,ic,ie))).le.1.E-10) then 
             print*,"Possible problem: sign change with"
             print*,"try using more GL points or changing"
             print*,"SCL in tangens mapping"
             write(6,444) ic,e0(ie),p(ip-1,ic,ie),val 
     1       ,imag(zmass(ip,ic,ie))
             write(6,444) ic,e0(ie),p(ip,ic,ie),real(green(ip-1,ic,ie))
     1        ,imag(zmass(ip-1,ic,ie))
 444     format(I2,2x,'w=',2f8.3,' p:',2E14.6,' re(g):',E14.6
     1           ,' im(sig):',2E14.6)
          endif
         firs=val
        endif
        enddo
       endif
       enddo
       enddo


 1223  format('channel:',I2,' energy:',E14.6,' Im(zmas)',E14.6)

       return
       end

