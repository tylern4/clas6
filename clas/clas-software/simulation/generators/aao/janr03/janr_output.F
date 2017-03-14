      subroutine janr_output
      
      real q2,mpion(2),mpa,mpb,mp2,mp,nu,qpi1,qpi2
      data mpion/0.13498,0.13957/
      data nw/60/
      data mp/0.93827/
      data dum/0.0/
      
#include "janrtab.inc"  
#include "res_func.inc"    

      q2  = 0.5*(q2min+q2max)
      mpa = mpion(1)**2
      mpb = mpion(2)**2
      mp2 = mp**2
      eps = 1.0
      
      print *, 'JANR_OUTPUT: Tables at Q2=',q2,' Ebeam = ',ebeam
      
      open(unit=23,file='mf1g-jan-pi0.tbl',type='UNKNOWN')
      open(unit=24,file='mf1g-jan-pip.tbl',type='UNKNOWN')
      open(unit=25,file='rspf-jan-pi0.tbl',type='UNKNOWN')
      open(unit=26,file='rspf-jan-pip.tbl',type='UNKNOWN')
      
      do iw=1,nw
        w   = 1.1 + float(iw-1)*0.01
        if (ebeam.gt.0) then
          nu  = (w*w+q2-mp2)/2./mp
          ep  = ebeam-nu
          eps = 1./(1.+2.*(q2+nu**2)/(4.*ebeam*ep-q2))
        endif
        if (eps.gt.0) then
        qpi1=sqrt(((w*w-mp2+mpa)/2./w)**2-mpa)
        qpi2=sqrt(((w*w-mp2+mpb)/2./w)**2-mpb)
        ratio = qpi2/qpi1        
        do ic=1,10
        coscm=-0.9+(ic-1)*0.2
        do if=1,24
        phi=7.5+float(if-1)*15
        call janr_run(w,q2,eps,coscm,phi,.true.) 
        if (mod(iw,2).gt.0) then
        write(23,100) w,q2,eps,coscm,phi,sigma(1),sigma(1)/10.,dum,dum,dum
        write(24,100) w,q2,eps,coscm,phi,sigma(2),sigma(2)/10.,dum,dum,dum
        endif
        enddo
        write(25,101) w,q2,eps,coscm,robs(6,1),robs(7,1),robs(8,1)
        write(26,101) w,q2,eps,coscm,robs(6,2),robs(7,2),robs(8,2)
        enddo
        endif
      enddo
      
100   format(5f10.3,5e15.5) 
101   format(4f10.3,3e15.5)

      close(23)
      close(24)     
      close(25)     
      close(26)     
      
      end
                      
