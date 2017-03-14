      subroutine allk_output
      
      include 'mp.inc'
      real*8 wcmmev,qsgev,fact,ebeam,qmin,wmin,delq,delw
      common / c2001 / wcmmev(1000),qsgev(1000),fact(1000),
     &ebeam,qmin,wmin,delq,delw,meshq,meshw,imaxdt
      
      real rm_p,mpi2,rmp2,m_pi(2),nu
      logical iupd
      
      data rmp/0.93828/
      data m_pi/0.1349764,0.13957/
      data pi/3.14159/
      
      rmp2 = rmp**2
      eps = 0.0

      print *, 'ALLK_OUTPUT: Multipole table at Q2=',qsgev(1),' Ebeam = ',ebeam
      
      open(unit=21,file='allk-pi0.tbl',type='UNKNOWN')
      open(unit=22,file='allk-pip.tbl',type='UNKNOWN')
      open(unit=25,file='rspf-allk-pi0.tbl',type='UNKNOWN')
      open(unit=26,file='rspf-allk-pip.tbl',type='UNKNOWN')
      iupd = .true.
      
      do iw=1,imaxdt
        w   = wcmmev(iw)/1000.
        q2  = qsgev(iw)
        nu  = (w*w+q2-rmp2)/2./rmp
        if (ebeam.gt.0) then
          eep  = ebeam-nu
          eps = 1./(1.+2.*(q2+nu**2)/(4.*ebeam*eep-q2))
        endif
        if (nu.gt.0) then
c        do ic=1,10
        do ic=180,2,-2
        coscm=-0.9+(ic-1)*0.2
c        coscm=-1.0+(ic-1)*0.05
        coscm=cos(ic*3.14159/180.)
        do if=1,24
        phi=7.5+float(if-1)*15
        iupd = .true.
        call allk_run(q2,w,eps,coscm,phi,1,iupd,iw) 
        write(21,100) w,q2,eps,coscm,phi,robs(1,1),robs(1,1)/10.,dum,dum,dum   
        iupd = .true.    
        call allk_run(q2,w,eps,coscm,phi,2,iupd,iw) 
        write(22,100) w,q2,eps,coscm,phi,robs(1,2),robs(1,2)/10.,dum,dum,dum
        enddo
        write(25,101) w,q2,eps,coscm,robs(6,1),robs(7,1),robs(8,1),robs(9,1),
     1  robs(4,1),robs(10,1),robs(11,1),robs(12,1),robs(13,1),robs(14,1)
        write(26,101) w,q2,eps,coscm,robs(6,2),robs(7,2),robs(8,2),robs(9,2),
     1  robs(4,2),robs(10,2),robs(11,2),robs(12,2),robs(13,2),robs(14,2)
        enddo
        endif
      enddo
      
100   format(5f10.3,5e15.5) 
101   format(4f7.3,10e12.3)

      close(21)
      close(22)
      close(25)
      close(26)
      
      return
      
      open(unit=25,file='sigt-allk-pi0.tbl',type='UNKNOWN')
      open(unit=26,file='sigt-allk-pip.tbl',type='UNKNOWN')
      
      do w=1.08,1.34,0.005
        iupd = .true.
        call allk_run(0.0,w,0.0,0.0,0.0,1,iupd,0)
        call sigtot(sigt,sig32)
        write(25,102) w,sigt,sig32
        iupd = .true.
        call allk_run(0.0,w,0.0,0.0,0.0,2,iupd,0)
        call sigtot(sigt,sig32)
        write(26,102) w,sigt,sig32
      enddo
      
102   format(f7.3,2e12.3)
      
      close(25)
      close(26)

      end