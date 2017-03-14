      subroutine aao_test(epq2,epw,epeps,epcos,epphi,epirea,
     *sigma0,sigu,sigt,sigl,sigi)
     
c
c--------------------------------------------------------------------------------------
c
c   Author  : Kyungseon Joo
c   Date    : Jan.  2000
c   Purpose : Calculate xsection and response functions for give kinematics
c             using fitting parameters from PI_ANA program.
c
c--------------------------------------------------------------------------------------
c
	  IMPLICIT REAL*8 ( A - H , O - Z )
	  COMPLEX*16 AP,AM,BP,BM,CP,CM
	  COMPLEX*16 H(6),H1R,H2R,H3R,H4R,H5R,H6R
      COMMON
     *  /BP/  BP,BM
     *  /AP/  AP,AM
     *  /CP/  CP,CM
     *  /KIN/ Q,ZZK
     *  /XMINUIT/ XMINUIT
c
      DIMENSION a0(40), a1(40), a2(40)
	  DIMENSION AP(0:10),AM(0:10),BP(0:10),BM(0:10),CP(0:10),CM(0:10)
	  DIMENSION XMINUIT(500)
	  
      real epq2,epw,epeps,epcos,epphi,epirea,
     *sigma0,sigu,sigt,sigl,sigi  
      integer index(45), j, num_parm, nopt
      character*11 dum
      character*80 file
      real dum1
      data num_parm /45/
c
	  REAL*8 MPI,PI,MP
	  DATA MPI /.1349D0/, PI/3.141592654D0/, MP/.938D0/, nopt/0/
c
c      print *, epeps,epq2,epw,epcos,epphi
      eps = epeps
      q2  = epq2
      w   = epw
      theta = acos(epcos)*180/pi
      fi = epphi
      
c      read(5,*) ebeam
c      read(5,*) W
c      read(5,*) Q2
c      read(5,*) theta
c      read(5,*) fi

      if (nopt.eq.0) then
        
      do i = 1, 500
        xminuit(i) = 0.0d0
      enddo
      
      print *, 'Read from KJOO fit (1) or PWA fit(2): '
      read *, nopt
      
      if (nopt.eq.1) then

        open(unit=11, file='no_parm.dat', status='old')
        open(unit=12, file='test.dat', status='old') 
      
        do i = 1, num_parm
          read(11,*) index(i)
        enddo

        do i = 1, num_parm
          read(12,*) a0(i), a1(i), a2(i)
        enddo
     
        close(11)
        close(12)       

        do i = 1, num_parm
          j = index(i)
          xminuit(j) = a0(i) + a1(i)*Q2 + a2(i)*Q2**2
        enddo 
      else
        
        print *, 'Enter file name: '
        read *, file
        open(unit=11, file=file,status='old')
        
        do j = 1,500
        read(11,952,end=10) index(j),dum,xminuit(index(j))
        enddo
952     FORMAT (I4,1X,A11,G14.5)
10      close(11)

      endif
      do j = 1,45
        print *, index(j),xminuit(index(j))
      enddo
      endif
      
      IREA= 1    ! this program is for single pi0 production
c
	  THET= XMINUIT(64)*PI/180.
      if (thet.gt.1.e-4) then
	    XMINUIT(3)=XMINUIT(1)*DSIN(THET)/DCOS(THET)
	    XMINUIT(21)=XMINUIT(19)*DSIN(THET)/DCOS(THET)
	    theta111=datan(.90839*dtan(thet))
      endif
c
	  F=0.D0
c
c      anu = (W*W + Q2 - mp**2)/(2*mp)
c      s2 = Q2 / (4*ebeam*(ebeam - anu))
c      t2 = s2 / (1 - s2)
c      eps = 1./(1+2*(1+anu*anu/Q2)*t2)
c      print *, 'eps= ', eps
c
  	  Q=DSQRT(((W**2-MP**2+MPI**2)/(2.*W))**2-MPI**2)
	  ZZK=(W**2-MP**2)/(2.*W)
	  CALL APRES(IREA,Q2,W)
	  CALL AMRES(IREA,Q2,W)
	  CALL BPRES(IREA,Q2,W)
	  CALL BMRES(IREA,Q2,W)
	  CALL CPRES(IREA,Q2,W)
	  CALL CMRES(IREA,Q2,W)
c
	  FAKT=2.*W*Q/(W**2-MP**2)
c
	  DO J=1,6
c	    H(J)=DCMPLX(HIBORN(J,IREA,Q2,W,THETA))
        H(J)=(0.0d0,0.0d0)
	  ENDDO
c
c          print *, q2,w,theta,fi,eps
          H(1)=H(1)+H1R(IREA,Q2,W,THETA)
	  H(2)=H(2)+H2R(IREA,Q2,W,THETA)
	  H(3)=H(3)+H3R(IREA,Q2,W,THETA)
 	  H(4)=H(4)+H4R(IREA,Q2,W,THETA)
	  H(5)=H(5)+H5R(IREA,Q2,W,THETA)
	  H(6)=H(6)+H6R(IREA,Q2,W,THETA)
c
	  X1=0.D0
	  DO J=1,4
	    X1=X1+CDABS(H(J))**2
	  ENDDO
c
	  X1=X1/2.D0
	  X2=CDABS(H(5))**2+CDABS(H(6))**2
	  X3=DREAL(H(2)*DCONJG(H(3))-H(1)*DCONJG(H(4)))
c      X4=DSQRT(2.D0)*
c     *  DREAL(H(5)*DCONJG(H(4)-H(1))+H(6)*DCONJG(H(3)+H(2)))
      X4=DSQRT(2.D0)*
     *   DREAL(H(5)*DCONJG(H(1)-H(4))+H(6)*DCONJG(H(3)+H(2)))
c
      FI=FI+180
      if (FI.gt.360) FI=360-FI
      F1=FAKT*(X1+EPS*X2+EPS*X3*DCOS(2.*FI*PI/180.)+
     *   DSQRT(EPS*(EPS+1.)/2.)*X4*DCOS(FI*PI/180.))
c      print *, X1, X2, X3, X4
c      print *, 'Cross Section=', F1, ' ubarn/sr'
      
      sigma0 = F1
c
	  END

