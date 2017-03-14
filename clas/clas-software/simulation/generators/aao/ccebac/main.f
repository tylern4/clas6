c
c bjulia 17 10 2007, uses pgplot to produce plots
c
c
c bjd, added isw4 to recompute only bareampl
c
c
c modified to build chi2 also with total cross section
c
c modified to build chi2 comparing to photoproduction data
c
c ts gamma-eta cross section 
c
c  CCEBAC + readdata-v4.f
c
c ts 11-3-2007 extend LSJ (modify cc.inp2, init)
c
c  njmx = j_max+1/2
c  d-functions are prepared up to 2j = njmx*2-1
c
c  max(2J) of strong interaction potential is mxj defined in init 
c             consistent with max of  cc.inp
c  max(2J) of elmag brn  is jjmx in common/cmxj, defined in init
c             consistent with  cc.inp2  2*lmx2+1
c
  
      IMPLICIT REAL*8(A-H,O-y)
      IMPLICIT COMPLEX*16(Z)

      parameter (maxpar=500,maxmb=5 ,maxres=5,maxlsj=20,maxl=10)
      parameter (maxwcm=40,maxq2=20,maxmom=50,maxtheta=24)

      dimension par(maxpar),hpar(6)
      common/mesh00/nc,ne0,np1,ich(maxmb),
     1   ze0(maxwcm),am1(maxmb),am2(maxmb),istab(maxmb)
     1  ,zp(maxmom,maxmb,maxwcm),zwp(maxmom,maxmb,maxwcm)
     1  ,zp0(maxmb,maxwcm),zgreen(maxmom,maxmb,maxwcm)

      character*11 saidpwf(2)
      
      common/tsato0824/nchxx
      common / ctest / ieout,iqout
      common /onlybare/ isw4
      common /noprintout/ isw5
      common /etapi / jetapi
      common/lcs1/ipar,sca,ifit
      common/saidpw/ said(10,400), nsaid1, nsaid, saidpwf
      
      data saidpwf/'NONE','NONE'/
      
      EXTERNAL FCNdsg

      iqout = 1
      ieout = 1

      jetapi = 1  ! 1=pi N  2=eta N
      isw0   = 1  ! 1=Matsuyama mass (CCEBAC) 2=SL mass
      isw1   = 1  ! 1=CCEBAC momentum/weight 2=complex pole search
      isw2   = 2  ! -1=strong only 0=t,gamma 1=write t 2=read t
      isw3   = 0  ! 0=formatted 1=unformatted
      isw4   = 0  ! 0=all 1=bare only
      isw5   = 2  ! 1=no output in fort.400 fort.500 
      ifit   = 1  ! 1=fit 2=tables
      
      read(*,*) ifit,isw2,nchxx,ipar,sca,saidpwf(1),saidpwf(2)
      
c========================================================
c  strong interaction part
c=========================================================
  
c   initial parameters

c   cc.inp2   16
c   cc.inp    15
c   cc_minput 79

      open(unit=16,file='cc.inp2',  status='old')
      open(unit=15,file='cc.inp'   ,status='old')
      open(unit=79,file='cc_minput',status='old')

      call init(par,npar,isw0)

      close(unit=16)
      close(unit=15)
      close(unit=79)

c  restore parameters

      grad  = 0
      chi   = 0
      iflag = 3

      call fcn(npar,grad,chi,par,iflag)

c  calculate t-matrix

      if(isw2.ne.2) call caltmat(iflag,isw1)

c tsato 08-26-2007
      if(isw2.eq.-1) stop
c tsato 08-26-2007

      if(isw2.eq.1) then
      if(isw3.eq.0) then
      open(unit=51,file='f51',form='formatted'
     &    ,status='unknown')
      else
      open(unit=51,file='f51.u',form='unformatted'
     &    ,status='unknown')
      end if

      call tmatwrite(isw3)

      close(unit=51)
      stop
      end if

c  read necesary t-matrix and Gamma

      if(isw2.eq.2) then

      call momentum(isw1)

      call setsato

      if(isw3.eq.0) then
      open(unit=51,file='f51',form='formatted'
     &    ,status='old')
      else
      open(unit=51,file='f51.u',form='unformatted'
     &    ,status='old')
      end if

      call tmatread(isw3)

      close(unit=51)

      end if

c==========================================================
c   electromagnetic interaction part
c==========================================================

      call resonance    ! prepare resonance parameter of CLAS

c  Meson-cloud = \sum_{MB} \bar{\Gamma} |MB><MB| G_0 v_{MB,\gamma N}

c    nchx = 0   all MB states
c           1   only piN state

       open(unit=21,file='crs.dat',status='unknown')
c       open(unit=22,file='crs_xmgr',status='unknown')
c       open(unit=23,file='sigma_xmgr',status='unknown')

      grod=0
      npar=6

      OPEN (UNIT=76,FILE='cc_rub_hel',STATUS='UNKNOWN')
      OPEN (UNIT=77,FILE='cc_mou_hel',STATUS='UNKNOWN')
      OPEN (UNIT=78,FILE='cc_min_hel',STATUS='OLD')

      IPUNCH=76
      IWRITE=77
      IREAD=78

      CALL MINTIO(IREAD,IWRITE,IPUNCH)
      CALL MINUIT(FCNDSG,0) 
      
      call outemamp

      close(unit=21)

      stop
      end
