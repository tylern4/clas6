      program keppel_norad

c     This program makes an n-tuple that can be used with Paw to
c     make distributions of energies, angles, resonance
c     mass for ep inclusive scattering
c      The n-tuple contains the photon energy(EG), the true hadronic invariant
c     mass (W), 

      implicit none
#include "bcs.inc"
#include "mc.inc"
#include "names.inc"

      COMMON/ALPHA/ ALPHA,PI,MP,MEL,WG
      common /radcal/T0,es,ep,ps,pp,rs,rp,u0,pu,uu,cst0,snt0,csths,csthp
     * ,snths,snthp,pdotk,sdotk
      common /random/idum
      real*8 alpha,pi,mp,mel,wg,T0
      real*8 es,ep,ps,pp,rs,rp,u0,pu,uu,pdotk,sdotk
      real*8 cst0,snt0,csths,csthp,snths,snthp

      real beam_time,beam_time_max,beam_print
      real ebeam
      real epmax
      real eprng
      real epw
      real ep_min,ep_max,ep_test
       real ep_sav
      real events
      real fmcall
      real jacob
      real phir
      real px,py
      real q0
      real q2
      real q2_min,q2_max,q2max
      real qsq
      real qvecx
      real qvecz
      real ran
      real rotc,rots
      real rtest
      real s
      real signr
      real sig_ratio
      real sigr_max
      real*8 sig_tot,sig_sum
      real stest
      real t_elapse
      real th0
      real*8 th0_deg
      real theta
      real tries
      real itime1, itime2
      real uq2, uq2_min,uq2_max,uq2rng
      real w2
      real sig_int
      real*8 sig1(2),sig2(2),sig_nres(2),sig_res(2),
     &  sig_nrs(2),sig_rs(2),sigroper(2),w2_keppel
      logical goroper

      integer ir1
      integer iext
      integer j
      integer jj
      integer mcall
      integer mcall_max
      integer ntell,ntold

      integer*4 idum
      integer*4 ntries

c     Parameters for the n-tuple

      common /pawc/h(5000000)
      integer h,n,nevent,lrecl,istat,icycle
      parameter (n=8)
      real*4 ntp(n)
      
c     tag is the an array of names for the variables in the n-tuple.

      character*1 ich1
      character*3 month
      character*2 day
      character*2 year
      character*5 tag(n)
      character*30 filerz
      character*31 file_out
      character*31 file_sum
      character*31 file_bos
      character*8 recname
      character*28  ctime

      DATA PI   /3.1415926/
      DATA MP   /.938/
      DATA MEL  /.511E-3/
      data wg   /1.073/

      data filerz   /'keppel-noradg-1.0-00jan00.rz.0'/
      data file_out /'keppel-noradg-1.0-00jan00.out.0'/
      data file_sum /'keppel-noradg-1.0-00jan00.sum.0'/
      data file_bos /'keppel-noradg-1.0-00jan00.evt.0'/
      data ctime    /'                            '/

      goroper=.false.

c     set parameters
      alpha=1/137.

c     set up parameters for bos bank input to GSIM
      npart=1

      q(1) 	= -1
      id(1)	= 3		!Geant ID, e-
      pdgid(1)  = 11		!PDG ID, e-
    

      write(6,*)' Input the incident electron energy(GeV)'
      read(5,*)ebeam
      
c     calculate the incident momentum
      es = ebeam
      ps = sqrt(es**2-mel**2)
      rs = ps/es
      
c     cut off q2 at the value for 90 degree elastic scattering
      s  = .5
      q2max = 4.*ebeam**2*s/(1.+2.*ebeam*s/mp)
      
c     Choose two limits for Q**2
      write(6,*)'Input lower and upper limit for Q**2'
      read(5,*)q2_min,q2_max
      
      if (q2_max .gt. q2max)q2_max=q2max
      uq2_min = 1/q2_max
      uq2_max = 1/q2_min
      uq2rng  = uq2_max-uq2_min

c     Set the limits on the range of scattered electron energies.
      write(6,*)'Input lower and upper limit for scattered electron',
     * ' energy(GeV).'
      read(5,*)ep_min,ep_max
      
      epmax = es-(wg**2+q2_min-mp**2)/2./mp
      if (ep_max .lt. epmax)epmax=ep_max
      eprng = epmax-ep_min

c     Select the number of events desired in the rz file.
      write(6,*)' Input the desired beam time'
      read(5,*)beam_time_max
      
      beam_print = beam_time_max/25

      write(6,*)' Input a multiplication factor for sigr_max'
      read(5,*)fmcall
      if (fmcall .eq. 0.)then
         write(6,*)' Input sigr_max'
         read(5,*)sigr_max
      endif

 1    mcall_max = 0
      ntold     = 0
      events    = 0

c     Use the internal clock to initialize the random number generator

      call timex(itime1)
      call getunixtime(idum)
      call getasciitime(idum,ctime)
      idum=-idum
      month = ctime(5:7)
      day   = ctime(9:10)
      year  = ctime(23:24)
      if (day(1:1).eq. ' ')then
        ir1=48
        day(1:1)=char(ir1)
      endif
      
      write(6,*)'seed:',idum,' from start time ',ctime
      sig_int=ran(idum)

      nevent   = 0
      t_elapse = 0.
      itime2   = itime1
      ntries   = 0
      sig_int  = 0.
      sig_tot  = 0.

c     Name the output rz file according to beam energy.
c     filerz=keppel-radgen-1.6-0811.rz.0, for example.

      ir1   = ebeam
      ir1   = ir1+48
      ich1  = char(ir1)
      ir1   = ir1-48

      filerz(15:15) = ich1
      ir1           = 10*(ebeam-ir1)
      ir1           = ir1+48
      ich1          = char(ir1)

      filerz(17:17) = ich1

      file_out(13:17) = filerz(13:17)
      file_bos(13:17) = filerz(13:17)
      file_out(19:20) = day
      file_out(21:23) = month
      file_out(24:25) = year
      filerz(19:25)   = file_out(19:25)
      file_bos(19:25) = file_out(19:25)

      iext = 48
 17   open(unit=12,err=18,name=file_out,status='new')
      go to 19
      
 18   iext=iext+1
      if (iext .gt. 57)then
         write(6,*)' no output file extensions available'
         go to 99
      endif
      
      ich1=char(iext)
      file_out(31:31)=ich1
      go to 17
      
 19   continue
      filerz(30:30)=file_out(31:31)
      file_bos(31:31)=file_out(31:31)
      
c     Initialize BOS     
c     bosout = file_bos
c     recname = 'MCEVENT'
c     call bos_ini(recname)
  
c     set up the ntuple file

      lrecl=8191
      call hlimit(5000000)
      call hropen(1,'aaonoradgen',filerz,'n',lrecl,istat)
      call hbset('BSIZE',8176,istat)
      call hbnt(10,'aaonoradgen',' ')
      call hbname(10,'func1',ntp(1),'es,ep,thete,w,
     1qx,qz,q0,csthe')

      open(unit=12,file=file_out,status='new')

      write(12,*)' Keppel Calculation of Inclusive ep scattering'
      write(12,*)'starting time:', ctime
      write(12,*)' Incident electron energy =',ebeam,' GeV'
      write(12,*)'Electron Q**2 limits:',q2_min,q2_max
      write(12,*)'lower and upper limit for scattered electron',
     * ' energy(GeV):',ep_min,epmax

      if (fmcall .eq. 0.)then
         write(6,*)' sigr_max from input data =',sigr_max
         go to 20
      endif

c     Do a preliminary calculation to estimate the maximum value
c     of the integrand

c     calculate the scattering angle (single precision and double precision
c     version) in radians

 10   q2=q2_min
 
c     calculate the energy and momentum of the scattered electron,
c     and calculate Q**2 at the delta mass, 1.232 GeV.

      q0   = (1.232**2-mp**2+q2)/2./mp
      ep   = es-q0
      pp   = sqrt(ep**2-mel**2)
      rp   = pp/ep
      s    = q2/4/es/ep
      th0  = 2.*asin(sqrt(s))
      th0_deg = th0*180./pi
      T0   = th0
      snt0 = sin(th0)
      cst0 = cos(th0)

      qsq  = q2

      call h2model_thia(es,ep,th0_deg,w2_keppel,sig1,sig2,
     &  sig_nres,sig_res,sig_nrs,sig_rs,goroper,sigroper)

      sigr_max = sig2(1)/1000.
      jacob    = q2**2/(2.*es*ep)
      sigr_max = jacob*sigr_max

      write(6,*)'sigr_max',sigr_max
      write(12,*)'sigr_max',sigr_max
      sigr_max = sigr_max*fmcall
 25   write(6,*)'sigr_max changed to',sigr_max
      write(12,*)'sigr_max changed to',sigr_max

c   %%%%%%%%%%%%%%%%%%% Main Calculation  %%%%%%%%%%%%%%%%%%%%%%%
c     Use a Monte-Carlo to calculate a distribution of events
c     distributed according to the e-p cross section.
      
 20   continue
      ntries=ntries+1

      es  = ebeam
      ps  = sqrt(es**2-mel**2)
      rs  = ps/es

      uq2 = uq2_min+uq2rng*ran(idum)
      q2  = 1./uq2

c     calculate the energy and momentum of the scattered electron,
c     and calculate Q**2

      ep  = epmax-eprng*ran(idum)
      
c     check to see if the scattered electron energy is below the
c     detector threshold.

      if (ep .lt. ep_min)go to 20

      q0  = es-ep
      s   = q2/4/es/ep
      
c     cut off scattering at 60 degree
      if (s .gt. .25)go to 20
      
c     calculate the scattering angle (single precision and double precision
c     version) in radians
      
      th0   = 2.*asin(sqrt(s))
      theta = th0*180./pi
      T0    = th0
      snt0  = sin(th0)
      cst0  = cos(th0)

c     check to see if the scattered electron energy is above
c     the pion threshold for this angle.

      ep_test = (mp**2+2*mp*es-wg**2)/2./(mp+2.*es*s)
      
      if (ep .gt. ep_test)go to 20
      pp      = sqrt(ep**2-mel**2)
      rp      = pp/ep

      qsq     = q2
      
      if (qsq .le. 0.)then
         write(6,*)' Main-1:, qsq =',qsq
         go to 20
      endif
      
      qvecx  = -pp*sin(th0)
      qvecz  = ps-pp*cos(th0)

      w2     = mp**2+2*mp*q0-q2
      if (w2 .lt. mp**2)go to 20
      
      epw    = sqrt(w2)
      if (epw .lt. wg+0.002)go to 20
      
c     calculate kinematic quantities needed for the Mo and Tsai calculation

      u0 = es-ep+mp

c     calculate the non-radiative cross section

      th0_deg=th0*180./pi
      call h2model_thia(es,ep,th0_deg,w2_keppel,sig1,sig2,
     &  sig_nres,sig_res,sig_nrs,sig_rs,goroper,sigroper)

c     convert cross section from nbarns to micro barns

      signr  = sig2(1)/1000.
      jacob  = q2**2/(2.*es*ep)
      signr  = jacob*signr

c     Compare signr to the sigr_max to determine whether to generate an event
      sig_ratio = signr/sigr_max
      sig_tot   = sig_tot+signr
      
c     Choose the number of times, mcall, to call the routine used
c     to calculate kinematic quantities for the n-tuple.
      rtest=ran(idum)
      mcall=sig_ratio
      stest=sig_ratio-mcall
      if (stest .gt. rtest)mcall = mcall + 1
       
      if (mcall .gt. mcall_max) mcall_max=mcall
      if (mcall .gt. 10)then
         write(6,*)' mcall =',mcall
         write(6,*)es,ep,th0

         write(12,*)' mcall =',mcall
         write(12,*)'es,ep',es,ep
      endif
      
c     If mcall .gt. 0 generate mcall n-tuple events.
      if (mcall .eq. 0)go to 30

      ep_sav = ep

      if (mcall .gt. 1000)then
         write(6,*)' mcall = ',mcall
         stop
      endif

      do j=1,mcall

c     correct the following section on Jan. 23, 1999
         if (ep .lt. ep_min)then
            sig_tot=sig_tot-sigr_max
            go to 24
         endif
c     end of correction

         w2=mp**2+2*mp*(ebeam-ep)-2*es*ep*(1-cos(th0))+2.*mel**2
         epw=sqrt(w2)

c     Calculate the members of the n-tuple and ouput it to the rz file.
         ntp(1)=es
         ntp(2)=ep
         ntp(3)=theta
         ntp(4)=epw
         ntp(5)=qvecx
         ntp(6)=qvecz
         ntp(7)=q0
         ntp(8)=cst0
         call hfnt(10)
         nevent=nevent+1

         do jj = 1,npart
           v(jj,1) = 0.
           v(jj,2) = 0.
           v(jj,3) = 0.
         enddo
         
c     rotate all the momenentum by a random angle around the beam line
         phir=2.*pi*ran(idum)
         rotc=cos(phir)
         rots=sin(phir)
          
c     momentum of scattered electron:
         px = ep*sin(theta*pi/180.)
         py = 0.
         p(1,1) = px*rotc+py*rots
         p(1,2) = py*rotc-px*rots
         p(1,3) = ep*cos(theta*pi/180.)
         p(1,4) = ep
c         call bos_out	! Pack the BOS banks and write out to file        

 24      continue
       enddo

c     Talk to the user now and then.
       events  = nevent
       tries   = ntries
       sig_int = events/tries
       
c     electron phase space =2*pi*uq2rng*eprng
       sig_int = sig_int*sigr_max*(2.*pi*uq2rng*eprng)
       sig_sum = sig_tot*(2.*pi*uq2rng*eprng)/tries
       
c      beam time in seconds for luminosity of 10**34
       beam_time = events/sig_sum*1.E-4
       ntell     = beam_time/beam_print
       ntell     = ntell-ntold

       if (ntell .gt. 0)then
          write(6,*)' ntries, nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max
          write(12,*)' ntries, nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max
          write(6,*)' Integrated cross section (MC, numerical) ='
     +   ,sig_int,sig_sum, ' mu-barns'
          write(6,*)' Beam time at Lum=1.0E34 =',beam_time
     *    ,' seconds'
          write(12,*)' Integrated cross section =',sig_int,sig_sum
     *    ,' micro-barns'
          write(12,*)' Beam time at Lum=1.0E34 =',beam_time
     *    ,' seconds'

          call timex(itime2)
          t_elapse=t_elapse+INT(itime2-itime1)
          itime1=itime2
          write(6,*)' Elapsed CPU time = ',t_elapse/60,' minutes'
          write(12,*)' Elapsed CPU time = ',t_elapse/60,' minutes'
          ntold=ntold+1
       endif

c     Do we have enough events in the n-tuple?
      if (beam_time .gt. beam_time_max)go to 50
       
30    go to 20

c     Close out the n-tuple file
 50   call hrout(0,icycle,' ')
      call hrend('aaonoradgen')

      close(12)

      file_sum(1:26)=file_out(1:26)
      file_sum(30:31)=file_out(30:31)
      open(unit=14,name=file_sum,status='new')
      write(14,*)' Keppel Calculation of Inclusive ep Scattering'
      write(14,*)' Starting time: ',ctime
      write(14,*)' Incident electron energy =',ebeam,' GeV'

      write(14,*)'Electron Q**2 limits:',q2_min,q2_max
      write(14,*)'lower and upper limit for scattered electron',
     * ' energy(GeV):',ep_min,epmax
      write(14,*)'sigr_max',sigr_max
      write(14,*)' ntries, nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max
      write(14,*)' Integrated cross section =',sig_int,sig_sum
     *    ,' micro-barns'
      write(14,*)' Beam time at Lum=1.0E34 =',events/sig_sum*1.E-4
     *    ,' seconds'
      write(14,*)' Elapsed CPU time = ',t_elapse/60,' minutes'
      write(14,*)' CPU time/event = ', t_elapse/nevent,' sec'

      close(14)
      
c     CALL bos_end(recname)

 99   continue

      stop
      end
      
c    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      function ran(idum)
c     Random number generator used because I can't find one in the
c     library.

      implicit none
      integer*4 idum
      integer*4 mbig,mseed, mz
      real ran,fac
      parameter (mbig=1000000000, mseed=161803398,mz=0,fac=1./mbig)
      integer*4 i,ii,inext,inextp,k
      integer*4 mj,mk,ma(55)
      save inext,inextp,ma

c     Initialization section:
      if (idum .lt. 0.)then
         mj=mseed-idum
         mj=mod(mj,mbig)
         ma(55)=mj
         mk=1
         do  i=1,54
            ii=mod(21*i,55)
            ma(ii)=mk
            mk=mj-mk
            if(mk .lt. mz)mk=mk+mbig
            mj=ma(ii)
         enddo
         do k=1,4
            do i=1,55
               ma(i)=ma(i)-ma(1+mod(i+30,55))
               if(ma(i) .lt. mz)ma(i)=ma(i)+mbig
            enddo
         enddo
         inext=0
         inextp=31
         idum=1
      endif
 25   inext=inext+1
      if(inext .eq. 56)inext=1
      inextp=inextp+1
      if(inextp .eq. 56)inextp=1
      mj=ma(inext)-ma(inextp)
      if(mj .lt. mz)mj=mj+mbig
      ma(inext)=mj
      idum=mj
      ran=mj*fac
      if (ran .eq. 0. .or. ran .eq. 1.)go to 25
      if (ran .lt. 0. .or. ran .gt. 1.)then
         write(6,*)' random error, ran =',ran
         go to 25
      endif

      return
      end


c    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      subroutine gauss(x,y,sigma_x,sigma_y)

c     calculate two random numbers, x, y,  for gaussian distributions
c     with s.d. of sigma_x and sigma_y.

      implicit none

      common /random/idum

      real x,y,sigma_x,sigma_y
      real r1,r2,pi
      real ran

      integer*4 idum
      data pi/3.14159/
      
      r1 = ran(idum)
      r2 = ran(idum)
      r1 = sqrt(-2.*alog(r1))
      r2 = 2.*pi*r2
      x  = sigma_x*r1*cos(r2)
      y  = sigma_y*r1*sin(r2)
      
      return
      end
