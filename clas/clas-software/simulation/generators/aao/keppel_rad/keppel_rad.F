      program keppel_rad

c     This program makes an n-tuple that can be used with Paw to
c     make distributions of energies, angles, resonance
c     mass resulting from internal bremmstrahlung associated with pion
c     production on a proton. The exact integration formula of Mo and Tsai
c     is used.
c      The n-tuple contains the photon energy(EG), the true hadronic invariant
c     mass (W), 
c     and the photon angles relative to the q vector, (CSTHK, PHIK).
c
c     This program forces the monte carlo to concentrate on the regions
c     of photon emission along the directions of the incident and
c     scattered electrons.
c
c     The electrons are radiated as they pass through the target. Resolution
c     of detectors is not folded into the results.  If this is desired it
c     should be done with a second program that can operate on the n-tuple
c     and make a new version.

      implicit none
#include "bcs.inc"
#include "mc.inc"
#include "names.inc"

      COMMON/ALPHA/ ALPHA,PI,MP,MEL,WG
      common /radcal/T0,es,ep,ps,pp,rs,rp,u0,pu,uu,cst0,snt0,csths,csthp
     * ,snths,snthp,pdotk,sdotk
      common /random/idum
      real*8 ek,Tk,delta
      real*8 alpha,pi,mp,mel,wg,T0
      real*8 es,ep,ps,pp,rs,rp,u0,pu,uu,pdotk,sdotk
      real*8 cst0,snt0,csths,csthp,snths,snthp

      real beam_time,beam_time_max,beam_print
      real csran,csrng,csrnge,csrngb,delphi
      real cstk
      real cstk1,cstk2
      real cstmp
      real csdotk,cpdotk,cqdotk
      real delinf
      real deltar
      real ek_max
      real ekmax
      real ekx,eky,ekz
      real epmax
      real eprng
      real epw
      real ep_min,ep_max,ep_test
       real ep_sav
      real events
      real f
      real fmcall
      real g
      real jacob
      real kexp
      real mcfac
      real mpfac
      real phik
      real phir
      real px,py
      real q0
      real q2
      real q2_min,q2_max,q2max
      real qsq
      real qvecx
      real qvecz
      real ran
      real reg1,reg2,reg3,reg4
      real rn1,rn2
      real rotc,rots
      real rtest
      real s
      real s1
      real s2
      real sigma
      real signr
      real sigr,sig_ratio
      real sigr_max
      real sigr1
      real*8 sig_tot,sig_sum
      real sntk
      real sp
      real spence
      real stest
      real t_elapse
      real th0
      real*8 th0_deg
      real theta
      real tk_max
      real tp
      real tries
      real itime1, itime2
      real ts
      real uek
      real uq2, uq2_min,uq2_max,uq2rng
      real w2
      real wreal
      real x1
      real x2
      real targs,targp,xs,eloss,gxs,xtest,ebeam,t_targ,bfac
      real sig_int
      real*8 sig1(2),sig2(2),sig_nres(2),sig_res(2),
     &  sig_nrs(2),sig_rs(2),sigroper(2),w2_keppel
      logical goroper
      integer dismc(6,100)
      integer intreg

      integer i
      integer ir1
      integer iext
      integer j
      integer jj
      integer mcall
      integer mcall_max
      integer ntell,ntold

      integer*4 idum
      integer*4 ntries


c     Parameters for the n-tuple, which is named func1 and contains
c     15 elements per event.
      common /pawc/h(5000000)
      integer h,n,nevent,lrecl,istat,icycle
      parameter (n=15)
      real*4 ntp(n)
c     tag is the an array of names for the variables in the n-tuple.
      character*1 ich1
      character*3 month
      character*2 day
      character*2 year
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

      data filerz   /'keppel-radgen-1.0-00jan00.rz.0'/
      data file_out /'keppel-radgen-1.0-00jan00.out.0'/
      data file_sum /'keppel-radgen-1.0-00jan00.sum.0'/
      data file_bos /'keppel-radgen-1.0-00jan00.evt.0'/
      data ctime    /'                            '/

      goroper=.false.

      do j=1,6
        do i=1,100
         dismc(j,i)=0
        enddo
      enddo

c     set up parameters for breaking the monte-carlo integration region
c     over csthk into 5 parts:
        csrng=.04

c      Region sizes suggested for 4 GeV:
c        reg1=.23, reg2=.14, reg3=.11, reg4=.10
c
 14     write(6,*)' Input the sizes of the integration regions'
        read(5,*)reg1,reg2,reg3,reg4
        reg2=reg1+reg2
        reg3=reg2+reg3
        reg4=reg3+reg4
        if (reg4 .gt. .95)then
         write(6,*)' The sum of the region sizes must be less than .95'
           go to 14
        endif
c     set parameters
      alpha=1/137.

c     set up parameters for bos bank input to GSIM
      write(6,*)' input 1 for electron only in the bos bank'
      write(6,*)' input 2 to get the  photon as well'        
      read(5,*)npart
      if (npart .ne. 2)npart=1

      q(1) 	= -1
      id(1)	= 3		!Geant ID, e-
      pdgid(1)  = 11		!PDG ID, e-
      if (npart .eq. 2)then
         q(2)=0
         id(2)     =  1        !Geant ID, photon
         pdgid(2)  =  22	!PDG ID, photon
      endif
    

      write(6,*)' Input the target thickness, (r.l.)'
      read(5,*)t_targ

      bfac=4./3.
      t_targ=bfac*t_targ


      write(6,*)' Input the incident electron energy(GeV)'
      read(5,*)ebeam
c     calculate the incident momentum
      es=ebeam
      ps=sqrt(es**2-mel**2)
      rs=ps/es
c     cut off q2 at the value for 90 degree elastic scattering
      s=.5
      q2max=4.*ebeam**2*s/(1.+2.*ebeam*s/mp)
c     Choose two limits for Q**2
      write(6,*)'Input lower and upper limit for Q**2'
      read(5,*)q2_min,q2_max
      if (q2_max .gt. q2max)q2_max=q2max
      uq2_min=1/q2_max
      uq2_max=1/q2_min
      uq2rng=uq2_max-uq2_min

c     Set the limits on the range of scattered electron energies.
      write(6,*)'Input lower and upper limit for scattered electron',
     * ' energy(GeV).'
      read(5,*)ep_min,ep_max
      epmax=es-(wg**2+q2_min-mp**2)/2./mp
      if (ep_max .lt. epmax)epmax=ep_max
      eprng=epmax-ep_min


c     Choose a maximum value for the range of photon energies to consider.
      write(6,*)' Input delta = minimum photon energy for integration'
      read(5,*)delta

c     Select the number of events desired in the rz file.
      write(6,*)' Input the desired beam-time (seconds)'
      read(5,*)beam_time_max
      beam_print=beam_time_max/25

      write(6,*)' Input a multiplication factor for sigr_max'
      read(5,*)fmcall
      if (fmcall .eq. 0.)then
         write(6,*)' Input sigr_max'
         read(5,*)sigr_max
      endif


 1    mcall_max=0
      ntold=1
      events=0

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
      cstk=ran(idum)

      nevent=0

      t_elapse=0.
      itime2=itime1

      ntries=0
      sig_int=0.
      sig_tot=0.

c     Name the output rz file according to beam energy.
c     filerz=keppel-radgen-1.6-0811.rz.0, for example.


      ir1=ebeam
      ir1=ir1+48
      ich1=char(ir1)
      ir1=ir1-48

      filerz(15:15)=ich1
      ir1=10*(ebeam-ir1)
      ir1=ir1+48
      ich1=char(ir1)

      filerz(17:17)=ich1

      file_out(13:17)=filerz(13:17)
      file_bos(13:17)=filerz(13:17)
      file_out(19:20)=day
      file_out(21:23)=month
      file_out(24:25)=year
      filerz(19:25)=file_out(19:25)
      file_bos(19:25)=file_out(19:25)

      iext=48
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
     

        bosout = file_bos
        recname = 'MCEVENT'

c     Initialize BOS
     
c        call bos_ini(recname)
  
c     set up the ntuple file
      lrecl=8191
      call hlimit(5000000)
      call hropen(1,'aaoradgen',filerz,'n',lrecl,istat)
      call hbset('BSIZE',8176,istat)
      call hbnt(10,'aaoradgen',' ')
      call hbname(10,'func1',ntp(1),'es,ep,thete,w,wreal,
     1eg,csthk,phik,qx,qz,q0,csthe,egx,egy,egz')
     
      open(unit=12,file=file_out,status='new')

      write(12,*)' Keppel Calculation of Inclusive ep scattering'
      write(12,*)'starting time:', ctime
      write(12,*)'Target thickness =',t_targ*3./4.,' (r.l.)'
      write(12,*)' Incident electron energy =',ebeam,' GeV'

      write(12,*)'Electron Q**2 limits:',q2_min,q2_max
      write(12,*)'lower and upper limit for scattered electron',
     * ' energy(GeV):',ep_min,epmax
      write(12,*)' Minimum photon energy for integration (delta):',delta


c     Use a new variable in place of ek. Let uek=exp(-kek*ek)
c     ek=-(1/kexp)alog(uek).  The factor of 5. was chosen empirically for kexp
c     by looking at the ek spectrum for E0=1.6 GeV.
c     Let uek range from 0 to 1. Then ek will range from 0 and infinity.
c     This requires a jacobian. Jacobian=1./(kexp*uek)=(1./kexp)exp(kexp*ek)

      kexp=5.

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
      q0=(1.232**2-mp**2+q2)/2./mp
      ep=es-q0
      pp=sqrt(ep**2-mel**2)
      rp=pp/ep
      s=q2/4/es/ep
      th0=2.*asin(sqrt(s))
      theta=th0*180./pi
      T0=th0
      snt0=sin(th0)
      cst0=cos(th0)

c     calculate kinematic quantities needed for the Mo and Tsai calculation
      u0=es-ep+mp
      pu=sqrt(ps**2+pp**2-2*ps*pp*cst0)
      uu=u0**2-pu**2
      csths=(ps-pp*cst0)/pu
      csthp=(ps*cst0-pp)/pu
      snths=sqrt(1.-csths**2)
      snthp=sqrt(1.-csthp**2)
      ts=acos(csths)
      tp=acos(csthp)
      qsq=q2
      sp=es*ep-ps*pp*cst0

      sigr_max=0.
      cstk1=(es-ep*cst0)/(sqrt(es**2+ep**2-2*es*ep*cst0))
      cstk2=(es*cst0-ep)/(sqrt(es**2+ep**2-2*es*ep*cst0))
      csrnge=csrng
      if ((1.-cstk1) .lt. csrnge)csrnge=1.-cstk1
      if ((cstk1-cstk2) .lt. 2.*csrnge)csrnge=0.5*(cstk1-cstk2)
      csrngb=csrng/40.
      if (csrngb .gt. csrnge/5.)csrngb=csrnge/5.
      delphi=pi/9.
      phik=(ran(idum)-0.5)*delphi
      mpfac=delphi/2./pi

      ek=delta
      
      jacob=exp(kexp*ek)/kexp*(q2**2/(2*es*ep))
      do i=1,10000

        csran=ran(idum)
        if (csran .gt. .3)then
            cstk=2.*csrngb*(ran(idum)-0.5)+cstk1
        else
            cstk=2.*csrngb*(ran(idum)-0.5)+cstk2
        endif
        mcfac=csrngb /reg1
        phik=(ran(idum)-0.5)*delphi
        mpfac=delphi/2./pi

        Tk=acos(cstk)
        sntk=sin(Tk)
        sdotk=es*ek-ps*ek*cstk*csths-ps*ek*sntk*snths*cos(phik)
        pdotk=ep*ek-pp*ek*cstk*csthp-pp*ek*sntk*snthp*cos(phik)

        sigr= sigma(ek,Tk)
        sigr=sigr*mcfac*mpfac
        sigr=sigr*jacob
        if (sigr .gt. sigr_max)then
           sigr_max=sigr
           ek_max=ek
           tk_max=Tk
        endif
      enddo

      write(6,*)'sigr_max,ek_max,tk_max',
     * sigr_max,ek_max,tk_max
      write(12,*)'sigr_max,ek_max,tk_max',
     * sigr_max,ek_max,tk_max
      sigr_max=sigr_max*fmcall
 25   write(6,*)'sigr_max changed to',sigr_max
      write(12,*)'sigr_max changed to',sigr_max



c   %%%%%%%%%%%%%%%%%%% Main Calculation  %%%%%%%%%%%%%%%%%%%%%%%
c     Use a Monte-Carlo to calculate a distribution of nmax events
c     distributed according to the Mo-Tsai integrand.
      
 20   continue
      ntries=ntries+1

c     calculate the energy of the electron at the scattering point
c     after making its way through the target.  First, randomly
c     choose the interaction point.

      targs=t_targ*ran(idum)
      targp=t_targ-targs
c     Now calculate the radiation loss

 22   xs=ran(idum)
      eloss=xs**(1./targs)
      gxs=1.-eloss
      xtest=ran(idum)
      if (xtest.gt.gxs)go to 22
      es=ebeam*(1.-eloss)
c     Cut off the incident energy at e_s=ebeam/4.
      if (es .lt. ebeam/4.)go to 20
      ps=sqrt(es**2-mel**2)
      rs=ps/es


      uq2=uq2_min+uq2rng*ran(idum)
      q2=1./uq2

c     calculate the energy and momentum of the scattered electron,
c      and calculate Q**2

      ep=epmax-eprng*ran(idum)
c     check to see if the scattered electron energy is below the
c     detector threshold.
      if (ep .lt. ep_min)go to 20

      q0=es-ep
      s=q2/4/es/ep
c     cut off scattering at 90 degree
      if (s .gt. .5)go to 20
      th0=2.*asin(sqrt(s))
      theta=th0*180./pi
      T0=th0
      snt0=sin(th0)
      cst0=cos(th0)
c     calculate the scattering angle (single precision and double precision
c     version) in radians


c     check to see if the scattered electron energy is above
c     the pion threshold for this angle.
      ep_test=(mp**2+2*mp*es-wg**2)/2./(mp+2.*es*s)
      if (ep .gt. ep_test)go to 20
      pp=sqrt(ep**2-mel**2)
      rp=pp/ep

      qsq=q2
      if (qsq .le. 0.)then
         write(6,*)' Main-1:, qsq =',qsq
         go to 20
      endif
      qvecx=-pp*sin(th0)
      qvecz=ps-pp*cos(th0)

      w2=mp**2+2*mp*q0-q2
      if (w2 .lt. mp**2)go to 20
      epw=sqrt(w2)
      if (epw .lt. wg+0.002)go to 20
c     calculate kinematic quantities needed for the Mo and Tsai calculation
      u0=es-ep+mp
      pu=ps**2+pp**2-2*ps*pp*cst0
      if (pu .le. 0.)then
         write(6,*)' Main-2, pu**2 =',pu
         go to 20
      endif
      pu=sqrt(pu)
      uu=u0**2-pu**2
      csths=(ps-pp*cst0)/pu
      csthp=(ps*cst0-pp)/pu
      snths=1.-csths**2
      if (snths**2 .le. 0.)then
         write(6,*)' Main-3: snths =',snths
         go to 20
      endif
      snths=sqrt(snths)
      snthp=1.-csthp**2
      if (snthp .le. 0.)then
         write(6,*)' Main-4: snthp**2 =',snthp
         go to 20
      endif
      snthp=sqrt(snthp)
      ts=acos(csths)
      tp=acos(csthp)
      sp=es*ep-ps*pp*cst0


      cstk1=csths
      cstk2=csthp
      csrnge=csrng
        if (cstk1 .lt. cstk2)then
           cstmp=cstk1
           cstk1=cstk2
           cstk2=cstmp
        endif
        if ((1.-cstk1) .lt. csrnge)csrnge=1.-cstk1
        if ((cstk1-cstk2) .lt. 2.*csrnge)csrnge=0.5*(cstk1-cstk2)
        csrngb=csrng/40.
        if (csrngb .gt. csrnge/5.)csrngb=csrnge/5.
        csran=ran(idum)
        rn1=ran(idum)
        if (rn1 .gt. 0.5)then
           rn1=1.
         else
            rn1=-1.
        endif
        rn2=ran(idum)
        delphi=ps*cstk1*csrngb/pp/sqrt(1.-cst0**2)/sqrt(1.-cstk1**2)
       if (delphi .lt. pi/9.)delphi=pi/9.
       if (delphi .gt. 2.*pi)delphi=2.*pi
       if (cstk1 .gt. .995)delphi=2.*pi
       delphi=pi/9.
        if (csran .lt. reg1)then
           intreg=1
            cstk=cstk1+(2.*rn2-1.)*csrngb
            mcfac=csrngb /reg1
            phik=(ran(idum)-0.5)*delphi
            mpfac=delphi/2./pi
        elseif(csran .lt. reg2)then
           intreg=2
           cstk=cstk2+(2.*rn2-1.)*csrngb
            mcfac=csrngb /(reg2-reg1)
            phik=(ran(idum)-0.5)*delphi
            mpfac=delphi/2./pi
        elseif(csran .lt. reg3)then
           intreg=3
            cstk=cstk1+rn1*(csrngb+rn2*(csrnge-csrngb))
            mcfac=(csrnge-csrngb) /(reg3-reg2)
            phik=(ran(idum)-0.5)*delphi
            mpfac=delphi/2./pi
        elseif(csran .lt. reg4)then
           intreg=4
            cstk=cstk2+rn1*(csrngb+rn2*(csrnge-csrngb))
            mcfac=(csrnge-csrngb) /(reg4-reg3)
            phik=(ran(idum)-0.5)*delphi
            mpfac=delphi/2./pi
        else
           intreg=5
 45        cstk=2.*rn2-1.
           phik=2.*pi*(ran(idum)-0.5)
           if (abs(cstk-cstk1) .lt.  csrnge .or.
     &      abs(cstk-cstk2) .lt. csrnge)then
              if(abs(phik).lt. delphi/2.)go to 45
           endif
c     combine mcfac and mpfac into one factor and set mpfac=1
           mcfac=(1.-csrnge*delphi/pi)/(1.-reg4)
           mpfac=1.
        endif


      Tk=acos(cstk)
      sntk=sin(Tk)
c     change the following on Jan. 19, 1999
c      phran=ran(idum)

c      if (phran .lt. .2)then
c      else
c 48      phik=2*pi*(ran(idum)-0.5)
c         if(abs(phik).lt. pi/180.)go to 48
c         mpfac=1.25
c      endif
c     end of jan 19, 1999 correction

      ekmax=0.5*(uu-wg**2)/(u0-pu*cstk)
      if (ekmax .gt. ebeam)then
         write(6,*)' Main-5: ekmax =',ekmax
         ekmax=ebeam
      endif
c     choose ek by making a change of variables
 78   uek=ran(idum)
      if (uek .lt. 0.1E-20)then
         write(6,*)' Main-6: uek =',uek
         go to 20
      endif
      ek=-alog(uek)/kexp
      if (ek .gt. ekmax)go to 20


      if (ek .lt. delta)then
        intreg=6
c
c     calculate the non-radiative cross section
        th0_deg=th0*180./pi
        call h2model_thia(es,ep,th0_deg,w2_keppel,sig1,sig2,
     &  sig_nres,sig_res,sig_nrs,sig_rs,goroper,sigroper)

c     convert cross section from nbarns to micro barns
        signr=sig2(1)/1000.
c     set g=0
        f=signr/(2.*(alpha*ep/qsq)**2)/(mp*cos(th0/2)**2)
        g=0.
c     Calculate the radiative correction factor deltar for the cross
c      section.  This includes vertex corrections and the integration
c     up to photon energies of delta.
        x1=(ep-es)/ep
        x2=(es-ep)/es
        s1=spence(x1)
        s2=spence(x2)
        deltar=-(alpha/pi)*(28./9. -(13./6.)*dlog(2.*sp/mel**2)
     &   -s1-s2)
        delinf=-(alpha/pi)*dlog(es*ep/delta**2)*(dlog(2.*sp/mel**2)-1.)
      
        sigr1=signr*(1.+deltar)*exp(delinf)
c	calculate average differential cross section in the region
c	from ek=0 to delta, and from cos(thetak)=-1 to 1.

	sigr=sigr1/delta/4./pi
        if (sigr .gt. 0.)then
           go to 28
        else
          write(6,*)' Main-6: sigr =',sigr
          go to 20
        endif
c       end of section for calculation with ek < delta.
c       Normally, go to statement 28
      endif

      sdotk=es*ek-ps*ek*cstk*csths-ps*ek*sntk*snths*cos(phik)
      pdotk=ep*ek-pp*ek*cstk*csthp-pp*ek*sntk*snthp*cos(phik)

      sigr= sigma(ek,Tk)
      if (sigr .gt. 0.)go to 28
      write(6,*)' Main-7: sigr =',sigr
      go to 20

28    jacob=exp(kexp*ek)/kexp/(2*es*ep)*q2**2
      sigr=sigr*jacob


c     Compare sigr to the sigr_max to determine whether to generate
c     an event.
      sigr=mcfac*mpfac*sigr
      sig_ratio=sigr/sigr_max
      sig_tot=sig_tot+sigr
c     Choose the number of times, mcall, to call the routine used
c     to calculate kinematic quantities for the n-tuple.
      rtest=ran(idum)
      mcall=sig_ratio
      stest=sig_ratio-mcall
      if (stest .gt. rtest)mcall = mcall + 1
       
      if (mcall .gt. mcall_max) mcall_max=mcall
      if (mcall .gt. 10)then
         write(6,*)' mcall =',mcall,' intreg=',intreg
         write(6,*)es,ep,th0,ek
         write(6,*)'cstk1,cstk2,cstk,phik',cstk1,cstk2,cstk,phik
         write(6,*)' csrnge=',csrnge
         write(6,*) ekx,eky,ekz
         write(6,*)wreal


         csdotk=cstk*csths+sntk*snths*cos(phik)
         cpdotk=cstk*csthp+sntk*snthp*cos(phik)
         cqdotk=(ps*csdotk-pp*cpdotk)/sqrt(ps**2+pp**2-2*ps*pp*cos(th0))
         write(6,*)'es,ep,ek',es,ep,ek
         write(6,*)'cstk,phik,csdotk,cpdotk'
     +   ,cstk,phik,csdotk,cpdotk
         write(6,*)'cqdotk',cqdotk

         write(12,*)' mcall =',mcall,' intreg=',intreg
         write(12,*)'es,ep,ek',es,ep,ek
         write(12,*)'cstk,phik,csdotk,cpdotk'
     +   ,cstk,phik,csdotk,cpdotk
         write(12,*)'cqdotk',cqdotk
      endif
c     If mcall .gt. 0 generate mcall n-tuple events.
      if (mcall .eq. 0)go to 30

      ep_sav=ep

      if (mcall .lt. 100)then
         dismc(intreg,mcall)=dismc(intreg,mcall)+1
      else
         dismc(intreg,100)=dismc(intreg,100)+1
      endif
      if (mcall .gt. 1000)then
         write(6,*)' mcall = ',mcall
         stop
      endif

      do j=1,mcall

c     Calculate the radiation loss for the electron leaving the target

 222     xs=ran(idum)
         eloss=xs**(1./targp)
         gxs=1.-eloss
         xtest=ran(idum)
         if (xtest.gt.gxs)go to 222
         ep=ep_sav*(1.-eloss)

c     correct the following section on Jan. 23, 1999
         if (ep .lt. ep_min)then
            sig_tot=sig_tot-sigr_max
            go to 24
         endif
c     end of correction


         w2=mp**2+2*mp*(ebeam-ep)-2*ebeam*ep*(1-cos(th0))+2.*mel**2
         epw=sqrt(w2)
        call kinem(ebeam,es,ep,th0,ek,cstk,phik,ekx,eky,ekz,wreal)

c     Calculate the members of the n-tuple and ouput it to the rz file.
         ntp(1)=es
         ntp(2)=ep
         ntp(3)=theta
         ntp(4)=epw
         ntp(5)=wreal
         ntp(6)=ek
         ntp(7)=cstk
         ntp(8)=phik*180./pi
         ntp(9)=qvecx
         ntp(10)=qvecz
         ntp(11)=q0
         ntp(12)=cst0
         ntp(13)=ekx
         ntp(14)=eky
         ntp(15)=ekz
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
c     momentum of radiated photon
         if (npart .eq. 2)then
c     suppress radiated photons when E-gamma is less than delta
           if (ek .le. delta)then
             p(2,1)=0.
             p(2,2)=0.
             p(2,3)=1.e-5
             p(2,4)=1.e-5
           else
             p(2,1) = ekx*rotc+eky*rots
             p(2,2) = eky*rotc-ekx*rots
             p(2,3) = ekz
             p(2,4) = ek
           endif
         endif
c         call bos_out	! Pack the BOS banks and write out to file        

 24      continue
       enddo

c     Talk to the user every now and then.
       tries=ntries
       events=nevent
c     photon phase space = 4*pi*delta-omega=4*pi (after change of variables)
c     electron phase space =2*pi*uq2rng*eprng

       sig_sum=sig_tot*(4.*pi)*(2.*pi*uq2rng*eprng)/tries
       beam_time=events/sig_sum*1.E-4     !beam-time in seconds
       ntell=beam_time/beam_print

       if (ntell .gt. ntold)then
          sig_int=events/tries
          sig_int=sig_int*sigr_max*(4.*pi)*(2.*pi*uq2rng*eprng)
          write(6,*)' ntries, nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max
          write(12,*)' ntries, nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max

          write(6,*)' Integrated cross section (MC, numerical) ='
     +   ,sig_int,sig_sum, ' mu-barns'
          write(6,*)' Beam time at Lum=1.0E34 =', beam_time,
     *    ' seconds'
          write(12,*)' Integrated cross section =',sig_int,sig_sum
     *    ,' micro-barns'
          write(12,*)' Beam time at Lum=1.0E34 =',beam_time,
     *    ' seconds'


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
      call hrend('aaoradgen')


      close(12)



      file_sum(1:26)=file_out(1:26)
      file_sum(30:31)=file_out(30:31)
      open(unit=14,name=file_sum,status='new')
      write(14,*)' Keppel Calculation of Inclusive ep Scattering'
      write(14,*)' Starting time: ',ctime
      write(14,*)'Target thickness =',t_targ*3./4.,' (r.l.)'
      write(14,*)' Incident electron energy =',ebeam,' GeV'

      write(14,*)'Electron Q**2 limits:',q2_min,q2_max
      write(14,*)'lower and upper limit for scattered electron',
     * ' energy(GeV):',ep_min,epmax
      write(14,*)' Minimum photon energy for integration (delta):',delta
      write(14,*)'sigr_max,ek_max,tk_max',
     * sigr_max,ek_max,tk_max
      write(14,*)' ntries, nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max
      write(14,*)' Integrated cross section =',sig_int,sig_sum
     *    ,' micro-barns'
      write(14,*)' Beam time at Lum=1.0E34 =',events/sig_sum*1.E-4
     *    ,' seconds'
      write(14,*)' Elapsed CPU time = ',t_elapse/60,' minutes'
      write(14,*)' CPU time/event = ', t_elapse/nevent,' sec'

      write(14,1425)reg1,reg2-reg1,reg3-reg2, reg4-reg3,1.-reg4
      write(14,*)'csrng =',csrng
 1425 format(' size of cosine regions:',5f5.2)
      do intreg=1,6
        write(14,1410)intreg,(dismc(intreg,j),j=1,100)
      enddo

 1410 format(' Distribution of mcall values, region', i2 /10(1x,10i7/))

      close(14)
c      CALL bos_end(recname)

 99   continue



      stop
      end



      real function sigma(ek,Tk)

c     Calculate the single pion electroproduction cross section with
c      radiative tail, according to the prescription in Mo and Tsai.
c     Mo and Tsai  calculate dsigma/d_omega dp for (omega>delta)
c     omega is the energy of the radiated photon.

c     The Mo and Tsai cross section for the 3-3 resonance is replaced with
c     the AO cross section for single pion production from the proton.


      implicit none
      COMMON/ALPHA/ ALPHA,PI,MP,MEL,WG
      common /radcal/T0,es,ep,ps,pp,rs,rp,u0,pu,uu,cst0,snt0,csths,csthp
     * ,snths,snthp,pdotk,sdotk

      real*8 ek,Tk
      real*8 alpha,pi,mp,mel,wg
      real*8 T0,es,ep,ps,pp,rs,rp,u0,pu,uu,pdotk,sdotk
      real*8 cst0,snt0,csths,csthp,snths,snthp
      real*8 csthk,snthk,qq,mf2
      real*8 sp
      real*4 ffac1,ffac2,ffac3,ffac4,ffac5,ffac6,ffac
      real*4 gfac1,gfac2,gfac3,gfac4,gfac

      real*4 f,g
      real*4 sig_r,sigf
      real*4 qsq,epw,th0
      real*8 th0_deg
      real*4 q0
      real*4 signr

      real*8 sig1(2),sig2(2),sig_nres(2),sig_res(2),
     &  sig_nrs(2),sig_rs(2),sigroper(2),w2_keppel
      logical goroper

      goroper=.false.
      th0=T0
      csthk=cos(Tk)
      snthk=sin(Tk)
      qq=2*mel**2-2*es*ep+2*ps*pp*cst0-2*ek*(es-ep)+2*ek*pu*csthk
      mf2=uu-2*ek*(u0-pu*csthk)
      if (mf2 .lt. wg**2 .or. qq .ge. 0.)then
         sigma=0.
         return
      endif

      epw=sqrt(mf2)

      sp=es*ep-ps*pp*cst0

      ffac1=-(mel/pdotk)**2*(2.*es*(ep+ek)+qq/2)
      ffac2=-(mel/sdotk)**2*(2.*ep*(es-ek)+qq/2)
      ffac3=-2.
      ffac4=2/sdotk/pdotk*(mel**2*(sp-ek**2)+sp*(2*es*ep-sp+ek*(es-ep)))       
      ffac5=(2*(es*ep+es*ek+ep*ep)+qq/2-sp-mel**2)/pdotk
      ffac6=-(2*(es*ep-ep*ek+es*es)+qq/2-sp-mel**2)/sdotk
      ffac=ffac1+ffac2+ffac3+ffac4+ffac5+ffac6
      if (ffac .le. 0.)then
         sigma=0.1e-30
         return
      endif

      gfac1=mel**2*(2*mel**2+qq)* (1./(pdotk**2)+1./(sdotk**2) )
      gfac2=4.
      gfac3=4.*sp*(sp-2*mel**2)/pdotk/sdotk
      gfac4=(2*sp+2*mel**2-qq)*(1./pdotk-1./sdotk)

      gfac=gfac1+gfac2+gfac3+gfac4
      if (gfac .le. 0.)then
         sigma=0.1e-30
         return
      endif
      qsq=-qq
      q0=es-ep

c     Use the Keppel cross section to fake up the values of f and g
c     needed for the Mo and Tsai integration.
c     This should be compared to the formulas given in Mo and Tsai
c     for the 3-3 resonance.  We ought to agree pretty well in the
c     3-3 resonance region.

      th0_deg=th0*180./pi
        call h2model_thia(es,ep,th0_deg,w2_keppel,sig1,sig2
     & ,sig_nres,
     &  sig_res,sig_nrs,sig_rs,goroper,sigroper)

c     convert cross section from nbarns to microbarns

        if (sig2(1) .gt. 0.)then
         signr=sig2(1)/1000.
        else
          sigma=.1E-30
          return
        endif

c     set g=0
        f=signr/(2.*(alpha*ep/qsq)**2)/(mp*cos(th0/2)**2)
        g=0.
c   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


c     ??????????????????????????
c     The following formula is the same as B.8 in Mo and Tsai,
c     except I have divided by 2pi.  It seems to me that the
c     result doesn't make sense otherwise.
c     ?????????????????????????


      sig_r=((alpha**3/(2*pi*qq)**2)/mp)*(ep/es)*ek
      sigf=mp**2*f*ffac+g*gfac

 124   sigma=sig_r*sigf
c       write(6,*)' sigma: sigma=',sigma
c       write(6,*)es,ep,th0_deg,w2_keppel,signr,sig2(2)
c       write(6,*)f,ffac,sigf
      return
      end


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

c   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      real function spence(x)
c     Calculate the Spence function needed for the Mo and Tsai formula.

      implicit none
      real x
      real pi
      real sintp,sintn
      pi=3.14159

      if (abs(x) .lt. 0.1)then
         spence=x+x**2/4.
c         write(6,*)' spence: abs(x) .lt. 0.1'
         return
      endif

      if (x .gt. 0.99 .and. x .lt. 1.01)then
         spence=pi**2/6.
c         write(6,*)' spence: x=1.'
         return
      endif

      if (x .gt. -1.01 .and. x .lt. -0.99)then
         spence=-pi**2/12.
c         write(6,*)' spence: x= -1.'
         return
      endif

      if (x .gt. 0.)then
         spence=.1025+sintp(x)
c         write(6,*)' x .gt. 0.'
         return
      endif
      spence=-0.0975+sintn(x)
c      write(6,*)' spence: x .lt. 0.'
      return
      end

c   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      real function sintp(x)
      implicit none
      real x
      real xstep,sum,y,arg
      integer i

      xstep=(x-.1)/100.
      sum=0.
      y=.1-xstep/2.
      do i=1,100
        y=y+xstep
        arg=abs(1.-y)
        sum=sum-alog(arg)/y
      enddo
      sintp=sum*xstep
      return
      end

c    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      real function sintn(x)
      implicit none
      real x,xa,ystep,y,sum
      integer i

      xa=abs(x)
      ystep=(xa-0.1)/100.
      sum=0.
      y=.1-ystep/2.
      do i=1,100
        y=y+ystep
        sum=sum-alog(1.+y)/y
      enddo
      sintn=sum*ystep
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

         pi=3.14159
         r1=ran(idum)
         r2=ran(idum)
         r1=sqrt(-2.*alog(r1))
         r2=2.*pi*r2
         x=sigma_x*r1*cos(r2)
         y=sigma_y*r1*sin(r2)
      return
      end


c   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      subroutine kinem(ebeam,es,ep,th0,ephot,cstk,phik,
     * ephotx,ephoty,ephotz,wreal)

c
c     Input:
c         ebeam = incident electron beam energy
c         es  = incident electron energy at interaction point
c         ep = scattered electron energy
c         th0 = electron scattering angle
c         ephot = energy of radiated photon
c         cstk = cosine of the photon angle (relative to the q vector)
c         phik = azimuthal angle of photon
c     Output:
c         wreal = true hadronic invariant mass

         

      implicit none

      real*8 es,ep,ps,pp,ephot

      real cstk
      real csthe
      real cstq
      real csphk
      real ebeam
      real ephotx,ephoty,ephotz,eph_dot_q
      real mel
      real mp
      real nu
      real phik
      real pi
      real q2,qvec
      real qx,qz
      real snphk
      real snthe
      real sntk
      real sntq
      real th0
      real w2
      real wmin
      real wreal


      mp=.938
      mel=0.511E-3
      pi=3.14159
      wmin=mp+.130
      csthe=cos(th0)
      snthe=sin(th0)
      nu=es-ep
      ps=abs(es**2-mel**2)
      pp=abs(ep**2-mel**2)
      ps=sqrt(ps)
      pp=sqrt(pp)
      q2=2.*es*ep-2.*ps*pp*csthe-2.*mel**2
      w2=mp**2-q2+2.*mp*nu
c     get components of the q vector
      qx=-pp*snthe
      qz=ps-pp*csthe
      qvec=sqrt(qx**2+qz**2)
c     get components of the photon vector
      if (abs(cstk) .gt. 1.)then
         write(6,*)' missm-1: cstk =',cstk
         cstk=cstk/abs(cstk)
      endif
      sntk=sqrt(1.-cstk**2)
      csphk=cos(phik)
      snphk=sin(phik)

      cstq=qz/qvec
      sntq=sqrt(1.-cstq**2)
      ephotx=ephot*(sntk*csphk*cstq-cstk*sntq)
      ephoty=ephot*sntk*snphk
      ephotz=ephot*(cstk*cstq+sntk*csphk*sntq)

c     calculate the dot product of the photon vector and the q-vector
      eph_dot_q=ephotx*qx+ephotz*qz
c     calculate the mass of the actual hadronic system for the two
c     photon directions.

      wreal=(w2-2.*ephot*(nu+mp)+2.*eph_dot_q)
      if (wreal .gt. 0.)then
         wreal=sqrt(wreal)
      else
         wreal=0.
      endif
      return
      end

c   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
