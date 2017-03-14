      program kaon_rad
c
c 
c           This program makes an n-tuple that can be used with Paw to
c     make distributions of energies, angles, resonance
c     mass resulting from internal bremmstrahlung associated with pion
c     production on a proton.
c     The peaking approximation is used, but is supplemented outside
c     the radiative peaks using the exact integration formula of Mo and Tsai.
c
c      The n-tuple contains the photon energy(EG), the true hadronic invariant
c     mass (W), the components of the proton momentum (PPX, PPY, PPZ),
c     the proton energy (EP), the pion momentum (PPIX, PPIY, PPIZ) and
c     pion energy (EPI), the  angles for the hadronic
c     decay in the hadronic frame (CSTCM, PHICM), the missing mass (MM),
c     and the photon angles relative to the q vector, (CSTHK, PHIK).
c
c     This program forces the monte carlo to concentrate on the regions
c     of photon emission along the directions of the incident and
c     scattered electrons.
c
c     The electrons are radiated as they pass through the target, both
c     before and after the primary reaction.
c
c     Resolution of detectors is not folded into the results.

      implicit none
#include "bcs.inc"
#include "mc.inc"
#include "names.inc"

      COMMON/ALPHA/ ALPHA,PI,MP,MPI,MEL,WG,EPIREA
      common /radcal/T0,es,ep,ps,pp,rs,rp,u0,pu,uu,cst0,snt0,csths,csthp
     * ,snths,snthp,pdotk,sdotk
      common /random/idum
      common /flag/flag_nu_cm2
      real*8 ek,Tk,delta
      real*8 alpha,pi,mp,mpi,mel,wg,T0
      real*8 mkp,mla,msi,m_h
      real*8 es,ep,ps,pp,rs,rp,u0,pu,uu,pdotk,sdotk
      real*8 cst0,snt0,csths,csthp,snths,snthp

      real t_s
      real elas_sel,alpha_k,arg
      real ereg, mkfac
      real csran,csrng
      real csthcm
      real csthcm_max,csthcm_p_max,csthcm_m_max
      real cstk
      real cstk1,cstk2
      real cstmp
      real delinf
      real deltar
      real ek_max
      real ekmax
      real epeps
      real epi
      real epmax
      real eprng
      real eprot
      real epw
      real ep_min,ep_max,ep_test
      real ep_sav
      real events
      real f
      real fkt
      real fmcall
      real g
      real jacob
      real kexp
      real kfac
      real mcfac
      real mm2
      real mpfac
      real mpi0
      real mpip
      real mpi_s,m_h_s
      real nu
      real phik
      real ppx,ppy,ppz
      real ppix,ppiy,ppiz
      real phicm
      real phicm_max,phicm_p_max,phicm_m_max
      real phir
      real px,py
      real q0
      real q2
      real q2_min,q2_max
      real w_min, w_max
      real qsq
      real qvecx
      real qvecz
      real ran
      real reg1,reg2
      real rotc, rots
      real rtest
      real s
      real s1
      real s2
      real sigi,sigip
      real sigl
      real sigma,sig_peak
      real sigma0
      real signr
      real sigr,sig_ratio
      real sigr_max,sigr_p_max,sigr_m_max
      real sigr1
      real*8 sig_tot,sig_sum
      real sigt
      real sigu
      real sink
      real sntk
      real sp
      real spence
      real stest
      real t_elapse
      real th0
      real theta
      real tk_max,tk_p_max,tk_m_max
      real tp
      real tries
      real ts
      real uek
      real uq2, uq2_min,uq2_max,uq2rng
      real w2
      real wreal
      real x1
      real x2
      real targs,targp,xs,eloss,gxs,xtest,ebeam,t_targ,bfac
      real sig_int
      real itime1, itime2
      real the_e_min, the_e_max,q2max,q2min

      integer get_spin
      integer flag_e_hel
      integer epirea
      integer i
      integer j,jj
      integer mcall
      integer mcall_max
      integer nprint,ntell,ntold
      integer ncycle
      integer sp_peak
      integer peak_credit,peak_credit_tmp,mcall_tmp


      integer*4 idum
      integer*4 ntries
      integer   flag_nu_cm2



c     Parameters for the n-tuple, which is named func1 and contains
c     15 elements per event.
      common /pawc/h(1000000)
      integer h,n,nevent,nmax,lrecl,istat,icycle
      parameter (n=26)
      real*4 ntp(n)
c     tag is the an array of names for the variables in the n-tuple.
      character*5 tag(n)
      character*132 filerz
      character*132 file_out
      character*132 file_sum
      character*132 file_bos
      character*28  ctime
      character*8 recname

      data tag /' ES  ',' EP  ','THETE','  W  ','WREAL',
     * ' PPX ',' PPY ',' PPZ ','EPROT','PPIX ',
     *  'PPIY ','PPIZ ',' EPI ','CSTCM','PHICM',' MM  ','  EG ',
     * 'CSTHK','PHIK ', ' QX  ',' QZ  ',' Q0  ','CSTHE','EHEL ',
     * ' Q2  ','  T  '/
c
      DATA PI   /3.1415926/
      DATA MPIP /.1395700/
      DATA MPI0 /.1349764/
      DATA MP   /.9382799/
      DATA MKP  /0.493677/
      DATA MLA  /1.115684/
      DATA MSI  /1.19255/
      DATA MEL  /.511E-3/
c
      call getenvf('FRZN',filerz)
      call getenvf('FOUT',file_out)
      call getenvf('FSUM',file_sum)
c
c     set up parameters for breaking the monte-carlo integration region
c     over csthk into 3 parts:
        csrng=.05
        reg1=0.49
        reg2=0.49
        ereg=0.5
        events=0.
        peak_credit=0
        alpha_k=0.

c     set parameters
      alpha=1/137.

c     set up parameters for bos bank input to GSIM
      write(6,*)' input 2 for two charged particles in the bos bank'
      write(6,*)' input 4 to get the neutral hadron and photon as well'        
      read(5,*)npart
      if (npart .ne. 4) npart=2

      q(1) 	= -1
      id(1)	= 3		!Geant ID e-
      pdgid(1)  = 11		!PDG ID, e-
      if (npart .eq. 4)then
         q(4)=0
         id(4)     =  1        !Geant ID, photon
         pdgid(4)  =  22	!PDG ID, photon
      endif

c     Choose whether a neutral or charged pion is made in the reaction
 5    write(6,*)' Input epirea (1 for lambda, 2 for sigma)'
      read(5,*)epirea
      IF(EPIREA .ne. 1 .and. epirea .ne. 2)go to 5
c
      write(6,*) 'Input electron helicity (0 for unpol, 1 for pol)'
      read(5,*) flag_e_hel
c
      write(6,*)' Input the target thickness, (r.l.)'
      read(5,*)t_targ
      write(6,*)' Input the incident electron energy(GeV)'
      read(5,*)ebeam
c
c     Choose two limits for W
      write(6,*)'Input lower and upper limit for W'
      read(5,*)w_min, w_max
c
c     Choose two limits for Q**2
      write(6,*)'Input lower and upper limit for Q**2'
      read(5,*)q2_min,q2_max
c
c     Choose two limits for electron scattering angle
      write(6,*)'Input lower and upper limit for electron', 
     * ' scattering angle in (deg)'
      read(5,*)the_e_min,the_e_max
c
      write(6,*)'Input lower and upper limit for scattered electron',
     * ' energy(GeV).'
      read(5,*)ep_min,ep_max
c     Choose a maximum value for the range of photon energies to consider.
      write(6,*)' Input delta = minimum photon energy for integration'
      read(5,*)delta

c     Select the number of events desired in the rz file.
      write(6,*)' Input the desired number of events'
      read(5,*)nmax
      nprint=nmax/25

      write(6,*)' Input a multiplication factor for sigr_max'
      read(5,*)fmcall


c     Choose whether a neutral or charged pion is made in the reaction
      IF(EPIREA.EQ.1)then
         M_H=MLA
         MPI=MKP
         id(2)	   = 11		!Geant ID, Kaon
         q(2) = 1
         pdgid(2)  = 321    !PDG ID, Kaon
         if (npart .eq. 4)then
            id(3)     = 18      !Geant ID, Lambda
            pdgid(3)  = 3122	!PDG ID, Lambda
            q(3) = 0
         endif
      elseif (epirea.eq.2) then
         M_H=MSI
         MPI=MKP
         id(2)     = 11         !Geant ID, Kaon
         q(2) = 1
         pdgid(2)  = 321		!PDG ID, Kaon
         if (npart .eq. 4)then
            id(3)	  = 20		!Geant ID, Sigma
            pdgid(3)  = 3112	!PDG ID, Sigma
            q(3) = 0
         endif
      endif

      mpi_s		= sngl(mpi)		!Single precision pion mass
      m_h_s     = sngl(m_h)
      wg		= m_h+mpi+.0005	!minimum hadron mass for pion production
c
      bfac=4./3.
      t_targ=bfac*t_targ


c     calculate the incident momentum
      es=ebeam
      ps=sqrt(es**2-mel**2)
      rs=ps/es
c
      s			= sin(the_e_max*pi/180./2)**2
      q2max		= 4.*ebeam**2*s/(1.+2.*ebeam*s/mp)
      s         = sin(the_e_min*pi/180./2)**2
      q2min     = 4.*ebeam**2*s/(1.+2.*ebeam*s/mp)

      if (q2_max.gt.q2max) q2_max=q2max
      if (q2_min.lt.q2min) q2_min=q2min
c
      uq2_min=1/q2_max
      uq2_max=1/q2_min
      uq2rng=uq2_max-uq2_min

c     Set the limits on the range of scattered electron energies.
      epmax=es-(wg**2+q2_min-mp**2)/2./mp
      if (ep_max .lt. epmax)epmax=ep_max
      eprng=epmax-ep_min

      ncycle=0

 1    mcall_max=0
      ntold=0

c     Use the internal clock to initialize the random number generator

      call timex(itime1)
      call getunixtime(idum)
      call getasciitime(idum,ctime)
      idum=-idum
      write(6,*)'seed:',idum,' from start time ',ctime
      cstk=ran(idum)

      nevent=0

      t_elapse=0.
      itime2=itime1

      ntries=0
      sig_int=0.
      sig_tot=0.

c     Initialize BOS
     
      if (ncycle .eq. 0)then

        bosout = file_bos
        recname = 'MCEVENT'

c     Initialize BOS
     
        call bos_ini(recname)
        ncycle=1

      else
      close (33)
      bosout=file_bos
      call fparm ('OPEN ' //recname// 'UNIT=33 '// 
     &     'FILE="'//bosout//'"'// 
     &     ' RECL=32760 ACTION=WRITE STATUS=NEW FORM=BINARY')

      endif
 
c     set up the ntuple file
      lrecl=1024
      call hlimit(100000)
      call hropen(1,'aopeakgen',filerz,'n',lrecl,istat)
      call hbookn(10,'func1',n,'aopeakgen',1000,tag)

      open(unit=12,file=file_out,status='new')

      write(12,*)' AO Calculation of Single Pion Production'
      write(12,*)'starting time:', ctime
      write(12,*)' Epirea (1 for pi0, 3 for pi+) =',epirea
      write(12,*)'Target thickness =',t_targ,' (r.l.)'
      write(12,*)' Incident electron energy =',ebeam,' GeV'

      write(12,*)'Electron Q**2 limits:',q2_min,q2_max
      write(12,*)'lower and upper limit for scattered electron',
     * ' energy(GeV):',ep_min,epmax
      write(12,*)' Minimum photon energy for integration (delta):',delta

c     Do a preliminary calculation to estimate the maximum value
c     of the integrand

c     calculate the scattering angle (single precision and double precision
c     version) in radians

 10   q2=q2_min
c     calculate the energy and momentum of the scattered electron,
c     and calculate Q**2 at the delta mass, 1.232 GeV.
      q0=(2.00**2-mp**2+q2)/2./mp
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

c     Use a new variable in place of ek. Let uek=exp(-kek*ek)
c     ek=-(1/kexp)alog(uek).  The value, kek=5., was chosen empirically
c     by looking at the ek spectrum for E0=1.6 GeV.
c     Let uek range from 0 to 1. Then ek will range from 0 and infinity.
c     This requires a jacobian. Jacobian=1./(kexp*uek)=(1./kexp)exp(kexp*ek)
      kexp=5.
c     Let ek=delta*exp(alpha_k*u), where alpha_k=log(ebeam/delta)
c     jacobian=delta*alpha_k*exp(alpha*u)=alpha_k*ek
      arg=ebeam/delta
      alpha_k=alog(arg)
c
      sigr_max=0.
      sigr_p_max = 0.
      sigr_m_max = 0.
c
      cstk1=(es-ep*cst0)/(sqrt(es**2+ep**2-2*es*ep*cst0))
      cstk2=(es*cst0-ep)/(sqrt(es**2+ep**2-2*es*ep*cst0))
      if (cstk1 .gt. .98)cstk1=.98
      if (cstk2 .gt. cstk1-.04)cstk2=cstk1-.04

      phik=0.
      ek=delta
      jacob=alpha_k*ek*(q2**2/(2*es*ep))
      do i=1,1000
         csthcm=2.*(ran(idum)-0.5)
         phicm=360.0*ran(idum)

        csran=ran(idum)
        if (csran .gt. .75)then
            cstk=cstk1+csrng/2.5
        elseif (csran .gt. .5)then
           cstk=cstk1-csrng/2.5
        elseif (csran .gt. .25)then
           cstk=cstk2+csrng/2.5
        else
            cstk=cstk2-csrng/2.5
        endif
        Tk=acos(cstk)
        sntk=sin(Tk)
        sdotk=es*ek-ps*ek*cstk*csths-ps*ek*sntk*snths*cos(phik)
        pdotk=ep*ek-pp*ek*cstk*csthp-pp*ek*sntk*snthp*cos(phik)
        if (flag_e_hel.eq.0) then
          sigr= sigma(ek,Tk,csthcm,phicm,0)
          sigr=sigr*csrng /reg1
          sigr=sigr*jacob
          if (sigr .gt. sigr_max)then
            sigr_max=sigr
            tk_max=Tk
            csthcm_max=csthcm
            phicm_max=phicm
          endif
        else
          sigr= sigma(ek,Tk,csthcm,phicm,1)
          sigr=sigr*csrng /reg1
          sigr=sigr*jacob
          if (sigr .gt. sigr_p_max)then
            sigr_p_max=sigr
            tk_p_max=Tk
            csthcm_p_max=csthcm
            phicm_p_max=phicm  
          endif
          sigr= sigma(ek,Tk,csthcm,phicm,-1)
          sigr=sigr*csrng /reg1
          sigr=sigr*jacob
          if (sigr .gt. sigr_m_max)then
            sigr_m_max=sigr
            tk_m_max=Tk
            csthcm_m_max=csthcm
            phicm_m_max=phicm  
          endif
        endif
      enddo
      if (flag_e_hel.eq.0) then
        write(6,*)'sigr_max,tk_max,csthcm_max,phicm_max',
     *  sigr_max,tk_max,csthcm_max,phicm_max
        write(12,*)'sigr_max,tk_max,csthcm_max,phicm_max',
     *  sigr_max,tk_max,csthcm_max,phicm_max
        sigr_max=sigr_max*fmcall
        write(6,*)'sigr_max changed to',sigr_max
        write(12,*)'sigr_max changed to',sigr_max
      else
        write(6,*)'sigr_p_max,tk_p_max,csthcm_p_max,phicm_p_max',
     *  sigr_p_max,tk_p_max,csthcm_p_max,phicm_p_max
        write(12,*)'sigr_p_max,tk_p_max,csthcm_p_max,phicm_p_max',
     *  sigr_p_max,tk_p_max,csthcm_p_max,phicm_p_max
c
        write(6,*)'sigr_m_max,tk_m_max,csthcm_m_max,phicm_m_max',
     *  sigr_m_max,tk_m_max,csthcm_m_max,phicm_m_max
        write(12,*)'sigr_m_max,tk_m_max,csthcm_m_max,phicm_m_max',
     *  sigr_m_max,tk_m_max,csthcm_m_max,phicm_m_max
c
        sigr_p_max=sigr_p_max*fmcall
        write(6,*)'sigr_p_max changed to',sigr_p_max
        write(12,*)'sigr_p_max changed to',sigr_p_max  
        sigr_m_max=sigr_m_max*fmcall
        write(6,*)'sigr_m_max changed to',sigr_m_max
        write(12,*)'sigr_m_max changed to',sigr_m_max      
      endif


c   %%%%%%%%%%%%%%%%%%% Main Calculation  %%%%%%%%%%%%%%%%%%%%%%%
c     Use a Monte-Carlo to calculate a distribution of nmax events
c     distributed according to the Mo-Tsai integrand.
      e_hel = 0
 20   continue
      ntries=ntries+1
      flag_nu_cm2 = 0
      if(flag_e_hel.eq.1) e_hel = get_spin(idum)
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
c     Cut off the radiation at es=ebeam/4.
      if (es .lt. ebeam/4.)go to 20
      ps=sqrt(es**2-mel**2)
      rs=ps/es


C     Make a random choice of Q**2
      uq2=uq2_min+uq2rng*ran(idum)
      q2=1./uq2
      qsq=q2

c     calculate the energy and momentum of the scattered electron,
c      and calculate Q**2

      ep=epmax-eprng*ran(idum)
c     check to see if the scattered electron energy is below the
c     detector threshold.
      if (ep .lt. ep_min)go to 20

      q0=es-ep
      w2=mp**2+2*mp*q0-q2
      if (w2 .lt. wg**2+.002)go to 20
      epw=sqrt(w2)
      if(epw.gt.w_max) go to 20
c
c     calculate the electron scattering angle, s=sin(theta/2)**2 
      s=q2/4/es/ep

c     cut off scattering at 90 degree
      if (s .gt. .5)go to 20

c     calculate the scattering angle (single precision and double precision
c     version) in radians
      th0=2.*asin(sqrt(s))
      theta=th0*180./pi
      T0=th0
      snt0=sin(th0)
      cst0=cos(th0)


c     check to see if the scattered electron energy is above
c     the pion threshold for this angle.
      ep_test=(mp**2+2*mp*es-wg**2)/2./(mp+2.*es*s)
      if (ep .gt. ep_test)go to 20
      pp=sqrt(ep**2-mel**2)
      rp=pp/ep

      qvecx=-pp*sin(th0)
      qvecz=ps-pp*cos(th0)
c
c     calculate kinematic quantities needed for the Mo and Tsai calculation
      u0=es-ep+mp
      pu=ps**2+pp**2-2*ps*pp*cst0
      pu=sqrt(pu)
      uu=u0**2-pu**2
      csths=(ps-pp*cst0)/pu

      csthp=(ps*cst0-pp)/pu
      snths=1.-csths**2
      if (snths .lt. 0.)go to 20
      snths=sqrt(snths)
      snthp=1.-csthp**2
      snthp=sqrt(snthp)
      ts=acos(csths)

      tp=acos(csthp)
      sp=es*ep-ps*pp*cst0

      cstk1=csths
      cstk2=csthp
        if (cstk1 .lt. cstk2)then
           cstmp=cstk1
           cstk1=cstk2
           cstk2=cstmp
        endif
        if ((1.-cstk1) .lt. csrng)cstk1=1.-csrng
        if ((cstk1-cstk2) .lt. 2.*csrng)cstk2=cstk1-2.*csrng

        csran=ran(idum)
 71     if (csran .lt. reg1)then
            cstk=cstk1+(2.*ran(idum)-1.)*csrng
            mcfac=1. /reg1
            sp_peak=1 
	     phik=0.
         elseif(csran .lt. reg1+reg2)then
            cstk=cstk2+(2.*ran(idum)-1.)*csrng
            mcfac=1./(reg2)
            sp_peak=-1
            phik=0.
         else
 72         cstk=-2.*(ran(idum)-0.5)
            phik=2.*pi*(ran(idum)-0.5)
            sp_peak=0
            mcfac=1./(1.-reg1-reg2)
        endif


      Tk=acos(cstk)
      sntk=sin(Tk)
      mpfac=1.
      ekmax=0.5*(uu-wg**2)/(u0-pu*cstk)
c     choose ek by making a change of variables

 78   elas_sel=ran(idum)
      if (elas_sel .gt. ereg)then
         uek=ran(idum)
         ek=delta*exp(alpha_k*uek)
         jacob=alpha_k*ek
         mkfac=1./(1.-ereg)
       else
         ek=delta*ran(idum)
         mkfac=1./ereg
         jacob=delta
      endif

      if (ek .gt. ekmax)go to 20

c
      csthcm=-1.+2.*ran(idum)
c      csthcm = -0.1 + 0.2*ran(idum)
c
      phicm=360.0*ran(idum)
c     phicm= 90.0 + 0.18*ran(idum)

      if (ek .le. delta)then      
c
c     calculate the non-radiative cross section
c
        call wjc(qsq,epw**2,csthcm,phicm,th0,EPIREA,e_hel,
     >    sigma0,sigu,sigl,sigt,sigi,sigip,t_s)
        if(flag_nu_cm2.eq.1) go to 20

c    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c     The AO program calculates sigu,sigl,sigt and sigi with
c     the kinematic factors included. To convert them to the
c     form needed for the above problem we must use
        epeps=1.+2.*(1+q0**2/qsq)*s/(1-s)
        epeps=1./epeps
        kfac=(w2-mp**2)/2./mp
c       cfac=1/Gamma_T
c        cfac=2*pi**2*qsq*(es/ep)*(1-epeps)/kfac/alpha
c        sigu=sigu*cfac
c        sigl=sigl*cfac
c        sigt=sigt*cfac
c        sigi=sigi*cfac
c   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c     Mo and Tsai give a formula for the cross section in terms
c     of amplitudes, f and g.  Here we try to extract f and g
c     from the AO cross section.  I may not have this part right.

c        sig0=4.*pi**2*alpha/mp**2
c     The following would work if there were no polarization terms
c      f=2.*kfac/(mp**3)*qsq/(qsq+q0**2)*(sigu+sigl)/sig0
c      g=(2.*kfac/mp)*(sigu)/sig0
c     I don't know what to do with the polarization terms: these give
c     the dependence on the center of mass phi decay angle.  I try
c     the following:
        nu=(w2+qsq-mp**2)/2/mp
        f=(1./(2*pi**2*alpha*mp)) * (kfac/(1.+nu**2/qsq)) * (sigu+sigl)
        g=mp/(2*pi**2*alpha) * kfac * sigu
        fkt=(w2-mp**2+mpi**2)/2./epw
        fkt=sqrt(fkt**2-mpi**2)*2.*epw/(w2-mp**2)
        f=f*fkt
        g=g*fkt

c     Calculate the non-radiative cross section.
        signr=2*(alpha*ep/qsq)**2
     &   * (f*mp*cos(th0/2)**2+2*g*sin(th0/2)**2/mp)
c     Modulate the cross section with the polarization and interference
c     terms
        signr=signr * (1.+(epeps*sigt*cos(phicm*pi/90.)
     &  +sqrt(epeps*(1.+epeps)/2)*sigi*cos(phicm*pi/180.)
     &  +sqrt(epeps*(1.-epeps)/2)*sigip*sin(phicm*pi/180.)*e_hel)
     &  /(sigu+epeps*sigl))


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
        mcfac=1.
        mpfac=1.
c        sigr=sigr1/delta
        if (sigr .gt. 0.)go to 28
        go to 20

      endif
c
      sdotk=es*ek-ps*ek*(cstk*csths+sntk*snths*cos(phik))
      pdotk=ep*ek-pp*ek*(cstk*csthp+sntk*snthp*cos(phik))
      if (sp_peak .ne. 0)then
        phik=0.
        mpfac=1.
        sigr=sig_peak(ek,Tk,csthcm,phicm,sp_peak,sigu,sigl,sigt,sigi
     1       ,sigip,e_hel)
        if(flag_nu_cm2.eq.1) go to 20
      else
        mpfac=1.
c         sdotk1=es*ek
c         pdotk1=ep*ek-pp*ek*csths
c         sdotk2=es*ek-ps*ek*csthp
c         pdotk2=ep*ek
c         sdotk3=sdotk
c         pdotk3=pdotk
c         sdotk=sdotk1
c         pdotk=pdotk1
c         sigr1=sigma(ek,Tk,csthcm,phicm)
c         sdotk=sdotk2
c         pdotk=pdotk2
c         sigr2=sigma(ek,Tk,csthcm,phicm)
c         sdotk=sdotk3
c         pdotk=pdotk3
c         sigr3=sigma(ek,Tk,csthcm,phicm)
c         sigr=sigr3-(sigr1+sigr2)
        sigr=sigma(ek,Tk,csthcm,phicm,e_hel)
        if(flag_nu_cm2.eq.1) go to 20
      endif

28    jacob=jacob/(2*es*ep)*q2**2

      sigr=sigr*jacob
c     Make an arbitrary normalization of sigr for purposes of
c     the monte carlo selection.  The idea here is not to generate
c     too many events for the same ek, and not to spend too much
c     time spinning our wheels generating no events.
      sigr=mcfac*mpfac*mkfac*sigr
c
      if(flag_e_hel.eq.0) then
        sig_ratio=sigr/sigr_max
      elseif(flag_e_hel.eq.1.and.e_hel.eq.1) then
        sig_ratio=sigr/sigr_p_max
      elseif(flag_e_hel.eq.1.and.e_hel.eq.-1) then
        sig_ratio=sigr/sigr_m_max
      endif
c

      if (ek .gt. delta .and. sp_peak .eq. 0 .and. sig_ratio .ge. 2.)
     +  go to 71

c     Choose the number of times, mcall, to call the routine used
c     to calculate kinematic quantities for the n-tuple.
      rtest=ran(idum)
      mcall=sig_ratio
      stest=sig_ratio-mcall
      if (stest .gt. rtest)mcall=mcall+1


      if (mcall .gt. mcall_max)mcall_max=mcall

      sig_tot=sig_tot+sigr
c
c     increment ntries by the value of mcall. It is also incremented by
c     1 at the beginning of the loop
      if (mcall .eq. 0)go to 30

      if (mcall .gt. 10)then
        if (sp_peak .eq. 0)go to 20
        write(6,*) flag_nu_cm2, 'flagfff'
	    write(6,*)'es,ep,theta,1000*ek,tk,phik,csthcm,phicm,sig_ratio'
        write(6,931)es,ep,theta,1000*ek,tk,phik,csthcm,phicm,sig_ratio
        write(6,*)(es*ek-sdotk)/ps/ek,(ep*ek-pdotk)/pp/ek
        write(6,*)cstk,cstk1,cstk2,sp_peak,e_hel
931	    format(3f8.3,f8.1,4f8.3,e12.4)
      endif

      if (ek .gt. delta)then
        if (sp_peak .eq. 0)then
          peak_credit=peak_credit+mcall
        else
          mcall_tmp=mcall
          peak_credit_tmp=peak_credit
          peak_credit=peak_credit-mcall
          if (peak_credit .lt. 0)peak_credit=0
          mcall=mcall-peak_credit_tmp
          if (mcall .le.0)go to 20
        endif
      endif

      ep_sav=ep

      do j=1,mcall

c     Calculate the radiation loss for the electron leaving the target

 222     xs=ran(idum)
         eloss=xs**(1./targp)
         gxs=1.-eloss
         xtest=ran(idum)
         if (xtest.gt.gxs)go to 222
         ep=ep_sav*(1.-eloss)
         if (ep .lt. ep_min)go to 24
         call missm(ebeam,es,ep,th0,ek,cstk,phik,m_h_s,mpi_s,
     *  ppx,ppy,ppz,eprot,ppix,ppiy,ppiz,epi,csthcm,phicm,wreal,mm2)

         if(mm2 .eq. 0. .or. ep .lt. ep_min)go to 24

         w2=mp**2+2*mp*(ebeam-ep)-2*es*ep*(1-cos(th0))+2.*mel**2
         epw=sqrt(w2)
         if(epw.lt.w_min) go to 20
         if(epw.gt.w_max) go to 20
c
c     Calculate the members of the n-tuple and ouput it to the rz file.
         ntp(1)=es
         ntp(2)=ep
         ntp(3)=theta
         ntp(4)=epw
         ntp(5)=wreal
         ntp(6)=ppx
         ntp(7)=ppy
         ntp(8)=ppz
         ntp(9)=eprot
         ntp(10)=ppix
         ntp(11)=ppiy
         ntp(12)=ppiz
         ntp(13)=epi
         ntp(14)=csthcm
         ntp(15)=phicm
         ntp(16)=mm2
         ntp(17)=ek
         ntp(18)=cstk
         ntp(19)=phik*180./pi
         ntp(20)=qvecx
         ntp(21)=qvecz
         ntp(22)=q0
         ntp(23)=cst0
         ntp(24)=e_hel
         ntp(25)=qsq
         ntp(26)=t_s
         call hfn(10,ntp)
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
c     momentum of scattered kaon
         p(2,1) = ppix*rotc+ppiy*rots
         p(2,2) = ppiy*rotc-ppix*rots
         p(2,3) = ppiz
         p(2,4) = epi
         if (npart .eq. 4)then
           p(3,1) = ppx*rotc+ppy*rots
           p(3,2) = ppy*rotc-ppx*rots
           p(3,3) = ppz
           p(3,4) = eprot
    	   sink=sqrt(1.-cstk**2)
           p(4,1) = ek*sink*cos(phik)
           p(4,2) = ek*sink*sin(phik)
           p(4,3) = ek*cstk
           p(4,4) = ek
c     suppress radiated photons when E-gamma is less than delta
           if (ek .le. delta)then
             p(4,1)=0.
             p(4,2)=0.
             p(4,3)=1.e-5
             p(4,4)=1.e-5
           endif
         endif
         call bos_out 	! Pack the BOS banks and write out to file        
 24      continue
       enddo


c     Talk to the user every now and then.
       ntell=nevent/nprint-ntold
       if (ntell .gt. 0)then
          write(6,*)' ntries, nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max
          write(12,*)' ntries, nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max
          events=nevent
          tries=ntries
          sig_int=events/tries
c     photon phase space = 4*pi*delta-omega=4*pi (after change of variables)
c     hadron phase space = 4*pi
c     electron phase space =2*pi*ucrng*eprng

          sig_int=sig_int*sigr_max*(4.*pi)**2*(2.*pi*uq2rng*eprng)
          sig_sum=sig_tot*(4.*pi)**2*(2.*pi*uq2rng*eprng)/tries

          write(6,*)' Integrated cross section =',sig_int,sig_sum,
     *    ' micro-barns'
          write(6,*)' Beam time at Lum=1.0E34 =',events/sig_sum*1.E-4
     *    ,' seconds'
          write(12,*)' Integrated cross section =',sig_int,sig_sum
     *    ,' micro-barns'
          write(12,*)' Beam time at Lum=1.0E34 =',events/sig_sum*1.E-4
     *    ,' seconds'
          call timex(itime2)
          t_elapse=t_elapse+INT(itime2-itime1)
          itime1=itime2
          write(6,*)' Elapsed CPU time = ',t_elapse/60,' minutes'
          write(12,*)' Elapsed CPU time = ',t_elapse/60,' minutes'
          ntold=ntold+1
       endif

c     Do we have enough events in the n-tuple?
       if (nevent .gt. nmax)go to 50
 30    go to 20


c     Close out the n-tuple file
 50   call hrout(0,icycle,' ')
      call hrend('aopeakgen')
      close(12)

      open(unit=14,file=file_sum,status='new')
      write(14,*)' AO Calculation of Single Pion Production'
      write(14,*)' Epirea (1 for pi0, 3 for pi+) =',epirea
      write(14,*)'Target thickness =',t_targ,' (r.l.)'
      write(14,*)' Incident electron energy =',ebeam,' GeV'

      write(14,*)'Electron Q**2 limits:',q2_min,q2_max
      write(14,*)'lower and upper limit for scattered electron',
     * ' energy(GeV):',ep_min,epmax
      write(14,*)' Minimum photon energy for integration (delta):',delta
      write(14,*)'sigr_max,ek_max,tk_max,csthcm_max,phicm_max',
     * sigr_max,ek_max,tk_max,csthcm_max,phicm_max
      write(14,*)' ntries, nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max
      write(14,*)' Integrated cross section =',sig_int,sig_sum
     *    ,' micro-barns'
      write(14,*)' Beam time at Lum=1.0E34 =',events/sig_sum*1.E-4
     *    ,' seconds'
      write(14,*)' Elapsed CPU time = ',t_elapse/60,' minutes'
      write(14,*)' CPU time/event = ', t_elapse/nevent,' sec'
      close(14)
 99   continue

      end

      real function sigma(ek,Tk,epcos,epphi,e_hel)

c     Calculate the single pion electroproduction cross section with
c      radiative tail, according to the prescription in Mo and Tsai.
c     Mo and Tsai  calculate dsigma/d_omega dp for (omega>delta)
c     omega is the energy of the radiated photon.

c     The Mo and Tsai cross section for the 3-3 resonance is replaced with
c     the AO cross section for single pion production from the proton.


      implicit none
      COMMON/ALPHA/ ALPHA,PI,MP,MPI,MEL,WG,EPIREA
      common /radcal/T0,es,ep,ps,pp,rs,rp,u0,pu,uu,cst0,snt0,csths,csthp
     * ,snths,snthp,pdotk,sdotk
      common /flag/flag_nu_cm2
      real*8 ek,Tk
      real*8 alpha,pi,mp,mpi,mel,wg
      real*8 T0,es,ep,ps,pp,rs,rp,u0,pu,uu,pdotk,sdotk
      real*8 cst0,snt0,csths,csthp,snths,snthp
      real*8 csthk,snthk,qq,mf2
      real*8 sp
      real*4 ffac1,ffac2,ffac3,ffac4,ffac5,ffac6,ffac
      real*4 gfac1,gfac2,gfac3,gfac4,gfac

      real*4 f,g,fkt
      real*4 sig_r,sigf
      real nu
c     arguments for sigma_calc:
      real*4 qsq,epw,th0,sigma0,sigu,sigt,sigl,sigi,sigip
      real epcos,epphi
      real*4 q0,kfac
      real s2,epeps, t_s
     
      integer e_hel
      integer epirea
      integer flag_nu_cm2

      sigma=0.
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

c     Use the AO cross section to fake up the values of f and g
c     needed for the Mo and Tsai integration.
c     This should be compared to the formulas given in Mo and Tsai
c     for the 3-3 resonance.  We ought to agree pretty well in the
c     3-3 resonance region.
c
      call wjc(qsq,epw**2,epcos,epphi,th0,EPIREA,e_hel,
     >    sigma0,sigu,sigl,sigt,sigi,sigip,t_s)
c
      if(flag_nu_cm2.eq.1) return
c
c     According to my calculations
c     F=(2K/Mp**3)[q2/(q2+nu**2)][(sigt+sigl)/sig0]
c     G=(2K/Mp)(sigt/sig0)
c     where K=(W**2-Mp**2)/(2Mp)
c     sig0=4*pi**2*alpha/mp**2=127 microbarns
C     In terms of the sigt and sigl, the cross section,
c     differential in omega_e and E_e, is
c     sigma=[alpha/(4 pi**2][2 * K * E_s]/[q2 * E_s * (1-epsilon)]
c     *  (sigt+epsilon*sigl)
c     where
c     1/epsilon = 1+2tan(theta_e/2)**2(1+nu**2/q2)

c    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c     The AO program calculates sigu,sigl,sigt and sigi with
c     the kinematic factors included. To convert them to the
c     form needed for the above problem we must use
      s2=(1.-cst0)/2.
      epeps=1+2*(1+q0**2/qsq)*s2/(1-s2)

      if (epeps .gt. 1.)go to 120
        sigma=0.1e-30
        return
 120  epeps=1./epeps

      Kfac=(mf2-mp**2)/2./mp
c      cfac=2*pi**2*qsq*es*(1-epeps)/kfac/ep/alpha
c      sigu=sigu*cfac
c      sigl=sigl*cfac
c      sigt=sigt*cfac
c      sigi=sigi*cfac
c   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c      sig0=4.*pi**2*alpha/mp**2
c     The following would work if there were no polarization terms
c      f=2.*kfac/(mp**3)*qsq/(qsq+q0**2)*(sigu+sigl)/sig0
c      g=(2.*kfac/mp)*(sigu)/sig0
c     I don't know what to do with the polarization terms: these give
c     the dependence on the center of mass phi decay angle.  I try
c     the following and modulate the cross section later:
      nu=(mf2-mp**2+qsq)/2./mp
      if (nu .le. 0.)then
        sigma=0.1e-30
        return
      endif

      f=kfac/(2.*pi**2*alpha*mp)/(1.+nu**2/qsq)*(sigu+sigl)
      g=mp/(2.*pi**2*alpha)*kfac*sigu
      fkt=(epw**2-mp**2+mpi**2)/2./epw
      fkt=sqrt(fkt**2-mpi**2)*2.*epw/(epw**2-mp**2)
      f=f*fkt
      g=g*fkt

c      f_old=2.*kfac/(mp**3)*qsq/(qsq+q0**2)*(sigu+sigl)/sig0
c      g_old=(2.*kfac/mp)*(sigu)/sig0

c     ??????????????????????????
c     The following formula is the same as B.8 in Mo and Tsai,
c     except I have divided by 2pi.  It seems to me that the
c     result doesn't make sense otherwise.
c     ?????????????????????????


      sig_r=((alpha**3/(2*pi*qq)**2)/mp)*(ep/es)*ek
      sigf=mp**2*f*ffac+g*gfac
      if (sigf .gt. 0.)go to 122
      sigma=0.1e-30
      return

c     Modulate the cross section with the polarization and interference
c     terms
 122  if (sigu+epeps*sigl .gt. 0.)go to 123
      sigma=0.1e-30
      return

 123  sigf=sigf * (1.+(epeps*sigt*cos(epphi*pi/90.)
     &  +sqrt(epeps*(1.+epeps)/2)*sigi*cos(epphi*pi/180.)
     &  +sqrt(epeps*(1.-epeps)/2)*sigip*sin(epphi*pi/180.)*e_hel)
     &  /(sigu+epeps*sigl))
c      print *, sigf, e_hel
      if (sigf .gt. 0.)go to 124
      sigma=0.1e-30
      return

 124  sigma=sig_r*sigf

      return
      end


      real function sig_peak(ek,Tk,epcos,epphi,sp_peak,
     1       sigu,sigl,sigt,sigi,sigip,e_hel)

c     Calculate the single pion electroproduction cross section with
c      radiative tail, according to the prescription in Mo and Tsai.
c     Mo and Tsai  calculate dsigma/d_omega dp for (omega>delta)
c     omega is the energy of the radiated photon.

c     The Mo and Tsai cross section for the 3-3 resonance is replaced with
c     the AO cross section for single pion production from the proton.


      implicit none
      COMMON/ALPHA/ ALPHA,PI,MP,MPI,MEL,WG,EPIREA
      common /radcal/T0,es,ep,ps,pp,rs,rp,u0,pu,uu,cst0,snt0,csths,csthp
     * ,snths,snthp,pdotk,sdotk
      common /flag/flag_nu_cm2

      real*8 ek,Tk
      real*8 alpha,pi,mp,mpi,mel,wg
      real*8 T0,es,ep,ps,pp,rs,rp,u0,pu,uu,pdotk,sdotk
      real*8 cst0,snt0,csths,csthp,snths,snthp
      real*8 csthk,snthk,qq,mf2
      real*8 sp
      real*4 ffac1,ffac2,ffac3,ffac4,ffac5,ffac6,ffac
      real*4 gfac1,gfac2,gfac3,gfac4,gfac

      real*4 f,g,fkt
      real*4 sig_r,sigf
      real nu
c     arguments for sigma_calc:
      real*4 qsq,epw,th0,sigma0,sigu,sigt,sigl,sigi,sigip
      real epcos,epphi
      real*4 q0,kfac
      real s2,epeps
      real pksk,pk2,sk2,pk1,sk1
      real arg,pi2,pi4
      real t_s

      integer e_hel
      integer epirea
      integer sp_peak
      integer flag_nu_cm2

      sig_peak=0.
c      pi4=4.*pi
      pi4=1.
      pi2=pi4/2.
      th0=T0
      csthk=cos(Tk)
      snthk=sin(Tk)
      qq=2*mel**2-2*es*ep+2*ps*pp*cst0-2*ek*(es-ep)+2*ek*pu*csthk
      mf2=uu-2*ek*(u0-pu*csthk)
      if (mf2 .lt. wg**2 .or. qq .ge. 0.)then
         sig_peak=0.
         return
      endif

      epw=sqrt(mf2)

      sp=es*ep-ps*pp*cst0
      arg=2.*sp/mel**2
      pksk=(1./ek**2)*(1./sp)*alog(arg)
      pk2=1/ek**2
      sk2=1./ek**2
      arg=2.*ep/mel
      pk1=1./(ek*ep)*alog(arg)
      arg=2.*es/mel
      sk1=1./(ek*es)*alog(arg)
		
c	do s peak
      if (sp_peak .eq. 1)then
        ffac1=0.
        ffac2=-pi4*sk2*(2.*ep*(es-ek)+qq/2)
        ffac3=-pi4
        ffac4=2.*pi2*pksk*(mel**2*(sp-ek**2)+sp*(2*es*ep-sp+ek*(es-ep)))       
        ffac5=0.
        ffac6=-pi4*(2*(es*ep-ep*ek+es*es)+qq/2-sp-mel**2)*sk1

        gfac1=pi4*(2*mel**2+qq)* sk2
        gfac2=2.*pi4
        gfac3=4.*pi2*sp*(sp-2*mel**2)*pksk
        gfac4=pi4*(2*sp+2*mel**2-qq)*(-sk1)

      else
        ffac1=-pi4*pk2*(2.*es*(ep+ek)+qq/2)
        ffac2=0.
        ffac3=-pi4
        ffac4=2.*pi2*pksk*(mel**2*(sp-ek**2)+sp*(2*es*ep-sp+ek*(es-ep)))       
        ffac5=pi4*(2*(es*ep+es*ek+ep*ep)+qq/2-sp-mel**2)*pk1
        ffac6=0.

        gfac1=pi4*(2*mel**2+qq)* pk2
        gfac2=2.*pi4
        gfac3=4.*pi2*sp*(sp-2*mel**2)*pksk
        gfac4=pi4*(2*sp+2*mel**2-qq)*pk1
 
      endif
        ffac=ffac1+ffac2+ffac3+ffac4+ffac5+ffac6
        if (ffac .le. 0.)then
         sig_peak=0.1e-30
         return
        endif
        gfac=gfac1+gfac2+gfac3+gfac4
        if (gfac .le. 0.)then
          sig_peak=0.1e-30
         return
        endif
      
      qsq=-qq
      q0=es-ep

c     Use the AO cross section to fake up the values of f and g
c     needed for the Mo and Tsai integration.
c     This should be compared to the formulas given in Mo and Tsai
c     for the 3-3 resonance.  We ought to agree pretty well in the
c     3-3 resonance region.
c
          call wjc(qsq,epw**2,epcos,epphi,th0,EPIREA,e_hel,
     >    sigma0,sigu,sigl,sigt,sigi,sigip,t_s)
c
      if(flag_nu_cm2.eq.1) return
c
c     According to my calculations
c     F=(2K/Mp**3)[q2/(q2+nu**2)][(sigt+sigl)/sig0]
c     G=(2K/Mp)(sigt/sig0)
c     where K=(W**2-Mp**2)/(2Mp)
c     sig0=4*pi**2*alpha/mp**2=127 microbarns
C     In terms of the sigt and sigl, the cross section,
c     differential in omega_e and E_e, is
c     sigma=[alpha/(4 pi**2][2 * K * E_s]/[q2 * E_s * (1-epsilon)]
c     *  (sigt+epsilon*sigl)
c     where
c     1/epsilon = 1+2tan(theta_e/2)**2(1+nu**2/q2)

c    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c     The AO program calculates sigu,sigl,sigt and sigi with
c     the kinematic factors included. To convert them to the
c     form needed for the above problem we must use
      s2=(1.-cst0)/2.
      epeps=1+2*(1+q0**2/qsq)*s2/(1-s2)

      if (epeps .le. 1.)then
        sig_peak=0.1e-30
        return
      endif
      epeps=1./epeps

      Kfac=(mf2-mp**2)/2./mp
c      cfac=2*pi**2*qsq*es*(1-epeps)/kfac/ep/alpha
c      sigu=sigu*cfac
c      sigl=sigl*cfac
c      sigt=sigt*cfac
c      sigi=sigi*cfac
c   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c      sig0=4.*pi**2*alpha/mp**2
c     The following would work if there were no polarization terms
c      f=2.*kfac/(mp**3)*qsq/(qsq+q0**2)*(sigu+sigl)/sig0
c      g=(2.*kfac/mp)*(sigu)/sig0
c     I don't know what to do with the polarization terms: these give
c     the dependence on the center of mass phi decay angle.  I try
c     the following and modulate the cross section later:
      nu=(mf2-mp**2+qsq)/2./mp
      if (nu .le. 0.)then
        sig_peak=0.1e-30
        return
      endif

      f=kfac/(2.*pi**2*alpha*mp)/(1.+nu**2/qsq)*(sigu+sigl)
      g=mp/(2.*pi**2*alpha)*kfac*sigu
      fkt=(epw**2-mp**2+mpi**2)/2./epw
      fkt=sqrt(fkt**2-mpi**2)*2.*epw/(epw**2-mp**2)
      f=f*fkt
      g=g*fkt

c      f_old=2.*kfac/(mp**3)*qsq/(qsq+q0**2)*(sigu+sigl)/sig0
c      g_old=(2.*kfac/mp)*(sigu)/sig0

c     ??????????????????????????
c     The following formula is the same as B.8 in Mo and Tsai,
c     except I have divided by 2pi.  It seems to me that the
c     result doesn't make sense otherwise.
c     ?????????????????????????


      sig_r=((alpha**3/(2*pi*qq)**2)/mp)*(ep/es)*ek
      sigf=mp**2*f*ffac+g*gfac

c     Modulate the cross section with the polarization and interference
c     terms

 123  sigf=sigf * (1.+(epeps*sigt*cos(epphi*pi/90.)
     &  +sqrt(epeps*(1.+epeps)/2)*sigi*cos(epphi*pi/180.)
     &  +sqrt(epeps*(1.-epeps)/2)*sigip*sin(epphi*pi/180.)*e_hel)
     &  /(sigu+epeps*sigl))

 124  sig_peak=sig_r*sigf
       
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
         mj=mseed+iabs(idum)
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
      if (ran .le. 0. .or. ran .ge. 1.)then
         write(6,*)' random error, ran =',ran,inext,inextp,mj
         go to 25
      endif

      return
      end

c   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      subroutine missm(ebeam,es,ep,th0,ephot,cstk,phik,m_h,mpi,ppx,
     * ppy,ppz,eprot,ppix,ppiy,ppiz,epi,csthcm,phicm,wreal,mm2)

c
c     Input:
c         ebeam = incident electron beam energy
c         es  = incident electron energy at interaction point
c         ep = scattered electron energy
c         th0 = electron scattering angle
c         ephot = energy of radiated photon
c         cstk = cosine of the photon angle (relative to the q vector)
c         phik = azimuthal angle of photon
c         csthcm = cosine of hadronic decay angle in the hadronic frame
c         phicm = phi angle in the hadronic frame
c     Output:
c         ppx, ppy, ppz = proton momentum components
c         eprot = proton energy
c         ppix,ppiy,ppiz = pion momentum components
c         epi = pion energy
c         wreal = true hadronic invariant mass
c         mm2 = experimental missing mass

         
c     Choose the hadronic decay angles randomly and calculate the
c     missing mass, the proton momenta and pion momenta.

      implicit none
      common /random/idum

      real*8 es,ep,ps,pp,ephot

      real beta
      real cstk
      real csthe
      real cstq
      real csthcm
      real cxx,cxy,cxz,cyx,cyy,cyz,czx,czy,czz
      real csphk
      real csphi
      real ebeam
      real ephotx,ephoty,ephotz,eph_dot_q
      real epcm
      real epi
      real epicm
      real eprot
      real ewreal
      real gamma
      real mel
      real mp
      real mpi, m_h
      real mm2
      real nu
      real pfac
      real phicm
      real phik
      real pi
      real ppix,ppiy,ppiz
      real ppx,ppy,ppz
      real ppiwx,ppiwy,ppiwz
      real ppwx,ppwy,ppwz
      real pstar
      real pwrx,pwry,pwrz,pwr
      real q2,qvec
      real qx,qz
      real q_dot_pp
      real snphk
      real snphi
      real snthcm
      real snthe
      real sntk
      real sntq
      real th0
      real w2
      real wmin
      real wreal

      integer*4 idum
c

      mp=.938
      mel=0.511E-3
      pi=3.14159
      wmin=mp+mpi
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
      if (wreal .le. wmin**2)go to 30
      wreal=sqrt(wreal)
c     go to the end of the loop if the hadronic mass is below the pion
c     threshold.

c     calculate the energy of the actual hadronic system, with radiation
      ewreal=nu+mp-ephot


c    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c     Calculate the momentum components of the nucleon and pion in the
c     Lab frame

      snthcm=(1.-csthcm**2)
      if (snthcm .le. 0.)then
         snthcm=0.0000001
      endif
      snthcm=sqrt(snthcm)
      

c     Calculate cos and sin of phicm
      csphi=cos(phicm*pi/180.)
      snphi=sin(phicm*pi/180.)

c     calculate laboratory components of the resonance momentum vector
         pwrz=qz-ephotz
         pwrx=qx-ephotx
         pwry=-ephoty
         pwr=sqrt(pwrz**2+pwrx**2+pwry**2)
c     calculate the relativistic factors, gamma and beta, for the resonance
         beta=pwr/ewreal
         gamma=ewreal/wreal

c     define angle cosines for the laboratory system with the resonance
c     as the z axis.  Choose the y axis of the resonance frame perpendicular
c     to the laboratory x axis.
         pfac=sqrt(pwry**2+pwrz**2)
         cxx=pfac/pwr
         cxy=-pwrx*pwry/pfac/pwr
         cxz=-pwrx*pwrz/pfac/pwr
         cyx=0
         cyy=pwrz/pfac
         cyz=-pwry/pfac
         czx=pwrx/pwr
         czy=pwry/pwr
         czz=pwrz/pwr

c     calculate the momentum of the pion and proton in the resonance frame
         pstar=( (wreal**2-m_h**2-mpi**2)**2/4.-(m_h*mpi)**2 )/wreal**2
         if (pstar .le. 0.)then
            pstar=0.000001
         endif
         pstar=sqrt(pstar)
c     Calculate the energy of the proton and pion in the resonance center
c     of mass frame.
         epcm  = sqrt(pstar**2+m_h**2)
         epicm = sqrt(pstar**2+mpi**2)

c     Calculate the pion momentum components and energy in the lab frame
c     where the z axis is the direction of the momentum of the resonance.
c     The center of mass angles are pion angles.
         ppiwx=pstar*snthcm*csphi
         ppiwy=pstar*snthcm*snphi
         ppiwz=gamma*(pstar*csthcm+beta*epicm)
         epi=gamma*(epicm+beta*pstar*csthcm)

c     Calculate momentum components of the pion in
c     the lab frame where the z axis is along the incident beam
         ppix=ppiwx*cxx+ppiwy*cyx+ppiwz*czx
         ppiy=ppiwx*cxy+ppiwy*cyy+ppiwz*czy
         ppiz=ppiwx*cxz+ppiwy*cyz+ppiwz*czz

c     Calculate the proton energy and momentum components
c     in the lab resonance frame.
c     The center of mass angles are pion angles.
         ppwx=-ppiwx
         ppwy=-ppiwy

c         ppwz=-ppiwz+gamma*beta*(epicm+epcm)
c        epicm+epcm=wreal
         ppwz=gamma*beta*wreal-ppiwz

         eprot=gamma*wreal-epi

c     Rotate the lab momentum components of the proton into the frame
c     where the z axis is along the incident electron beam.
         ppx=ppwx*cxx+ppwy*cyx+ppwz*czx
         ppy=ppwx*cxy+ppwy*cyy+ppwz*czy
         ppz=ppwx*cxz+ppwy*cyz+ppwz*czz
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c     Calculate the square of the missing mass, associated with the
c     momentum components of the charged hadron:
         q_dot_pp=qx*ppix+qz*ppiz
         mm2=-q2+mp**2+mpi**2+2*mp*(nu-epi)-2*nu*epi+2*q_dot_pp
         return
 30      mm2=0.
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
         return
      endif

      if (x .gt. 0.99 .and. x .lt. 1.01)then
         spence=pi**2/6.
         return
      endif

      if (x .gt. -1.01 .and. x .lt. -0.99)then
         spence=-pi**2/12.
         return
      endif

      if (x .gt. 0.)then
         spence=.1025+sintp(x)
         return
      endif
      spence=-0.0975+sintn(x)
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
C======================================================================
      function get_spin(iseed)
C----------------------------------------------------------------------
C-
C-   Purpose : Get spin (1 or -1)
C-
C-   Inputs  : random seed
C-
C-   Outputs : get_spin
C----------------------------------------------------------------------
      implicit none
	  integer get_spin
	  integer iseed
      real    random, ran 

	  random = ran(iseed)
      random = 0.5 - ran(iseed)
      get_spin = 1
      if(random.lt.0) get_spin = -1

	  return
	  end
