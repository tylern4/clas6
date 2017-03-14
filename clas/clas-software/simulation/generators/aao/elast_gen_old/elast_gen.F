      program elas_gen
c
c     This program makes an n-tuple that can be used with Paw to
c     make distributions of energies, angles, resonance
c     mass resulting from internal bremmstrahlung associated with elastic
c     scattering from a proton.
c     The exact integration formula of Mo and Tsai
c     is used.
c      The n-tuple contains the photon energy(EG), the true hadronic invariant
c     mass (W), the components of the proton momentum (PPX, PPY, PPZ),
c     the proton energy (EP),
c     the missing mass (MM),
c     and the photon angles relative to the q vector, (CSTHK, PHIK).

c     Some tricks are employed to make the monte-carlo concentrate
c     on the regions where the cross section is large.

      implicit none
#include "mc.inc"

      common /random/idum
      common /bos_events/es,ep,th0,ppx,ppy,ppz,eprot,egam,delta

      real cst0,cst00,cstmin,cstk,egam,es,ep0,ep,eprot,erng
      real epmin0
      real mm2,sig_el
      real mp,mel,phik,pi,ps,pp,ppx,ppy,ppz,ran,rtest
      real rs,s,sigr,sigr_max,sigr1,snt0,stest
      real th_min,th_max,th0,Tk,t_elapse,w,wmax
      real phran,mpfac,cran,mcfac,cstk1,cstk2,den
      real delta,jacob,ucos,ucos0,ucrng
      real a1,a2,a3
      real uek,epk
      real targs,targp,ebeam,t_targ
      real theta_pr,phi_pr
      real egdiv
      real sig_tot
      real cdiv1,cdiv2,cdiv3

      integer j,mcall,mcall_max,nprint,ntell,ntold
      integer elas_inc
      real    itime1, itime2
      integer*4 idum

      character*132 filerz
      character*132 fileout
      character*28 ctime
      character*8 recname

c     Parameters for the n-tuple, which is named func1 and contains
c     15 elements per event.
      common /pawc/h(1000000)
      integer*4 ntries

      integer h,n,nevent,nmax,lrecl,istat,icycle
      parameter (n=14)
      real*4 ntp(n)
c     tag is the an array of names for the variables in the n-tuple.
      character*5 tag(n)

      data tag /'  ES ','  EP ','THETE', '  W  ',' PPRX',' PPRY'
     * ,' PPRZ','EPROT','THETP','PHIPR',' MM  ','CSTHK','PHIK ','  EG '/


      DATA PI  /3.14159/,mp/0.938/,mel/0.511E-3/,ntold/0/
c      egdiv is the fraction of the time we look in the range of
c     photon energies from 0 to delta.
      data egdiv/0.5/
      data cdiv1/0.6/,cdiv2/0.8/,cdiv3/0.95/
c
      call getenvf('FRZN',filerz)
      call getenvf('FOUT',fileout)

c
      call par_setup(ebeam,epmin0,es,ep,ps,rs,t_targ,th_min,
     * th_max,nmax,delta,nprint,filerz,fileout,elas_inc)

      ntries=0
      nevent=0
      wmax=3.
      sig_tot=0.
      call timex(itime1)
      call bos_setup(recname,ebeam)
      call getunixtime(idum)
      call getasciitime(idum,ctime)
      idum=-idum
      write(6,*)'seed:',idum,' from start time ',ctime
c
      t_elapse=0.
      itime2=itime1
c

c
c     set up the ntuple file
      lrecl=1024
      call hlimit(100000)
      call hropen(1,'elasgen',filerz,'n',lrecl,istat)
      call hbookn(10,'func1',n,'elasgen',1000,tag)


      call sigmax_calc(sigr_max,th_min,es,ps,epmin0,wmax,erng,
     * delta,egdiv,cdiv1)

c     Use a Monte-Carlo to calculate a distribution of nmax events
c     distributed according to the Mo-Tsai integrand.

c     Choose two limits for the electron scattering angle:
      cst00=cos(th_min*pi/180.)
      cstmin=cos(th_max*pi/180.)

      ucos0=1./(1.-cstmin)
      ucrng=1./(1-cst00)-ucos0

 20   continue
      ntries=ntries+1

c     calculate the energy of the electron at the scattering point
c     after making its way through the target.  First, randomly
c     choose the interaction point.
 21   targs=t_targ*ran(idum)
      targp=t_targ-targs

c     Now calculate the radiation loss
      es=ebeam
      call stragl(es,targs)

c     Cut off the calculation at 1.4*epmin0
      if (es .lt. 1.4*epmin0)go to 20

      ps=sqrt(es**2-mel**2)
      rs=ps/es

c     calculate the scattering angle
c     calculate s=sin(theta/2)**2, cos(theta)

      ucos=ucos0+ucrng*ran(idum)
      cst0=1.-1./ucos
      jacob=1./ucos**2

      th0=acos(cst0)
      snt0=sin(th0)
      s=sin(th0/2.)**2
c     calculate the energy of the scattered electron in the absence
c     of radiation:
      a1=(mel**2+mp*es)**2+(mel*ps*cst0)**2
      a2=(es+mp)*(mel**2+mp*es)
      a3=(es+mp)**2-(ps*cst0)**2
      ep0=(a2+sqrt(a2**2-a1*a3))/a3

c     randomly choose the energy and momentum of the scattered electron,
c      and calculate Q**2


      uek=ran(idum)
      if (uek .lt. egdiv)then
         if (elas_inc .eq. 1)go to 20
         epk=delta*ran(idum)
         ep=ep0-epk
         if (ep .lt. epmin0)go to 20
         pp=ep-0.5*mel**2/ep
         jacob=jacob*(delta/erng/egdiv)
      else
         epk=delta+(erng-delta)*ran(idum)
         ep=ep0-epk
         if (ep .lt. epmin0)go to 20
         pp=ep-0.5*mel**2/ep
         jacob=jacob*((1.-delta/erng)/(1.-egdiv))
         go to 41
      endif


c     Event is in the elastic peak
      call elas_cor(es,th0,delta,sig_el)
c     Make an effective cross section differential in
c     ek, csthk,phik.
      sigr1=sig_el*jacob/delta/4./pi

      den=sqrt(ps**2+pp**2-2.*ps*pp*cst0)
      cstk1=(ps-pp*cst0)/den
      cstk2=(ps*cst0-pp)/den

      if (ran(idum) .lt. .5)then
         cstk = cstk1
      else
         cstk=cstk2
      endif
      phik=0.
c     The following calculation of the photon energy is approximate
      egam=epk

      sigr=sigr1/sigr_max
      mpfac=1.
      mcfac=1.
      go to 48

 41   den=sqrt(ps**2+pp**2-2.*ps*pp*cst0)
      cstk1=(ps-pp*cst0)/den
      cstk2=(ps*cst0-pp)/den
      if (cstk1 .gt. 0.995)cstk1=0.995
      if (cstk2 .gt. cstk1-.01)cstk2=cstk1-.01
      cran=ran(idum)
      if (cran .lt. cdiv1)then
         cstk=cstk1+.01*(ran(idum)-.5)
         mcfac=(.01/2.)/cdiv1
      elseif(cran .lt. cdiv2)then
         cstk=cstk2+.01*(ran(idum)-.5)
         mcfac=(.01/2.)/(cdiv2-cdiv1)
      elseif (cran .lt. cdiv3)then
 44      cstk=ran(idum)
         if (abs(cstk-cstk1) .lt. .01 .or. abs(cstk-cstk2) .lt. .01)
     *   go to 44
         mcfac=1./2./(cdiv3-cdiv2)
      else
         cstk=-ran(idum)
         mcfac=1./2./(1.-cdiv3)
      endif

      Tk=acos(cstk)
      phran=ran(idum)
      if (phran .lt. .2)then
         phik=(ran(idum)-.5)*pi/45.
c         mpfac=(pi/45./(2*pi))/0.2=1/18.
         mpfac=1/18.
      else
 47      phik=2.*pi*(ran(idum)-0.5)
         if(abs(phik) .lt. pi/90.)go to 47
c         mpfac=((2*pi-pi/45.)/(2.*pi))/0.8
         mpfac=(1.-1./90.)/0.8
      endif
      call elast_rad(es,ep,th0,cstk,phik,egam,sigr1)
      sigr1=sigr1*jacob
      if (sigr1 .lt. 0.)sigr1=0.

c     Normalize sigr according to the integration region.

      sigr1=mpfac*mcfac*sigr1
c     Compare the cross section to the maximum value
      sigr=sigr1/sigr_max

c     Calculate the number of times, mcall, to call the routine used
c     to calculate kinematic quantities for the n-tuple.
 48   rtest=ran(idum)
      mcall=sigr
      stest=sigr-mcall
      if (stest .gt. rtest)mcall = mcall + 1
      if (mcall .gt. mcall_max) mcall_max=mcall
      if (mcall .gt. 20)then
         write(6,*)'mcall = ',mcall
         write(6,*)'es,ep,th0,cstk,phik,egam,sigr',
     *   es,ep,th0,cstk,phik,egam,sigr
      endif
      sig_tot=sig_tot+sigr1

c     If mcall .gt. 0 generate mcall n-tuple events.
      if (mcall .eq. 0)go to 20
c      ntries=ntries+mcall-1

      do j=1,mcall
c     Calculate the radiation loss for the electron leaving the target
        call stragl(ep,targp)

        if (ep .lt. epmin0)go to 20
        call missm(ebeam,es,ep,th0,egam,cstk,phik,ppx,ppy,ppz,
     *  eprot,theta_pr,phi_pr,mm2)

        if (ep .lt. epmin0)go to 20
c     Calculate W as if there were no photon radiated.

        w=sqrt(mp**2+2*mp*(ebeam-ep)-4.*ebeam*ep*sin(th0/2)**2)
c     Increment the n-tuple.
        ntp(1)=es
        ntp(2)=ep
        ntp(3)=th0*180./pi
        ntp(4)=w
        ntp(5)=ppx
        ntp(6)=ppy
        ntp(7)=ppz
        ntp(8)=eprot
        ntp(9)=theta_pr*180./pi
        ntp(10)=phi_pr*180./pi
        ntp(11)=mm2
        ntp(12)=cstk
        ntp(13)=phik*180./pi
        ntp(14)=egam
        call hfn(10,ntp)

        call bos_cal
        call bos_out	! Pack the BOS banks and write out to file        

        nevent=nevent+1
       enddo

c     Talk to the user every now and then.
       ntell=nevent/nprint-ntold
       if (ntell .gt. 0)then
          call int_out(ntries,nevent,mcall_max,sigr_max,sig_tot,ucrng
     * ,erng,itime1,itime2,t_elapse)
          ntold=ntold+1
       endif
       
c     Do we have enough events in the n-tuple?
       if (nevent .gt. nmax)go to 50
 30    go to 20

c     Close out the n-tuple file
 50   call hrout(0,icycle,' ')
      call hrend('elasgen')

      CALL bos_end(recname)
c
      call timex(itime2)
      t_elapse=t_elapse+INT(itime2-itime1)

      write(6,*) ' Elapsed CPU time = ',t_elapse/60,' minutes'
      write(12,*)' Elapsed CPU time = ',t_elapse/60,' minutes'
      stop
      end

C======================================================================
      FUNCTION RAN(IDUM)
C----------------------------------------------------------------------
C-
C- Purpose and Methods : Flat Random Numbers (from Numerical Recipes:"RAN2").
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      SAVE
C
C Function parameters
      INTEGER IDUM
      REAL    RAN
C
C Local parameters
      INTEGER M, IA, IC
      REAL RM
      PARAMETER (M=714025, IA=1366, IC=150889, RM=1.4005112E-6)
C
C Local variables
      INTEGER J, IY, IFF, IR(97)
      DATA IFF /0/
C
C
      IF(IDUM.LT.0 .OR. IFF.EQ.0) THEN
        IFF = 1
        IDUM = MOD(IC-IDUM,M)
        DO 11 J=1,97
          IDUM = MOD(IA*IDUM+IC,M)
          IR(J) = IDUM
 11     CONTINUE
        IDUM = MOD(IA*IDUM+IC,M)
        IY = IDUM
      ENDIF
C
      J = 1 + (97*IY)/M
      IF(J.GT.97 .OR. J.LT.1) PAUSE
      IY   = IR(J)
      RAN = IY*RM
      IDUM = MOD(IA*IDUM+IC,M)
      IR(J)= IDUM
C
      RETURN
      END
c---------------------------------------------------------------------
       subroutine missm(ebeam,es,ep,th0,egam,cstk,phik,ppx,ppy,
     *  ppz,eprot,theta_pr,phi_pr,mm2)

c
c     Input:
c         ebeam = incident electron energy
c	  es = incident energy at point of interaction in target
c         ep = scattered electron energy
c         th0 = electron scattering angle
c         egam = energy of radiated photon
c         cstk = cosine of the photon angle (relative to the q vector)
c     Output:
c         phik = azimuthal angle of photon
c         ppx, ppy, ppz = proton momentum components
c         mm2 = experimental missing mass

         
c     Choose the phi angle of the photon randomly.
c     Choose the hadronic decay angles randomly and calculate the
c     missing mass, the proton momenta and pion momenta.

       implicit none
      common /random/idum

      real csphip
      real csphk
      real csthe
      real csthp
      real cstk
      real cstq
c      real del_p
c      real del_phi
c      real del_the
c      real delthe
c      real delphi
      real ebeam
      real egam
      real egamx,egamy,egamz
      real eprot
      real ep
      real eph_dot_q
      real es
      real mel
      real mm2
      real mp
      real nu
      real pbeam
      real phik
      real pi
      real ppx,ppy,ppz
      real pps,pprot
      real pp
      real ps
      real q2
      real q_dot_pp
      real qx,qz,qvec
c      real ran,r1,r2
      real snphip
      real snphk
      real snthe
      real snthp
      real sntk
      real sntq
      real th0
      real theta_pr
      real phi_pr
      real w2

      integer*4 idum

c      DATA del_p/.01/,del_the/.001/,del_phi/.001/

      mp=.938
      mel=0.511E-3
      pi=3.14159
      csthe=cos(th0)
      snthe=sin(th0)
      nu=es-ep
      ps=sqrt(es**2-mel**2)
      pp=sqrt(ep**2-mel**2)
      q2=2.*es*ep-2.*ps*pp*csthe-2.*mel**2
      w2=mp**2-q2+2.*mp*nu

c     get components of the q vector in the lab frame.
c     By definition the scattered electron has a positve x component
c     in the lab frame.
      qx=-pp*snthe
      qz=ps-pp*csthe
      qvec=sqrt(qx**2+qz**2)

c     get components of the photon vector in the lab frame
      sntk=sqrt(1.-cstk**2)

      csphk=cos(phik)
      snphk=sin(phik)

      cstq=qz/qvec
      sntq=sqrt(1.-cstq**2)
      egamx=egam*(sntk*csphk*cstq-cstk*sntq)
      egamy=egam*sntk*snphk
      egamz=egam*(cstk*cstq+sntk*csphk*sntq)

c     calculate the dot product of the photon vector and the q-vector
      eph_dot_q=egamx*qx+egamz*qz


c     calculate laboratory components of the proton momentum vector
         ppz=qz-egamz
         ppx=qx-egamx
         ppy=-egamy
         pps=ppx**2+ppy**2+ppz**2
         pprot=sqrt(pps)
         eprot=sqrt(pprot**2+mp**2)
         csthp=ppz/pprot
         snthp=sqrt(1.-csthp**2)
         theta_pr=acos(csthp)
         csphip=ppx/pprot/snthp
         snphip=ppy/pprot/snthp
         phi_pr=atan2(snphip,csphip)

c     smear proton angles by del_the (rad).
c         r1=ran(idum)
c         r2=2.*pi*ran(idum)
c         r1=sqrt(-2.*alog(r1))
c         delthe=del_the*r1*cos(r2)
c         delphi=del_phi*r1*sin(r2)

c     smear proton energy by del_p =delta p/ p 

c         r1=ran(idum)
c         r2=2.*pi*ran(idum)
c         r1=sqrt(-2.*alog(r1))
c         pprot=pprot*(1.+del_p*r1*cos(r2))
c         eprot=sqrt(pprot**2+mp**2)

c         ppx=pprot*
c     *   (snthp*csphip+csthp*csphip*delthe-snthp*snphip*delphi)

c         ppy=pprot*
c     *   (snthp*snphip+csthp*snphip*delthe+snthp*csphip*delphi)

c         ppz=pprot*(csthp-snthp*delthe)

c     smear out th0 and ep to account for experimental resolution
c         r1=ran(idum)
c         r2=2.*pi*ran(idum)
c         r1=sqrt(-2.*alog(r1))

c         ep=ep*(1.+del_p*r1*cos(r2))
c         pp=sqrt(ep**2-mel**2)
c         th0=th0+del_the*r1*sin(r2)

         csthe=cos(th0)
         snthe=sin(th0)

c     Calculate the square of the missing mass, associated with the
c     proton momentum components and the electron beam energy:

         nu=ebeam-ep
         pbeam = sqrt(ebeam**2-mel**2)
         q2=2.*ebeam*ep-2.*pbeam*pp*csthe-2.*mel**2

c     get components of the q vector in the lab frame.
c     By definition the scattered electron has a positve x component
c     in the lab frame.
         qx=-pp*snthe
         qz=pbeam-pp*csthe
         q_dot_pp=qx*ppx+qz*ppz

         mm2=-q2+2*mp**2+2*mp*(nu-eprot)-2*nu*eprot+2*q_dot_pp

         return

 30      mm2=0.
         return

         end

      subroutine elast_rad(es_4,ep_4,thetae_4,csthk_4,phik_4,
     & egam_4,d3sig_4)
c     calculate the exact one photon radiative cross section for
c     elastic ep scattering. Reference: Mo and Tsai, equation B-5

c     Inputs: 
c             es= incident electron energy
c             thetae= electron scattering angle

c     ep=scattered electron energy

c     thetak=photon angle with respect to the q vector

c     Output: Distributions of various quantities calculated on the
c     assumption that both the proton and the scattered electron are
c     detected, and that the interaction proceeded through a resonance.
c     These can be compared to distributions obtained from AO in which
c     the interaction was actually through resonance production.

c     The calculations are made for a fixed incident electron energy and
c     a series of scattered electron energies.

c     Units are GeV

      implicit none

      real*8 es,ep,thetae,csths,snths,csthp,snthp,thetas,thetap
      real*8 pvec, svec
      real*8 csthk,snthk,egam
      real*8 qvec,q0,q2,sp,u2
      real*8 trm1,trm2,trm3,trm4,trm5,trm6,trm7,trm8,trm9,trm10
      real*8 trmf,trmg,Ge,Gm,tau,F0,G0,fac1,d3sig
      real*8 alpha, me,mp
      real*8 w2,w
      real*8 pi
      real*8 q2e
      real*8 phik,sdotk,pdotk
      real*4 es_4,ep_4,thetae_4,csthk_4,phik_4,egam_4,d3sig_4

      es=es_4
      ep=ep_4
      thetae=thetae_4
      csthk=csthk_4
      phik=phik_4

      me=0.000511
      mp=.938
      pi=3.14159
      alpha=1/137.
      d3sig=0.

c     calculate the scattered electron energy for elastic
c     scattering:


      pvec=ep-0.5*me**2/ep
      svec=es-0.5*me**2/es
      q2e=-2.*me**2+2.*es*ep-2.*svec*pvec*dcos(thetae)
      w2=mp**2+2*mp*(es-ep)-q2e
      w=dsqrt(w2)
      if (w .gt. 2.5 .or. w .lt. mp)go to 99


      qvec=dsqrt(svec**2+pvec**2-2*svec*pvec*dcos(thetae))
      q0=es-ep

      sp=es*ep-svec*pvec*dcos(thetae)
      u2=(es-ep+mp)**2-qvec**2
      csths=( svec**2-pvec*svec*dcos(thetae) )/svec/qvec
      csthp=(svec*pvec*dcos(thetae)-pvec**2)/pvec/qvec
      thetas=dacos(csths)
      thetap=dacos(csthp)
      snths=dsin(thetas)
      snthp=dsin(thetap)
      snthk=dsqrt(1-csthk**2)

      egam=0.5*(u2-mp**2)/(es-ep+mp-qvec*csthk)
      if (egam .lt. 0.)egam=0.1E-6
      sdotk=es*egam-svec*egam*(csthk*csths+snthk*snths*dcos(phik))
      pdotk=ep*egam-pvec*egam*(csthk*csthp+snthk*snthp*dcos(phik))

c


c     calculate the one-photon radiative cross section
c     q2=four-momentum transfer to the proton
      q2=2*me**2-2*ep*es+2*pvec*svec*dcos(thetae)-2*egam*(es-ep)
     * +2*egam*qvec*csthk
      if (q2 .gt. 0.)write(6,*)es,ep,thetae,egam,csthk
      trm1=-(me/pdotk)**2*(2*es*(ep+egam)+q2/2)
      trm2=-(me/sdotk)**2 *(2*ep*(es-egam)+q2/2)
      trm3=-2
      trm4=2/sdotk/pdotk
      trm4=trm4*(me**2*(sp-egam**2)+sp*(2*es*ep-sp+egam*(es-ep)))
      trm5= (1./pdotk)*(2.*(es*ep+es*egam+ep**2)+q2/2-sp-me**2)
      trm6=-(1./sdotk)*(2.*(es*ep-ep*egam+es**2)+q2/2-sp-me**2)
      trmf=trm1+trm2+trm3+trm4+trm5+trm6
      trm7=me**2*(2*me**2+q2)*(1./pdotk**2+1./sdotk**2)
      trm8=4.
      trm9=4.*sp*(sp-2.*me**2)/pdotk/sdotk
      trm10=(2.*sp+2*me**2-q2)*(1./pdotk-1./sdotk)
      trmg=trm7+trm8+trm9+trm10
      Ge=1./(1.-(q2/0.71))**2
      Gm=2.793*Ge
      tau=-q2/4./Mp**2
      F0=4.*(Ge**2+tau*Gm**2)/(1+tau)
      G0=-q2*Gm**2
      fac1=alpha**3/(2.*pi)**2*(ep/es)*egam/mp/2./(q2**2)
      fac1=fac1/(q0+mp-qvec*csthk)

c     d3sig is the cross section, differential in electron scattering
c     solid angle and energy and in the photon angle (relative to qvec):
      d3sig=fac1*(mp**2*F0*trmf+G0*trmg)
      if (d3sig .lt. 0.)write(6,*)q2,trmf,trmg
 99   continue
c     convert cross section to micro-barns
      d3sig_4=d3sig*389.37966
      egam_4=egam

      return
      end




      subroutine elas_cor(es,theta,delta,sigel)
      implicit none
c     Calculate radiative correction for elastic electron scattering,
c     using equation II.6 of Mo and Tsai, Rev. Mod. Phys. 41,205-235, (1969).
c     The correction assumes that the peak is integrated from the
c     elastic peak down to a scattered electron energy of E_elas-Delta.

c     The elastic tail here is calculate with a peaking approximation
c     that should be valid close to the elastic peak, which is where it
c     can be most easily confused with single pion production.
c     The exact formula for the tail due to internal bremsstrahlung
c     is given in appendix B of Mo and Tsai and could be incorporated
c     if needed.

c     The straggling for an electron that passes through a target
c     thickness t, is included, where t has units of rad. lengths.
c     If the straggling effect for electrons emerging from the
c     main scattering event are already included in a subsequent
c     simulation, one should eliminate the correction for the
c     post straggling included in this calculation.

      real*8 spence,arg
      real me,mp,pi,alpha,z,t,es,theta_d,theta,delta
      real b,cst1,eta,bt,sigunp,sigel,eel,qs
      real mpi
      real znuc,deltac(27),del_mo,delta_t,idel
      real arg11,arg15,arg19,arg23
      real epr,e1,e3,e4,gamma4,beta4
      real radcor
      real snth
      real bfunc


      pi=2.*acos(0.)
      mp=.9382
      mpi=.13957
      me=0.000511
      alpha=1/137.

c     Calculate correction for hydrogen or deuterium elastic scattering
      znuc=1.

c     calculate b for hydrogen
      z=1
      b=bfunc(z)
c      ' Input thickness, rad. leng.'
      t=0.
      theta_d=180*theta/pi
      snth=sin(theta)
      cst1=1.-cos(theta)
      eel=es/(1.+es/mp*cst1)
      epr=es+mp-eel
      gamma4=epr/mp
      beta4=sqrt(1.-1/gamma4**2)
      e1=es
      e3=eel
      e4=epr
      eta=es/eel
      qs=2.*es*eel*cst1
      deltac(1)=28./9.-13./6.*alog(qs/me**2)
      deltac(2)= (alog(qs/me**2) - 1. + 2.*alog(eta) )
     *  * ( 2.*alog(e1/delta) - 3.*alog(eta) )
      arg=(e3-e1)/e3
      deltac(3)=-spence(arg)
      deltac(4)=znuc**2*alog(e4/mp)
      deltac(5)=znuc**2*alog(mp/eta/delta)*
     * (alog((1.+beta4)/(1.-beta4))/beta4-2.)
      deltac(6)=znuc**2*0.5/beta4*(alog((1+beta4)/(1.-beta4))*
     * alog((e4+mp)/2/mp))
      arg=sqrt((e4-mp)*(1.+beta4)/(e4+mp)/(1.-beta4))
      deltac(7)=-znuc**2*spence(-arg)/beta4
      arg=(e3-mp)/e1
      deltac(8)=znuc*spence(arg)
      arg=(mp-e3)*mp/(2.*e3*e4-mp*e1)
      deltac(9)=-znuc*spence(arg)
      arg=2.*e3*(mp-e3)/(2*e3*e4-mp*e1)
      deltac(10)=znuc*spence(arg)
      arg11=(2*e3*e4-mp*e1)/e1/(mp-2*e3)
      arg11=abs(arg11)
      deltac(11)=znuc*alog(arg11)*alog(mp/2/e3)
      arg=(e3-e4)/e3
      deltac(12)=-znuc*spence(arg)
      arg=(e4-e3)*mp/(2.*e1*e4-mp*e3)
      deltac(13)=znuc*spence(arg)
      arg=2.*e1*(e4-e3)/(2*e1*e4-mp*e3)
      deltac(14)=-znuc*spence(arg)
      arg15=(2*e1*e4-mp*e3)/e3/(mp-2*e1)
      arg15=abs(arg15)
      deltac(15)=-znuc*alog(arg11)*alog(mp/2/e1)
      arg=(e1-mp)/e1
      deltac(16)=-znuc*spence(arg)
      arg=(mp-e1)/e1
      deltac(17)=znuc*spence(arg)
      arg=2.*(mp-e1)/mp
      deltac(18)=-znuc*spence(arg)
      arg19=abs(mp/(2*e1-mp))
      deltac(19)=-znuc*alog(arg19)*alog(mp/2/e1)
      arg=(e3-mp)/e3
      deltac(20)=znuc*spence(arg)
      arg=(mp-e3)/e3
      deltac(21)=-znuc*spence(arg)
      arg=2*(mp-e3)/mp
      deltac(22)=znuc*spence(arg)
      arg23=abs(mp/(2*e3-mp))
      deltac(23)=znuc*alog(arg23)*alog(mp/2/e3)
      arg=(e1-e3)/e1
      deltac(24)=-spence(arg)
      arg=(e4-mp)*(1-beta4)/(e4+mp)/(1+beta4)
      arg=sqrt(arg)
      deltac(25)=znuc**2*spence(arg)/beta4
      arg=(e4-mp)/(e4+mp)
      arg=sqrt(arg)
      deltac(26)=-znuc**2*spence(arg)/beta4
      deltac(27)=znuc**2*spence(-arg)/beta4


      del_mo=0.
      do idel=1,27
         del_mo=del_mo+deltac(idel)
      enddo
      del_mo=-alpha*del_mo/pi

c     calculate the elastic peak
c

      call epelas(es,theta,sigunp)
      bt=b*t
      delta_t=-0.5*bt*(alog(es/eta**2/delta)+alog(eel/delta))
      radcor=1.+del_mo+delta_t
      sigel=radcor*sigunp
 99   return
      end

      real function bfunc(z)
      implicit none
      real z,xi

      xi=alog(1440.)-2.*alog(z)/3.
      xi=xi/(alog(183.)-alog(z)/3.)
      bfunc= (4./3.)* (1. + ((z+1.)/(z+xi))/(alog(183.)-alog(z)/3.)/9.)
      return
      end

      subroutine epelas(E0,theta,sig_unp)

c     calculate cross section elastic scattering of electrons on protons.

      implicit none

      real alpha
      real cstf
      real csth2
      real e0
      real e_pr
      real ge,gm
      real ge2,gm2
      real mp,nu
      real q2
      real r
      real sig_mott,sig_unp
      real snth2
      real tau
      real theta

      data mp/.9382/

      alpha=1./137.

c	calculate Q**2
	cstf=1-cos(theta)
	q2=2*e0**2*cstf/(1.+e0/mp*cstf)

	nu=q2/2/mp
	e_pr=e0-nu
	tau=q2/4./mp**2
	snth2=q2/4./e0/e_pr
	csth2=1.-snth2
	snth2=sqrt(snth2)
	csth2=sqrt(csth2)
	ge=1./(1.+q2/.71)**2
	r=1./2.7928
	gm=ge/r
	ge2=ge**2
	gm2=gm**2

c	

c	calculate elastic scattering cross section, differential
c       in electron scattering angle.

	sig_mott=alpha**2/4./e0**2/snth2**4
c	convert cross section from (GeV)**{-2}/sr to microbarns/sr
	sig_mott=389.37966 *sig_mott

c       calculate the unpolarized cross section
	sig_unp=sig_mott*e_pr/e0*( (ge2+tau*gm2)*csth2**2/(1.+tau)
     1 + 2. *tau*gm2*snth2**2)

99	return
	end


      real*8 function spence(x)
      implicit none
      real*8 x
      real*8 pi
      real*8 sintp,sintn
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

      real*8 function sintp(x)
      implicit none
      real*8 x
      real*8 xstep,sum,y,arg
      integer i

      xstep=(x-.1)/100.
      sum=0.
      y=.1-xstep/2.
      do i=1,100
        y=y+xstep
        arg=abs(1.-y)
        sum=sum-dlog(arg)/y
      enddo
      sintp=sum*xstep
      return
      end



      real*8 function sintn(x)
      implicit none
      real*8 x,xa,ystep,y,sum
      integer i

      xa=abs(x)
      ystep=(xa-0.1)/100.
      sum=0.
      y=.1-ystep/2.
      do i=1,100
        y=y+ystep
        sum=sum-dlog(1.+y)/y
      enddo
      sintn=sum*ystep
      return
      end

      subroutine bos_cal
c     calculate output quantities for the bos banks
      implicit none
      common /bos_events/es,ep,th0,ppx,ppy,ppz,eprot,egam,delta
      common /random/idum
      real es,ep,th0,mel,ppx,ppy,ppz,eprot,egam,delta
      real phi_r,sin_phir,cos_phir,pxtmp,pytmp
      real pi
      real ran
      integer*4 idum
      integer jj

#include "bcs.inc"
#include "mc.inc"
      
      data pi/3.14159/, mel/0.511E-3/

      do jj = 1,npart
        v(jj,1) = 0.0
        v(jj,2) = 0.0
        v(jj,3) = 0.0
      enddo

      p(1,1) = sqrt(ep**2-mel**2)*sin(th0)
      p(1,2) = 0.
      p(1,3) = sqrt(ep**2-mel**2)*cos(th0)
      p(1,4) = ep
      p(2,1) = ppx
      p(2,2) = ppy
      p(2,3) = ppz
      p(2,4) = eprot
      if (npart .eq. 3)then
        p(3,1) = -ppx-p(1,1)
        p(3,2) = -ppy
        p(3,3) = es-p(1,3)-ppz
        p(3,4) = egam
        if (egam .lt. delta)then
           p(3,1) = 0.
           p(3,2) = 0.
           p(3,3) = 1.e-4
           p(3,4) = 1.e-4
        endif
      endif
c     Rotate all momenta by a random angle around the z axis
      phi_r=2*pi*ran(idum)
      sin_phir = sin(phi_r)
      cos_phir = cos(phi_r)
      do jj=1,npart
        pxtmp = p(jj,1)
        pytmp = p(jj,2)
        p(jj,1) = pxtmp*cos_phir+pytmp*sin_phir
        p(jj,2) = pytmp*cos_phir-pxtmp*sin_phir
      enddo

      return
      end

C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      subroutine stragl(e1,thik)
      implicit none
      common /random/idum
      real e1,thik,xs,eloss,gxs,xtest
      real ran
      integer*4 idum

 10   xs=ran(idum)
      eloss=xs**(1./thik)
      gxs=1.-eloss
      xtest=ran(idum)
      if (xtest.gt.gxs)go to 10
      e1=e1*(1.-eloss)
      return
      end

      subroutine bos_setup(recname,ebeam)
      implicit none

#include "mc.inc"

      character*8 recname
      real ebeam

      recname = 'MCEVENT'
c     Initialize BOS
     
      call bos_ini(recname)

      write(6,*)' Input 3 if you want the GSIM bos bank to include',
     * ' the photon'
      read(5,*)npart
      if (npart .ne. 3)npart=2
      write(12,*)' Number of particles per event in bos bank =',npart
      q(1) 	= -1
      q(2)	=  1
      id(1)	= 3		!Geant ID
      id(2)	= 14		!Geant ID
      pdgid(1)  = 11		!PDG ID,  electron
      pdgid(2)  = 2212		!PDG ID,  proton
      if (npart .eq. 3)then
        q(3)      = 0
        id(3)     = 1
        pdgid(3)  = 22		!PDG ID,  photon
      endif
      return
      end

C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine par_setup(ebeam,epmin0,es,ep,ps,rs,t_targ,th_min,
     * th_max,nmax,delta,nprint,filerz,fileout,elas_inc)

      implicit none
      real ebeam,epmin0,es,ep,ps,rs,t_targ
      real th_min,th_max
      real delta
      real mel
      real bfac
      integer nmax,nprint,elas_inc
      character*132 filerz
      character*132 fileout

      mel = 0.000511
      write(6,*)' Input the target thickness, (r.l.)'
      read(5,*)t_targ
      write(6,*)' Target thickness is ',t_targ,' r.l.'


      write(6,*)' Input the incident electron energy(GeV)'
      read(5,*)ebeam
      write(6,501)ebeam
 501  format(' Incident electron energy = ',f7.3,' GeV')
      write(6,*)' Input the minimum allowed scattered electron energy'
      read(5,*)epmin0
      write(6,*)epmin0
 502  format(' Minimum allowed scattered electron energy =',f7.3,' MeV')

      es=ebeam

      open(unit=12,file=fileout,status='new')

      write(12,*)' Incident energy is ',ebeam,' GeV'
      write(12,*)' Minimum accepted scattered electron energy is',epmin0
      write(12,*)' Target thickness is ',t_targ,' r.l.'

c     Specify the minimum and maximum scattering angles
      write(6,*)' input the minimum and maximum electron',
     * ' scattering angles, (degrees)'
      read(5,*)th_min,th_max

      write(6,*)' Electron scattering angles range from',
     * th_min,' to',th_max,' degrees'
      write(12,*)' Electron scattering angles range from',
     * th_min,' to',th_max,' degrees'

c     Decide whether to include the elastic peak in the n-tuple
      write(6,*)' Input 0 to include elastic peak, 1 to exclude it'
      read(5,*)elas_inc
      if (elas_inc .ne. 1)then
         write(6,*)' Elastic peak included'
         write(12,*)' Elastic peak included'
      else
         write(6,*)' Elastic peak excluded'
         write(12,*)' Elastic peak excluded'
      endif

c     calculate the incident momentum
      ps=es-0.5*mel**2/es
      rs=ps/es

c     Choose the resolution cut for the elastic peak.  For radiated
c     photon energies less than delta, the reaction
c     is considered non-radiative.
      delta=.005


c     Select the number of events desired in the rz file.
      write(6,*)' Input the desired number of events in the ntuple'
      read(5,*)nmax
      if (nmax .gt. 95000)nmax=95000
      nprint=nmax/25

      bfac=4./3.
      t_targ=bfac*t_targ

      return
      end


      subroutine sigmax_calc(sigr_max,th_min,es,ps,epmin0,wmax,erng,
     * delta,egdiv,cdiv1)

c     Do a preliminary calculation to estimate the maximum value
c     of the integrand.
c     Examine the tail of the cross section at
c     scattered energy a distance delta from the elastic peak
c     and  at the minimum scattering angle.

      implicit none
      real sigr_max,th_min,es,ps,epmin0,delta
      real egdiv,cdiv1
      real sigr
      real wmax
      real th0,pi,s,cst0,snt0,ucos,jacob
      real a1,a2,a3,ep0
      real epmin,erng,ep,pp
      real den,cstk1,cstk2,cstk
      real mel,mp
      real phik,tk,cstk_max
      real mcfac,mpfac,fmcall
      real egam
      real ran
      integer*4 idum
      integer i

      DATA PI  /3.14159/,mp/0.938/,mel/0.511E-3/

      sigr_max=0.


c     calculate the scattering angle
      th0=th_min*pi/180.
c     calculate s=sin(theta/2)**2, cos(theta)
      s=sin(th0/2.)**2
      cst0=cos(th0)
      snt0=sin(th0)
      ucos=1./(1-cst0)
      jacob=1./ucos**2

      a1=(mel**2+mp*es)**2+(mel*ps*cst0)**2
      a2=(es+mp)*(mel**2+mp*es)
      a3=(es+mp)**2-(ps*cst0)**2
      ep0=(a2+sqrt(a2**2-a1*a3))/a3

      epmin=(mp**2+2*mp*es-wmax**2)/(4*es*s+2*mp)
      if (epmin .lt. epmin0)epmin=epmin0
      erng=ep0-epmin
      write(6,*)' Integration range for photon energy =',erng
      write(12,*)' Mininum photon energy =',delta
      write(12,*)' Maximum photon energy =',erng
c     calculate the energy and momentum of the scattered electron,
      ep=ep0-delta
      pp=ep-0.5*mel**2/ep

      den=sqrt(ps**2+pp**2-2.*ps*pp*cst0)
      cstk1=(ps-pp*cst0)/den
      if (cstk1 .gt. 0.995)cstk1=0.995
      cstk2=(ps*cst0-pp)/den
      if (cstk2+.01 .gt.cstk1)cstk2=cstk1-.01 
      phik=0.

      mcfac=(.01/2.)/cdiv1
      mpfac=1./18.
      do i=1,200
        cstk=.01*(ran(idum)-.5)+cstk1

        Tk=acos(cstk)
        call elast_rad(es,ep,th0,cstk,phik,egam,sigr)
        sigr=sigr*jacob
        if (sigr .gt. sigr_max)then
           cstk_max=cstk
           sigr_max=sigr
        endif
      enddo
      sigr_max=sigr_max*mcfac*mpfac*(delta/erng/egdiv)

c     Now check the cross section at epmin, photon radiated along beam.
        den=sqrt(ps**2+epmin**2-2.*ps*epmin*cst0)
        cstk1=(ps-epmin*cst0)/den
        if (cstk1 .gt. 0.995)cstk1=0.995
      do i=1,100
         cstk=cstk1+.01*(ran(idum)-0.5)
         call elast_rad(es,epmin,th0,cstk,phik,egam,sigr)
         sigr=sigr*jacob*mpfac*mcfac/(1.-egdiv)
         if (sigr .gt. sigr_max)then
           cstk_max=cstk
           sigr_max=sigr
           ep=epmin
         endif
      enddo
      write(6,*)' Maximum cross section =',sigr_max
      write(6,*)' es, ep, th_min,cstk_max=',es,ep,th_min,cstk_max
      write(12,*)' Maximum cross section =',sigr_max
      write(12,*)' es, ep, th_min,cstk_max=',es,ep,th_min,cstk_max
      write(6,*)' Input a factor for adjusting the maximum'
      read(5,*)fmcall
      sigr_max=fmcall*sigr_max
      write(6,*)' maximum cross section changed to ',sigr_max
      write(12,*)' maximum cross section changed to ',sigr_max
      return
      end

      subroutine int_out(ntries,nevent,mcall_max,sigr_max,sig_tot,ucrng
     * ,erng,itime1,itime2,t_elapse)

      real sigr_max,ucrng,erng,pi
      real t_elapse, itime1, itime2
      real sig_int,sig_sum
      real sig_tot
      integer*4 ntries


      integer nevent, mcall_max

      data pi/3.14159/

      write(6,*)' ntries,nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max
      write(12,*)' ntries,nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max
      sig_int=nevent
      sig_int=sig_int/ntries
      sig_int=sig_int*sigr_max*4.*pi*ucrng*2.*pi*erng
      sig_sum=sig_tot*4.*pi*ucrng*2*pi*erng/ntries
      write(6,*)' sig_int = ',sig_int,sig_sum
          
      write(6,*)' Number of seconds at Lum=1E34 is',
     *    (1.E-4)*nevent/sig_int
      write(12,*)' sig_int = ',sig_int,sig_sum
      write(12,*)' Number of seconds at Lum=1E34 is',
     *    (1.E-4)*nevent/sig_int

      call timex(itime2)
      t_elapse=t_elapse+INT(itime2-itime1)
      itime1 = itime2
      write(6,*) ' Elapsed CPU time = ',t_elapse/60,' minutes'
      write(12,*)' Elapsed CPU time = ',t_elapse/60,' minutes'
      return
      end
