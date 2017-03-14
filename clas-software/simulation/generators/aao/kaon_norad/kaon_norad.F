      program kaon_norad
c
c     This program makes an n-tuple that can be used with Paw and GSIM to
c     make distributions of energies, angles, resonance mass resulting 
c     from non-radiative pion production on a proton.
c     The n-tuple contains the true hadronic invariant mass (W), the components 
c     of the proton momentum (PPX, PPY, PPZ),
c     the proton energy (EP), the pion momentum (PPIX, PPIY, PPIZ) and
c     pion energy (EPI), the  angles for the hadronic
c     decay in the hadronic frame (CSTCM, PHICM), the missing mass (MM).
c
c     Resolution of detectors is not folded into the results.  If this is desired it
c     should be done with a second program that can operate on the n-tuple
c     and make a new version.

      implicit none
      
#include "bcs.inc"
#include "mc.inc"

      COMMON/ALPHA/ ALPHA,PI,MP,MPI,MEL,WG,EPIREA
      common /random/idum
      
      real*8 alpha,pi,mp,mpi,mel,wg,T0
      real*8 mkp,mla,msi,m_h
      real*8 es,ep,ps,pp,rs,rp
      real*8 cst0,snt0
      real*8 sig_tot,sig_sum
      real*8 ephot

      real t_s
      real csthcm, csthcm_max, cstk,csthcm_p_max,csthcm_m_max
      real epi, epmax, eprng, eprot, epw
      real ebeam,ep_min,ep_max,ep_test, events, evirt
      real fmcall
      real gam_flux
      real itime1, itime2
      real jacob
      real mm2, mpi0,mpip,mpi_s, m_h_s
      real ppx,ppy,ppz,px,py
      real ppix,ppiy,ppiz
      real phicm,phicm_max,phik,phir,phicm_p_max,phicm_m_max
      real q0,q2,q2_min,q2_max,q2max,q2min
      real w_min, w_max
      real qsq,qvecx,qvecz
      real ran,rotc,rots,rtest
      real s,sigi,sigl,sigt,sigu,sigma0,sig_ratio,stest
      real sigip
      real signr,signr_max,signr_p_max,signr_m_max
      real t_elapse,th0,theta,tries
      real uq2, uq2_min,uq2_max,uq2rng
      real w2,wreal, the_e_min, the_e_max
      real t_targ
      real sig_int

      integer get_spin
      integer flag_e_hel
      integer epirea
      integer i,iext1,iext2,iquest
      integer j,jj
      integer mcall,mcall_max
      integer ncycle
      integer nprint,ntell,ntold

      integer*4 idum
      integer*4 ntries
      
      real*4 ntp(21)

      common /pawc/h(1000000)
      common/quest/iquest(100)
      common/ntup/ntp
      
      integer h,nevent,nmax,istat,icycle
      
      character*132 filerz,file_out,file_sum
      character*28 ctime
      character*8 recname

      DATA PI   /3.1415926/
      DATA MPIP /.1395700/
      DATA MPI0 /.1349764/
      DATA MP   /.9382799/
      DATA MKP  /0.493677/
      DATA MLA  /1.115684/
      DATA MSI  /1.19255/
      DATA MEL  /.511E-3/

      call getenvf('FRZN',filerz)
      call getenvf('FOUT',file_out)
      call getenvf('FSUM',file_sum)

      alpha=1/137.
      
c     npart: 	number of particles in BOS banks - 2(e-,h+) 3(e-,h+,h0)
c     epirea: 	final state hadron (1-Lambda, 2-Sigma)
c     ebeam: 	incident electron energy (GeV)
c     q2_*:	q**2 limits (GeV**2)
c     ep_*:	scattering electron limits (GeV)
c     nmax:	number of output events
c     fmcall:   scale factor for sigm_max

c     set up parameters for bos bank input to GSIM
      write(6,*)' input 2 for two charged particles in the bos bank'
      write(6,*)' input 3 to get the neutral hadron'        
      read(5,*)npart
c      if (npart .ne. 3)npart=2
c
c     Choose whether a neutral or charged pion is made in the reaction
 5    write(6,*)' Input epirea (1 for LAMBDA, 2 for SIGMA)'
      read(5,*)epirea
      IF(EPIREA .ne. 1 .and. epirea .ne. 2)go to 5
c
      write(6,*) 'Input electron helicity (0 for unpol, 1 for pol)'
      read(5,*) flag_e_hel
c
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
      write(6,*)'Input lower and upper limit for electron 
     *  scattering angle in (deg)'
      read(5,*)the_e_min,the_e_max
c
      write(6,*)'Input lower and upper limit for scattered electron',
     * ' energy(GeV).'
      read(5,*)ep_min,ep_max

c     Select the number of events desired in the rz file.
      write(6,*)' Input the desired number of events'
      read(5,*)nmax
      nprint=nmax/25
c
      write(6,*)' Input a multiplication factor for sigr_max'
      read(5,*)fmcall
c
      id(1)		= 3		!Geant ID, e-
      pdgid(1)  	= 11		!PDG ID, e-
      q(1) 		= -1

c     Choose whether a neutral or charged pion is made in the reaction
      IF(EPIREA.EQ.1)then
         M_H=MLA
         MPI=MKP
         id(2)	   = 11		!Geant ID, Kaon
         q(2) = 1
         pdgid(2)  = 321    !PDG ID, Kaon
         if (npart .eq. 3)then
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
         if (npart .eq. 3)then
            id(3)	  = 20		!Geant ID, Sigma
            pdgid(3)  = 3112	!PDG ID, Sigma
            q(3) = 0
         endif
      endif

      mpi_s		= sngl(mpi)		!Single precision pion mass
      m_h_s     = sngl(m_h)
      wg		= m_h+mpi+.0005	!minimum hadron mass for pion production

      es		= ebeam
      ps		= sqrt(es**2-mel**2)
      rs		= ps/es
      
      s			= sin(the_e_max*pi/180./2)**2
      q2max		= 4.*ebeam**2*s/(1.+2.*ebeam*s/mp)
      s         = sin(the_e_min*pi/180./2)**2
      q2min     = 4.*ebeam**2*s/(1.+2.*ebeam*s/mp)

      if (q2_max.gt.q2max) q2_max=q2max
      if (q2_min.lt.q2min) q2_min=q2min

      uq2_min		= 1/q2_max
      uq2_max		= 1/q2_min
      uq2rng		= uq2_max-uq2_min

      epmax		= es-(wg**2+q2_min-mp**2)/2./mp

      iext1		= 48
      iext2		= 48      

      if (ep_max.lt.epmax) epmax=ep_max
      
      eprng		= epmax-ep_min

      nprint		= nmax/25

      ncycle		= 0
 1    mcall_max		= 0
      ntold		= 0

      call timex(itime1)		!Initial seed from internal clock
      call getunixtime(idum)
      call getasciitime(idum,ctime)
      
      idum		= -idum
      cstk		= ran(idum)
      itime2		= itime1 
      nevent		= 0
      t_elapse		= 0.
      ntries		= 0
      sig_int		= 0.
      sig_tot		= 0.

      open(unit=12,file=file_out,status='new')
      open(unit=14,file=file_sum,status='new')

      call hlimit(1000000)
      iquest(10)	= 65000
      call hropen(1,'AONORAD',filerz,'NQ',8191,istat)
      call hbset('BSIZE',8176,istat)
      call hbnt(10,'AONORAD',' ')
      call hbname(10,' ',0,'$clear')
      call hbname(10,'AONORAD',ntp(1),'ES:R,EP:R,THETE:R,
     *W:R,PPX:R,PPY:R,PPZ:R,EPROT:R,PPIX:R,PPIY:R,PPIZ:R,
     *EPI:R,CSTCM:R,PHICM:R,MM:R,QX:R,QZ:R,Q0:R,CSTHE:R,EHEL:R,
     *Q2:R')

      recname = 'MCEVENT'
      call bos_ini(recname)

      write(12,*)' AO Calculation of Single Pion Production'
      write(12,*)' Seed:',idum,' from start time ',ctime
      write(12,*)' Epirea (1 for pi0, 3 for pi+) =',epirea
      write(12,*)' Target thickness =',t_targ,' (r.l.)'
      write(12,*)' Incident electron energy =',ebeam,' GeV'
      write(12,*)' Electron Q**2 limits:',q2_min,q2_max
      write(12,*)' Lower and upper limit for scattered electron',
     * ' (GeV):',ep_min,epmax

c     Do a preliminary calculation to estimate the maximum value
c     of the integrand

 10   q2		= q2_min
      epw		= 2.1
      q0		= (epw**2-mp**2+q2)/2./mp	!Q**2 at delta mass
      ep		= es-q0
      pp		= sqrt(ep**2-mel**2)
      rp		= pp/ep
      s			= q2/4/es/ep
      th0		= 2.*asin(sqrt(s))
      theta		= th0*180./pi
      t0		= th0
      snt0		= sin(th0)
      cst0		= cos(th0)
      evirt		= (epw**2-mp**2)/2./mp
      qsq		= q2
      gam_flux		= alpha/4/pi**2*ep/es*evirt/qsq*
     +  (2. + 1./tan(th0/2)**2/(1.+q0**2/qsq))

      jacob		= q2**2/(2.*es*ep)
c
      signr_max   = 0.
      signr_p_max = 0.
      signr_m_max = 0.  

c    
      do i=1,1000
        csthcm		= 2.*(ran(idum)-0.5)
        phicm		= 180*ran(idum)
        if (flag_e_hel.eq.0) then
          e_hel = 0
          call wjc(qsq,epw**2,csthcm,phicm,th0,EPIREA,e_hel,
     >    sigma0,sigu,sigl,sigt,sigi,sigip,t_s)
          signr		= gam_flux*sigma0*jacob
          if (signr .gt. signr_max)then
            signr_max=signr
            csthcm_max=csthcm
            phicm_max=phicm
          endif
        else
          e_hel = 1
          call wjc(qsq,epw**2,csthcm,phicm,th0,EPIREA,e_hel,
     >    sigma0,sigu,sigl,sigt,sigi,sigip,t_s)
          signr		= gam_flux*sigma0*jacob
          if (signr .gt. signr_p_max)then
            signr_p_max=signr
            csthcm_p_max=csthcm
            phicm_p_max=phicm  
          endif
          e_hel = -1
          call wjc(qsq,epw**2,csthcm,phicm,th0,EPIREA,e_hel,
     >    sigma0,sigu,sigl,sigt,sigi,sigip,t_s)
          signr		= gam_flux*sigma0*jacob
          if (signr .gt. signr_m_max)then
            signr_m_max=signr
            csthcm_m_max=csthcm
            phicm_m_max=phicm  
          endif
        endif
      enddo

      if (flag_e_hel.eq.0) then
        write(6,*)'signr_max,csthcm_max,phicm_max',
     *  signr_max,csthcm_max,phicm_max
        write(12,*)'signr_max,csthcm_max,phicm_max',
     *  signr_max,csthcm_max,phicm_max
        signr_max=signr_max*fmcall
        write(6,*)'signr_max changed to',signr_max
        write(12,*)'signr_max changed to',signr_max
      else
        write(6,*)'signr_p_max,csthcm_p_max,phicm_p_max',
     *  signr_p_max,csthcm_p_max,phicm_p_max
        write(12,*)'signr_p_max,csthcm_p_max,phicm_p_max',
     *  signr_p_max,csthcm_p_max,phicm_p_max
c
        write(6,*)'signr_m_max,csthcm_m_max,phicm_m_max',
     *  signr_m_max,csthcm_m_max,phicm_m_max
        write(12,*)'signr_m_max,csthcm_m_max,phicm_m_max',
     *  signr_m_max,csthcm_m_max,phicm_m_max
c
        signr_p_max=signr_p_max*fmcall
        write(6,*)'signr_p_max changed to',signr_p_max
        write(12,*)'signr_p_max changed to',signr_p_max  
        signr_m_max=signr_m_max*fmcall
        write(6,*)'signr_m_max changed to',signr_m_max
        write(12,*)'signr_m_max changed to',signr_m_max      
      endif
c   %%%%%%%%%%%%%%%%%%% Main Calculation  %%%%%%%%%%%%%%%%%%%%%%%
      e_hel = 0
 20   continue
      ntries		= ntries+1
      if(flag_e_hel.eq.1) e_hel = get_spin(idum)
      es		= ebeam
      uq2		= uq2_min+uq2rng*ran(idum)
      q2		= 1./uq2

      ep		= epmax-eprng*ran(idum)		
      
      if (ep .lt. ep_min) go to 20	!Electron energy above detector threshold?

      q0		= es-ep
      s			= q2/4/es/ep
      
      if (s .gt. .5) go to 20		!Electron angle < 90 deg?
      
      th0		= 2.*asin(sqrt(s))
      theta		= th0*180./pi
      T0		= th0
      snt0		= sin(th0)
      cst0		= cos(th0)
      ep_test		= (mp**2+2*mp*es-wg**2)/2./(mp+2.*es*s)	
            
      if (ep .gt. ep_test) go to 20	!Electron energy above pion threshold?

      pp		= sqrt(ep**2-mel**2)
      qsq		= q2
      
      if (qsq .le. 0.)then
         write(6,*)' Unphysical Q**2 =',qsq
         go to 20
      endif
      
      qvecx		= -pp*sin(th0)
      qvecz		= ps-pp*cos(th0)

      w2		= mp**2+2*mp*q0-q2
      
      if (w2 .lt. mp**2) go to 20
c      
      epw		= sqrt(w2)
c
      if(epw.lt.w_min.or.epw.gt.w_max) go to 20    
c  
      if (epw .lt. wg+0.002)go to 20

      csthcm    = -1.+2.*ran(idum)
      phicm		= 360.*ran(idum)

        call wjc(qsq,epw**2,csthcm,phicm,th0,EPIREA,e_hel,
     >    sigma0,sigu,sigl,sigt,sigi,sigip,t_s)

      evirt		= (epw**2-mp**2)/2./mp
      gam_flux		= alpha/4/pi**2*ep/es*evirt/qsq*
     +  (2. + 1./tan(th0/2)**2/(1.+q0**2/qsq))

      signr		= sigma0*gam_flux


28    jacob		= q2**2/(2.*es*ep)
      signr		= signr*jacob
      if(flag_e_hel.eq.0) then
        sig_ratio = signr/signr_max
      elseif(flag_e_hel.eq.1.and.e_hel.eq.1) then
        sig_ratio = signr/signr_p_max
      elseif(flag_e_hel.eq.1.and.e_hel.eq.-1) then
        sig_ratio = signr/signr_m_max
      endif
c
      sig_tot		= sig_tot+signr

      
c     Choose the number of times, mcall, to call the routine used
c     to calculate kinematic quantities for the n-tuple.

      rtest		= ran(idum)
      mcall		= sig_ratio
      stest		= sig_ratio-mcall
      
      if (stest .gt. rtest) mcall = mcall + 1
      if (mcall .gt. mcall_max) mcall_max=mcall

c     Increment ntries by the value of mcall. It is also incremented by
c     1 at the beginning of the loop
c     if (mcall .gt. 0)ntries=ntries+mcall-1
c     if mcall .gt. 0 generate mcall n-tuple events.

      if (mcall .eq. 0) go to 30
      
      do j=1,mcall
        ephot		= 0.
        cstk		= 1.
        phik		= 0.
c
        call missm(ebeam,es,ep,th0,ephot,cstk,phik,m_h_s,mpi_s,ppx,
     *  ppy,ppz,eprot,ppix,ppiy,ppiz,epi,csthcm,phicm,wreal,mm2)
c
        if(mm2 .eq. 0. .or. ep .lt. ep_min) go to 24
c
        w2		= mp**2+2*mp*(ebeam-ep)-2*es*ep*(1-cos(th0))+2.*mel**2
        epw		= sqrt(w2)
c
        if(epw.lt.w_min.or.epw.gt.w_max) go to 24
c
        ntp(1)		= es
        ntp(2)		= ep
        ntp(3)		= theta
        ntp(4)		= epw
        ntp(5)		= ppx
        ntp(6)		= ppy
        ntp(7)		= ppz
        ntp(8)		= eprot
        ntp(9)		= ppix
        ntp(10)		= ppiy
        ntp(11)		= ppiz
        ntp(12)		= epi
        ntp(13)		= csthcm
        ntp(14)		= phicm
        ntp(15)		= mm2
        ntp(16)		= qvecx
        ntp(17)		= qvecz
        ntp(18)		= q0
        ntp(19)		= cst0
        ntp(20)     = e_hel
        ntp(21)     = qsq
c        
        call hfnt(10)
c        
        nevent		= nevent+1
c        
        do jj = 1,npart
          v(jj,1) = 0.
          v(jj,2) = 0.
          v(jj,3) = 0.
        enddo
c         
        phir		= 2.*pi*ran(idum)	!Rotate all momentum around beam line randomly
        rotc		= cos(phir)
        rots		= sin(phir)

        px 		    = ep*sin(theta*pi/180.)	!Scattered electron
        py 		    = 0.
        p(1,1) 		= px*rotc+py*rots
        p(1,2) 		= py*rotc-px*rots
        p(1,3) 		= ep*cos(theta*pi/180.)
        p(1,4) 		= ep
        p(2,1) 	    = ppix*rotc+ppiy*rots	!Charged Kaon
        p(2,2) 	    = ppiy*rotc-ppix*rots
        p(2,3) 	    = ppiz
        p(2,4) 	    = epi
        if (npart .eq. 3)then               !Lambda or Sigma
            p(3,1)	= ppx*rotc+ppy*rots
            p(3,2)	= ppy*rotc-ppx*rots
            p(3,3)	= ppz
            p(3,4)	= eprot
        endif

        call bos_out	! Pack the BOS banks and write out to file
 24     continue
      enddo
      
      ntell		= nevent/nprint-ntold
      
      if (ntell .gt. 0)then
        write(6,*)' ntries, nevent, mcall_max: '
     *             ,ntries,nevent,mcall_max
        write(12,*)' ntries, nevent, mcall_max: '
     *              ,ntries,nevent,mcall_max
        events		= nevent
        tries		= ntries
        sig_int		= events/tries
          
c     hadron phase space = 4*pi
c     electron phase space =2*pi*uq2rng*eprng
        if(flag_e_hel.eq.1) then
          signr_max = (signr_p_max + signr_m_max) / 2.0
        endif
        sig_int		= sig_int*signr_max*(4.*pi)*(2.*pi*uq2rng*eprng)
        sig_sum		= sig_tot*(4.*pi)*(2.*pi*uq2rng*eprng)/tries

        write(6,*)' Integrated cross section =',sig_int,sig_sum,
     *    ' micro-barns'
        write(6,*)' Beam time at Lum=1.0E34 =',events/sig_sum*1.E-4
     *    ,' seconds'
        write(12,*)' Integrated cross section =',sig_int,sig_sum
     *    ,' micro-barns'
        write(12,*)' Beam time at Lum=1.0E34 =',events/sig_sum*1.E-4
     *    ,' seconds'
        call timex(itime2)
        t_elapse	= t_elapse+INT(itime2-itime1)
        itime1		= itime2
        write(6,*) ' Elapsed CPU time = ',t_elapse/60,' minutes'
        write(12,*)' Elapsed CPU time = ',t_elapse/60,' minutes'
        ntold		= ntold+1
      endif

      if (nevent .gt. nmax) go to 50
 30   go to 20

 50   call hrout(0,icycle,' ')
      call hrend('AONORAD')
      call bos_end(recname)
      call getasciitime(idum,ctime)
      
      write(14,*)' AO Calculation of Single Pion Production',
     + ' without radiative corrections'
      write(14,*) ' ending time ', ctime
      write(14,*)' Epirea (1 for pi0, 3 for pi+) =',epirea
      write(14,*)' Target thickness =',t_targ,' (r.l.)'
      write(14,*)' Incident electron energy =',ebeam,' GeV'
      write(14,*)' Electron Q**2 limits:',q2_min,q2_max
      write(14,*)' Lower and upper limit for scattered electron',
     * ' energy(GeV):',ep_min,epmax
      write(14,*)'signr_max,csthcm_max,phicm_max',
     * signr_max,csthcm_max,phicm_max
      write(14,*)' ntries, nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max

      write(14,*)' Elapsed CPU time = ',t_elapse/60,' minutes'
      write(14,*)' Integrated cross section =',sig_int,sig_sum
     *    ,' micro-barns'
      write(14,*)' Beam time at Lum=1.0E34 =',events/sig_sum*1.E-4
     *    ,' seconds'
      close(unit=12)
      close(unit=14)
      close(unit=1)

 99   continue
 
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

c   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      subroutine missm(ebeam,es,ep,th0,ephot,cstk,phik,m_h,mpi,ppx,
     * ppy,ppz,eprot,ppix,ppiy,ppiz,epi,csthcm,phicm,wreal,mm2)


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
      real m_h
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
      real mpi
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

      mp	= .938
      mel	= 0.511E-3
      pi	= 3.14159
      wmin	= mp+mpi
      csthe	= cos(th0)
      snthe	= sin(th0)
      nu	= es-ep
      ps	= abs(es**2-mel**2)
      pp	= abs(ep**2-mel**2)
      ps	= sqrt(ps)
      pp	= sqrt(pp)
      q2	= 2.*es*ep-2.*ps*pp*csthe-2.*mel**2
      w2	= mp**2-q2+2.*mp*nu
      qx	= -pp*snthe
      qz	= ps-pp*csthe
      qvec	= sqrt(qx**2+qz**2)
      if (abs(cstk) .gt. 1.)then
         write(6,*)' missm-1: cstk =',cstk
         cstk	= cstk/abs(cstk)
         go to 30
      endif
      sntk	= sqrt(1.-cstk**2)
      csphk	= cos(phik)
      snphk	= sin(phik)

      cstq	= qz/qvec
      sntq	= sqrt(1.-cstq**2)
      
c      ephotx=ephot*(sntk*csphk*cstq-cstk*sntq)
c      ephoty=ephot*sntk*snphk
c      ephotz=ephot*(cstk*cstq+sntk*csphk*sntq)

      ephot	= 0.
      ephotx	= 0.
      ephoty	= 0.
      ephotz	= 0.
      
c     Calculate the dot product of the photon vector and the q-vector
c     eph_dot_q=ephotx*qx+ephotz*qz

      eph_dot_q	= 0.

c     Calculate the mass of the actual hadronic system for the two
c     photon directions.

      wreal	= (w2-2.*ephot*(nu+mp)+2.*eph_dot_q)
      if (wreal .le. wmin**2) go to 30	!Hadronic mass below pion threshold
      wreal	= sqrt(wreal)
      
c     Calculate the energy of the actual hadronic system, with radiation

      ewreal	= nu+mp-ephot

c     Calculate the momentum components of the nucleon and pion in the lab

      snthcm	= (1.-csthcm**2)
      if (snthcm .le. 0.)then
         write(6,*)' missm-2: snthcm =',snthcm
         snthcm = 0.0000001
         go to 30
      endif
      snthcm	= sqrt(snthcm)

c     Calculate cos and sin of phicm

      csphi	= cos(phicm*pi/180.)
      snphi	= sin(phicm*pi/180.)

c     Calculate laboratory components of the resonance momentum vector

      pwrz	= qz-ephotz
      pwrx	= qx-ephotx
      pwry	= -ephoty
      pwr	= sqrt(pwrz**2+pwrx**2+pwry**2)
      
c     Calculate the relativistic factors, gamma and beta, for the resonance

      beta	= pwr/ewreal
      gamma	= ewreal/wreal

c     Define angle cosines for the laboratory system with the resonance
c     as the z axis.  Choose the y axis of the resonance frame perpendicular
c     to the laboratory x axis.

      pfac	= sqrt(pwry**2+pwrz**2)
      cxx	= pfac/pwr
      cxy	= -pwrx*pwry/pfac/pwr
      cxz	= -pwrx*pwrz/pfac/pwr
      cyx	= 0
      cyy	= pwrz/pfac
      cyz	= -pwry/pfac
      czx	= pwrx/pwr
      czy	= pwry/pwr
      czz	= pwrz/pwr

c     Calculate the momentum of the pion and proton in the resonance frame
      pstar	= ((wreal**2-m_h**2-mpi**2)**2/4.-(m_h*mpi)**2 )/wreal**2
      if (pstar .le. 0.) then
        write(6,*)'missm-3: pstar=',pstar
        pstar	= 0.000001
        go to 30
      endif
      pstar	= sqrt(pstar)
      
c     Calculate the energy of the proton and pion in the resonance center
c     of mass frame

      epcm  	= sqrt(pstar**2+m_h**2)
      epicm 	= sqrt(pstar**2+mpi**2)

c     Calculate the pion momentum components and energy in the lab frame
c     where the z axis is the direction of the momentum of the resonance.
c     The center of mass angles are pion angles.

      ppiwx	= pstar*snthcm*csphi
      ppiwy	= pstar*snthcm*snphi
      ppiwz	= gamma*(pstar*csthcm+beta*epicm)
      epi	= gamma*(epicm+beta*pstar*csthcm)
      
c     Calculate momentum components of the pion in
c     the lab frame where the z axis is along the incident beam

      ppix	= ppiwx*cxx+ppiwy*cyx+ppiwz*czx
      ppiy	= ppiwx*cxy+ppiwy*cyy+ppiwz*czy
      ppiz	= ppiwx*cxz+ppiwy*cyz+ppiwz*czz

c     Calculate the proton energy and momentum components
c     in the lab resonance frame
c     The center of mass angles are pion angles

      ppwx	= -ppiwx
      ppwy	= -ppiwy

c     ppwz=-ppiwz+gamma*beta*(epicm+epcm)
c     epicm+epcm=wreal

      ppwz	= gamma*beta*wreal-ppiwz
      eprot	= gamma*wreal-epi

c     Rotate the lab momentum components of the proton into the frame
c     where the z axis is along the incident electron beam

      ppx	= ppwx*cxx+ppwy*cyx+ppwz*czx
      ppy	= ppwx*cxy+ppwy*cyy+ppwz*czy
      ppz	= ppwx*cxz+ppwy*cyz+ppwz*czz

c     Calculate the square of the missing mass, associated with the
c     momentum components of the charged hadron

      if (mpi .lt. .137) then
        q_dot_pp	= qx*ppx+qz*ppz
        mm2		= -q2+2*mp**2+2*mp*(nu-eprot)-2*nu*eprot+2*q_dot_pp
      else
        q_dot_pp	= qx*ppix+qz*ppiz
        mm2		= -q2+mp**2+mpi**2+2*mp*(nu-epi)-2*nu*epi+2*q_dot_pp
      endif

      return
      
 30   mm2=0.
 
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
C======================================================================

