      subroutine elast_gen(file1,file2,file3,file4,f,d,g_,tl,tr,
     +			vx,vy,vz,be,emn,smn,smx,p_,M,c)
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

c     This version of the program makes only one new file and then quits.

      implicit none
#include "bcs.inc"
#include "mc.inc"
#include "names.inc"

      common /random/idum
      common /stdpar/mel,mp,pi,alpha
      common /bos_events/es,ep,th0,ppx,ppy,ppz,eprot,egam,delta,
     * egamx,egamy,egamz
      common /parset/ebeam,epmin0,ps,rs,t_targ,bfac,th_min,th_max
     * ,nmax,nprint, elas_inc,recname,id1,id2,id3,iext1,fmcall
     * ,sigr_max,month,day,year,filerz,file_out,file_sum,file_bos
      common /new_parset/vertex_x,vertex_y,vertex_z,hydrogen_rad,v_z
      
      real mel,mp,pi,alpha
      real es,ep,th0,ppx,ppy,ppz,eprot,egam,delta,egamx,egamy,egamz

      real ebeam,epmin0,ps,rs,t_targ,bfac,th_min,th_max,fmcall
      real r_targ,d
      integer nmax,nprint,elas_inc,id1,id2,id3,iext1
      integer ir1
      character*132 filerz
      character*132 file_out
      character*132 file_sum
      character*132 file_bos
      character*8 recname
      character*28 ctime
      character*3 month
      character*2 day
      character*2 year

      real cst0,cst00,cstmin,cstk,ep0,erng
      real csrng, csrnge,csrngb,cran2
      real mm2,sig_el
      real phik,pp,ran,rtest
      real s,sigr,sigr_max,sigr1,snt0,stest,sigr_all
      real Tk,tsec0,tsec1,t_elapse,w,wmax
      real mpfac,cran,mcfac,cstk1,cstk2,den
      real jacob,ucos,ucos0,ucrng
      real a1,a2,a3
      real q2,nu,qvec
      real epmin
      real uek,epk
      real ep_sav
      real targs,targp
      real theta_pr,phi_pr
      real egdiv
      real*8 sig_tot
      real cdiv1,cdiv2,cdiv3,cdiv4,frac
      real tries
      real sig_int
      real space
      real sig_sum
      real vertex_x,vertex_y,vertex_z,hydrogen_rad,temp,v_z
      
      integer*8 ntries
      integer*4 idum

      integer j,mcall,mcall_max,ntell,ntold
      integer dismc(6,100), intreg

      real f,tl,tr,vx,vy,vz,be,emn,smn,smx,c
      integer g_,p_,M
      character*132 file1,file2,file3,file4

c     Parameters for the n-tuple, which is named func1 and contains
c     15 elements per event.
      common /pawc/h(5000000)

      integer h,n,nevent,lrecl,istat,icycle
      parameter (n=21)
      real*4 ntp(n)
c     tag is the an array of names for the variables in the n-tuple.
      character*5 tag(n)

      data tag /'  ES ','  EP ','THETE', '  W  ',' PPRX',' PPRY'
     * ,' PPRZ','EPROT','THETP','PHIPR',' MM  ','CSTHK','PHIK ','  EG ',
     * ' EGX ',' EGY ',' EGZ ',' VX  ', ' VY  ', ' VZ  ', ' Q2  '/


      DATA mp/0.938/,mel/0.511E-3/
c     egdiv is the fraction of the time we look in the range of
c     photon energies from 0 to delta.
      data egdiv/0.2/

      print *,'fortran routine elast_gen entered...'

      file_bos = file1
      file_out = file2
      filerz   = file3
      file_sum = file4
      data ctime  /'                            '/


c      print *,file_bos
c      print *,file_out
c      print *,filerz
c      print *,file_sum

      csrng=.04
      egdiv = f


 11   continue
      cdiv1 = 0.25
      cdiv2 = 0.25
      cdiv3 = 0.05
      cdiv4 = 0.15
      cdiv2=cdiv1+cdiv2
      cdiv3=cdiv2+cdiv3
      cdiv4=cdiv3+cdiv4
      if (cdiv4 .ge. .95)then
         write(6,*) ' The four inputs must sum to a number less than 0.95'
         write(6,*)' Re-enter the four numbers'
         go to 11
      endif
      pi=2.*acos(0.)
      call timex(tsec0)
      call getunixtime(idum)
      call getasciitime(idum,ctime)
      month = ctime(5:7)
      day   = ctime(9:10)
      year  = ctime(23:24)
      if (day(1:1).eq. ' ')then
       ir1=48
       day(1:1)=char(ir1)
      endif

      idum=-idum
      write(6,*)'seed:',idum,' from start time ',ctime
      cstk=ran(idum)


      sigr_all=0.
      wmax=3.0
      bfac=4./3.
      alpha=1./137.01
      call bos_setup(g_)
c
c     Set-up output files and do the monte-carlo event generation


 1    call par_setup(file1,file2,file3,file4,f,d,tl,vx,vy,vz,be,
     +				emn,smn,smx,p_,M,c)
    
      call timex(tsec0)

      write(12,*)' Elastic Scattering, Date and time:',ctime


c     set up the ntuple file
      lrecl=1024
      call hlimit(1000000)
      call hropen(1,'elasgen',filerz,'n',lrecl,istat)
      call hbookn(10,'func1',n,'elasgen',1000,tag)
c     Open new BOS file

      bosout = file_bos
c     Initialize BOS
        call bos_ini(file_bos)

      do intreg=1,6
        do j=1,100
          dismc(intreg,j)=0
        enddo
      enddo

c     Choose two limits for the electron scattering angle:
      cst00=cos(th_min*pi/180.)
      cstmin=cos(th_max*pi/180.)

      ucos0=1./(1.-cstmin)
      ucrng=1./(1-cst00)-ucos0
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

      if (fmcall .ne. 0.)then 
        call sigmax_calc(targs,ucos0,ucrng,egdiv,
     + erng,ebeam,epmin0,delta,cdiv1,cdiv2,cdiv3,cdiv4,csrng,
     + sigr_max)
        sigr_max=fmcall*sigr_max
      endif

      write(6,*)' sigr_max changed to', sigr_max


c     Use a Monte-Carlo to calculate a distribution of nmax events
c     distributed according to the Mo-Tsai integrand.


      ntries=0
      nevent=0
     
      sig_tot=0.
      ntold=0
      mcall_max=0

 20   continue
      ntries=ntries+1

c     calculate the energy of the electron at the scattering point
c     after making its way through the target.  First, randomly
c     choose the interaction point.
    
c     Change from units of r.l. to cm
 21   t_targ = hydrogen_rad * t_targ / bfac 
      r_targ = tr
      targs=t_targ*ran(idum)

c     Change into proper coordinate system
      vertex_z = vz + targs - t_targ / 2.0
c      print*,t_targ, targs, vertex_z
c     calculates the distance that the electron stays in the target

      targp=r_targ/sin(th0)
      temp=(t_targ - targs)/cos(th0)
      if (temp .lt. targp) targp = temp

c     Back to units of r.l.      
      t_targ = bfac * t_targ / hydrogen_rad 
      targs  = bfac * targs / hydrogen_rad      

c     Now calculate the radiation loss
      es=ebeam
      call stragl(es,targs)


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
      intreg=6
      call elas_cor(es,th0,sig_el)
c     Make an effective cross section differential in
c     ek, csthk,phik.
      sigr1=sig_el*jacob/delta/4./pi
      sig_tot=sig_tot+sigr1

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
      q2=4.*es*ep*s
      nu=es-ep
      qvec=sqrt(q2+nu**2)
      egam=(2.*mp*nu-q2)/2./(mp+nu-qvec*cstk)
c      egam=epk

      sigr=sigr1/sigr_max
      mpfac=1.
      mcfac=1.
      go to 48

 41   den=sqrt(ps**2+pp**2-2.*ps*pp*cst0)
      cstk1=(ps-pp*cst0)/den
      cstk2=(ps*cst0-pp)/den
      csrnge=csrng
      if ((1.-cstk1) .lt. csrnge)csrnge=1.-cstk1
      if (abs(cstk1- cstk2).lt.2.*csrnge)csrnge=abs(cstk1-cstk2)/2.
      csrngb=csrng/40.
      if (csrngb .gt. csrnge/5.)csrngb=csrnge/5.
      cran=ran(idum)
      cran2=ran(idum)
      if (cran2 .lt. .5)then
         cran2=-1.
      else
         cran2=1.
      endif
      if (cran .lt. cdiv1)then
         cstk=cstk1+csrngb*(2.*ran(idum)-1.)
         mcfac=csrngb/cdiv1
         phik=(ran(idum)-.5)*pi/9.
         mpfac=1/18.
	 intreg=1
      elseif(cran .lt. cdiv2)then
         cstk=cstk1+cran2*(csrngb+(csrnge-csrngb)*ran(idum))
         mcfac=(csrnge-csrngb)/(cdiv2-cdiv1)
         phik=(ran(idum)-.5)*pi/9.
         mpfac=1/18.
	 intreg=2
      elseif (cran .lt. cdiv3)then
         cstk=cstk2+csrngb*(2.*ran(idum)-1.)
         mcfac=csrngb/(cdiv3-cdiv2)
         phik=(ran(idum)-.5)*pi/9.
         mpfac=1/18.
	 intreg=3
      elseif(cran .lt. cdiv4)then
         cstk=cstk2+cran2*(csrngb+(csrnge-csrngb)*ran(idum))
         mcfac=(csrnge-csrngb)/(cdiv4-cdiv3)
         phik=(ran(idum)-.5)*pi/9.
         mpfac=1/18.
	 intreg=4
      else
 44      cstk=2.*ran(idum)-1.
         phik=2.*pi*(ran(idum)-0.5)
         if (abs(cstk-cstk1) .lt. csrnge
     +    .or. abs(cstk-cstk2) .lt. csrnge)then
            if (abs(phik) .lt. pi/18.)go to 44
         endif
	 intreg=5
c     The following line corrected on Jan. 15, 1999
         mcfac=(1.-2.*csrnge)/(1.-cdiv4)
         mpfac=17./18.
      endif

 45   Tk=acos(cstk)

      call elast_rad(es,ep,th0,cstk,phik,egam,sigr1)
      sigr1=sigr1*jacob
      if (sigr1 .lt. 0.)sigr1=0.

c     Normalize sigr according to the integration region.

      sigr1=mpfac*mcfac*sigr1
      sig_tot=sig_tot+sigr1
c     Compare the cross section to the maximum value
      sigr=sigr1/sigr_max

c     Calculate the number of times, mcall, to call the routine used
c     to calculate kinematic quantities for the n-tuple.
 48   rtest=ran(idum)
      mcall=int(sigr)
      stest=sigr-mcall
      if (stest .gt. rtest)mcall = mcall + 1
      if (mcall .gt. mcall_max) mcall_max=mcall

c     If mcall .gt. 0 generate mcall n-tuple events.
      if (mcall .lt. 1)go to 20
      if (mcall .lt. 100)then
        dismc(intreg,mcall)=dismc(intreg,mcall)+1
      else
        dismc(intreg,100)=dismc(intreg,100)+1
      endif
       ep_sav=ep
       do j=1,mcall
c     Calculate the radiation loss for the electron leaving the target
        ep=ep_sav
c        call stragl(ep,targp)

        if (ep .lt. epmin0)go to 55

        call missm(cstk,phik,theta_pr,phi_pr,mm2)

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
        ntp(15)=egamx
        ntp(16)=egamy
        ntp(17)=egamz
        ntp(18)=vertex_x
        ntp(19)=vertex_y
        ntp(20)=vertex_z
        ntp(21)=q2

        call hfn(10,ntp)
        nevent=nevent+1

        call bos_cal
        call bos_out	! Pack the BOS banks and write out to file        
 55     continue
      enddo

c     Talk to the user every now and then.
       ntell=nevent/nprint-ntold
       if (ntell .gt. 0)then
          call int_out(ntries,nevent,mcall_max,sigr_max,sig_tot,ucrng
     * ,erng,tsec0,tsec1,t_elapse)
          ntold=ntold+1
      endif
       
c     Do we have enough events in the n-tuple?
       if (nevent .gt. nmax)go to 50
 30    go to 20

c     Close out the n-tuple file
 50   call hrout(0,icycle,' ')
      call hrend('elasgen')

c     Close out the bos event file
      CALL bos_end(recname)

      call timex(tsec1)
      t_elapse=tsec1-tsec0

      write(6,1199)t_elapse
      write(12,1199)t_elapse
 1199 format(' Elapsed CPU time = ',f10.1,' seconds')
c     Close out the n-tuple file

      write(14,*)'Start time ',ctime
      write(14,5004)ntries,nevent,mcall_max
 5004 format(' ntries =',i10,',  successes =',i8,', mcall_max = ',i3)

      write(14,5005)t_elapse/60
 5005 format(' Elapsed CPU time = ',f8.2,' minutes')

      do intreg=1,5
        if (intreg .eq. 1)frac=cdiv1
        if (intreg .eq. 2)frac=cdiv2-cdiv1
        if (intreg .eq. 3)frac=cdiv3-cdiv2
        if (intreg .eq. 4)frac=cdiv4-cdiv3
        if (intreg .eq. 5)frac=1.-cdiv4
        write(14,5022)intreg,frac,(dismc(intreg,j),j=1,100)
      enddo

      write(14,5023)egdiv,(dismc(6,j),j=1,100)

 5022 format(' distribution of mcall values in the inelastic tail',
     * ' region',i4/
     * ' fraction of tail calculation spent here:', f6.3/10(10i7/))
 5023 format(' distribution of mcall values in the elastic peak'/
     * ' fraction of total integration spent here:',f6.3/10(10i7/))

      write(14,5008)nevent/t_elapse
 5008 format(' number of successes per cpu second =',f7.3)
      write(14,5009)nevent/t_elapse/mcall_max
 5009 format(' Quality factor for the run =',f10.5)
      sig_int=nevent
      sig_int=sig_int/ntries
      space=4.*pi*ucrng*2.*pi*erng
      sig_int=sig_int*sigr_max*space
      tries=ntries
      sig_sum=sig_tot*space/tries
      write(14,5012)sigr_max
 5012 format(' Reference differential cross section was',f10.6)
      write(14,5006)sig_int,sig_sum
 5006 format(' Integrated cross section (direct)=',f10.5,' micro-barns'/
     + ' Integrated cross section (from successes)=',f10.5,' micro-barns')
      write(14,5007)nevent/sig_sum*1.E-4
 5007 format(' Beam time at Luminosity =1.0E34 =',f8.3,' seconds')


      close(unit=12)
      close(unit=14)

      stop
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
      if (ran .eq. 0. .or. ran .eq. 1.)go to 25
      if (ran .lt. 0. .or. ran .gt. 1.)then
         write(6,*)' random error, ran =',ran
         go to 25
      endif

      return
      end


       subroutine missm(cstk,phik,theta_pr,phi_pr,mm2)

       implicit none

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

      common /random/idum
      common /stdpar/mel,mp,pi,alpha
      common /bos_events/es,ep,th0,ppx,ppy,ppz,eprot,egam,delta,
     * egamx,egamy,egamz

      common /parset/ebeam,epmin0,ps,rs,t_targ,bfac,th_min,th_max
     * ,nmax,nprint, elas_inc,recname,id1,id2,id3,iext1,fmcall
     * ,sigr_max,month,day,year,filerz,file_out,file_sum,file_bos
      common /new_parset/vertex_x,vertex_y,vertex_z,hydrogen_rad,v_z
     
      real mel,mp,pi,alpha

      real es,ep,th0,ppx,ppy,ppz,eprot,egam,delta,egamx,egamy,egamz

      real ebeam,epmin0,ps,rs,t_targ,bfac,th_min,th_max,fmcall
      real vertex_x,vertex_y,vertex_z,hydrogen_rad,v_z
     
      integer nmax,nprint,elas_inc,id1,id2,id3,iext1
      character*132 filerz
      character*132 file_out
      character*132 file_sum
      character*132 file_bos
      character*8 recname
      character*3 month
      character*2 day
      character*2 year


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
      real eph_dot_q
      real mm2
      real nu
      real pbeam
      real phik
      real pps,pprot
      real pp
      real q2
      real q_dot_pp
      real qx,qz,qvec
c      real ran,r1,r2
      real sigr_max
      real snphip
      real snphk
      real snthe
      real snthp
      real sntk
      real sntq
      real theta_pr
      real phi_pr
      real w2

      integer*4 idum

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
      real*8 pi
      real*8 w2,w
      real*8 q2e
      real*8 phik,sdotk,pdotk
      real  es_4,ep_4,thetae_4,csthk_4,phik_4,egam_4,d3sig_4

      pi=acos(0.)*2.
      es=es_4
      ep=ep_4
      thetae=thetae_4
      csthk=csthk_4
      phik=phik_4

      me=0.000511
      mp=.938
      alpha=1/137.01
      d3sig=0.

c     calculate the scattered electron energy for elastic
c     scattering:


      pvec=ep-0.5*me**2/ep
      svec=es-0.5*me**2/es
      q2e=-2.*me**2+2.*es*ep-2.*svec*pvec*dcos(thetae)
      w2=mp**2+2*mp*(es-ep)-q2e
      w=dsqrt(w2)
      if (w .lt. mp)go to 99


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
      if (q2 .gt. 0.)write(6,*)' q2 >0.',es,ep,thetae,egam,csthk
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
 99   continue
c     convert cross section to micro-barns
      d3sig_4=d3sig*389.37966
      egam_4=egam

      return
      end




      subroutine elas_cor(es,theta,sigel)
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
      common /stdpar/mel,mp,pi,alpha
      common /rad_cor/del_radcor
      real del_radcor(400)
      real mel,mp,pi,alpha

      real es,theta
      real cst1,sigunp,sigel,eel,qs
      real epr
      real radcor
      integer iq


      cst1=1.-cos(theta)
      eel=es/(1.+es/mp*cst1)
      epr=es+mp-eel
      qs=2.*es*eel*cst1
      iq=qs*50.
      iq=iq+1
      if (iq .gt. 400)then
         write(6,*)' Q2 = ',qs,' exceeds programmed range'
         stop
      endif
c     calculate the elastic peak
c

      call epelas(es,theta,sigunp)
      radcor=del_radcor(iq)
      sigel=radcor*sigunp
      return
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
      common /stdpar/mel,mp,pi,alpha

      real mel,mp,pi,alpha
      real csth2
      real e0
      real e_pr
      real f1,f2
      real ge,gm
      real ge2,gm2
      real q2
      real sig_mott,sig_unp
      real snth2
      real tau
      real theta



c	calculate Q**2
        snth2=(1.-cos(theta))/2.
        csth2=1.-snth2
        e_pr=e0/(1.+2.*(e0/mp)*snth2)
	q2=4.*e0*e_pr*snth2

	tau=q2/4./mp**2

	ge=1./(1.+q2/.71)**2
	gm=2.7928*ge
	ge2=ge**2
	gm2=gm**2

c	

c	calculate elastic scattering cross section, differential
c       in electron scattering angle.

	sig_mott=alpha**2/4./e0**2/snth2**2
c	convert cross section from (GeV)**{-2}/sr to microbarns/sr
	sig_mott=389.37966 *sig_mott

c       calculate the unpolarized cross section
        f1=(ge2+tau*gm2)*csth2/(1.+tau)
        f2= 2. *tau*gm2*snth2
	sig_unp=sig_mott*(e_pr/e0) * (f1+f2)
99	return
	end


      real*8 function spence(x)
      implicit none
      real*8 x
      real*8 pi
      real*8 sintp,sintn
      pi=2.*acos(0.)

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
      common /stdpar/mel,mp,pi,alpha
      common /bos_events/es,ep,th0,ppx,ppy,ppz,eprot,egam,delta,
     * egamx,egamy,egamz
      common /random/idum
      common /new_parset/vertex_x,vertex_y,vertex_z,hydrogen_rad,v_z      

      real mel,mp,pi,alpha
      real es,ep,th0,ppx,ppy,ppz,eprot,egam,delta,egamx,egamy,egamz
      real phi_r,sin_phir,cos_phir,pxtmp,pytmp
      real ran
      real vertex_x,vertex_y,vertex_z,hydrogen_rad,v_z
      integer*4 idum
      integer jj

#include "bcs.inc"
#include "mc.inc"
#include "names.inc"
      
      do jj = 1,npart
        v(jj,1) = vertex_x
        v(jj,2) = vertex_y
        v(jj,3) = vertex_z
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
        p(3,1) = egamx
        p(3,2) = egamy
        p(3,3) = egamz
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

      subroutine bos_setup(g_)
      implicit none

#include "bcs.inc"
#include "mc.inc"
#include "names.inc"

      integer g_

      npart = g_
      if (npart .ne. 3)npart=2
      write(6,*)' Number of particles per event in bos bank =',npart
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

      subroutine par_setup(file1,file2,file3,file4,f,d,tl,vx,vy,vz,be,
     +				emn,smn,smx,p_,M,c)

      implicit none

      common /stdpar/mel,mp,pi,alpha
      common /bos_events/es,ep,th0,ppx,ppy,ppz,eprot,egam,delta,
     * egamx,egamy,egamz

      common /parset/ebeam,epmin0,ps,rs,t_targ,bfac,th_min,th_max
     * ,nmax,nprint, elas_inc,recname,id1,id2,id3,iext1,fmcall
     * ,sigr_max,month,day,year,filerz,file_out,file_sum,file_bos
      common /new_parset/vertex_x,vertex_y,vertex_z,hydrogen_rad,v_z

     
#include "mc.inc"


      real f,d,tl,vx,vy,vz,be,emn,smn,smx,c
      integer p_,M
      character*132 file1,file2,file3,file4

      real mel,mp,pi,alpha

      real es,ep,th0,ppx,ppy,ppz,eprot,egam,delta,egamx,egamy,egamz
      real sigr_max
      real ebeam,epmin0,ps,rs,t_targ,bfac,th_min,th_max,fmcall
      real vertex_x,vertex_y,vertex_z,hydrogen_rad,v_z      
      integer nmax,nprint,elas_inc,id1,id2,id3,iext1

      character*132 filerz
      character*132 file_out
      character*132 file_sum
      character*132 file_bos
      character*8 recname
      character*3 month
      character*2 day
      character*2 year

c      character*1 ich1
c      integer ir1


      t_targ = tl

      hydrogen_rad = 865 ! hydrogen radtion length (cm)
      t_targ = bfac * t_targ / hydrogen_rad 

      vertex_x = vx
      vertex_y = vy
      v_z      = vz

      ebeam = be
      write(6,501)ebeam
 501  format(' Incident electron energy = ',f7.3,' GeV')
      epmin0 = emn
      write(6,*)epmin0
 502  format(' Minimum allowed scattered electron energy =',f7.3,' MeV')

      es=ebeam

c     Specify the minimum and maximum scattering angles
      th_min = smn
      th_max = smx
c     Decide whether to include the elastic peak in the n-tuple
      elas_inc = p_
c     Select the number of events desired in the rz file.
      nmax = M
      nprint=nmax/25

      sigr_max = c


c     calculate the incident momentum
      ps=es-0.5*mel**2/es
      rs=ps/es

c     Choose the resolution cut for the elastic peak.  For radiated
c     photon energies less than delta, the reaction
c     is considered non-radiative

      delta = d
      call elas_radcor(es,delta)

c     Name the output rz file according to energy.
c     filerz=elasgen-1.6-0000.rz.00, for example.

      recname = 'MCEVENT'

      open(unit=12,name=file_out)
      open(unit=14,name=file_sum)

      write(12,*)' Incident energy is ',ebeam,' GeV'
      write(12,*)' Minimum accepted scattered electron energy is',epmin0
      write(12,*)' Target thickness is ',t_targ/bfac,' r.l.'
      write(12,*)' Number of particles per event in bos bank =',npart
      write(14,*)' Radiative elastic scattering'
      write(14,5001)ebeam
 5001 format(' Incident electron energy =',f7.3,' GeV')
      write(14,5002)epmin0
 5002 format(' Minimum accepted scattered electron energy is',f7.3,
     +  ' GeV')
      write(14,5003)t_targ/bfac
 5003 format(' Target thickness =',f7.4,' (r.l.)')
      write(14,5004)npart
 5004 format(' Each event in the bos bank contains',i2,' particles')
      write(6,5005)th_min,th_max
      write(12,5005)th_min,th_max
      write(14,5005)th_min,th_max
 5005 format(' Electron scattering angles range from ',f5.1,' to ',
     + f5.1,' degrees')

      if (elas_inc .ne. 1)then
         write(6,*)' Elastic peak included'
         write(12,*)' Elastic peak included'
         write(14,*)' Elastic peak included'
      else
         write(6,*)' Elastic peak excluded'
         write(12,*)' Elastic peak excluded'
         write(14,*)' Elastic peak excluded'
      endif

      return
      end



      subroutine int_out(ntries,nevent,mcall_max,sigr_max,sig_tot,ucrng
     * ,erng,tsec0,tsec1,t_elapse)

      implicit none
      common /stdpar/mel,mp,pi,alpha
      real mel,mp,pi,alpha

      real sigr_max,ucrng,erng,tsec0,tsec1
      real t_elapse
      real sig_int,sig_sum
      real*8 sig_tot
      integer*8 ntries
      real space,tries

      integer nevent, mcall_max


      write(6,*)' ntries,nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max
      write(12,*)' ntries,nevent, mcall_max: '
     *    ,ntries,nevent,mcall_max
      sig_int=nevent
      sig_int=sig_int/ntries
      space=4.*pi*ucrng*2.*pi*erng
      sig_int=sig_int*sigr_max*space
      tries=ntries
      sig_sum=sig_tot*space/tries
      write(6,*)' sig_int = ',sig_int,sig_sum
          
      write(6,*)' Number of seconds at Lum=1E34 is',
     *    (1.E-4)*nevent/sig_int
      write(12,*)' sig_int = ',sig_int,sig_sum
      write(12,*)' Number of seconds at Lum=1E34 is',
     *    (1.E-4)*nevent/sig_int

      call timex(tsec1)
      t_elapse=tsec1-tsec0
      write(6,110)t_elapse
      write(12,110)t_elapse
 110  format(' Elapsed CPU time = ',f8.1,' seconds')

      return
      end

c  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      
      subroutine sigmax_calc(targs,ucos0,ucrng,egdiv,
     + erng,ebeam,epmin0,delta,cdiv1,cdiv2,cdiv3,cdiv4,csrng,
     + sigr_max)
      implicit none

      common /random/idum
      common /stdpar/mel,mp,pi,alpha
      common /new_parset/vertex_x,vertex_y,vertex_z,hydrogen_rad,v_z
      
      real mel,mp,pi,alpha
      real targs,ucos0,ucrng,egdiv,erng,ebeam,epmin0,delta
      real cdiv1,cdiv2,cdiv3,cdiv4,sigr_max
      real csrng,csrnge,csrngb
      real cran2
      real es,ep,egam
      real vertex_x,vertex_y,vertex_z,hydrogen_rad,temp,v_z
      
      real t_targ,targp
      real ps
      real rs
      real cst0
      real ucos
      real jacob,th0,snt0,s,a1,a2,a3,ep0
      real epk,pp
      real den,cstk,cstk1,cstk2,cran,mcfac,mpfac,Tk,phik
      real sigr1
      real ran
      real bfac
      integer*4 ntries
      integer*4 idum

      ntries=0
      sigr_max=0.
      bfac = 4./3.

20    continue
      ntries=ntries+1

c     calculate the energy of the electron at the scattering point
c     after making its way through the target.  First, randomly
c     choose the interaction point.

c     Change from units of r.l. to cm
 21   t_targ = hydrogen_rad * t_targ / bfac 
      targs=t_targ*ran(idum)

c     Change into proper coordinate system
      vertex_z = v_z + targs - t_targ / 2.0
c      print*,t_targ, targs, vertex_z
c     calculates the distance that the electron stays in the target

c     Back to units of r.l.      
      t_targ = bfac * t_targ / hydrogen_rad 
      targs  = bfac * targs / hydrogen_rad      
 

c     Now calculate the radiation loss
      es=ebeam
      call stragl(es,targs)


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


         epk=delta+(erng-delta)*ran(idum)
         ep=ep0-epk
         if (ep .lt. epmin0)go to 20
         pp=ep-0.5*mel**2/ep
         jacob=jacob*((1.-delta/erng)/(1.-egdiv))
      den=sqrt(ps**2+pp**2-2.*ps*pp*cst0)
      cstk1=(ps-pp*cst0)/den
      cstk2=(ps*cst0-pp)/den

      csrnge=csrng
      if ((1.-cstk1) .lt. csrnge)csrnge=1.-cstk1
      if (abs(cstk1- cstk2).lt.2.*csrnge)csrnge=abs(cstk1-cstk2)/2.
      csrngb=csrng/40.
      if (csrngb .gt. csrnge/5.)csrngb=csrnge/5.
      cran=ran(idum)
      cran2=ran(idum)
      if (cran2 .lt. .5)then
         cran2=-1.
      else
         cran2=1.
      endif
      if (cran .lt. cdiv1)then
         cstk=cstk1+csrngb*(2.*ran(idum)-1.)
         mcfac=csrngb/cdiv1
         phik=(ran(idum)-.5)*pi/9.
         mpfac=1/18.
      elseif(cran .lt. cdiv2)then
         cstk=cstk1+cran2*(csrngb+(csrnge-csrngb)*ran(idum))
         mcfac=(csrnge-csrngb)/(cdiv2-cdiv1)
         phik=(ran(idum)-.5)*pi/9.
         mpfac=1/18.
      elseif (cran .lt. cdiv3)then
         cstk=cstk2+csrngb*(2.*ran(idum)-1.)
         mcfac=csrngb/(cdiv3-cdiv2)
         phik=(ran(idum)-.5)*pi/9.
         mpfac=1/18.
      elseif(cran .lt. cdiv4)then
         cstk=cstk2+cran2*(csrngb+(csrnge-csrngb)*ran(idum))
         mcfac=(csrnge-csrngb)/(cdiv4-cdiv3)
         phik=(ran(idum)-.5)*pi/9.
         mpfac=1/18.
      else
 44      cstk=2.*ran(idum)-1.
         phik=2.*pi*(ran(idum)-0.5)
         if (abs(cstk-cstk1) .lt. csrnge
     +    .or. abs(cstk-cstk2) .lt. csrnge)then
            if (abs(phik) .lt. pi/18.)go to 44
         endif
         mcfac=(1.-2.*csrnge)/(1.-cdiv4)
         mpfac=17./18.
      endif

      Tk=acos(cstk)

      call elast_rad(es,ep,th0,cstk,phik,egam,sigr1)
      sigr1=sigr1*jacob
      if (sigr1 .lt. 0.)sigr1=0.
c     Normalize sigr according to the integration region.

      sigr1=mpfac*mcfac*sigr1
c     Compare the cross section to the maximum value
      if (sigr1 .gt. sigr_max)sigr_max=sigr1
      if (ntries .lt. 50000)go to 20
      write(6,*)' sigr_max =',sigr_max

      return
      end

c  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      subroutine elas_radcor(es,delta)
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
      common /stdpar/mel,mp,pi,alpha
      common /rad_cor/del_radcor
      real del_radcor(400)
      real mel,mp,pi,alpha

      real*8 spence,arg
      real es,theta_d,theta,delta
      real cst1,eta,eel,qs
      real znuc,deltac(27),del_mo,idel
      real arg11,arg15,arg19,arg23
      real epr,e1,e3,e4,gamma4,beta4
      real snth,s2
      integer iq


c     Calculate correction for hydrogen or deuterium elastic scattering
      znuc=1.

c     calculate b for hydrogen
c      z=1
c      b=bfunc(z)
c      ' Input thickness, rad. leng.'
c      t=0.
      do iq=1,400
         del_radcor(iq)=0.
      enddo
      do iq=1,400
         qs=iq
         qs=qs/50.
         s2=qs/4./es/(es-qs/2./mp)
         s2=sqrt(s2)
         theta=2.*asin(s2)
      theta_d=180*theta/pi
c      if (theta_d .gt. 90.)return

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
      deltac(1)=28./9.-13./6.*alog(qs/mel**2)
      deltac(2)= (alog(qs/mel**2) - 1. + 2.*alog(eta) )
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
      del_radcor(iq)=1.+del_mo
      enddo

 99   return
      end

