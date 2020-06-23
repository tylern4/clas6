      subroutine kine_smear_vx(vx_offset)
c_begin_doc
c
c  Documentation for subroutine kine_smear_vx
c    incl.           subroutine brems_angle
c                    subroutine sspline, ssplint
c
c  Purpose:
c  --------
c     smear vertex position (-> vx_offset(1:3)) depending on 
c     (a) ffread card entries:
c               beam_offset_ff(1) [offset in x], 
c               beam_offset_ff(2) [offset in y],
c               beam_sigma_ff     [beam width, cf. emittance at target]
c     (b) target length      (for smearing in z dir.)
c     (c) type of projectile (special treatment for photon beam data)
c
c  I/O parameters:
c  --------------
c     output:  vx_offset(1:3)  - smeared values (vertex offset in x,y,z dir.) 
c
c  Author:
c  -------
c     F.Klein  July'99
c     D.Sober  Feb.'98   (routine brem_angle.F)
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: kine_smear_vx.F,v $')
      parameter (crevis= '$Revision: 1.6 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/08/14 22:20:51 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'KINE_SMEAR_VX')
      parameter (crauth= 'Franz Klein')
      data crcsid/'
     1$Id: kine_smear_vx.F,v 1.6 2009/08/14 22:20:51 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "gcnum.inc"
#include "wcs.inc"
#include "gcflag.inc"
#include "gconst.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar.inc"
#include "ffpar_tg.inc"

c  i/o parameters:
c  ---------------
      real  vx_offset(3)        !output: offset of vertex position

c  local variables:
c  ----------------
      character*80 crmess
      integer jmchd,jtagr,mlink
      integer jmctk,nrow_mctk,ncol_mctk,ind,it
      integer ivol
      real    ggauss, rnd(2)
      logical first_time, Ephot_flag,valid_sigma
      data first_time /.true./

      real angle_rat, max_angle_rat, char_angle, k_by_E0, r, Rmax, beam2tg_off
      real zcoll, ztarg, colldiam
      data zcoll/580.0/          ! radiator to collimator (cm)
      data ztarg/2198.0/         ! collimator to target (cm)
      data colldiam/0.861/     ! collimator diameter (cm)
*  NOTE: collimation angle can be changed for 
*        smaller collimators (0.20cm, 0.26cm, 0.46cm, etc) 
*        via ffread card "SIGBEAM -0.20"  etc (neg.sign indicates to use incoherent 
*        bremsstrahlung formula, not a common fixed value for sigma_beam)
*
c_end_var


c  executable code:
c  ----------------

*  check whether option 'smear_vx' is chosen;
*  check beam offset wrt. target parameters 

      if(first_time)then
        first_time=.false.
        call GLOOK('TARG',IQ(jvolum+1), NVOLUM, ivol)
        if(ivol.LE.0) then
          call recmes(CRNAME,'W','NO vertex smearing because TARGET geometry not defined')
          beam_sigma_ff=0.0
          Ephot_flag =.FALSE.
        else
          Ephot_flag=.NOT.nogeom(st).AND.Beam_energy_ff(1).GT.Beam_energy_ff(2)
          valid_sigma= Beam_sigma_ff.GT.0.0 .AND. Beam_sigma_ff.LT.10.
          if(Beam_offset_ff(1).NE.0.0 .OR. Beam_offset_ff(2).NE.0.0) then
            write(crmess,'(A,2F8.3)') 'Apply Vertex correction:  Beam offsets (x,y):',Beam_offset_ff
            call recmes(CRNAME,'I',crmess)
          endif
          beam2tg_off=SQRT((beam_offset_ff(1)-tg_pos_ff(1))**2+
     &                   (beam_offset_ff(2)-tg_pos_ff(2))**2)
          if(beam2tg_off.GT.tgpar_save(2)) then
            call recmes(CRNAME,'W','Beam offsets are set too large: you miss the target!!')
            beam2tg_off   =tgpar_save(2)
          endif
        endif
        if(Beam_sigma_ff.NE.0.0) then
          if(valid_sigma) then
            write(crmess,'(A,F10.4)') 'Smear Vertex position:  Sigma=',Beam_sigma_ff
            call recmes(CRNAME,'I',crmess)
          elseif(Ephot_flag) then
            call recmes(CRNAME,'I','Smear Vertex position according Bremsstrahlung distribution')
            if(Beam_sigma_ff.GT.-1.0.AND.Beam_sigma_ff.LE.-0.2) colldiam=-Beam_sigma_ff
            write(crmess,'(A,F6.4)')' ... for collimator diameter ',colldiam
            call recmes(CRNAME,'I',crmess)
           endif
        endif
        
        if(Ephot_flag) then
           char_angle = 0.511E-3/beam_energy_ff(1)
           Rmax       = 0.5*colldiam/zcoll*ztarg
*    max_angle_rat (ratio) =  max.angle  / characteristic angle
           max_angle_rat =  Rmax/ztarg  / char_angle
           Rmax = MIN(Rmax,(1.1*tgpar_save(2)-beam2tg_off))
        endif
      endif

*  initialize output parameters
      vx_offset(1)=beam_offset_ff(1)
      vx_offset(2)=beam_offset_ff(2)
      vx_offset(3)=0.
************************************************************
*
*  stop here if option 'smear_vx' not chosen
*
      if(beam_sigma_ff.EQ.0.0) return
*
************************************************************

*  get info from MC HeaD bank (if availale)
#define MCHD_Q2      rjw(jmchd+8)
#define MCHD_EPHOT   rjw(jmchd+9)

      jmchd=mlink(jw,'MCHD',0)
      if(jmchd.GT.0) then
         if(beam_energy_ff(1).LE.MCHD_EPHOT) jmchd=0
      else
*  or check for a beam photon (start vertex<=0 && ending vertex#>0)
         jmctk=mlink(jw,'MCTK',0)
         if(jmctk.gt.0) then
            nrow_mctk=jw(jmctk-4)
            ncol_mctk=jw(jmctk-5)
            do it=1,nrow_mctk
               ind=jmctk+(it-1)*ncol_mctk
               if(jw(ind+7).EQ.22.AND.jw(ind+9).LE.0.AND.jw(ind+10).GT.0) then 
                  beam_energy_ff(2) = rjw(ind+4)
               endif
            enddo
         else
            jtagr=mlink(jw,'TAGR',0)
            if(jtagr.gt.0) then
               beam_energy_ff(2) = rjw(jtagr+1)
            endif
         endif
      endif


*************************************************************
*
*  smear vertex position  (in z: flat distribution over target length)
*  position vertex with UPSTPOS + TGPOS ffread cards (s.morrow)
      call grndm(rnd,2)
* z_offset=z_off.of scatt.chamber+upstream end of tg.cell+tg.length*random
      vx_offset(3)=tgpar_save(12)+tgpar_save(7)+tgpar_save(1)*rnd(1)

*  for transversal smearing: different procedures for electron/photon beam
*    (steered by ffread card: Beam_sigma_ff (-> put to nonsense value))

      k_by_e0=-1.
      if(.NOT.valid_sigma) then
         if(jmchd.GT.0 .AND. MCHD_Q2.EQ.0.0) then 
            k_by_e0 = MCHD_EPHOT / beam_energy_ff(1) 
         elseif(Ephot_flag) then
            k_by_e0 = beam_energy_ff(2) / beam_energy_ff(1) 
         endif
      endif

*  (a) photon beam:

      if(k_by_e0.GE.0) then
*  throw photon angle and test whether generated angle is outside collimator:
        angle_rat=100.
        r=100.
        do while (angle_rat.gt.max_angle_rat .OR. r.GT.Rmax)
           call brem_angle(k_by_e0,angle_rat)
           r = angle_rat*char_angle*ztarg      ! radius at target
        enddo

*  (b) electron beam

      elseif(valid_sigma) then
         r = ggauss(0.0,Beam_sigma_ff)

*  (c) no valid beam sigma value: only smear z-position
      else
         return
      endif
*
      vx_offset(1) = vx_offset(1) + r * COS(TWOPI*rnd(2))
      vx_offset(2) = vx_offset(2) + r * SIN(TWOPI*rnd(2))

      end

*********************************************************************

      subroutine brem_angle(kE0,ang)

      implicit none

      real kE0, ang

c     D. Sober  17-Feb-98
c     Generate bremsstrahlung angular distribution for Monte Carlo use.
c     Input:
c           kE0 = k/E0
c              Optional:  if kE0=0, use ang. dist. for k/E0=0.80
c              Otherwise, scale the output angle to put the peak at the
c              correct angle, using an approximate correction factor.
c     Output:
c           ang = brems. photon angle in units of Theta_crit = m/E0
c     Subprograms required (from Numerical Recipes):
c           subroutine SSPLINE():    set up cubic spline
c           subroutine SSPLINT():    interpolate using cubic spline

c     Algorithm:
c      Use the combined composition-rejection method with different
c      composition functions in three different angular regions.
c      For a general reference on Monte Carlo sampling, see "The EGS4
c      Code System", SLAC Report 265 (1985), Section 2.2 (but none of the
c      physics in this routine comes from EGS4).

c      (Definition:  URV = uniform random variate between 0 and 1)
c      1.  Choose one of three regions according to whether URV<prob1,
c           prob1<URV<prob2, URV>prob2.
c      2.  If region 1:  (0 < ang < ang1) dsigma/dtheta = const.*ang
c               Let ang = SQRT(URV/ang1)
c          If region 2:  (ang1 < ang < ang2) dsigma/dtheta = const.
c               Let ang = ang1+(ang2-ang1)*URV
c          If region 3:  (ang > ang2)  dsigma/dtheta = const./ang**b
c               Let ang = ang2/(URV**(1/(b-1))
c      3.  Calculate ds1 = true dsigma/dtheta(ang) by spline interpolation
c           in table of precisely calculated values.
c      4.  Calculate ds2 = dsigma/dtheta of generated function at ang.
c      5.  Generate URV.
c           If URV < ds1/ds2, keep event and exit.
c           If URV > ds1/ds2, go back to step 1.

c     CAUTION:
c     All the numbers in the following statements are closely
c     interrelated.  DO NOT MODIFY any constants unless you know
c     exactly what you are doing.
c     Data for k/E0 = 0.80:

      real expon, bb, ang1, ang2, prob1, prob2, param1, param2, param3
      parameter (expon=2.45, bb=1./(expon-1.))  ! exponent in region 3
      parameter (ang1=0.3386, ang2=1.4457)      ! region boundaries
      parameter (prob1=.07446, prob2=0.56144)   ! prob. of region 1, region 1+2
      parameter (param1=1.193, param2=.404, param3=.9967) ! mult. factors

      character*80 crmess
      real rnd(3)
      real test_cs, real_cs, ratio
      integer ncsdat, ncall
      real  dsig(50),theta(50),yderiv(50)
c     dsigma/dtheta calculated by integration of
c      dsigma/dk/dOmega_e over cone of photon angles, using RHF screening.
c     Data for k/E0 = 0.80:
      data ncall /0/
      data ncsdat /39/
      data dsig/0.,
     *   1.19312E-01,2.25025E-01,3.07579E-01,3.63348E-01,3.93936E-01,
     *   4.04004E-01,3.99120E-01,3.84348E-01,3.63687E-01,3.40013E-01,
     *   2.90743E-01,2.44815E-01,2.05011E-01,1.71657E-01,1.44115E-01,
     *   9.50394E-02,6.49017E-02,4.58483E-02,3.33839E-02,2.49504E-02,
     *   1.90740E-02,1.17942E-02,7.74643E-03,5.33807E-03,3.82376E-03,
     *   2.82740E-03,1.47914E-03,8.65516E-04,5.48663E-04,3.69033E-04,
     *   1.89798E-04,1.10126E-04,6.94481E-05,4.65676E-05,2.38725E-05,
     *   1.38300E-05,8.71950E-06,5.84968E-06,11*0./
      data theta/
     *  0., .1,.2,.3,.4,.5, .6,.7,.8,.9,1.0, 1.2,1.4,1.6,1.8,2.0,
     *  2.5,3.0,3.5,4.0,4.5,  5.0,6.0,7.0,8.0,9.0,
     *  10.0,12.5,15.0,17.5,20.0, 25.0,30.0,35.0,40.0,50.0,
     *  60.0,70.0,80.0,11*0./

      if(ncall.eq.0) then
c      Set up for cubic spline interpolation (Numerical Recipes routine):
       call SSPLINE(theta,dsig,ncsdat,1.e30,1.e30,yderiv)
       ncall=1
       call recmes('KINE_SMEAR_VX','I','Routine BREM_ANGLE initialized')
      end if

10    call grndm(rnd,3)
      if(rnd(1).le.prob1) then       ! region 1, ang < ang1
       ang=ang1*SQRT(rnd(2))
       test_cs=param1*ang    ! linear
      elseif(rnd(1).le.prob2) then   ! region 2, ang1 < ang < ang2
       ang=ang1+(ang2-ang1)*rnd(2)
       test_cs=param2        ! constant
      else                       ! region 3, ang > ang1
       ang=ang2/rnd(2)**bb
       test_cs=param3/(ang**expon)
      end if

c     Rejection testing:  compare test_cs with real_cs
c     Find real_cs by spline interpolation (Numerical Recipes routine):
      call SSPLINT(theta,dsig,yderiv,ncsdat,ang,real_cs)
      ratio=real_cs/test_cs
      if(ratio.gt.1.03) then
        write(crmess,'(A,3F10.6)') 'Error in brem_ang:',ang,real_cs,test_cs
        call recmes('KINE_SMEAR_VX','W',crmess)
      end if
      if(rnd(3).gt.ratio) goto 10
      if(kE0.gt.0. .and. kE0.lt.1.) then   ! Scale the angle so that peak
        ang=ang*(1.+0.26*(kE0-0.80))       ! is approximately in the right place
      end if
      return
      end

C  (C) Copr. 1986-92 Numerical Recipes Software $=)J31A2.
      SUBROUTINE ssplint(xa,ya,y2a,n,x,y)
      implicit none
      integer   n,nmax
      PARAMETER (NMAX=500)
      REAL      x,y,xa(NMAX),y2a(NMAX),ya(NMAX) 
      INTEGER k,khi,klo
      REAL    a,b,h
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=xa(khi)-xa(klo)
      if (h.eq.0.) pause 'bad xa input in ssplint'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software $=)J31A2.
      SUBROUTINE sspline(x,y,n,yp1,ypn,y2)
      implicit none
      INTEGER   n,NMAX
      PARAMETER (NMAX=500)
      REAL      yp1,ypn,x(NMAX),y(NMAX),y2(NMAX)
      INTEGER  i,k
      REAL     p,qn,sig,un,u(NMAX)

      if (yp1.gt..99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do 11 i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/
     &                  (x(i+1)-x(i-1))-sig*u(i-1))/p
11    continue
      if (ypn.gt..99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
12    continue
      return
      END







