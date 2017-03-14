c     This function choose one of the momentum corrections based on 
c     particle mass and kinematics
c
c     Inputs:
c        torus  : integer, torus current
c        ipid   : integer, particle id (in PDG definition)
c                  = 211 : pi+
c                  =-211 : pi-
c                  = 321 : K+
c                  =-321 : K-
c                  =2212 : p
c        Pm     : real,    momentum before correction
c        theta  : real,    polar angle
c     Outputs(return value)
c        ifunc   : integer, choice of correction function
c                  < 0 : error
c                  = 1 : pppm MM correction
c                  = 2 : pppm TM correction
c                  = 3 : K0   NB correction
c
c
c     author T. Mibe
      integer function choosepcor(torus,ipid,Pm,theta)

      implicit none 
      
      integer torus, ipid
      real    Pm, theta, thave
      integer ifunc, i, ith

      integer  pidpip, pidpim, pidkp, pidkm, pidpr
      parameter (pidpip= 211)
      parameter (pidpim=-211)
      parameter (pidkp = 321)
      parameter (pidkm =-321)
      parameter (pidpr =2212)


      ifunc= -1

c     Check theta bin
      do i=0,22
         thave = 12+8*(i-1)
         if (abs(theta-thave).le.4) then
            ith = i
         endif
      enddo
      if (ith.gt.15) ith = 15   ! if theta>128 deg, set theta=124 deg.
      if (ith.eq. 0) ith =  1   ! if theta<8 deg, set theta=12 deg.

      
c     pi+ corrections
      if (ipid.eq.pidpip) then       ! pi+  (NB)
         ifunc = 3
      else if (ipid.eq.pidpim) then  ! pi-  (TM)
         ifunc = 2
      else if (ipid.eq.pidkp) then   ! K+   (NB)
         ifunc = 3
      else if (ipid.eq.pidkm) then   ! K-   (TM)
         ifunc = 2
      else if (ipid.eq.pidpr) then   ! proton (TM)
         ifunc = 2
      else
         write(*,*)'choosepcor: unknown particle id', ipid
         ifunc = -2
      endif

      choosepcor = ifunc
      return
      end
c     G10 momentum correction
c
c     Status: debug
c     Author: T. Mibe, N. Baltzell, M. Mirazita
c
c     Inputs:
c        torus  : integer, torus current (Ampere)
c        ipid   : integer, particle id (in PDG definition)
c                  = 211 : pi+
c                  =-211 : pi-
c                  = 321 : K+
c                  =-321 : K-
c                  =2212 : p
c        pold   : real,  3 momentum before correction
c        icor   : choice of correction functions
c                  = 1 : pppm MM correction
c                  = 2 : pppm TM correction
c                  = 3 : K0   NB correction
c                  = 4 : Combined correction (currently, 1+3)
c     Outputs
c        pnew   : real, 3 momentum after correction
c        istat  : integer,
c                  < 0 : error
c                  = 1 : correction was measured
c                  = 2 : correction was not measured, extrapolated
c
      subroutine g10pcor(torus,ipid,pold,icor,pnew,istat)

      implicit none
      integer  torus, ipid, icor, istat, ifunc
      real     pold(3), pnew(3)
      integer  ifield, icharge
      real     Pm, phi, theta, Pc, Ec, deg2rad, bfactor
      integer  choosepcor
      parameter(deg2rad=3.1415927/180.)

      integer  lo_curr,hi_curr
      data     lo_curr,hi_curr/2250,3375/
      integer  pidpip, pidpim, pidkp, pidkm, pidpr
      parameter (pidpip= 211)
      parameter (pidpim=-211)
      parameter (pidkp = 321)
      parameter (pidkm =-321)
      parameter (pidpr =2212)

      istat   = 0
      pnew(1) = 0.
      pnew(2) = 0.
      pnew(3) = 0.
      if (torus.lt.0) then
         write(*,*)'g10pcor:ERROR: invalid torus current', torus
         istat = -1
         return
      endif
      if ((icor.lt.0).or.(icor.gt.4)) then
         write(*,*)'g10pcor:ERROR: invalid switch', icor
         istat = -3
         return
      endif

c *** Get particle charge
      if ((ipid.eq.pidpip).or.(ipid.eq.pidkp).or.(ipid.eq.pidpr)) then
         icharge = 1
      else if ((ipid.eq.pidpim).or.(ipid.eq.pidkm)) then
         icharge =-1
      else
         write(*,*)'g10pcor:ERROR: unknown particle ID', ipid
         istat = -2
         return
      endif
      Pm    = sqrt(pold(1)**2+pold(2)**2+pold(3)**2)
      theta = acos(pold(3)/Pm) /deg2rad
      phi   = atan2(pold(2),pold(1))/deg2rad
      if(phi.lt.-30) phi = phi+360.

c *** Choose correction function
      if (icor.eq.4) then 
         ifunc = choosepcor(torus,ipid,Pm,theta)
      else
         ifunc = icor
      endif

C *** Scaling factor based on torus current and momentum
      if ((ifunc.eq.1).or.(ifunc.eq.2)) then
         if (torus.eq.2250) then
            ifield = 1
            bfactor= 1.0
         else if (torus.eq.3375) then
            ifield = 2
            bfactor= 1.0
         else
            if (abs(torus-lo_curr).lt.abs(torus-hi_curr)) then
               ifield=1
               bfactor= float(lo_curr)/float(torus)
            else
               ifield=2
               bfactor= float(hi_curr)/float(torus)
            endif
         endif
      else if (ifunc.eq.3) then
         ifield = 1
         if (torus.eq.2250) then
            bfactor = 1.0
         else
            bfactor = float(lo_curr)/float(torus)
         endif
      else
         write(*,*)'g10pcor:ERROR: unknown function ', ifunc
         istat = -2
         return
      endif

      Pm = Pm*bfactor

      if (ifunc.eq.1) then       ! MM's correction
         call pcorpppm_mm(ifield,icharge,Pm,theta,phi,Pc,Ec,istat)
      else if (ifunc.eq.2) then  ! TM's correction
         call pcorpppm_tm(ifield,icharge,Pm,theta,phi,Pc,Ec,istat)
      else if (ifunc.eq.3) then  ! NB's correction
         call pcork0_nb(ifield,icharge,Pm,theta,phi,Pc,Ec,istat)
      endif

      pnew(1) = Pc*sin(theta*deg2rad)*cos(phi*deg2rad)/bfactor
      pnew(2) = Pc*sin(theta*deg2rad)*sin(phi*deg2rad)/bfactor
      pnew(3) = Pc*cos(theta*deg2rad)                 /bfactor

      return
      end
c     Nathan Baltzell's  momentum correction based on Ks0-->pi+pi-
c     Inputs:
c        ifield : integer, torus field setting
c                  = 1 : 2250A
c                  = 2 : 3375A
c        icharge: integer, charge of particle 
c        p      : real,    momentum before correction
c        theta  : real,    polar angle
c        phi    : real,    azimuthal angle (-30~330 degree)
c                  sector 1 (-30, 30)
c                  sector 2 ( 30, 90)
c                  sector 3 ( 90,150)
c                  sector 4 (150,210)
c                  sector 5 (210,270)
c                  sector 6 (270,330)
c     Outputs
c        Pc     : real,  momentum after correction
c        Ec     : real,  error of the correction
c        istat  : integer,
c                  < 0 : error
c                  = 1 : correction was measured
c                  = 2 : correction was not measured
c
c     author T. Mibe
      subroutine pcork0_nb(ifield,icharge,Pm,theta,phi,Pc,Ec,istat)

      implicit none 
      
      integer ifield, icharge, istat
      real    Pm, theta, phi, Pc, Ec

      real    px0, py0, pz0, PI, rphi
      integer igood, q0, itheta, isector, i
      real    pcork0s
      
      real    epip(4,6), epim(4,6)
c pi+ Theta 1
      epip(1,1)= 0.000692194497933023
      epip(1,2)= 0.00130503503913463
      epip(1,3)= 0.00093194418635855
      epip(1,4)= 0.000697155987183374
      epip(1,5)= 0.000640682271572901
      epip(1,6)= 0.000857314190313181
c pi+ Theta 2
      epip(2,1)= 0.00112706694541218
      epip(2,2)= 0.000769777331915447
      epip(2,3)= 0.000990848805007076
      epip(2,4)= 0.000730227154003769
      epip(2,5)= 0.000711158670668274
      epip(2,6)= 0.00078346102576569
c pi+ Theta 3
      epip(3,1)= 0.000747207973700763
      epip(3,2)= 0.000787084456522932
      epip(3,3)= 0.00119248543302675
      epip(3,4)= 0.000612970363904325
      epip(3,5)= 0.000678904249510194
      epip(3,6)= 0.00107483592070837
c pi+ Theta 4
      epip(4,1)= 0.00111618176973197
      epip(4,2)= 0.00129513460914802
      epip(4,3)= 0.00130617607472498
      epip(4,4)= 0.000968539962733598
      epip(4,5)= 0.00122297772713652
      epip(4,6)= 0.00105720912508973

c pi- Theta 1
      epim(1,1)= 0.00116686006727139
      epim(1,2)= 0.000757229177876899
      epim(1,3)= 0.00120286788525549
      epim(1,4)= 0.000739405894828958
      epim(1,5)= 0.000726169059419232
      epim(1,6)= 0.00124064308924002
c pi- Theta 2
      epim(2,1)= 0.000544558561837304
      epim(2,2)= 0.000840687138009625
      epim(2,3)= 0.000922786236334903
      epim(2,4)= 0.000504194109331292
      epim(2,5)= 0.00100750424907476
      epim(2,6)= 0.000905156624187149
c pi- Theta 3
      epim(3,1)= 0.000875194653911346
      epim(3,2)= 0.000925920025523263
      epim(3,3)= 0.000769300457538624
      epim(3,4)= 0.000754615538437952
      epim(3,5)= 0.000832068892032384
      epim(3,6)= 0.0
c pi- Theta 4
      epim(4,1)= 0.00119814745509083
      epim(4,2)= 0.00117076899650601
      epim(4,3)= 0.00181808312992094
      epim(4,4)= 0.00109672567282551
      epim(4,5)= 0.00165866168066588
      epim(4,6)= 0.00136647765171592

c     initializaion
      Pc   = -1000
      Ec   = 0
      istat= -1
      igood= -1
      itheta=-1
c
      PI = acos(-1.)
      px0= Pm*sin(theta*PI/180.)*cos(phi*PI/180.)
      py0= Pm*sin(theta*PI/180.)*sin(phi*PI/180.)
      pz0= Pm*cos(theta*PI/180.)

c     Low field only
      if (ifield.eq.2) return

c     sector
      isector = -1
      do i=1,6
         if ((phi.ge.-30+60*(i-1)).and.(phi.lt.-30+60*(i))) then
            isector= i
            rphi   = phi - 60*(i-1)
         endif
      enddo
      if (isector.lt.0) return

c     pi- corrections
      if (icharge.eq.-1) then
         if (theta.le.15) then
         else if (theta.le.33) then
            itheta=1
            if ((Pm.ge.0.38).and.(Pm.le.1.0)) igood = 1
         else if (theta.le.48) then
            itheta=2
            if ((Pm.ge.0.21).and.(Pm.le.0.8)) igood = 1
         else if (theta.le.81) then
            itheta=3
            if ((Pm.ge.0.14).and.(Pm.le.0.6)) igood = 1
         else if (theta.le.120) then
            itheta=4
            if ((Pm.ge.0.10).and.(Pm.le.0.4)) igood = 1
         endif
         if (itheta.gt.0) then
            Ec = epip(itheta,isector)
         endif
      else if (icharge.eq.+1) then
         if (theta.le.6) then
         else if (theta.le.18) then
            itheta=1
            if ((Pm.ge.0.15).and.(Pm.le.1.2)) igood = 1
         else if (theta.le.30) then
            itheta=2
            if ((Pm.ge.0.14).and.(Pm.le.1.0)) igood = 1
         else if (theta.le.54) then
            itheta=3
            if ((Pm.ge.0.10).and.(Pm.le.0.7)) igood = 1
         else if (theta.le.120) then
            itheta=4
            if ((Pm.ge.0.08).and.(Pm.le.0.4)) igood = 1
         endif
         if (itheta.gt.0) then
            Ec = epim(itheta,isector)
         endif
      else
         write(*,*)'pcork0_nb: unknown particle id', icharge
         istat = -2

      endif

      Pc = pcorK0s(icharge,px0,py0,pz0)

      if (igood.eq.1) then
         istat = 1
      else
         istat = 2
      endif

      return
      end



c     author N. Baltzell
      REAL FUNCTION PCORK0S(q,px0,py0,pz0)

      integer q,ithe,isec
      real px0,py0,pz0,p1,p0,the,phi

c     linear phi dependence - 4 theta bins and 6 sectors
      real aparm(6,2,4) ! -
      real aparp(6,2,4) ! +
c     quadratic global momentum dependence
      real pparm(3) ! -
      real pparp(3) ! +

      p0 = sqrt( px0**2 + py0**2 + pz0**2 )
      p1 = p0
      PCORK0S = p0 ! input momentum

c First, find possible errors -> return input momentum
      if( q.ne.-1.and.q.ne.1 ) return  ! no correction
      the = acos(pz0/p0) *180./3.141592
      phi = atan(py0/px0)*180./3.141592
      if(px0.lt.0) phi = phi+180.
      if(phi.lt.-30) phi = phi+360.
      if(phi.gt.330) phi = phi-360.
      if(the.gt.180.or.the.lt.0)   return ! no correction
      if(phi.gt.330.or.phi.lt.-30) return ! no correction

c MOMENTUM DEPENDENCE PARAMETERS
      pparm(1) =  0.001309
      pparm(2) = -0.006117
      pparm(3) =  0.007155
      pparp(1) =  0.002436
      pparp(2) = -0.011655
      pparp(3) =  0.012140

c PHI DEPENDENCE PARAMETERS
c 0 < theta < 33 (-)
      aparm(1,1,1) = -0.0028709
      aparm(2,1,1) = -0.00204849
      aparm(3,1,1) = -0.00124416
      aparm(4,1,1) = -0.0006538
      aparm(5,1,1) = -0.00245588
      aparm(6,1,1) = -0.00306787
      aparm(1,2,1) =  5.92131e-06
      aparm(2,2,1) = -0.000139486
      aparm(3,2,1) =  1.72194e-05
      aparm(4,2,1) = -8.59115e-05
      aparm(5,2,1) = -0.000126681
      aparm(6,2,1) = -4.95702e-06
c 33 < theta < 48 (-)
      aparm(1,1,2) = -0.00221767
      aparm(2,1,2) =  0.000554396
      aparm(3,1,2) =  0.000201387
      aparm(4,1,2) = -0.00115231
      aparm(5,1,2) = -0.00198496
      aparm(6,1,2) = -0.00328397
      aparm(1,2,2) = -7.72867e-05
      aparm(2,2,2) = -0.00016143
      aparm(3,2,2) = -6.75647e-05
      aparm(4,2,2) = -0.000164746
      aparm(5,2,2) = -0.000207807
      aparm(6,2,2) = -9.8782e-05
c 48 < theta < 81 (-)
      aparm(1,1,3) =  1.15233e-06
      aparm(2,1,3) =  0.00251902
      aparm(3,1,3) =  0.00103878
      aparm(4,1,3) =  0.000621699
      aparm(5,1,3) =  0.000694473
      aparm(6,1,3) = -0.00075916
      aparm(1,2,3) = -0.000233629
      aparm(2,2,3) = -0.000333463
      aparm(3,2,3) = -0.000426809
      aparm(4,2,3) = -0.000378046
      aparm(5,2,3) = -0.000292799
      aparm(6,2,3) = -0.000280189
c 81 < theta < 180 (-)
      aparm(1,1,4) =  0.00253522
      aparm(2,1,4) =  0.0102952
      aparm(3,1,4) =  0.00444268
      aparm(4,1,4) =  0.00428438
      aparm(5,1,4) =  0.00567705
      aparm(6,1,4) =  0.0015116
      aparm(1,2,4) = -0.000460878
      aparm(2,2,4) = -0.000407892
      aparm(3,2,4) = -0.000363258
      aparm(4,2,4) = -0.000392865
      aparm(5,2,4) = -0.000409297
      aparm(6,2,4) = -0.00036871
c 0 < theta < 18 (+)
      aparp(1,1,1) =  0.00121251
      aparp(2,1,1) = -0.00417251
      aparp(3,1,1) = -0.00278882
      aparp(4,1,1) =  0.00157576
      aparp(5,1,1) =  0.000771925
      aparp(6,1,1) =  0.00207998
      aparp(1,2,1) = -2.45327e-05
      aparp(2,2,1) = -4.98219e-05
      aparp(3,2,1) = -0.000167266
      aparp(4,2,1) = -0.000220149
      aparp(5,2,1) = -0.000159886
      aparp(6,2,1) = -0.000293169
c 18 < theta < 30 (+)
      aparp(1,1,2) =  0.000324894
      aparp(2,1,2) = -0.00362691
      aparp(3,1,2) = -0.00191738
      aparp(4,1,2) = -0.000435924
      aparp(5,1,2) = -0.00199774
      aparp(6,1,2) =  5.49575e-05
      aparp(1,2,2) = -0.000191954
      aparp(2,2,2) = -0.000170692
      aparp(3,2,2) = -0.000214619
      aparp(4,2,2) = -0.000288746
      aparp(5,2,2) = -0.000154779
      aparp(6,2,2) = -0.000246275
c 30 < theta < 54 (+)
      aparp(1,1,3) =  0.000297495
      aparp(2,1,3) = -0.00113373
      aparp(3,1,3) = -0.000580969
      aparp(4,1,3) = -0.000976977
      aparp(5,1,3) = -0.00179485
      aparp(6,1,3) = -0.00139143
      aparp(1,2,3) = -0.000306386
      aparp(2,2,3) = -0.000310556
      aparp(3,2,3) = -0.000330488
      aparp(4,2,3) = -0.000524708
      aparp(5,2,3) = -0.000381546
      aparp(6,2,3) = -0.000427089
c 54 < theta < 180 (+)
      aparp(1,1,4) =  0.00317386
      aparp(2,1,4) =  0.000838974
      aparp(3,1,4) =  0.00443875
      aparp(4,1,4) =  0.000553896
      aparp(5,1,4) =  0.000758088
      aparp(6,1,4) =  0.00186038
      aparp(1,2,4) = -0.000201917
      aparp(2,2,4) = -0.00040742
      aparp(3,2,4) = -0.000389817
      aparp(4,2,4) = -0.000583283
      aparp(5,2,4) = -0.000372777
      aparp(6,2,4) = -0.000376769

c     get sector
      if(phi.lt.30 ) then 
         isec = 1
      elseif(phi.lt. 90) then
         isec = 2
      elseif(phi.lt.150) then
         isec = 3
      elseif(phi.lt.210) then
         isec = 4
      elseif(phi.lt.270) then
         isec = 5
      else
         isec = 6
      endif

      if(q.eq.1) then
c     POSITIVE CHARGE CORRECTION

c        find theta bin
         if(the.le.18) then
            ithe=1
         elseif(the.le.30) then
            ithe=2
         elseif(the.le.54) then
            ithe=3
         else
            ithe=4
         endif

         p1 = p0
c        phi dependence ( deltaP/P = linear in phi )
         p1 = p1 + p0*aparp(isec,1,ithe)
         p1 = p1 + p0*aparp(isec,2,ithe)*(phi-60.*(isec-1))

         p0 = p1
c        momentum dependence ( deltaP = quadratic in momentum )
         if( p0.gt.0.5 ) p0=0.5 ! constant correction above 500 MeV/c
         p1 = p1 + pparp(1) + pparp(2)*p0 + pparp(3)*p0*p0

      elseif(q.eq.-1) then
c     NEGATIVE CHARGE CORRECTION

c        find theta bin
         if(the.le.33) then
            ithe=1
         elseif(the.le.48) then
            ithe=2
         elseif(the.le.81) then
            ithe=3
         else
            ithe=4
         endif

         p1 = p0
c        phi dependence ( deltaP/P = linear in phi )
         p1 = p1 + p0*aparm(isec,1,ithe)
         p1 = p1 + p0*aparm(isec,2,ithe)*(phi-60.*(isec-1))

         p0 = p1
c        momentum dependence ( deltaP = quadratic in momentum )
         if( p0.gt.0.5 ) p0=0.5 ! constant correction above 500 MeV/c
         p1 = p1 + pparm(1) + pparm(2)*p0 + pparm(3)*p0*p0

      endif

      PCORK0S = p1

      return
      end
      subroutine pcorpppm_mm(ifield,icharge,Pm,theta,phi,Pc,Ec,istat)

      implicit none 

      integer  ifield, icharge, icor, istat, ifunc
      real     Pm, theta, phi, Pc, Ec

      real pig,deg2rad
      parameter(pig=3.1415927)
      parameter(deg2rad=pig/180.)

      real mpro,mpio
      data mpro,mpio/0.9383,0.1396/
      real old(4),new(4)

      old(1)=Pm*sin(deg2rad*theta)*cos(phi*deg2rad)
      old(2)=Pm*sin(deg2rad*theta)*sin(phi*deg2rad)
      old(3)=Pm*cos(deg2rad*theta)

C *** here should be put the energy with the actual mass of the particle
      if (icharge.gt.0) then
         old(4)=sqrt(Pm*Pm+mpro*mpro)
      else if (icharge.lt.0) then
         old(4)=sqrt(Pm*Pm+mpio*mpio)
      endif

C *** choose the charge of the particle
      if (icharge.gt.0) then
         call corr_mom_pro(old,new,ifield,istat)
      else if (icharge.lt.0) then
         call corr_mom_pim(old,new,ifield,istat)
      endif
      
C *** outputs
      Pc=sqrt(new(1)**2.+new(2)**2.+new(3)**2.)
C *** no error 
      Ec=0.


      return
      end

****************************************************************************************
      subroutine corr_mom_pro(old,new,ifield,istat)

      implicit none
      real old(4),new(4)
      integer ifield,istat

      integer nsect,ntbin,fit_npar
      parameter(nsect=6)
      parameter(ntbin=18)
      parameter(fit_npar=4)
      real tetamin,tetamax,tetawid
      data tetamin,tetamax,tetawid/0.,144.,8./

C *** Theta bin range for the calculation
      integer thetabin_min,thetabin_max
      data thetabin_min,thetabin_max/2,16/

C *** parameters of the fits for low field
      real momcorr_par_low(fit_npar,ntbin,nsect)
      data momcorr_par_low/
     +         1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.98829973,   0.00856499,  -0.00010982,   0.00000000
     +     ,   0.99549115,   0.00184008,   0.00003225,   0.00000000
     +     ,   0.99763191,   0.00157166,  -0.00026285,   0.00000000
     +     ,   0.99597353,   0.00431594,  -0.00044609,   0.00000000
     +     ,   0.98992777,   0.00852459,  -0.00054207,   0.00000000
     +     ,   0.98608422,   0.01017904,  -0.00054147,   0.00000000
     +     ,   0.98109084,   0.01274328,  -0.00047895,   0.00000000
     +     ,   0.98122030,   0.01274086,  -0.00042248,   0.00000000
     +     ,   0.98166525,   0.01285537,  -0.00035368,   0.00000000
     +     ,   0.98241311,   0.01285851,  -0.00032569,   0.00000000
     +     ,   0.97834682,   0.01500989,  -0.00029177,   0.00000000
     +     ,   0.98451066,   0.01301013,  -0.00028773,   0.00000000
     +     ,   0.98256373,   0.01547128,  -0.00014584,   0.00000000
     +     ,   0.98174399,   0.01675536,  -0.00014455,   0.00000000
     +     ,   0.98759633,   0.01494134,  -0.00007694,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00176334,  -0.01648752,  -0.00025253,   0.00000000
     +     ,   0.99668723,  -0.00578446,  -0.00006734,   0.00000000
     +     ,   0.99602216,  -0.00122274,  -0.00017340,   0.00000000
     +     ,   0.99501956,   0.00158120,  -0.00028383,   0.00000000
     +     ,   0.99402297,   0.00276703,  -0.00034392,   0.00000000
     +     ,   0.98850381,   0.00639320,  -0.00035643,   0.00000000
     +     ,   0.98332232,   0.00982609,  -0.00037019,   0.00000000
     +     ,   0.98186260,   0.01092773,  -0.00040348,   0.00000000
     +     ,   0.97989708,   0.01200039,  -0.00039367,   0.00000000
     +     ,   0.97517306,   0.01442046,  -0.00047594,   0.00000000
     +     ,   0.97294080,   0.01544641,  -0.00046563,   0.00000000
     +     ,   0.97169644,   0.01633623,  -0.00044403,   0.00000000
     +     ,   0.96832925,   0.01932466,  -0.00041575,   0.00000000
     +     ,   0.96674365,   0.02062430,  -0.00031502,   0.00000000
     +     ,   0.96678400,   0.02129803,  -0.00023191,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99975586,  -0.00881025,  -0.00067183,   0.00000000
     +     ,   0.99198908,   0.00311341,  -0.00034109,   0.00000000
     +     ,   0.99601257,   0.00175467,  -0.00020947,   0.00000000
     +     ,   0.99628651,   0.00194962,  -0.00017277,   0.00000000
     +     ,   0.98912007,   0.00581535,  -0.00043238,   0.00000000
     +     ,   0.98847574,   0.00808010,  -0.00034588,   0.00000000
     +     ,   0.99655849,   0.00422340,  -0.00039538,   0.00000000
     +     ,   0.99166095,   0.00543158,  -0.00024073,   0.00000000
     +     ,   0.98849994,   0.00724709,  -0.00025884,   0.00000000
     +     ,   0.98323542,   0.01210343,  -0.00029789,   0.00000000
     +     ,   0.97970271,   0.01478004,  -0.00028689,   0.00000000
     +     ,   0.98025346,   0.01303859,  -0.00029040,   0.00000000
     +     ,   0.97583497,   0.01581664,  -0.00007154,   0.00000000
     +     ,   0.97319251,   0.01871771,  -0.00010316,   0.00000000
     +     ,   0.97992718,   0.01576899,  -0.00009606,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99298817,   0.01186823,  -0.00040872,   0.00000000
     +     ,   0.99708539,   0.00431951,  -0.00029729,   0.00000000
     +     ,   0.99818945,   0.00436874,  -0.00024149,   0.00000000
     +     ,   0.99692774,   0.00527219,  -0.00020044,   0.00000000
     +     ,   0.99172390,   0.00767894,  -0.00029278,   0.00000000
     +     ,   0.98514414,   0.01117972,  -0.00040617,   0.00000000
     +     ,   0.98079658,   0.01295951,  -0.00046401,   0.00000000
     +     ,   0.97986948,   0.01392962,  -0.00049176,   0.00000000
     +     ,   0.97801763,   0.01436858,  -0.00056084,   0.00000000
     +     ,   0.97682840,   0.01529139,  -0.00053880,   0.00000000
     +     ,   0.97569591,   0.01474605,  -0.00058446,   0.00000000
     +     ,   0.97384363,   0.01663836,  -0.00061508,   0.00000000
     +     ,   0.97498053,   0.01733980,  -0.00064824,   0.00000000
     +     ,   0.97008145,   0.02051418,  -0.00068750,   0.00000000
     +     ,   0.96619403,   0.02350752,  -0.00036699,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.98468220,   0.01756167,  -0.00017232,   0.00000000
     +     ,   0.98869407,   0.01009843,  -0.00012175,   0.00000000
     +     ,   0.98562598,   0.01072552,  -0.00019277,   0.00000000
     +     ,   0.98604536,   0.01156472,  -0.00025489,   0.00000000
     +     ,   0.98960006,   0.00714825,  -0.00035182,   0.00000000
     +     ,   0.97952116,   0.01456101,  -0.00040607,   0.00000000
     +     ,   0.97564346,   0.01587344,  -0.00046774,   0.00000000
     +     ,   0.97586983,   0.01502947,  -0.00046917,   0.00000000
     +     ,   0.97277272,   0.01863419,  -0.00036990,   0.00000000
     +     ,   0.97363949,   0.02035284,  -0.00017483,   0.00000000
     +     ,   0.97262251,   0.02048915,   0.00007422,   0.00000000
     +     ,   0.97781610,   0.01507343,  -0.00002994,   0.00000000
     +     ,   0.97837770,   0.01562417,  -0.00008096,   0.00000000
     +     ,   0.98097295,   0.01659027,  -0.00003428,   0.00000000
     +     ,   0.98099983,   0.01765046,   0.00007120,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99356341,   0.01252305,  -0.00065173,   0.00000000
     +     ,   0.99945074,   0.00203080,  -0.00049619,   0.00000000
     +     ,   0.99713266,   0.00433623,  -0.00047427,   0.00000000
     +     ,   0.99127483,   0.00883214,  -0.00048361,   0.00000000
     +     ,   0.98961747,   0.00930199,  -0.00041667,   0.00000000
     +     ,   0.98632056,   0.01098522,  -0.00044519,   0.00000000
     +     ,   0.98485363,   0.01187986,  -0.00044273,   0.00000000
     +     ,   0.98507899,   0.01204607,  -0.00044924,   0.00000000
     +     ,   0.98461008,   0.01230106,  -0.00042522,   0.00000000
     +     ,   0.98365027,   0.01311961,  -0.00037567,   0.00000000
     +     ,   0.98109484,   0.01410087,  -0.00035617,   0.00000000
     +     ,   0.98275518,   0.01452666,  -0.00032107,   0.00000000
     +     ,   0.99037963,   0.01386428,  -0.00020345,   0.00000000
     +     ,   0.98977935,   0.01446990,  -0.00020068,   0.00000000
     +     ,   0.96284765,   0.02536324,  -0.00022466,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000/
C *** moemntum range for the calculation
      real pmin_low(ntbin),pmax_low(ntbin)
      data pmin_low/0.000,0.425,0.275,0.275,0.275,0.275,0.275,0.275,
     +              0.275,0.275,0.275,0.275,0.275,0.275,0.275,0.275,
     +              0.000,0.000/
      data pmax_low/0.000,2.375,2.525,2.375,2.075,1.775,1.625,1.475,
     +              1.325,1.175,1.025,1.025,0.875,0.725,0.725,0.575,
     +              0.000,0.000/

C *** parameters of the fits for high field
      real momcorr_par_high(fit_npar,ntbin,nsect)
      data momcorr_par_high/
     +         1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99789166,  -0.00267418,  -0.00003412,   0.00000000
     +     ,   0.99682516,   0.00060365,  -0.00004587,   0.00000000
     +     ,   0.99665344,   0.00296341,  -0.00036930,   0.00000000
     +     ,   0.99555349,   0.00443753,  -0.00045790,   0.00000000
     +     ,   0.99284154,   0.00569656,  -0.00050317,   0.00000000
     +     ,   0.98924500,   0.00749754,  -0.00050297,   0.00000000
     +     ,   0.98563492,   0.00951649,  -0.00044252,   0.00000000
     +     ,   0.98468751,   0.01016698,  -0.00039081,   0.00000000
     +     ,   0.98623604,   0.00963176,  -0.00030855,   0.00000000
     +     ,   0.98491675,   0.01049133,  -0.00025382,   0.00000000
     +     ,   0.98280710,   0.01203375,  -0.00022701,   0.00000000
     +     ,   0.98049247,   0.01324640,  -0.00017752,   0.00000000
     +     ,   0.97693884,   0.01676663,  -0.00002086,   0.00000000
     +     ,   0.97912389,   0.01680754,  -0.00010757,   0.00000000
     +     ,   0.99329257,   0.01136174,  -0.00010429,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99702293,  -0.01623467,  -0.00005986,   0.00000000
     +     ,   0.99656087,  -0.00829347,  -0.00009527,   0.00000000
     +     ,   0.99513805,  -0.00262347,  -0.00021360,   0.00000000
     +     ,   0.99504000,   0.00015603,  -0.00035326,   0.00000000
     +     ,   0.99623197,  -0.00054973,  -0.00042147,   0.00000000
     +     ,   0.99116582,   0.00301890,  -0.00040542,   0.00000000
     +     ,   0.98736793,   0.00623087,  -0.00042231,   0.00000000
     +     ,   0.98597294,   0.00734770,  -0.00041428,   0.00000000
     +     ,   0.98442471,   0.00831469,  -0.00044536,   0.00000000
     +     ,   0.98130679,   0.01024461,  -0.00049195,   0.00000000
     +     ,   0.97676164,   0.01239990,  -0.00046335,   0.00000000
     +     ,   0.97581661,   0.01394010,  -0.00041659,   0.00000000
     +     ,   0.97591758,   0.01461793,  -0.00041476,   0.00000000
     +     ,   0.97089660,   0.01673085,  -0.00044014,   0.00000000
     +     ,   0.96000725,   0.02526829,  -0.00014303,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99153215,  -0.00535899,  -0.00043736,   0.00000000
     +     ,   0.99500984,  -0.00167024,  -0.00028930,   0.00000000
     +     ,   0.99725431,  -0.00123636,  -0.00024174,   0.00000000
     +     ,   0.99823761,  -0.00175502,  -0.00023604,   0.00000000
     +     ,   0.99229795,   0.00189036,  -0.00048352,   0.00000000
     +     ,   0.98958629,   0.00588228,  -0.00039757,   0.00000000
     +     ,   0.99784440,   0.00060867,  -0.00045255,   0.00000000
     +     ,   0.99731255,   0.00064944,  -0.00028425,   0.00000000
     +     ,   0.99160516,   0.00447349,  -0.00038715,   0.00000000
     +     ,   0.98581427,   0.00953181,  -0.00037295,   0.00000000
     +     ,   0.98569548,   0.00892511,  -0.00036631,   0.00000000
     +     ,   0.98052704,   0.01215902,  -0.00032765,   0.00000000
     +     ,   0.97591138,   0.01498344,  -0.00024873,   0.00000000
     +     ,   0.97611040,   0.01647420,  -0.00030938,   0.00000000
     +     ,   0.97831768,   0.01607198,  -0.00000423,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99321884,   0.01023834,  -0.00040951,   0.00000000
     +     ,   0.99891675,   0.00188343,  -0.00031781,   0.00000000
     +     ,   0.99784923,   0.00399158,  -0.00028675,   0.00000000
     +     ,   0.99669564,   0.00442656,  -0.00030927,   0.00000000
     +     ,   0.99316210,   0.00589504,  -0.00039103,   0.00000000
     +     ,   0.98970640,   0.00728853,  -0.00048632,   0.00000000
     +     ,   0.98697400,   0.00877449,  -0.00055240,   0.00000000
     +     ,   0.98686594,   0.00868798,  -0.00057397,   0.00000000
     +     ,   0.98437768,   0.01020113,  -0.00062466,   0.00000000
     +     ,   0.98298484,   0.01062067,  -0.00062716,   0.00000000
     +     ,   0.98059469,   0.01175181,  -0.00065913,   0.00000000
     +     ,   0.97730798,   0.01424390,  -0.00067609,   0.00000000
     +     ,   0.97715431,   0.01525238,  -0.00069619,   0.00000000
     +     ,   0.97427201,   0.01795442,  -0.00081449,   0.00000000
     +     ,   0.95213532,   0.02855529,  -0.00084077,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.98269749,   0.01742866,  -0.00026810,   0.00000000
     +     ,   0.98829067,   0.00972943,  -0.00020647,   0.00000000
     +     ,   0.98639369,   0.01134488,  -0.00028877,   0.00000000
     +     ,   0.98824728,   0.00961863,  -0.00030791,   0.00000000
     +     ,   0.98578274,   0.01072666,  -0.00039798,   0.00000000
     +     ,   0.98339170,   0.01200827,  -0.00040950,   0.00000000
     +     ,   0.98255110,   0.01155219,  -0.00049501,   0.00000000
     +     ,   0.98015690,   0.01260142,  -0.00052556,   0.00000000
     +     ,   0.97934115,   0.01422535,  -0.00038799,   0.00000000
     +     ,   0.97506845,   0.01906078,  -0.00013247,   0.00000000
     +     ,   0.97991049,   0.01635644,   0.00012710,   0.00000000
     +     ,   0.97814023,   0.01558467,  -0.00008695,   0.00000000
     +     ,   0.97905564,   0.01513802,  -0.00010125,   0.00000000
     +     ,   0.97222638,   0.02032766,  -0.00017553,   0.00000000
     +     ,   0.98254281,   0.01727939,   0.00030830,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.15968978,  -0.14423414,   0.00637993,   0.00000000
     +     ,   0.99663836,   0.00463499,  -0.00050280,   0.00000000
     +     ,   0.99612159,   0.00370723,  -0.00047369,   0.00000000
     +     ,   0.99419141,   0.00620668,  -0.00048351,   0.00000000
     +     ,   0.99191254,   0.00745126,  -0.00046945,   0.00000000
     +     ,   0.98920155,   0.00917750,  -0.00044453,   0.00000000
     +     ,   0.98814005,   0.00930491,  -0.00046857,   0.00000000
     +     ,   0.98763317,   0.00941992,  -0.00047458,   0.00000000
     +     ,   0.98717248,   0.00977801,  -0.00046363,   0.00000000
     +     ,   0.98797303,   0.00960595,  -0.00043842,   0.00000000
     +     ,   0.98627669,   0.01058309,  -0.00039408,   0.00000000
     +     ,   0.98622048,   0.01090012,  -0.00036881,   0.00000000
     +     ,   0.98331201,   0.01403454,  -0.00036807,   0.00000000
     +     ,   0.98704261,   0.01364556,  -0.00024460,   0.00000000
     +     ,   0.98357809,   0.01621700,  -0.00033080,   0.00000000
     +     ,   0.94545877,   0.03558980,  -0.00052687,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000/
C *** moemntum range for the calculation
      real pmin_high(ntbin),pmax_high(ntbin)
      data pmin_high/0.000,0.275,0.275,0.275,0.275,0.275,0.275,0.275,
     +              0.275,0.275,0.275,0.275,0.275,0.275,0.275,0.275,
     +              0.000,0.000/
      data pmax_high/0.000,2.825,2.675,2.375,2.225,1.925,1.775,1.475,
     +              1.325,1.175,1.025,0.875,0.875,0.725,0.725,0.725,
     +              0.000,0.000/

      real par(fit_npar)
      

      external v3magF
      real v3magF

      real mass,mag3,theta,phi,theta_ave,phi_sector
      integer thetabin,thetabin_next,sector
      real corr,c1,c2,t1,t2,f1,f2
      integer k
      real pmin,pmax,pfit


      mag3=v3magF(old)
      call v4dir_degF(old,theta,phi)

      thetabin=theta/tetawid+1
      theta_ave=tetamin+tetawid*(thetaBin-0.5)
      if (theta.lt.theta_ave) then
         thetabin_next=thetaBin-1
      else 
         thetabin_next = thetabin+1
      endif

      if ((phi.le.30).or.(phi.gt.330)) then
         sector=1
      else 
         sector = 1 + (phi+30) / 60.;
      endif

      phi_sector = phi - 60.*(sector-1);
      if (phi.gt.330.) phi_sector = phi_sector-360.;


C *** Check theta and momentum range
      istat=1

      if (thetabin.gt.thetabin_max) then
         thetabin=thetabin_max
         istat=2
      else if (thetabin.lt.thetabin_min) then
         thetabin=thetabin_min
         istat=2
      endif

      if (thetabin_next.gt.thetabin_max) then
         thetabin_next=thetabin_max
      else if (thetabin_next.lt.thetabin_min) then
         thetabin_next=thetabin_min
      endif

      pfit=mag3
      if (ifield.eq.1) then
         pmin=pmin_low(thetabin)
         pmax=pmax_low(thetabin)
      else 
         pmin=pmin_high(thetabin)
         pmax=pmax_high(thetabin)
      endif
      if (mag3.lt.pmin) then
         pfit=pmin
         istat=2
      else if (mag3.gt.pmax) then
         pfit=pmax
         istat=2
      endif


* *** correction from actual theta bin
      if (ifield.eq.1) then
         do k=1,fit_npar
            par(k)=momcorr_par_low(k,thetabin,sector)
         enddo
      else
         do k=1,fit_npar
            par(k)=momcorr_par_high(k,thetabin,sector)
         enddo
      endif
      c1=par(1)+par(3)*phi_sector+par(2)/(pfit-par(4))
      t1=theta_ave 
      f1=abs(theta-t1)/tetawid
      
* *** correction from next closest theta bin
      if (ifield.eq.1) then
         do k=1,fit_npar
            par(k)=momcorr_par_low(k,thetabin_next,sector)
         enddo
      else
         do k=1,fit_npar
            par(k)=momcorr_par_high(k,thetabin_next,sector)
         enddo
      endif
      c2=par(1)+par(3)*phi_sector+par(2)/(pfit-par(4))
      t2=tetamin+tetawid*(thetaBin_next-0.5)
      f2=abs(theta-t2)/tetawid

* *** weighted average of the two theta bin corrections
      corr=(f1*c2+f2*c1)/(f1+f2)
      do k=1,3
         new(k)=corr*old(k)
      enddo
      mass=sqrt(old(4)**2.-mag3**2.)
      mag3=mag3*corr
      new(4)=sqrt(mass**2.+mag3**2.)


      return
      end

****************************************************************************************
      subroutine corr_mom_pim(old,new,ifield,istat)

      implicit none
      real old(4),new(4)
      integer ifield,istat

      integer nsect,ntbin,fit_npar
      parameter(nsect=6)
      parameter(ntbin=18)
      parameter(fit_npar=4)
      real tetamin,tetamax,tetawid
      data tetamin,tetamax,tetawid/0.,144.,8./

C *** Theta bin range for the calculation
      integer thetabin_min,thetabin_max
      data thetabin_min,thetabin_max/3,17/

C *** parameters of the fits for low field
      real momcorr_par_low(fit_npar,ntbin,nsect)
      data momcorr_par_low/
     +         1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99942929,  -0.00174883,  -0.00023932,   0.00000000
     +     ,   1.00027466,  -0.00213343,  -0.00018210,   0.00000000
     +     ,   0.99773920,  -0.00019863,  -0.00016468,   0.00000000
     +     ,   0.99954182,  -0.00098095,  -0.00018362,   0.00000000
     +     ,   1.00028479,  -0.00089109,  -0.00019283,   0.00000000
     +     ,   1.00149417,  -0.00092174,  -0.00017768,   0.00000000
     +     ,   1.00183737,  -0.00061012,  -0.00022822,   0.00000000
     +     ,   0.99950051,   0.00025137,  -0.00023135,   0.00000000
     +     ,   0.99768138,   0.00081662,  -0.00016942,   0.00000000
     +     ,   0.99850065,   0.00051790,  -0.00021140,   0.00000000
     +     ,   0.99648470,   0.00115880,  -0.00016961,   0.00000000
     +     ,   0.99331635,   0.00214333,  -0.00014061,   0.00000000
     +     ,   0.99271125,   0.00204260,  -0.00001838,   0.00000000
     +     ,   0.98771816,   0.00292780,   0.00030355,   0.00000000
     +     ,   0.98134243,   0.00424508,   0.00059404,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00071585,  -0.00269969,  -0.00011308,   0.00000000
     +     ,   0.99953026,  -0.00163083,  -0.00008044,   0.00000000
     +     ,   0.99860173,  -0.00079617,  -0.00016286,   0.00000000
     +     ,   0.99865091,  -0.00033711,  -0.00021255,   0.00000000
     +     ,   0.99922901,  -0.00040681,  -0.00030166,   0.00000000
     +     ,   1.00149512,  -0.00089414,  -0.00037381,   0.00000000
     +     ,   1.00237393,  -0.00065526,  -0.00037814,   0.00000000
     +     ,   1.00305939,  -0.00057756,  -0.00038599,   0.00000000
     +     ,   1.00299072,  -0.00025854,  -0.00033332,   0.00000000
     +     ,   1.00379395,  -0.00012292,  -0.00033706,   0.00000000
     +     ,   1.00347722,   0.00052209,  -0.00025408,   0.00000000
     +     ,   1.00351059,   0.00039147,  -0.00020344,   0.00000000
     +     ,   1.00216973,   0.00101534,  -0.00009351,   0.00000000
     +     ,   1.00029075,   0.00184418,   0.00015310,   0.00000000
     +     ,   1.00239825,   0.00122828,   0.00061163,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99520433,   0.00356936,   0.00009040,   0.00000000
     +     ,   0.99725497,   0.00155446,   0.00004502,   0.00000000
     +     ,   0.99934334,   0.00006306,  -0.00002461,   0.00000000
     +     ,   0.99908721,   0.00065926,  -0.00027651,   0.00000000
     +     ,   0.99917126,  -0.00016997,  -0.00033751,   0.00000000
     +     ,   0.99756449,   0.00055419,  -0.00046584,   0.00000000
     +     ,   0.99835259,   0.00030518,  -0.00053156,   0.00000000
     +     ,   0.99861205,   0.00058899,  -0.00055503,   0.00000000
     +     ,   1.00052309,   0.00030775,  -0.00045272,   0.00000000
     +     ,   1.00005054,   0.00081343,  -0.00059665,   0.00000000
     +     ,   0.99886298,   0.00132843,  -0.00053159,   0.00000000
     +     ,   0.99954236,   0.00105174,  -0.00050236,   0.00000000
     +     ,   0.99643880,   0.00207502,  -0.00046657,   0.00000000
     +     ,   0.99672961,   0.00132007,  -0.00042988,   0.00000000
     +     ,   1.00182903,  -0.00036077,  -0.00026232,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00330758,  -0.00130440,  -0.00006708,   0.00000000
     +     ,   0.99932903,   0.00124091,  -0.00002824,   0.00000000
     +     ,   0.99806058,   0.00150756,  -0.00011263,   0.00000000
     +     ,   0.99670082,   0.00197381,  -0.00024636,   0.00000000
     +     ,   0.99832976,   0.00132166,  -0.00030360,   0.00000000
     +     ,   1.00121188,   0.00032407,  -0.00038579,   0.00000000
     +     ,   1.00071120,   0.00086922,  -0.00042926,   0.00000000
     +     ,   0.99978065,   0.00104678,  -0.00047842,   0.00000000
     +     ,   0.99795967,   0.00152233,  -0.00045121,   0.00000000
     +     ,   0.99921107,   0.00115464,  -0.00045164,   0.00000000
     +     ,   0.99980712,   0.00071943,  -0.00041310,   0.00000000
     +     ,   0.99771017,   0.00173969,  -0.00048431,   0.00000000
     +     ,   0.99927306,   0.00132400,  -0.00060105,   0.00000000
     +     ,   0.99299163,   0.00253257,  -0.00070999,   0.00000000
     +     ,   0.99117345,   0.00080015,  -0.00010383,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99545705,   0.00441872,  -0.00007637,   0.00000000
     +     ,   1.00043726,   0.00073236,  -0.00008519,   0.00000000
     +     ,   0.99987835,   0.00090074,  -0.00015372,   0.00000000
     +     ,   1.00204575,   0.00021061,  -0.00018881,   0.00000000
     +     ,   1.00101340,   0.00114651,  -0.00021008,   0.00000000
     +     ,   1.00364912,   0.00022035,  -0.00025369,   0.00000000
     +     ,   1.00337112,   0.00041353,  -0.00023572,   0.00000000
     +     ,   0.99997765,   0.00112740,  -0.00023730,   0.00000000
     +     ,   0.98496616,   0.00386394,  -0.00012488,   0.00000000
     +     ,   0.99333411,   0.00382452,  -0.00052919,   0.00000000
     +     ,   0.99681288,   0.00322600,  -0.00055956,   0.00000000
     +     ,   0.99774843,   0.00297195,  -0.00044167,   0.00000000
     +     ,   0.99546009,   0.00299315,  -0.00037089,   0.00000000
     +     ,   0.99027282,   0.00390089,  -0.00024318,   0.00000000
     +     ,   0.98226672,   0.00460135,   0.00007034,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99615097,   0.00142068,   0.00001998,   0.00000000
     +     ,   0.99639887,   0.00102999,   0.00000811,   0.00000000
     +     ,   0.99634194,   0.00096472,  -0.00006818,   0.00000000
     +     ,   0.99779463,   0.00066602,  -0.00015361,   0.00000000
     +     ,   0.99879998,   0.00082185,  -0.00022741,   0.00000000
     +     ,   0.99883282,   0.00140148,  -0.00029008,   0.00000000
     +     ,   0.99833745,   0.00122348,  -0.00029623,   0.00000000
     +     ,   0.99638367,   0.00206104,  -0.00030217,   0.00000000
     +     ,   0.99586618,   0.00218378,  -0.00029012,   0.00000000
     +     ,   0.99748927,   0.00163120,  -0.00036841,   0.00000000
     +     ,   0.99642354,   0.00227876,  -0.00038741,   0.00000000
     +     ,   0.99079388,   0.00354775,  -0.00038651,   0.00000000
     +     ,   0.98085105,   0.00555738,  -0.00035519,   0.00000000
     +     ,   0.97903162,   0.00629707,  -0.00033476,   0.00000000
     +     ,   0.98716098,   0.00575658,   0.00037666,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000/
C *** momentum range for the calculation
      real pmin_low(ntbin),pmax_low(ntbin)
      data pmin_low/0.0,0.0,0.7,0.3,0.3,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
     +              0.1,0.1,0.1,0.1,0.1,0.0/
      data pmax_low/0.0,0.0,2.1,1.9,1.7,1.5,1.3,1.3,1.1,1.1,0.9,0.9,
     +              0.9,0.7,0.7,0.7,0.5,0.0/

C *** parameters of the fits for high field
      real momcorr_par_high(fit_npar,ntbin,nsect)
      data momcorr_par_high/
     +         1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99939978,  -0.00304508,  -0.00043030,   0.00000000
     +     ,   1.00126541,  -0.00362272,  -0.00009023,   0.00000000
     +     ,   0.99896795,  -0.00145284,  -0.00012794,   0.00000000
     +     ,   0.99727196,   0.00012834,  -0.00016639,   0.00000000
     +     ,   0.99918294,  -0.00081686,  -0.00021118,   0.00000000
     +     ,   1.00058830,  -0.00090178,  -0.00024255,   0.00000000
     +     ,   1.00022745,  -0.00027294,  -0.00026915,   0.00000000
     +     ,   0.99861282,   0.00042891,  -0.00021029,   0.00000000
     +     ,   0.99652797,   0.00129767,  -0.00022910,   0.00000000
     +     ,   0.99739212,   0.00097318,  -0.00026175,   0.00000000
     +     ,   0.99691170,   0.00123249,  -0.00029638,   0.00000000
     +     ,   0.99475110,   0.00084960,  -0.00016709,   0.00000000
     +     ,   0.99802417,   0.00117671,  -0.00003666,   0.00000000
     +     ,   0.99640489,   0.00137826,   0.00030894,   0.00000000
     +     ,   0.99078292,   0.00189456,   0.00046365,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99073607,   0.01097673,  -0.00008405,   0.00000000
     +     ,   0.99774861,   0.00108561,  -0.00010236,   0.00000000
     +     ,   1.00023234,  -0.00108031,  -0.00012603,   0.00000000
     +     ,   0.99942899,  -0.00010913,  -0.00023872,   0.00000000
     +     ,   1.00017047,  -0.00038477,  -0.00024535,   0.00000000
     +     ,   0.99968833,   0.00014939,  -0.00037063,   0.00000000
     +     ,   1.00175917,  -0.00025536,  -0.00039109,   0.00000000
     +     ,   1.00269198,  -0.00047743,  -0.00042233,   0.00000000
     +     ,   1.00277305,  -0.00012218,  -0.00038709,   0.00000000
     +     ,   1.00324547,  -0.00001331,  -0.00036201,   0.00000000
     +     ,   1.00301123,   0.00047331,  -0.00045185,   0.00000000
     +     ,   1.00486350,   0.00021751,  -0.00033200,   0.00000000
     +     ,   1.00564158,   0.00023625,  -0.00017430,   0.00000000
     +     ,   1.00384176,   0.00084912,   0.00008058,   0.00000000
     +     ,   1.00519013,  -0.00038435,   0.00033242,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99042022,   0.01491591,   0.00006366,   0.00000000
     +     ,   1.00010967,   0.00043258,   0.00002377,   0.00000000
     +     ,   0.99875367,   0.00075419,  -0.00003159,   0.00000000
     +     ,   0.99902743,   0.00077871,  -0.00018509,   0.00000000
     +     ,   1.00061488,  -0.00083859,  -0.00027991,   0.00000000
     +     ,   0.99695683,   0.00150351,  -0.00038094,   0.00000000
     +     ,   0.99848175,   0.00094489,  -0.00046963,   0.00000000
     +     ,   0.99959850,   0.00060161,  -0.00049258,   0.00000000
     +     ,   0.99939841,   0.00076193,  -0.00049493,   0.00000000
     +     ,   1.00028062,   0.00086664,  -0.00054061,   0.00000000
     +     ,   1.00045168,   0.00090745,  -0.00055656,   0.00000000
     +     ,   1.00076842,   0.00081870,  -0.00054633,   0.00000000
     +     ,   1.00044906,   0.00103455,  -0.00053088,   0.00000000
     +     ,   0.99786967,   0.00136976,  -0.00039201,   0.00000000
     +     ,   0.99586660,   0.00192710,  -0.00022036,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00111616,  -0.00012583,  -0.00000654,   0.00000000
     +     ,   0.99877065,   0.00137121,   0.00002054,   0.00000000
     +     ,   0.99835521,   0.00106459,  -0.00004018,   0.00000000
     +     ,   0.99747038,   0.00156326,  -0.00015578,   0.00000000
     +     ,   0.99825346,   0.00129965,  -0.00026454,   0.00000000
     +     ,   0.99936295,   0.00083138,  -0.00034766,   0.00000000
     +     ,   1.00024855,   0.00047224,  -0.00042592,   0.00000000
     +     ,   0.99936628,   0.00088796,  -0.00044680,   0.00000000
     +     ,   0.99730229,   0.00162444,  -0.00048041,   0.00000000
     +     ,   0.99713475,   0.00158104,  -0.00047519,   0.00000000
     +     ,   0.99701506,   0.00183183,  -0.00047770,   0.00000000
     +     ,   0.99653274,   0.00191629,  -0.00054444,   0.00000000
     +     ,   0.99716157,   0.00179856,  -0.00067058,   0.00000000
     +     ,   0.99222708,   0.00251187,  -0.00064518,   0.00000000
     +     ,   0.99582118,  -0.00040337,   0.00009549,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   0.99107480,   0.01009532,  -0.00009139,   0.00000000
     +     ,   1.00183010,  -0.00154848,  -0.00005810,   0.00000000
     +     ,   0.99904734,   0.00070680,  -0.00011540,   0.00000000
     +     ,   0.99959463,   0.00086864,  -0.00015395,   0.00000000
     +     ,   1.00110638,   0.00033936,  -0.00019667,   0.00000000
     +     ,   1.00060213,   0.00097371,  -0.00024457,   0.00000000
     +     ,   1.00111294,   0.00087571,  -0.00027097,   0.00000000
     +     ,   0.99683386,   0.00226261,  -0.00026232,   0.00000000
     +     ,   0.98405904,   0.00476798,  -0.00010381,   0.00000000
     +     ,   1.00387383,   0.00011194,  -0.00014610,   0.00000000
     +     ,   0.99715286,   0.00319015,  -0.00056986,   0.00000000
     +     ,   0.99656165,   0.00281558,  -0.00042116,   0.00000000
     +     ,   0.99255371,   0.00397444,  -0.00039315,   0.00000000
     +     ,   0.99328405,   0.00319711,  -0.00031782,   0.00000000
     +     ,   0.98996079,   0.00292481,   0.00001451,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000

     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000
     +     ,   1.02318418,  -0.03397067,  -0.00024375,   0.00000000
     +     ,   0.99888408,  -0.00124142,  -0.00001816,   0.00000000
     +     ,   0.99751800,   0.00023305,  -0.00006672,   0.00000000
     +     ,   0.99751365,   0.00060000,  -0.00015578,   0.00000000
     +     ,   0.99876195,   0.00051394,  -0.00021152,   0.00000000
     +     ,   0.99766469,   0.00155485,  -0.00029013,   0.00000000
     +     ,   0.99706674,   0.00168685,  -0.00027675,   0.00000000
     +     ,   0.99726409,   0.00165587,  -0.00031878,   0.00000000
     +     ,   0.99604201,   0.00226788,  -0.00033925,   0.00000000
     +     ,   0.99669135,   0.00199075,  -0.00036656,   0.00000000
     +     ,   0.99751294,   0.00195891,  -0.00044230,   0.00000000
     +     ,   0.99329489,   0.00285622,  -0.00045043,   0.00000000
     +     ,   0.98919123,   0.00353859,  -0.00046454,   0.00000000
     +     ,   0.98667711,   0.00408907,  -0.00039679,   0.00000000
     +     ,   0.98071587,   0.00675088,   0.00026661,   0.00000000
     +     ,   1.00000000,   0.00000000,   0.00000000,   0.00000000/
C *** momentum range for the calculation
      real pmin_high(ntbin),pmax_high(ntbin)
      data pmin_high/0.0,0.0,0.9,0.5,0.3,0.3,0.1,0.1,0.1,0.1,0.1,0.1,
     +              0.1,0.1,0.1,0.1,0.1,0.0/
      data pmax_high/0.0,0.0,2.5,2.1,1.9,1.7,1.5,1.3,1.1,1.1,0.9,0.9,
     +              0.9,0.7,0.7,0.7,0.5,0.0/

      real par(fit_npar)
      

      external v3magF
      real v3magF

      real mass,mag3,theta,phi,theta_ave,phi_sector
      integer thetabin,thetabin_next,sector
      real corr,c1,c2,t1,t2,f1,f2
      integer k
      real pmin,pmax,pfit


      mag3=v3magF(old)
      call v4dir_degF(old,theta,phi)

      thetabin=theta/tetawid+1
      theta_ave=tetamin+tetawid*(thetaBin-0.5)
      if (theta.lt.theta_ave) then
         thetabin_next=thetaBin-1
      else 
         thetabin_next = thetabin+1
      endif

      if ((phi.le.30).or.(phi.gt.330)) then
         sector=1
      else 
         sector = 1 + (phi+30) / 60.;
      endif

      phi_sector = phi - 60.*(sector-1);
      if (phi.gt.330.) phi_sector = phi_sector-360.;


C *** Check theta and momentum range
      istat=1

      if (thetabin.gt.thetabin_max) then
         thetabin=thetabin_max
         istat=2
      else if (thetabin.lt.thetabin_min) then
         thetabin=thetabin_min
         istat=2
      endif

      if (thetabin_next.gt.thetabin_max) then
         thetabin_next=thetabin_max
      else if (thetabin_next.lt.thetabin_min) then
         thetabin_next=thetabin_min
      endif

      pfit=mag3
      if (ifield.eq.1) then
         pmin=pmin_low(thetabin)
         pmax=pmax_low(thetabin)
      else 
         pmin=pmin_high(thetabin)
         pmax=pmax_high(thetabin)
      endif
      if (mag3.lt.pmin) then
         pfit=pmin
         istat=2
      else if (mag3.gt.pmax) then
         pfit=pmax
         istat=2
      endif



* *** correction for actual theta bin
      if (ifield.eq.1) then
         do k=1,fit_npar
            par(k)=momcorr_par_low(k,thetabin,sector)
         enddo
      else
         do k=1,fit_npar
            par(k)=momcorr_par_high(k,thetabin,sector)
         enddo
      endif
      c1=par(1)+par(3)*phi_sector+par(2)/(pfit-par(4))
      t1=theta_ave 
      f1=abs(theta-t1)/tetawid
      
* *** corection for next closest theta bin
      if (ifield.eq.1) then
         do k=1,fit_npar
            par(k)=momcorr_par_low(k,thetabin_next,sector)
         enddo
      else
         do k=1,fit_npar
            par(k)=momcorr_par_high(k,thetabin_next,sector)
         enddo
      endif
      c2=par(1)+par(3)*phi_sector+par(2)/(pfit-par(4))
      t2=tetamin+tetawid*(thetaBin_next-0.5)
      f2=abs(theta-t2)/tetawid

* *** weughted average of the two theta bin corrections
      corr=(f1*c2+f2*c1)/(f1+f2)
      do k=1,3
         new(k)=corr*old(k)
      enddo
      mass=sqrt(old(4)**2.-mag3**2.)
      mag3=mag3*corr
      new(4)=sqrt(mass**2.+mag3**2.)
      

      return
      end

      real function v3magF(v)
      implicit none
      real v(4)

      v3magF = sqrt(v(1)**2+v(2)**2+v(3)**2)

      return
      end

      subroutine v4dir_degF(v,theta,phi)
      implicit none
      real v3magF
      real v(4),theta,phi,mag
      real pig,deg2rad
      parameter(pig=3.1415927)
      parameter(deg2rad=pig/180.)

      mag   = v3magF(v)
      theta = acos(v(3)/mag)/deg2rad
      phi   = atan2(v(2),v(1))/deg2rad
      if(phi.lt.-30) phi = phi+360.

      return
      end
c     Tsutomu Mibe's  momentum correction based on (gd-->pppim)_t
c     Inputs:
c        ifield : integer, torus field setting
c                  = 1 : 2250A
c                  = 2 : 3375A
c        icharge: integer, charge of particle
c        p      : real,    momentum before correction
c        theta  : real,    polar angle
c        phi    : real,    azimuthal angle (-30~330 degree)
c                  sector 1 (-30, 30)
c                  sector 2 ( 30, 90)
c                  sector 3 ( 90,150)
c                  sector 4 (150,210)
c                  sector 5 (210,270)
c                  sector 6 (270,330)
c     Outputs
c        Pc     : real,  momentum after correction
c        Ec     : real,  error of the correction
c        istat  : integer,
c                  < 0 : error
c                  = 1 : correction was measured
c                  = 2 : correction was not measured
c
c     author T. Mibe
      subroutine pcorpppm_tm(ifield,icharge,Pm,theta,phi,Pc,Ec,istat)

      implicit none 
      
      vector cpim
      vector cpr
      vector cpimh
      vector cprh

      integer ifield, icharge, istat
      real    Pm, theta, phi, Pc, Ec
      integer ifirst, i, j, ibin, isector, idpart

      real    pnew

      integer ip,ith,iphi,imeas
      real    pave,thave,phiave

      real pmaxpim(15),pminpim(15),pmaxpr(15),pminpr(15)
      integer ipmaxpim(15),ipminpim(15),ipmaxpr(15),ipminpr(15)

      real corr

c     limits of available kinematics
      data pmaxpim
     +     /0.0,1.7,1.7,1.5,1.3,
     +      1.1,1.1,0.9,0.9,0.7,
     +      0.7,0.7,0.0,0.0,0.0/
      data pminpim
     +     /0.0,0.7,0.5,0.3,0.3,
     +      0.3,0.3,0.3,0.3,0.3,
     +      0.3,0.3,0.0,0.0,0.0/
      data pmaxpr
     +     /0.175,1.625,1.775,1.775,1.475,
     +      1.325,1.175,1.025,0.875,0.725,
     +      0.725,0.000,0.000,0.000,0.000/
      data pminpr
     +     /0.875,0.725,0.575,0.575,0.425,
     +      0.425,0.425,0.425,0.425,0.425,
     +      0.425,0.000,0.000,0.000,0.000/
      data ipmaxpim
     +     / 0, 8, 8, 7, 6, 5, 5, 4, 4, 3, 3, 3, 3, 0, 0/
      data ipminpim
     +     / 0, 4, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0/
      data ipmaxpr
     +     / 6, 9,10,10, 8, 7, 6, 5, 4, 3, 2, 0, 0, 0, 0/
      data ipminpr
     +     / 5, 4, 3, 3, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0/

c     initializaion
      Pc   = -1000
      Ec   =  0
      istat= -1
      ibin = -1
      ip   = -1
      ith  = -1
      iphi = -1

c     apply proton (pi-) corrections for positive(negagive) charged particle, 
      if (icharge.eq.+1) then
         idpart = 14
      else
         idpart =  9
      endif

c     pi- corrections
      if (idpart.eq.9) then
         do i=0,30
            pave = 0.20+0.20*(i-1)
            if (abs(Pm-pave).le.0.10) then
               ip = i
            endif
         enddo
         do i=0,21
            thave = 12+8*(i-1)
            if (abs(theta-thave).le.4) then
               ith = i
            endif
         enddo
         do i=1,60
            phiave= -27 + 6*(i-1)
            if (abs(phi-phiave).le.3) then
               iphi= i
            endif
         enddo

         if ((ip.ge.0).and.(ith.ge. 0).and.(iphi.gt.0)) then
            imeas = 1
            if (ith.gt.13) then
               ith  = 13
               imeas= -1
            else if (ith.lt.2) then
               ith  = 2
               imeas= -1
            endif
            if (Pm.gt.pmaxpim(ith)) then
               ip = ipmaxpim(ith)
               imeas= -1
            else if (Pm.lt.pminpim(ith)) then
               ip = ipminpim(ith)
               imeas= -1
            endif
            ibin = 60*13*(ip-1) + 60*(ith-1) + iphi
         endif

         if (ibin.le.0) then
            istat = 1
            return
         endif
         if (ifield.eq.1) then
            corr= cpim(ibin)
         else if (ifield.eq.2) then
            corr= cpimh(ibin)
         endif

         if (corr.gt.-1.0) then
            Pc   = Pm*corr+Pm
            if (imeas.eq.1) then
               istat= 1
            else
               istat= 2
            endif
         else
            Pc   =  Pm
            istat=  2
         endif

c     proton corrections
      else if (idpart.eq.14) then
         do i=0,30
            pave = 0.35+0.15*(i-1)
            if (abs(Pm-pave).le.0.075) then
               ip = i
            endif
         enddo
         do i=0,21
            thave = 12+8*(i-1)
            if (abs(theta-thave).le.4) then
               ith = i
            endif
         enddo
         do i=1,60
            phiave= -27 + 6*(i-1)
            if (abs(phi-phiave).le.3) then
               iphi= i
            endif
         enddo

         if ((ip.ge.0).and.(ith.ge. 0).and.(iphi.gt.0)) then
            imeas = 1
            if (ith.gt.9) then
               ith  =  9
               imeas= -1
            else if (ith.eq.0) then
               ith  = 1
               imeas= -1
            endif
            if (Pm.gt.pmaxpr(ith)) then
               ip = ipmaxpr(ith)
               imeas= -1
            else if (Pm.lt.pminpr(ith)) then
               ip = ipminpr(ith)
               imeas= -1
            endif
            ibin = 60*9*(ip-1) + 60*(ith-1) + iphi
         endif

         if (ibin.le.0) then
            istat = 1
            return
         endif

         if (ifield.eq.1) then
            corr= cpr(ibin)
         else if (ifield.eq.2) then
            corr= cprh(ibin)
         endif

         if (corr.gt.-1.0) then
            Pc   = Pm*corr+Pm
            if (imeas.eq.1) then
               istat= 1
            else
               istat= 2
            endif
         else
            Pc   =  Pm
            istat=  2
         endif

      else
         write(*,*)'pcorpppm_tm: unknown particle id', idpart
         istat = -2

      endif

      return
      end
