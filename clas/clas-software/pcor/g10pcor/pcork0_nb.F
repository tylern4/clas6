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
