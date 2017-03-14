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
