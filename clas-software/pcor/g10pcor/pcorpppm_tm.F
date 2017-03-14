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
      
#include "pcorpppm_tm.PAR"

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
