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
      integer ifunc

      integer  pidpip, pidpim, pidkp, pidkm, pidpr
      parameter (pidpip= 211)
      parameter (pidpim=-211)
      parameter (pidkp = 321)
      parameter (pidkm =-321)
      parameter (pidpr =2212)


      ifunc= -1

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
