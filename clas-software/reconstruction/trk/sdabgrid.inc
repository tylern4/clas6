C Magnetic field Table calculated for toroidal magnet with six coils
C (Table is calculated in a box which cover only a half sector)
C                                 Bogdan Niczyporuk, March 1991
C
C    NX  ,NY  ,NZ  - Number of bins for each coordinate in DST Lab system
C  Xmin,Ymin,Zmin  - Defines edges of the box in DST Lab system
C    DX,  DY,  DZ  - Number of bins for each coordinate in DST Lab system
C        typ  = 1  - big  TORUS
C        typ  = 2  - mini TORUS                    
C        typ  = 3  - big TORUS + mini TORUS
C           dtime  - Unix Time                    
C
C Ngr = 3*NX*NY*NZ - Number of grids 
C
C the same for a second field map (e.g. pol. target field)
C
C  For details see routine "sda_minit.F"
C     Imax_t,m are the maximum currents in the torus and the mini-torus.
C
      Integer    Ng, Mg
      PARAMETER (Ng = 660000)
      PARAMETER (Mg = 2000000)
      Integer    Imax_t,Imax_m
      PARAMETER (Imax_t = 3860)
      PARAMETER (Imax_m = 8000)
C
      INTEGER NX, NY, NZ, Cmax, dtime, Ngr
      INTEGER mx,my,mz,mgr, Cpmax,dptime
      REAL Xmin, Ymin, Zmin, DX, DY, DZ, Q(Ng)
      REAL xpmin,ypmin,zpmin,ddx,ddy,ddz, Qm(Mg)
C
      COMMON/sdabgrid/ NX,NY,NZ,Xmin,Ymin,Zmin,DX,DY,DZ,
     &                 Cmax,dtime,Ngr,Q                        
      COMMON /poltg/ mx,my,mz,xpmin,ypmin,zpmin,ddx,ddy,ddz,
     1                                 Cpmax,dptime,mgr,Qm
      save  /sdabgrid/
      save  /poltg/
C


