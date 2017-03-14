c  bgrid.inc

c  $Id: bgrid.inc,v 1.5 2005/09/26 18:16:41 avakian Exp $

      character*132 cibgrid
      data cibgrid/
     1'$Id: bgrid.inc,v 1.5 2005/09/26 18:16:41 avakian Exp $'
     1/

c  magnetic field table calculated for toroidal magnet with six coils
c  (table is calculated in a box which cover only a half sector)
c                                 bogdan niczyporuk, march 1991

c  nx  ,ny  ,nz    - number of bins for each coordinate in dst lab system
c  xmin,ymin,zmin  - defines edges of the box in dst lab system
c  dx,  dy,  dz    - number of bins for each coordinate in dst lab system
c  c_max           - maximum current [A] in a loop 
c        default setting: c_max(torus) = 3860 [A], c_max(mini) = 8000 [A], 
c                         c_max(ptg) = 124 [A]
c
c  dtime           - unix time  
c  ngr = 3*nx*ny*nz 

      integer max_grid
      parameter (max_grid=653000)

      integer nx, ny, nz, ngr, c_max, dtime 
      real xmin, ymin, zmin, dx, dy, dz, qfield

      common/bgrid/
     1     nx,ny,nz,xmin,ymin,zmin,dx,dy,dz,c_max,dtime,ngr,qfield(max_grid)

C                                
C And corresponding "COMMON/poltg/" for the Polarized Target Magnetic Field
C
      integer max_grid_ptg
      parameter (max_grid_ptg=2000000)

      INTEGER nx_ptg,ny_ptg,nz_ptg,ngr_ptg, c_max_ptg,dtime_ptg
      REAL Qptg(max_grid_ptg), xmin_ptg,ymin_ptg,zmin_ptg,dx_ptg,dy_ptg,dz_ptg
c
      common/poltg/nx_ptg,ny_ptg,nz_ptg,xmin_ptg,ymin_ptg,zmin_ptg,
     1                                dx_ptg,dy_ptg,dz_ptg,
     2                                c_max_ptg,dtime_ptg,ngr_ptg,Qptg

      save  /bgrid/
      save  /poltg/
