c      include file MFM_GRID
c
c_begin_doc
c  RCS ID string
c  $Id: mfm_grid.inc,v 1.2 1999/03/13 03:51:45 kossov Exp $
c
c  Documentation for include file MFM_GRID
c
c  Purpose: Initialization of the grid of the Magnetic Field Modified
c  -------- 
c
c  Used in routines: MFM_BRUN, MFM_SNAKE, MFM_GETMF, MFM_GET27
c  ----------------
c
c  Notes: In respect to SDA both Q & Qm are included in the COMMON block
c  ------
c
c  Author:   Mikhail Kossov    Created:  Fri May 09 18:41:47 EDT 1997
c  -------
c
c  Major revisions:
C  ---------------
c
c_end_doc
c
c_begin_var
c  variables for include file MFM_GRID:
c  -------------------------------------
c  RCS information: 
      CHARACTER*(132)  CIMFM_GRID
      DATA CIMFM_GRID/
     1'$Id: mfm_grid.inc,v 1.2 1999/03/13 03:51:45 kossov Exp $'/
c----6----------------------------------------------------------------72
C
C    Old Field:
C    ==========
C   NXP ,NYP ,NZP   - Number of bins for each coordinate in DST Lab system
C  Xmip,Ymip,Zmip   - Defines edges of the box in DST Lab system
C   DXp, DYp, DZp   - Number of bins for each coordinate in DST Lab system
C           Cmaxp   - Maximum current [A] in a loop for big or mini Torus 
C             dtm   - Unix Time                    
C
C  Ngm=3*NXp*NYp*NZp- Number of grids in Old Field 
C
C    New Field:
C    ==========
C    NX  ,NY  ,NZ   - Number of bins for each coordinate in DST Lab system
C  Xmin,Ymin,Zmin   - Defines edges of the box in DST Lab system
C    DX,  DY,  DZ   - Number of bins for each coordinate in DST Lab system
C            Cmax   - Maximum current [A] in a loop for big or mini Torus 
C           dtime   - Unix Time                    
C
C  Ngr = 3*NX*NY*NZ - Number of grids in New Field
C
C  For details see routines: "mfm_brun.F", "mfm_snake.F", "mfm_get27.F"
C  --------------------------------------------------------------------
C
      INTEGER NX, NY, NZ, Cmax, dtime, Ngr, Ng, Ngm, Imax_t, Imax_m, Imax_p
      INTEGER NXP, NYP, NZP, Cmaxp, dtm
C      PARAMETER (Ng = 892296)
      PARAMETER (Ng = 1784592)
      PARAMETER (Imax_t = 3860, Imax_m = 8000, Imax_p = 438213)
      REAL Xmin, Ymin, Zmin, DX, DY, DZ, Q(Ng)
      REAL Xmip, Ymip, Zmip, DXp, DYp, DZp, Qm(Ng)
C
      COMMON/MFM_GRID/ NX,NY,NZ,Xmin,Ymin,Zmin,DX,DY,DZ,Cmax,dtime,Ngr,Q,
     &                 NXP,NYP,NZP,Xmip,Ymip,Zmip,DXp,DYp,DZp,Cmaxp,dtm,Ngm,Qm
      SAVE  /MFM_GRID/
C
