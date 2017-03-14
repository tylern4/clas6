
      INTEGER MAXevents, MAXiter, Randomseed
      REAL Wmin,Wmax,Wstep
      REAL Q2min,Q2max,Q2step
      REAL cosmin,cosmax,cosstep
      REAL phimin,phimax,phistep
      REAL Ebeam, IntAccur
      INTEGER MaxQ2grid, MaxWgrid, Maxcosgrid, Maxphigrid
      PARAMETER (MaxQ2grid = 20)
      PARAMETER (MaxWgrid = 100)
      PARAMETER (Maxcosgrid = 20)
      PARAMETER (Maxphigrid = 30)
      REAL CS_TAB (MaxQ2grid,MaxWgrid,Maxcosgrid,Maxphigrid)
      CHARACTER*60 tabfile,inffile

      COMMON/janrtab/Ebeam,Wmin,Wmax,Wstep,Q2min,Q2max,Q2step,IntAccur,
     +cosmin,cosmax,cosstep,phimin,phimax,phistep,CS_TAB,tabfile,inffile




