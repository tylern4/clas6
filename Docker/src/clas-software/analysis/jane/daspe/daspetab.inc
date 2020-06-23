c      include file DASPETAB
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for include file DASPETAB
c
c  Purpose:
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author: S.Stepanyan  Created:  Wed Jun 23 17:25:11 EST 1999
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file DASPETAB:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIDASPETAB
      DATA CIDASPETAB/
     1'$Id$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in FIT_PAR common block
c  --------------------------------------------
c
c_end_var
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
c
      COMMON/daspetab/Ebeam,Wmin,Wmax,Wstep,Q2min,Q2max,Q2step,IntAccur,
     +cosmin,cosmax,cosstep,phimin,phimax,phistep,CS_TAB,tabfile,inffile
c----6----------------------------------------------------------------72




