c      include file ECHITS
c
c_begin_doc
c  RCS ID string
c  $Id: echits.inc,v 1.3 1997/06/06 14:55:02 stepanya Exp $
c
c  Documentation for include file ECHITS
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
c  Author:   S.Stepanyan      Created:  Thu Apr 10 21:44:31 EDT 1997
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
c  variables for include file ECHITS:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIECHITS
      DATA CIECHITS/
     1'$Id: echits.inc,v 1.3 1997/06/06 14:55:02 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in ECHITS common block
c  --------------------------------------------
      INTEGER Maxlayers
      PARAMETER (Maxlayers=3)
      INTEGER Maxechits
      PARAMETER (Maxechits=10)
      INTEGER energy, time
      PARAMETER (energy=1)
      PARAMETER (time=energy+1)
      INTEGER ec_i,ec_j,ec_di,ec_dj
      PARAMETER (ec_i=time+1)
      PARAMETER (ec_j=ec_i+1)
      PARAMETER (ec_di=ec_j+1)
      PARAMETER (ec_dj=ec_di+1)
      INTEGER ec_x,ec_y,ec_z
      PARAMETER (ec_x=ec_dj+1)
      PARAMETER (ec_y=ec_x+1)
      PARAMETER (ec_z=ec_y+1)
      INTEGER ec_dx,ec_dy,ec_dz,ec_i2,ec_j2,ec_i3,ec_j3
      PARAMETER (ec_dx=ec_z+1)
      PARAMETER (ec_dy=ec_dx+1)
      PARAMETER (ec_dz=ec_dy+1)
      PARAMETER (ec_i2=ec_dz+1)
      PARAMETER (ec_j2=ec_i2+1)
      PARAMETER (ec_i3=ec_j2+1)
      PARAMETER (ec_j3=ec_i3+1)
      INTEGER mom2, mom3, mom4, dist, path
      PARAMETER (dist=1)
      PARAMETER (mom2=dist+1)
      PARAMETER (mom3=mom2+1)
      PARAMETER (mom4=mom3+1)
      PARAMETER (path=mom4+1)
      INTEGER whole,inner,outer
      PARAMETER (whole=1)
      PARAMETER (inner=whole+1)
      PARAMETER (outer=inner+1)
      INTEGER ec_u,ec_v,ec_W
      PARAMETER (ec_u=1)
      PARAMETER (ec_v=ec_u+1)
      PARAMETER (ec_w=ec_v+1)
c
      INTEGER ecfit_nhit(maxlayers,maxsector)
      INTEGER ecfit_nhitU(maxechits,maxlayers,maxsector)
      INTEGER ecfit_nhitV(maxechits,maxlayers,maxsector)
      INTEGER ecfit_nhitW(maxechits,maxlayers,maxsector)
      INTEGER ec_matchid1(maxechits,maxlayers,maxsector)
      INTEGER ec_matchid2(maxechits,maxlayers,maxsector)
c
      REAL ecfit_hit(energy:ec_j3,maxechits,maxlayers,maxsector)
      REAL ec_peak(dist:path,maxechits,ec_u:ec_w,maxlayers,maxsector)
      REAL ec_c2match1(maxechits,maxlayers,maxsector)
      REAL ec_c2match2(maxechits,maxlayers,maxsector)
c_end_var
c
      COMMON/ECHITS/ecfit_nhit,ecfit_nhitU,ecfit_nhitV,ecfit_nhitW,
     +ec_matchid1,ec_matchid2,ecfit_hit,ec_peak,ec_c2match1,ec_c2match2
c----6----------------------------------------------------------------72




