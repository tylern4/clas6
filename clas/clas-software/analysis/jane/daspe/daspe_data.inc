c      include file daspe_data
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for include file DASPE_DATA
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
c  Author:  S.Stepanyan      Created:  Wed Jun 23 17:25:11 EST 1999
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
c  variables for include file DASPE_DATA:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIDASPE_DATA
      DATA CIDASPE_DATA/
     1'$Id$'
     2/
      INTEGER MAXpoints, Ibad(2,100), Nbad
      PARAMETER ( MAXpoints = 100000 )
      CHARACTER*60 Datafile
      CHARACTER*10 datform
      INTEGER point_stat(MAXpoints)
      REAL Wd(MAXpoints), costhd(MAXpoints), Q2d(MAXpoints)
      REAL phi_str(MAXpoints),epsilond(MAXpoints)
      REAL cross_sec(MAXpoints),st_error(MAXpoints)
      integer pi_typed(MAXpoints), obsvd(MAXpoints)
      Common/daspe_data/Wd,Q2d,costhd,phi_str,epsilond,cross_sec,
     +st_error,Datafile,Ibad,Nbad,point_stat,datform, pi_typed, obsvd
c----6----------------------------------------------------------------72
c  
