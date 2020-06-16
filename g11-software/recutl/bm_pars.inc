c      include file BM_PARS
c
c_begin_doc
c  RCS ID string
c  $Id: bm_pars.inc,v 1.3 1998/01/06 18:23:51 hovanes Exp $
c
c  Documentation for include file BM_PARS
c
c  Purpose:  holds beam current parameters
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:   Hovanes Egiyan        Created:  Wed Oct 02 10:11:57 EST 1997
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
c  variables for include file BM_PARS:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIBM_PARS
      DATA CIBM_PARS/
     1'$rcsId$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in BM_PARS common block
c  --------------------------------------------
c
c_end_var
c
C-
       integer ixMax
       parameter ( ixMax = 100 )

       real Q_TOT(ixMax), CURRENT(ixMax)
       real Q_TOT_LV(ixMax), TL_TU(ixMax) 
       integer iSpEvt, ix 
       logical Loop 
       
       integer CLOCK_UG_1, FCUP_UG_1, CLOCK_UG_2, FCUP_UG_2
       integer CLOCK_G1_1, FCUP_G1_1, CLOCK_G1_2, FCUP_G1_2
       integer CLOCK_G2_1, FCUP_G2_1, CLOCK_G2_2, FCUP_G2_2
       integer TRIG_SCALER_UG(12), TRIG_OR_UG
       integer MASTER_OR, PC, PS, TAC
C       
       COMMON/BM_PARS/ Q_TOT, Q_TOT_LV, TL_TU, CURRENT, 
     &                    iSpEvt, ix, Loop,      
     &        CLOCK_UG_1, FCUP_UG_1, CLOCK_UG_2, FCUP_UG_2,
     &	      CLOCK_G1_1, FCUP_G1_1, CLOCK_G1_2, FCUP_G1_2,
     &	      CLOCK_G2_1, FCUP_G2_1, CLOCK_G2_2, FCUP_G2_2,
     &	      TRIG_SCALER_UG, TRIG_OR_UG, MASTER_OR, PC, PS, TAC	
C-     
c----6----------------------------------------------------------------72




