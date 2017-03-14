c      include file TBTINF
c
c_begin_doc
c  RCS ID string
c  $Id: tbtinf.inc,v 1.4 1999/05/03 16:29:00 stepanya Exp $
c
c  Documentation for include file TBTINF
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
c  Author:   S.Stepanyan      Created:  Thu Apr 10 20:53:53 EDT 1997
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
c  variables for include file TBTINF:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CITBTINF
      DATA CITBTINF/
     1'$Id: tbtinf.inc,v 1.4 1999/05/03 16:29:00 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in TBTINF common block
c  --------------------------------------------
c
      LOGICAL tracking_tb
      INTEGER ntbt_trk
      INTEGER tbt_id(Maxlink)
      INTEGER tbt_sect(Maxlink)
      INTEGER tbt_lst(Maxlink)
      REAL tbt_c2(Maxlink)
      REAL tbt_p(Maxlink)
      REAL tbt_m(Maxlink)
      REAL tbt_q(Maxlink)
      REAL tbt_x(Maxplane,Maxlink)
      REAL tbt_y(Maxplane,Maxlink)
      REAL tbt_z(Maxplane,Maxlink)
      REAL tbt_cx(Maxplane,Maxlink)
      REAL tbt_cy(Maxplane,Maxlink)
      REAL tbt_cz(Maxplane,Maxlink)
      REAL tbt_r(Maxplane,Maxlink)
c_end_var
c
      COMMON/TBTINF/ntbt_trk,tbt_id,tbt_sect,tbt_lst, tbt_c2,tbt_p,tbt_m,tbt_q,
     +tbt_x,tbt_y,tbt_z,tbt_cx,tbt_cy,tbt_cz,tbt_r,tracking_tb 
c----6----------------------------------------------------------------72




