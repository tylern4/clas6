c      include file MC_NT
c
c_begin_doc
c  RCS ID string
c  $Id: mc_nt.inc,v 1.1 2003/06/05 02:03:33 stepanya Exp $
c
c  Documentation for include file MC_NT
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
c  Author:   S.Stepanyan      Created:  Tue Mar  4 16:43:26 EST 1997
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
c  variables for include file MC_NT:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIMC_NT
      DATA CIMC_NT/
     1'$Id: mc_nt.inc,v 1.1 2003/06/05 02:03:33 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in MC_NT common block
c  --------------------------------------------
c
c_end_var
c
      INTEGER max_part
      PARAMETER (max_part = 20)
      INTEGER mcnentr
      INTEGER mcnpart
      INTEGER mcst
      INTEGER mcid
      INTEGER mcpid
      REAL mctheta
      REAL mcphi
      REAL mcp
      REAL mcm
      REAL mcvx_x_el,mcvx_y_el,mcvx_z_el
      
      COMMON/MC_NT/mcnentr,mcnpart,
     +mcst(max_part),mcid(max_part),mcpid(max_part),
     +mctheta(max_part),mcphi(max_part),mcp(max_part),mcm(max_part),
     +mcvx_x_el,mcvx_y_el,mcvx_z_el
c----6----------------------------------------------------------------72




