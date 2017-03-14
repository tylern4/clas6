c      include file MC_NT
c
c_begin_doc
c  RCS ID string
c  $Id: mc_nt.inc,v 1.4 2009/03/14 17:31:56 fklein Exp $
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
     1'$Id: mc_nt.inc,v 1.4 2009/03/14 17:31:56 fklein Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in MC_NT common block
c  --------------------------------------------
c
c_end_var
c
      INTEGER max_mcpart
      PARAMETER (max_mcpart = 40)
      INTEGER mcnentr
      INTEGER mcnpart
      INTEGER mcst(max_mcpart)
      INTEGER mcid(max_mcpart)
      INTEGER mcpid(max_mcpart)
      REAL mctheta(max_mcpart)
      REAL mcphi(max_mcpart)
      REAL mcp(max_mcpart)
      REAL mcm(max_mcpart)
      REAL mcvx(max_mcpart),mcvy(max_mcpart),mcvz(max_mcpart)
      REAL mctof(max_mcpart)
      COMMON/MC_NT/mcnentr,mcnpart,
     +mcst,mcid,mcpid,mctheta,mcphi,mcp,mcm,mcvx,mcvy,mcvz,mctof
c----6----------------------------------------------------------------72




