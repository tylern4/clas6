c      include file SCRW_NT
c
c_begin_doc
c  RCS ID string
c  $Id: scrw_nt.inc,v 1.2 1997/09/10 16:22:13 stepanya Exp $
c
c  Documentation for include file SCRW_NT
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
c  Author: S.Stepanyan        Created:  Tue Feb 11 10:11:02 EST 1997
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
c  variables for include file SCRW_NT:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CISCRW_NT
      DATA CISCRW_NT/
     1'$Id: scrw_nt.inc,v 1.2 1997/09/10 16:22:13 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in SCRW_NT common block
c  --------------------------------------------
      INTEGER MAXschit
      PARAMETER (MAXschit = 10)
      INTEGER nschit
      INTEGER scsect
      INTEGER schid
      INTEGER scpid
      REAL sct
      REAL sce
      REAL scx
      REAL scy
      REAL scz
c
c_end_var
c
       COMMON/SCRW_NT/
     + nschit,
     + scsect(MAXschit),
     + schid(MAXschit),
     + scpid(MAXschit),
     + sct(MAXschit),
     + sce(MAXschit),
     + scx(MAXschit),
     + scy(MAXschit),
     + scz(MAXschit)
c----6----------------------------------------------------------------72




