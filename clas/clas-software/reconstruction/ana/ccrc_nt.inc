c      include file CCRC_NT
c
c_begin_doc
c  RCS ID string
c  $Id: ccrc_nt.inc,v 1.1 1997/06/11 04:46:17 stepanya Exp $
c
c  Documentation for include file CCRC_NT
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
c  Author:   S. Stepanyan      Created:  Sun Feb 16 13:12:51 EST 1997
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
c  variables for include file CCRC_NT:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CICCRC_NT
      DATA CICCRC_NT/
     1'$Id: ccrc_nt.inc,v 1.1 1997/06/11 04:46:17 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in CCRC_NT common block
c  --------------------------------------------
      INTEGER Maxcchit
      PARAMETER (Maxcchit=20)
c
      INTEGER ncchit
      INTEGER icchit(Maxcchit)
      INTEGER ccsect(Maxcchit)
      INTEGER ccsegm(Maxcchit)
      INTEGER ccphe(Maxcchit)
      REAL    cctime(Maxcchit)
      REAL    cctheta(Maxcchit)
      REAL    ccdtheta(Maxcchit)
      INTEGER ccphi(Maxcchit)
c_end_var
c
      COMMON/CCRC_NT/ncchit,icchit,ccsect,ccsegm,
     +ccphe,cctime,cctheta,ccphi
c----6----------------------------------------------------------------72




