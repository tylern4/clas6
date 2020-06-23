c      include file ECGMT
c
c_begin_doc
c  RCS ID string
c  $Id: ecgmt.inc,v 1.3 2002/12/20 02:20:01 claseg1 Exp $
c
c  Documentation for include file ECGMT
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
c  Author:     S.Stepanyan    Created:  Fri Apr 11 16:40:38 EDT 1997
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
c  variables for include file ECGMT:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIECGMT
      DATA CIECGMT/
     1'$Id: ecgmt.inc,v 1.3 2002/12/20 02:20:01 claseg1 Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in ECGMT common block
c  --------------------------------------------
c
      REAL EcBsc_r
      REAL n2sect(3,Maxsector)
      REAL EcBsc_d(whole:outer)
      REAL Ec_tDepth(whole:outer)
c_end_var
c
      COMMON/ECGMT/EcBsc_r,n2sect,EcBsc_d,Ec_tDepth
c----6----------------------------------------------------------------72




