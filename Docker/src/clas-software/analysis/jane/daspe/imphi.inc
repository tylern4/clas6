c      include file IMPHI
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for include file IMPHI
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
c  Author:         Created:  Thu Dec 12 19:51:49 EST 1996
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
c  variables for include file IMPHI:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIIMPHI
      DATA CIIMPHI/
     1'$Id$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in IMPHI common block
c  --------------------------------------------
c
c_end_var
c
       REAL ph,PHisot
       COMMON/IMPHI/ph(6,3),PHisot(6,3,2)
c----6----------------------------------------------------------------72




