c      include file GPAR
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for include file GPAR
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
c  Author:         Created:  Wed Dec 11 17:01:48 EST 1996
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
c  variables for include file GPAR:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIGPAR
      DATA CIGPAR/
     1'$Id$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in GPAR common block
c  --------------------------------------------
c
c_end_var
c
       REAL w2,E,Egamma,Epion,meta,m2pion
       REAL pi,a2,mn,mp,mn22,mp22,mn2,u,t,qk
       COMMON/GPAR/pi,a2,mn,mp,mn22,mp22,mn2,
     +      u,t,qk,w2,E,Egamma,Epion,meta,m2pion
c----6----------------------------------------------------------------72




