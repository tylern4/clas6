c      include file HELICITY
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for include file HELICITY
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
c  Author:         Created:  Mon Dec 16 19:29:24 EST 1996
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
c  variables for include file HELICITY:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIHELICITY
      DATA CIHELICITY/
     1'$Id$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in HELICITY common block
c  --------------------------------------------
c
c_end_var
c
      REAL h1(3,2),h2(3,2),h3(3,2),h4(3,2),h5(3,2),h6(3,2)
       COMMON/HELICITY/h1,h2,h3,h4,h5,h6
c----6----------------------------------------------------------------72




