c      include file ADD_AMPL
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for include file ADD_AMPL
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
c  variables for include file ADD_AMPL:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIADD_AMPL
      DATA CIADD_AMPL/
     1'$Id$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in ADD_AMPL common block
c  --------------------------------------------
c
c_end_var
c
       REAL Aphi(6,2,2)
       COMMON/ADD_AMPL/Aphi
c----6----------------------------------------------------------------72




