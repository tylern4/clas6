c      include file inpar
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for include file inpar
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
c  Author:         Created:  The March 16 17:01:48 EST 2000
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
      CHARACTER*(132)  CIINPAR
      DATA CIINPAR/
     1'$Id$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in GPAR common block
c  --------------------------------------------
c
c_end_var
c
       character*10 fitopt
       COMMON/inpar/fitopt
c----6----------------------------------------------------------------72




