c      include file DELTA_DAT
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for include file DELTA_DAT
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
c  Author:         Created:  Mon Dec 16 19:21:20 EST 1996
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
c  variables for include file KINPAR:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIDELTA_DAT
      DATA CIDELTA_DAT/
     1'$Id$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in KINPAR common block
c  --------------------------------------------
c
c_end_var
c
      REAL wsa(144),mp33i(144)
      COMMON/DELTA_DAT/wsa,mp33i
c----6----------------------------------------------------------------72




