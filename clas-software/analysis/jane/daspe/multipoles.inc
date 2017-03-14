c      include file MULTIPOLES
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for include file MULTIPOLES
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
c  Author:         Created:  Wed Dec 11 16:56:50 EST 1996
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
c  variables for include file MULTIPOLES:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIMULTIPOLES
      DATA CIMULTIPOLES/
     1'$Id$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in MULTIPOLES common block
c  --------------------------------------------
c
c_end_var
c
       INTEGER MAX1res
       PARAMETER (MAX1res=13)
       INTEGER MAX3res
       PARAMETER (MAX3res=13)
       REAL AM3,AE3,AS3  ! 
       REAL AM1,AE1,AS1  ! 
       REAL EE3,EM3,ES3
       REAL EE1,EM1,ES1
       COMMON/MULTIPOLES/EE3(MAX3res),EM3(MAX3res),ES3(MAX3res),
     +      EE1(2,MAX1res),EM1(2,MAX1res),ES1(2,MAX1res),
     +      AM3(MAX3res),AE3(MAX3res),AS3(MAX3res),
     +      AM1(2,MAX1res),AE1(2,MAX1res),AS1(2,MAX1res)
c----6----------------------------------------------------------------72




