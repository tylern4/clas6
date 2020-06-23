c      include file MULTIPOLE amplitudes
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
c----6----------------------------------------------------------------72
c  
c  Variables contained in MULTIPOLES common block
c  --------------------------------------------
c
c_end_var
c
       INTEGER MAXpoints
       PARAMETER (MAXpoints=50)
       REAL Wtab(MAXpoints)
       REAL AE01(MAXpoints),AE03(MAXpoints)   
       REAL AS01(MAXpoints),AS03(MAXpoints)   
       REAL AM13(MAXpoints),AS13(MAXpoints)  
       COMMON/MULTampl/Wtab,AE01,AE03,AS01,AS03,AM13,AS13
c----6----------------------------------------------------------------72




