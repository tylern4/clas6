c      include file ECPO_NT
c
c_begin_doc
c  RCS ID string
c  $Id: ecpo_nt.inc,v 1.1 1998/08/21 21:31:06 stepanya Exp $
c
c  Documentation for include file ECPO_NT
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
c  Author:         Created:  Fri Aug 14 00:42:29 EST 1996
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
c  variables for include file ECPO_NT:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIECPO_NT
      DATA CIECPO_NT/
     1'$Id: ecpo_nt.inc,v 1.1 1998/08/21 21:31:06 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in ECPO_NT common block
c  --------------------------------------------
c
c_end_var
c
      INTEGER  nhit 
      INTEGER  slh
      INTEGER  uid1
      INTEGER  vid1 
      INTEGER  wid1
      INTEGER  uid2
      INTEGER  vid2
      INTEGER  wid2
      INTEGER  mxhits
      PARAMETER (mxhits=30)
*
      COMMON/ECPO_NT/
     +nhit,
     +slh(mxhits),
     +uid1(mxhits),
     +uid2(mxhits),
     +vid1(mxhits),
     +vid2(mxhits),
     +wid1(mxhits),
     +wid2(mxhits)
c----6----------------------------------------------------------------72




