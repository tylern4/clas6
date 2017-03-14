c      include file CROSS_SEC
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for include file CROSS_SEC
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
c  Author:         Created:  Mon Dec 16 23:08:39 EST 1996
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
c  variables for include file CROSS_SEC:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CICROSS_SEC
      DATA CICROSS_SEC/
     1'$Id$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in CROSS_SEC common block
c  --------------------------------------------
c
c_end_var
c
       REAL sigma(3),sigma_leg(8,2)
       REAL sigma_tt(3),sigma_tpl(3),sigma_tl(3),sigma_tlp(3)
       REAL sigmaTLP(2) 
	COMMON/CROSS_SEC/sigma,sigma_leg,sigma_tt,sigma_tpl,
     +sigma_tl,sigma_tlp,sigmaTLP
c----6----------------------------------------------------------------72




