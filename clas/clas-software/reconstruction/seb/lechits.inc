c      include file LECHITS
c
c_begin_doc
c  RCS ID string
c  $Id: lechits.inc,v 1.3 1999/07/24 21:52:04 stepanya Exp $
c
c  Documentation for include file LECHITS
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
c  Author:   S.Stepanyan      Created:  Fri Mar  6 16:10:57 EST 1998
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
c  variables for include file LECHITS:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CILECHITS
      DATA CILECHITS/
     1'$Id: lechits.inc,v 1.3 1999/07/24 21:52:04 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in LECHITS common block
c  --------------------------------------------
c
c_end_var
c
       INTEGER lec_nhit(maxlayers,maxsector)
       REAL lec_hit(energy:ec_i2,maxechits,maxlayers,maxsector)
       COMMON/LECHITS/lec_nhit,lec_hit
c----6----------------------------------------------------------------72




