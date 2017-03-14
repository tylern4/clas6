c      include file SCGEOM
c
c_begin_doc
c  RCS ID string
c  $Id: scgeom.inc,v 1.2 1997/05/01 19:59:54 freyberg Exp $
c
c  Documentation for include file SCGEOM
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
c  Author:   S.Stepanyan      Created:  Thu Apr 10 23:14:04 EDT 1997
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
c  variables for include file SCGEOM:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CISCGEOM
      DATA CISCGEOM/
     1'$Id: scgeom.inc,v 1.2 1997/05/01 19:59:54 freyberg Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in SCGEOM common block
c  --------------------------------------------
      REAL sc_normal(3,4,Maxsector)
      REAL sc_rnormal(4,Maxsector)
c
c_end_var
c
      COMMON/SCGEOM/sc_normal,sc_rnormal
c----6----------------------------------------------------------------72




