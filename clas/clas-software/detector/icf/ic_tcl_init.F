      SUBROUTINE IC_TCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: ic_tcl_init.F,v 1.11 2008/11/13 15:55:23 rustam Exp $
c
c  Documentation for subroutine USER_TCL_INIT
c
c  Purpose:  routine for initializing IC tcl
c  --------
c
c  Input Parameters:  none
c  ----------------
c
c  Output Parameters:  none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Fri Dec  1 17:04:06 EST 1995
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "ictcl.inc"
#include "ic.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: ic_tcl_init.F,v 1.11 2008/11/13 15:55:23 rustam Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='IC_TCL_INIT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, i, ix, iy
      INTEGER ITCL_INT, ITCL_STRING, ITCL_INT_READ, LSTRING, ITCL_DOUBLE
      DATA ITCL_INT, ITCL_DOUBLE/1,2/
c_end_var
c
c  executable code for routine USER_TCL_INIT:
c----6----------------------------------------------------------------72
C     Set negative values in order to be able to set 0 thresholds later      
      ehitmind=-1.0
      egrplmind=-1.0
      egrpmind=-1.0
      z0d=-1000.0
      call tcl_linkvar('ehitmin',ehitmind,itcl_double)
      call tcl_linkvar('egrplmin',egrplmind,itcl_double)
      call tcl_linkvar('egrpmin',egrpmind,itcl_double)
      call tcl_linkvar('z0',z0d,itcl_double)
      ic_xpos_max=25.
      call tcl_linkvar('ic_xpos_max',ic_xpos_max,itcl_double)
      ic_ypos_max=25.
      call tcl_linkvar('ic_ypos_max',ic_ypos_max,itcl_double)

      RETURN
      END
c
c------------------------------------------------------------------------------




